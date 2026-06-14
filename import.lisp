;; importing handling stuff. e.g importing of API docs
(in-package :hactar)

;;* registry
(defvar *import-sources* (make-hash-table :test 'equal)
  "Hash table mapping source names to import source definitions.")

(defstruct import-source
  "Represents an import source with pattern matching and handler."
  name           ; Source name (symbol)
  pattern        ; Regex pattern string
  param-names    ; List of parameter names to extract from regex groups
  priority       ; Integer priority (higher = checked first)
  handler)       ; Function that retrieves content (package version) -> (values content title)

(defun register-import-source (name pattern param-names priority handler)
  "Register an import source in the global import sources table."
  (setf (gethash name *import-sources*)
        (make-import-source :name name
                            :pattern pattern
                            :param-names param-names
                            :priority priority
                            :handler handler)))

(defun match-import-source (uri)
  "Try to match URI against all registered import sources, returning (values source params) or (values nil nil)."
  (let ((sorted-sources (sort (loop for source being the hash-values of *import-sources*
                                   collect source)
                             #'> :key #'import-source-priority)))
    (loop for source in sorted-sources
          do (multiple-value-bind (match groups)
                 (cl-ppcre:scan-to-strings (import-source-pattern source) uri)
               (when match
                 (let ((params (loop for param-name in (import-source-param-names source)
                                     for group across groups
                                     collect (cons param-name group))))
                   (return-from match-import-source (values source params)))))
          finally (return (values nil nil)))))
;;* core
(defmacro defsource (name &rest args)
  "Define an import source with pattern matching."
  (let ((pattern nil)
        (params nil)
        (priority 10)
        (body nil)
        (state nil))
    ;; Parse args
    (loop for arg in args
          do (cond
               ((eq state :pattern)
                (setf pattern arg)
                (setf state nil))
               ((eq state :params)
                (setf params arg)
                (setf state nil))
               ((eq state :priority)
                (setf priority arg)
                (setf state nil))
               ((eq arg :pattern)
                (setf state :pattern))
               ((eq arg :params)
                (setf state :params))
               ((eq arg :priority)
                (setf state :priority))
               (t (push arg body))))
    (setf body (nreverse body))
    (let ((handler-lambda (if (and (= (length body) 1)
                                   (consp (first body))
                                   (eq (car (first body)) 'lambda))
                              (first body)
                              `(lambda ,params
                                 ,@body))))
      `(progn
         (register-import-source ',name
                                 ,pattern
                                 ',params
                                 ,priority
                                 ,handler-lambda)
         ',name))))

(defmacro defdocsource (&key name version platform uri)
  "Define a custom documentation route for a package."
  (declare (ignore platform))
  (let* ((version-pattern (if (str:ends-with-p "^" version)
                              (str:substring 0 -1 version)
                            version))
         (is-prefix-match (str:ends-with-p "^" version))
         (quoted-name (cl-ppcre:quote-meta-chars name))
         (quoted-version (cl-ppcre:quote-meta-chars version-pattern))
         (pattern (if is-prefix-match
                       (format nil "npm:(~A)@(~A.*)" quoted-name quoted-version)
                     (format nil "npm:(~A)@(~A)$" quoted-name quoted-version))))
    `(progn
       (push (lambda ()
               (unless (loop for r being the hash-values of *routes*
                             thereis (string= (route-pattern r) ,pattern))
                 (defroute ,pattern (package version)
 			   :params (:package :version)
 			   :priority 15
 			   (declare (ignore package version))
 			   (fetch-url-content ,uri))))
             *route-reinit-hooks*)
       (defroute ,pattern (package version)
 		 :params (:package :version)
 		 :priority 15
 		 (declare (ignore package version))
 		 (fetch-url-content ,uri)))))

;;; Extensible Format Parsing Registry
(defvar *import-formats* (make-hash-table :test 'equal)
  "Hash table mapping file extensions to parsing functions.")

(defun register-import-format (extension handler-fn)
  "Register a format parser for EXTENSION."
  (setf (gethash (string-downcase (string extension)) *import-formats*) handler-fn))

(defmacro defformat-importer (extension args &body body)
  "Define a parser for EXTENSION."
  `(register-import-format ,(string-downcase (string extension))
                           (lambda ,args ,@body)))

;; Default format importers
(defformat-importer "md" (content title path)
  (declare (ignore path))
  (list (list :title title :content content :tags '("markdown"))))

(defformat-importer "markdown" (content title path)
  (declare (ignore path))
  (list (list :title title :content content :tags '("markdown"))))

(defformat-importer "org" (content title path)
  (declare (ignore path))
  (list (list :title title :content content :tags '("org-mode"))))

(defformat-importer "json" (content title path)
  (declare (ignore path))
  (handler-case
      (let ((parsed (cl-json:decode-json-from-string content)))
        (cond
          ((and (listp parsed) (consp (car parsed)) (cdr (assoc :content (car parsed))))
           (loop for item in parsed
                 collect (list :title (or (cdr (assoc :title item)) title)
                               :content (cdr (assoc :content item))
                               :tags (cdr (assoc :tags item))
                               :covers (cdr (assoc :covers item))
                               :metadata item)))
          ((and (listp parsed) (cdr (assoc :content parsed)))
           (list (list :title (or (cdr (assoc :title parsed)) title)
                       :content (cdr (assoc :content parsed))
                       :tags (cdr (assoc :tags parsed))
                       :covers (cdr (assoc :covers parsed))
                       :metadata parsed)))
          (t
           (list (list :title title :content content :tags '("json"))))))
    (error ()
      (list (list :title title :content content :tags '("json"))))))

(defformat-importer "txt" (content title path)
  (declare (ignore path))
  (list (list :title title :content content :tags '("text"))))

(defun get-format-from-uri (uri)
  "Gets the file extension/format from a URI."
  (let ((dot-pos (position #\. uri :from-end t)))
    (if dot-pos
        (let ((ext (subseq uri (1+ dot-pos))))
          (if (position #\/ ext)
              "md"
              ext))
        "md")))

(defun parse-import-file (content title path)
  "Parse content using the registered format importer for path's extension."
  (let* ((ext (get-format-from-uri (namestring path)))
         (handler (gethash (string-downcase ext) *import-formats*)))
    (if handler
        (funcall handler content title path)
        (list (list :title title :content content)))))

;;; Helper function to clone a Git repository to a temp directory
(defun git-clone-to-temp (repo-url)
  "Clones a git repository to a temporary directory under the workspace."
  (let* ((temp-dir (merge-pathnames (format nil "tmp-git-~A/" (uuid:make-v4-uuid))
                                    (or *repo-root* (uiop:getcwd)))))
    (ensure-directories-exist temp-dir)
    (let ((cmd (format nil "git clone --depth 1 ~A ~A" repo-url (uiop:native-namestring temp-dir))))
      (format t "Cloning ~A ...~%" repo-url)
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program cmd :output :string :error-output :string :ignore-error-status t)
        (declare (ignore output error-output))
        (if (= exit-code 0)
            temp-dir
            (progn
              (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)
              nil))))))

(defun import-directory (dir &key tags covers meta)
  "Recursively import all markdown/org/json files in DIR."
  (let ((imported-files '()))
    (uiop:collect-sub*directories
     dir
     (constantly t)
     (constantly t)
     (lambda (subdir)
       (dolist (file (uiop:directory-files subdir))
         (let ((ext (pathname-type file)))
           (when (and ext (member ext '("md" "markdown" "org" "json" "txt") :test #'string-equal))
             (let* ((content (uiop:read-file-string file))
                    (title (pathname-name file))
                    (rel-path (uiop:enough-pathname file dir))
                    (doc-uri (format nil "git-file:~A" (uiop:native-namestring rel-path)))
                    (parsed-docs (parse-import-file content title file)))
               (dolist (doc parsed-docs)
                 (let ((final-tags (remove-duplicates (append tags (getf doc :tags)) :test #'string=))
                       (final-covers (or covers (getf doc :covers) (list title)))
                       (final-meta (append meta (getf doc :metadata))))
                   (let ((out-file (%import-emit-and-load doc-uri
                                                          (getf doc :title)
                                                          (getf doc :content)
                                                          :tags final-tags
                                                          :covers final-covers
                                                          :meta final-meta)))
                     (when out-file
                       (push out-file imported-files)))))))))))
    (nreverse imported-files)))

(defun generate-doc-metadata (content title source-uri)
  "Ask LLM to generate metadata (tags, summary) for imported content."
  (let* ((model-config (find-model-by-name *docs-meta-model*))
         (prompt (format nil "Analyze this documentation and generate metadata in JSON format.

Documentation Title: ~A
Source: ~A

Content:
~A

Return a JSON object with this exact structure.
{
  \"tags\": [\"tag1\", \"tag2\", \"tag3\"],
  \"summary\": \"A brief one-line summary\"
}

Tags should be relevant keywords/topics in lowercase. Summary should be one concise sentence. Include any extra fields you like."
                        title source-uri content)))
    (unless model-config
      (warn "Model ~A not found for doc metadata generation. Falling back to current model." *docs-meta-model*)
      (setf model-config *current-model*))

    (when model-config
      (handler-case
          (let* ((provider-type (intern (string-upcase (model-config-provider model-config)) :keyword))
                 (response-text (llm:complete provider-type
                                              `(((:role . "user") (:content . ,prompt)))
                                              :model (model-config-model-name model-config)
                                              :max-tokens 512
                                              :system-prompt "You are a metadata generator. Return only valid JSON."
                                              :response-format "json_object"
                                              :stream nil)))
            (when response-text
              (let* ((json-obj (cl-json:decode-json-from-string response-text))
                     (tags-array (cdr (assoc :tags json-obj)))
                     (summary (cdr (assoc :summary json-obj)))
                     (tags-list (when tags-array
                                  (if (vectorp tags-array)
                                      (coerce tags-array 'list)
                                      tags-array)))
                     (extra-metadata (loop for (key . value) in json-obj
                                          unless (member key '(:tags :summary))
                                          collect (cons key value))))
                (values tags-list summary extra-metadata))))
        (error (e)
          (warn "Error generating doc metadata: ~A" e)
          (values nil nil nil))))))

(defun fetch-import-content (uri)
  "Resolve URI to content, title, and metadata using registered import sources."
  (multiple-value-bind (source params)
      (match-import-source uri)
    (cond
      (source
       (let ((param-values (mapcar #'cdr params)))
         (apply (import-source-handler source) param-values)))
      (t
       (let ((path (or (probe-file uri)
                       (and *repo-root* (probe-file (merge-pathnames uri *repo-root*))))))
         (if path
             (values (uiop:read-file-string path)
                     (file-namestring path)
                     nil)
             (error "No import source matches URI: ~A" uri)))))))

(defun %import-emit-and-load (uri title content &key tags covers meta id related)
  "Write a (defdoc ...) lisp file for an import under hactar/docs/ and load it."
  (let* ((import-dir (uiop:ensure-directory-pathname
                      (or *docs-import-path* "./hactar/docs/")))
         (resolved-dir (if (uiop:absolute-pathname-p import-dir)
                           import-dir
                           (merge-pathnames import-dir (or *repo-root* (uiop:getcwd)))))
         (out-file (uiop:native-namestring
                    (merge-pathnames
                     (format nil "~A.lisp"
                             (cl-ppcre:regex-replace-all "[^A-Za-z0-9_.-]" uri "_"))
                     resolved-dir)))
         (covers-list (or (coerce (uiop:ensure-list covers) 'list) (list title)))
         (tags-list (coerce (uiop:ensure-list tags) 'list))
         (form `(defdoc ,title
                    :source ,(concatenate 'string "raw:" content)
                    :uri ,uri
                    :tags ',tags-list
                    :covers ',covers-list
                    :meta ',meta
                    ,@(when id `(:id ,id))
                    ,@(when related `(:related ',related)))))
    (ensure-directories-exist out-file)
    (with-open-file (s out-file :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
      (format s ";;; Imported from ~A~%(in-package :hactar)~%~%" uri)
      (let ((*print-case* :downcase)
            (*print-pretty* t)
            (*print-right-margin* 10000)
            (*print-circle* nil)
            (*print-readably* nil)
            (*package* (find-package :hactar)))
        (prin1 form s))
      (terpri s))
    (load out-file)
    out-file))

(defun execute-import (uri &key tags covers meta title source id related)
  "Execute an import for the given URI. Returns the emitted file paths."
  (handler-case
      (multiple-value-bind (source-obj params)
          (match-import-source uri)
        (cond
          ((and source-obj (eq (import-source-name source-obj) 'git-repo-source))
           (let* ((param-values (mapcar #'cdr params))
                  (repo-url (or (first param-values) (second param-values)))
                  (temp-dir (git-clone-to-temp repo-url)))
             (if temp-dir
                 (unwind-protect
                      (let ((files (import-directory temp-dir :tags tags :covers covers :meta meta)))
                        (format t "Successfully imported ~A docs from Git repository ~A~%" (length files) repo-url)
                        files)
                   (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore))
                 (error "Failed to clone repository: ~A" repo-url))))
          (t
           (multiple-value-bind (fetched-content fetched-title source-meta)
               (if source
                   (values source nil nil)
                   (fetch-import-content uri))
             (let ((content (or source fetched-content))
                   (display-title (or title fetched-title (format nil "Import from ~A" uri))))
               (if content
                   (let ((parsed-docs (parse-import-file content display-title uri))
                         (created-files '()))
                     (dolist (doc parsed-docs)
                       (let* ((doc-title (or title (getf doc :title) display-title))
                              (doc-content (or source (getf doc :content)))
                              (doc-tags (getf doc :tags))
                              (doc-covers (getf doc :covers))
                              (doc-meta (getf doc :metadata))
                              (final-tags (remove-duplicates (append tags doc-tags (cdr (assoc :tags source-meta))) :test #'string=))
                              (final-covers (or covers doc-covers (list doc-title)))
                              (final-meta (append meta doc-meta source-meta)))
                         (let ((out-file (%import-emit-and-load uri doc-title doc-content
                                                                :tags final-tags :covers final-covers :meta final-meta
                                                                :id id :related related)))
                           (when out-file
                             (push out-file created-files)))))
                     (let ((result-files (nreverse created-files)))
                       (when result-files
                         (format t "Successfully imported from ~A~%" uri)
                         (dolist (f result-files)
                           (format t "  Emitted and loaded: ~A~%" f)))
                       result-files))
                   (error "No content retrieved from ~A" uri)))))))
    (error (e)
      (format t "Error importing from ~A: ~A~%" uri e)
      nil)))

(defun import->lisp (uri out-file &key tags covers meta)
  "Fetch URI content and emit a (defdoc ...) form to OUT-FILE."
  (multiple-value-bind (content title source-meta)
      (fetch-import-content uri)
    (let* ((title (or title (format nil "Import from ~A" uri)))
           (final-meta (or meta source-meta))
           (tags-list (coerce (uiop:ensure-list tags) 'list))
           (covers-list (or (coerce (uiop:ensure-list covers) 'list)
                            (list title)))
           (form `(defdoc ,title
                      :source ,(concatenate 'string "raw:" content)
                      :uri ,uri
                      :tags ',tags-list
                      :covers ',covers-list
                      :meta ',final-meta)))
      (ensure-directories-exist out-file)
      (with-open-file (s out-file :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
        (format s ";;; Imported from ~A~%(in-package :hactar)~%~%" uri)
        (let ((*print-case* :downcase))
          (prin1 form s))
        (terpri s))
      out-file)))

;;* Sources (builtin)

(defsource npm-source
  :pattern "^npm:([^@]+)@(.+)$"
  :params (package-name version)
  :priority 10
  (lambda (package-name version)
    (let ((content (get-npm-docs package-name version)))
      (values content (format nil "~A@~A" package-name version)))))

(defsource file-source
  :pattern "^file:(.+)$"
  :params (filepath)
  :priority 10
  (lambda (filepath)
    (let* ((path (uiop:native-namestring (merge-pathnames filepath *repo-root*))))
      (if (probe-file path)
          (values (uiop:read-file-string path)
                  (file-namestring path))
          (error "File not found: ~A" path)))))

(defsource hactar-source
  :pattern "^hactar:(.+)$"
  :params (filepath)
  :priority 10
  (lambda (filepath)
    (let* ((path (uiop:native-namestring (merge-pathnames filepath *hactar-data-path*))))
      (if (probe-file path)
          (values (uiop:read-file-string path)
                  (file-namestring path))
          (error "Hactar file not found: ~A" path)))))

(defsource http-source
  :pattern "^https?://(.+)$"
  :params (url-path)
  :priority 5
  (lambda (url-path)
    (let* ((url (format nil "http~A://~A"
                       (if (search "https" url-path) "s" "")
                       url-path))
           (content (fetch-url-content url)))
      (if content
          (values content (format nil "Web: ~A" url))
          (error "Failed to fetch URL: ~A" url)))))

(defsource github-repo-source
  :pattern "^https?://github\\.com/([^/]+)/([^/]+)/?$"
  :params (user repo)
  :priority 15
  (lambda (user repo)
    (let* ((url (format nil "https://github.com/~A/~A" user repo))
           (readme-url (get-github-raw-url url)))
      (if readme-url
          (let ((content (fetch-url-content readme-url)))
            (values content (format nil "~A/~A README" user repo)))
          (error "Could not find README for GitHub repo: ~A/~A" user repo)))))

(defsource git-repo-source
  :pattern "^(?:git:(.+)|(https?://.+\\.git))$"
  :params (git-prefix-url https-git-url)
  :priority 12
  (lambda (git-prefix-url https-git-url)
    (let ((repo-url (or git-prefix-url https-git-url)))
      (values nil repo-url))))

;; Export built-in import source names and their param symbols so tests
;; that reference them from another package see the same symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(npm-source file-source hactar-source http-source github-repo-source git-repo-source
            package-name version filepath url-path user repo git-prefix-url https-git-url)))

;;* Commands
(defun %docs-import-common (args &key extra-tags usage)
  "Shared logic for docs import commands."
  (if (null args)
      (format t "~A~%" (or usage "Usage: <command> <uri> -tags=tag1,tag2 -covers=cover1"))
      (let* ((uri-arg (first args))
             (metadata-args (rest args))
	     (parsed-metadata (parse-cli-args-s (join-strings " " metadata-args)))
             (tags (getf parsed-metadata :tags))
             (covers (getf parsed-metadata :covers))
             (meta (getf parsed-metadata :meta))
             (title (getf parsed-metadata :title))
             (source (getf parsed-metadata :source))
             (id (getf parsed-metadata :id))
             (related (getf parsed-metadata :related))
             (provided-tags-list (cond
                                    ((vectorp tags) (coerce tags 'list))
                                    ((listp tags) tags)
                                    ((null tags) '())
                                    (t (list tags))))
             (final-tags (remove-duplicates (append provided-tags-list (or extra-tags '())) :test #'string=)))
        (execute-import uri-arg
                       :tags final-tags
                       :covers covers
                       :meta meta
                       :title title
                       :source source
                       :id id
                       :related related))))

(define-command import (args)
  "Import documentation from various sources (npm, file, http, github, etc.).
   Usage: /import <uri> -tags=tag1,tag2 -covers=cover1

   Examples:
     /import npm:react@19.2
     /import file:~/docs/REACT.md
     /import https://website.xyz/a-page
     /import https://github.com/user/repo"
  (%docs-import-common args :extra-tags nil
                           :usage "Usage: /import <uri> -tags=tag1,tag2 -covers=cover1")
  :sub t)

(define-command docs.import (args)
  "Import a text file into the documentation.
   Usage: /docs.import <uri> -tags=tag1,tag2 -covers=cover1"
  (%docs-import-common args :extra-tags nil
			    :usage "Usage: /docs.import <uri> -tags=tag1,tag2 -covers=cover1")
  :sub t)

(define-command import.docs (args)
  "Import documentation (alias of /docs.import).
   Usage: /import.docs <uri> -tags=tag1,tag2 -covers=cover1"
  (%docs-import-common args :extra-tags nil
                           :usage "Usage: /import.docs <uri> -tags=tag1,tag2 -covers=cover1")
  :sub t)

(define-command import.starter (args)
  "Import a starter document and automatically tag it with 'starter'.
   Usage: /import.starter <uri> -tags=tag1,tag2 -covers=cover1"
  (%docs-import-common args :extra-tags '("starter")
                           :usage "Usage: /import.starter <uri> -tags=tag1,tag2 -covers=cover1")
  :sub t)

(define-command import.lisp (args)
  "Import a URI by emitting a (defdoc ...) lisp file and loading it.
   Usage: /import.lisp <uri> [out-file]"
  (if (null args)
      (format t "Usage: /import.lisp <uri> [out-file]~%")
      (let* ((uri (first args))
             (import-dir (uiop:ensure-directory-pathname
                          (or *docs-import-path* "./hactar/docs/")))
             (resolved-dir (if (uiop:absolute-pathname-p import-dir)
                               import-dir
                               (merge-pathnames import-dir (or *repo-root* (uiop:getcwd)))))
             (out-file (or (second args)
                           (uiop:native-namestring
                            (merge-pathnames
                             (format nil "~A.lisp"
                                     (cl-ppcre:regex-replace-all "[^A-Za-z0-9_.-]" uri "_"))
                             resolved-dir)))))
        (handler-case
            (let ((file (import->lisp uri out-file)))
              (load file)
              (format t "Imported ~A -> ~A (emitted defdoc lisp and loaded)~%" uri file))
          (error (e)
            (format t "Import failed for ~A: ~A~%" uri e)))))
  :sub t)
