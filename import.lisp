;; importing handling stuff. e.g importing of API docs
(in-package :hactar)

;;; Import Source Registry
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

(defmacro defsource (name &rest args)
  "Define an import source with pattern matching.
   
   NAME: Source name (symbol)
   :PATTERN: A regex pattern string with capture groups
   :PARAMS: List of parameter names corresponding to regex capture groups
   :PRIORITY: Integer priority for matching (default 10, higher = checked first)
   BODY: Code that retrieves content, should return (values content title)
   
   Example:
   (defsource npm-source
     :pattern \"^npm:([^@]+)@(.+)$\"
     :params (:package-name :version)
     :priority 10
     (lambda (package-name version)
       (values (get-npm-docs package-name version)
               (format nil \"~A@~A\" package-name version))))"
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
                              `(lambda ,(or params '())
                                 ,@body))))
      `(progn
         (register-import-source ',name
                                ,pattern
                                ',params
                                ,priority
                                ,handler-lambda)
         ',name))))

(defmacro defdocsource (&key name version platform uri)
  "Define a custom documentation route for a package.
   NAME: Package name (string)
   VERSION: Version pattern (string, can use ^ for prefix match)
   PLATFORM: Platform name (currently only 'npm' supported)
   URI: Documentation URI (file path or URL)

   Adds capturing groups so that params are bound (tests expect non-nil params)."
  (declare (ignore platform))		; For now, assume npm
  (let* ((version-pattern (if (str:ends-with-p "^" version)
                              (str:substring 0 -1 version)
                            version))
         (is-prefix-match (str:ends-with-p "^" version))
         ;; Quote meta characters so dots are literal, then build capturing groups.
         (quoted-name (cl-ppcre:quote-meta-chars name))
         (quoted-version (cl-ppcre:quote-meta-chars version-pattern))
         (pattern (if is-prefix-match
                      (format nil "npm:(~A)@(~A.*)" quoted-name quoted-version)
                    (format nil "npm:(~A)@(~A)$" quoted-name quoted-version))))
    `(progn
       ;; Register a hook so routes can be re-created if *routes* is cleared (e.g., by tests).
       ;; Hook is idempotent: only (re)defines route if pattern absent.
       (push (lambda ()
               (unless (loop for r being the hash-values of *routes*
                             thereis (string= (route-pattern r) ,pattern))
                 (defroute ,pattern (package version)
			   :params (:package :version)
			   :priority 15
			   (declare (ignore package version))
			   (fetch-url-content ,uri))))
             *route-reinit-hooks*)
       ;; Define the actual route now.
       (defroute ,pattern (package version)
		 :params (:package :version)
		 :priority 15		; Higher priority than default routes
		 (declare (ignore package version))
		 (fetch-url-content ,uri)))))

(defun generate-doc-metadata (content title source-uri)
  "Ask LLM to generate metadata (tags, summary) for imported content using JSON response format.
   Returns three values: tags-list, summary, and extra-metadata (alist of additional fields)."
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
                     ;; Extract any additional metadata fields (excluding tags and summary)
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
    (if source
        (let ((param-values (mapcar #'cdr params)))
          (apply (import-source-handler source) param-values))
        (error "No import source matches URI: ~A" uri))))

(defun execute-import (uri &key tags covers meta)
  "Execute an import for the given URI.
   Returns the created document ID(s) or NIL on failure."
  (handler-case
      (multiple-value-bind (content title source-meta)
          (fetch-import-content uri)
        (setf title (or title (format nil "Import from ~A" uri)))

        (if content
            (progn
              ;; Generate metadata if not provided and no source metadata
              (unless tags
                (if source-meta
                    (setf tags (cdr (assoc :tags source-meta)))
                    (multiple-value-bind (generated-tags summary extra-meta)
                        (generate-doc-metadata content title uri)
                      (setf tags generated-tags)
                      (unless meta
                        ;; Merge summary with any extra metadata from LLM
                        (setf meta (append `((:summary . ,summary)) extra-meta))))))
              
              ;; Use source metadata if available and no CLI metadata provided
              (when (and source-meta (not meta))
                (setf meta source-meta))

              ;; Create the document
              (let ((doc-ids (docs-create :source uri
                                          :title title
                                          :content content
                                          :tags tags
                                          :covers covers
                                          :meta meta)))
                (when doc-ids
                  (format t "Successfully imported from ~A~%" uri)
                  (format t "  Created doc(s) with ID(s): ~A~%" doc-ids)
                  (format t "  Title: ~A~%" title)
                  (when tags
                    (format t "  Tags: ~{~A~^, ~}~%" tags)))
                doc-ids))
            (progn
              (format t "Error: No content retrieved from ~A~%" uri)
              nil)))
    (error (e)
      (format t "Error importing from ~A: ~A~%" uri e)
      nil)))

;;; Built-in Import Sources

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

;; Export built-in import source names and their param symbols so tests
;; that reference them from another package see the same symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(npm-source file-source hactar-source http-source github-repo-source
            package-name version filepath url-path user repo)))

;;; Commands

(defun %docs-import-common (args &key extra-tags usage)
  "Shared logic for docs import commands.
ARGS is the list of CLI args (strings).
EXTRA-TAGS is a list of strings to append to provided :tags metadata.
USAGE is the usage string to print when args are missing."
  (if (null args)
      (format t "~A~%" (or usage "Usage: <command> <uri> -tags=tag1,tag2 -covers=cover1"))
      (let* ((uri-arg (first args))
             (metadata-args (rest args))
	     (parsed-metadata (parse-cli-args-s  (join-strings " " metadata-args)))
             (tags (getf parsed-metadata :tags #()))
             (covers (getf parsed-metadata :covers #()))
             (meta (getf parsed-metadata :meta nil))
             (provided-tags-list (cond
                                   ((vectorp tags) (coerce tags 'list))
                                   ((listp tags) tags)
                                   ((null tags) '())
                                   (t (list tags))))
             (final-tags (remove-duplicates (append provided-tags-list (or extra-tags '())) :test #'string=)))
        (execute-import uri-arg 
                       :tags final-tags
                       :covers covers
                       :meta meta))))

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

(define-command docs-import (args)
  "Import a text file into the documentation database.
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
