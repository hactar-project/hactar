;; hypertext is knowledge layer used for docs, errors, api docs, starters etc
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defhypertext defapidoc
            register-protocol-route
            resolve-hypertext-uri
            load-hypertext-files
            hypertext-to-alist
            hypertext-to-error-alist)))

;;* Registry
(defstruct protocol-dispatcher
  name
  (registry (make-hash-table :test 'equal))
  dynamic-routes)

(defvar *protocol-dispatchers* (make-hash-table :test 'equal)
  "Global registry of protocol dispatchers, keyed by downcased protocol name.")

(defun get-or-create-dispatcher (protocol)
  "Get or create a protocol dispatcher."
  (let ((proto (string-downcase (string protocol))))
    (or (gethash proto *protocol-dispatchers*)
        (setf (gethash proto *protocol-dispatchers*)
              (make-protocol-dispatcher :name proto)))))

(eval-when (:load-toplevel :execute)
  (get-or-create-dispatcher "docs")
  (get-or-create-dispatcher "apidoc")
  (get-or-create-dispatcher "errors")
  (get-or-create-dispatcher "guide")
  (get-or-create-dispatcher "wiki"))

(defun parse-uri-protocol (uri-string)
  "Parse a URI like 'docs:js/react/latest' into (values protocol path)."
  (let* ((uri (string-trim '(#\Space #\Tab #\Newline #\Return) uri-string))
         (colon-pos (position #\: uri)))
    (if colon-pos
        (values (subseq uri 0 colon-pos) (subseq uri (1+ colon-pos)))
        (values "docs" uri))))

(defun register-protocol-route (protocol pattern handler-fn)
  "Register a dynamic route pattern for a specific protocol."
  (let ((dispatcher (get-or-create-dispatcher protocol)))
    (setf (protocol-dispatcher-dynamic-routes dispatcher)
          (append (remove pattern (protocol-dispatcher-dynamic-routes dispatcher) :key #'car :test #'string=)
                  (list (cons pattern handler-fn))))))

;;* Helpers
(defun slugify (text)
  "Convert text to a lowercase, url-friendly slug."
  (let* ((lowercased (string-downcase text))
         (clean (cl-ppcre:regex-replace-all "[^a-z0-9/]+" lowercased "-")))
    (string-trim '(#\-) clean)))

(defun strip-markdown (markdown-text)
  "Remove basic markdown headings, code block tags and bold styles."
  (let ((text markdown-text))
    (setf text (cl-ppcre:regex-replace-all "^#+\\s+" text ""))
    (setf text (cl-ppcre:regex-replace-all "```[a-zA-Z0-9]*" text ""))
    (setf text (cl-ppcre:regex-replace-all "\\*\\*" text ""))
    text))

(defun parse-markdown-headings (content)
  "Simple line-by-line parser to split content by headings."
  (let ((lines (str:lines content))
        (current-title nil)
        (current-lines '())
        (sections '()))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
        (cond
         ((or (str:starts-with? "## " trimmed)
              (str:starts-with? "### " trimmed))
          (let ((new-title (string-trim '(#\Space #\Tab) (subseq trimmed (1+ (position #\# trimmed :from-end t))))))
            (when current-title
              (push (list :title current-title :content (str:join #\Newline (nreverse current-lines))) sections)
              (setf current-lines '()))
            (setf current-title new-title)))
         (t
          (when current-title
            (push line current-lines))))))
    (when current-title
      (push (list :title current-title :content (str:join #\Newline (nreverse current-lines))) sections))
    (nreverse sections)))

(defun hypertext-to-alist (doc)
  "Convert a hypertext plist document to a context-compatible alist."
  (when doc
    (let* ((uri (getf doc :uri))
           (protocol (getf doc :protocol))
           (path (getf doc :path))
           (title (getf doc :title))
           (content (getf doc :content))
           (tags (getf doc :tags))
           (covers (getf doc :covers))
           (metadata (getf doc :metadata))
           (id (or (cdr (assoc :id metadata))
                   (cdr (assoc :doc-id metadata))
                   (format nil "ht-~A" (uuid:make-v4-uuid))))
           (source (or (cdr (assoc :source metadata)) uri)))
      (list (cons :id id)
            (cons :uri uri)
            (cons :title title)
            (cons :content content)
            (cons :tags tags)
            (cons :covers covers)
            (cons :source source)
            (cons :protocol protocol)
            (cons :path path)
            (cons :meta metadata)))))

(defun hypertext-to-error-alist (doc)
  "Convert an error hypertext plist document to an error alist."
  (when doc
    (let* ((metadata (getf doc :metadata))
           (code (or (cdr (assoc :code metadata)) (getf doc :path)))
           (stack (or (cdr (assoc :stack metadata)) (car (getf doc :covers))))
           (slug (or (cdr (assoc :slug metadata)) code))
           (message (or (cdr (assoc :message metadata)) (getf doc :content)))
           (cause (or (cdr (assoc :cause metadata)) ""))
           (solution (or (cdr (assoc :solution metadata)) ""))
           (title (getf doc :title))
           (tags (getf doc :tags))
           (id (or (cdr (assoc :id metadata)) (format nil "err-~A" (uuid:make-v4-uuid)))))
      (list (cons :id id)
            (cons :code code)
            (cons :stack stack)
            (cons :slug slug)
            (cons :title title)
            (cons :message message)
            (cons :cause cause)
            (cons :solution solution)
            (cons :tags tags)))))

;;* Core
(defun register-hypertext (uri-string args)
  "Register hypertext document under URI."
  (multiple-value-bind (protocol path) (parse-uri-protocol uri-string)
    (let* ((dispatcher (get-or-create-dispatcher protocol))
           (title (or (getf args :title) path))
           (content (or (getf args :content) ""))
           (tags (getf args :tags))
           (covers (getf args :covers))
           (metadata (getf args :metadata))
           (id (or (cdr (assoc :id metadata))
                   (cdr (assoc :doc-id metadata))
                   (format nil "ht-~A" (uuid:make-v4-uuid))))
           (final-metadata (if (assoc :id metadata)
                               metadata
                               (acons :id id metadata)))
           (sections (getf args :sections))
           (final-sections (or sections (parse-markdown-headings content)))
           (doc (list :uri uri-string
                      :protocol protocol
                      :path path
                      :title title
                      :content content
                      :sections final-sections
                      :tags tags
                      :covers covers
                      :metadata final-metadata)))
      (setf (gethash (string-downcase path) (protocol-dispatcher-registry dispatcher)) doc)

      ;; Sync to global lists for backward compatibility
      (cond
        ((or (string-equal protocol "errors") (string-equal protocol "error"))
         (let ((err-alist (hypertext-to-error-alist doc)))
           (setf *errors* (remove (cdr (assoc :code err-alist)) *errors*
                                  :key (lambda (e) (cdr (assoc :code e)))
                                  :test #'string=))
           (push err-alist *errors*)))
        (t
         (let ((doc-alist (hypertext-to-alist doc)))
           (setf *docs* (remove uri-string *docs*
                                :key (lambda (d) (cdr (assoc :uri d)))
                                :test #'string=))
           (push doc-alist *docs*))))
      doc)))

(defmacro defhypertext (uri &rest args &key title content tags covers metadata sections)
  "Define hypertext documentation."
  (declare (ignore title content tags covers metadata sections))
  (let ((uri-var (gensym "URI-"))
        (args-var (gensym "ARGS-")))
    `(let ((,uri-var ,uri)
           (,args-var (list ,@args)))
       (register-hypertext ,uri-var ,args-var))))

;;* Dynamic/Format Resolvers
(defun markdown-to-org-string (markdown-text)
  "A simple and clean markdown to org-mode syntax converter."
  (let ((lines (str:lines markdown-text))
        (in-code-block nil))
    (with-output-to-string (s)
      (dolist (line lines)
        (cond
          ((str:starts-with? "```" line)
           (if in-code-block
               (progn
                 (format s "#+end_src~%")
                 (setf in-code-block nil))
               (let ((lang (string-trim '(#\Space #\Tab) (subseq line 3))))
                 (format s "#+begin_src ~A~%" (if (string= lang "") "text" lang))
                 (setf in-code-block t))))
          (in-code-block
           (format s "~A~%" line))
          ((str:starts-with? "#" line)
           (let* ((h-count (loop for c across line while (char= c #\#) count 1))
                  (rest (subseq line h-count)))
             (if (and (> h-count 0) (or (= (length rest) 0) (char= (char rest 0) #\Space)))
                 (format s "~A~A~%" (make-string h-count :initial-element #\*) rest)
                 (format s "~A~%" line))))
          (t
           (format s "~A~%" line)))))))



(defun format-hypertext-content (content format)
  "Format content string into specified formats."
  (case format
    (:markdown content)
    (:org-mode (markdown-to-org-string content))
    (:json (cl-json:encode-json-to-string `((:content . ,content))))
    (:text (strip-markdown content))
    (t content)))

(defun format-hypertext-directory (docs format)
  "Format multiple matching docs as a directory index."
  (case format
    (:json
     (cl-json:encode-json-to-string
      (coerce (mapcar (lambda (d)
                        `((:uri . ,(getf d :uri))
                          (:title . ,(getf d :title))
                          (:summary . ,(or (cdr (assoc :summary (getf d :metadata))) ""))))
                      docs)
              'vector)))
    (:org-mode
     (with-output-to-string (s)
       (format s "* Documentation Directory~%")
       (dolist (d docs)
         (format s "** [[~A][~A]]~%" (getf d :uri) (getf d :title)))))
    (t
     (with-output-to-string (s)
       (format s "# Documentation Directory~%~%")
       (dolist (d docs)
         (format s "- [~A](~A)~%" (getf d :title) (getf d :uri)))
       (format s "~%---~%~%")
       (dolist (d docs)
         (format s "## ~A (~A)~%~%" (getf d :title) (getf d :uri))
         (format s "~A~%~%---~%~%" (getf d :content)))))))

(defun match-protocol-dynamic-routes (dispatcher path)
  "Check dynamic patterns on the protocol dispatcher."
  (dolist (route (protocol-dispatcher-dynamic-routes dispatcher))
    (let ((pattern (car route))
          (handler (cdr route)))
      (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings pattern path)
        (when match
          (let ((res (apply handler (coerce groups 'list))))
            (when res
              (return-from match-protocol-dynamic-routes res)))))))
  nil)

(defun find-protocol-content (dispatcher path)
  "Resolve doc by path inside dispatcher."
  (let* ((clean-path (string-downcase path))
         (doc (gethash clean-path (protocol-dispatcher-registry dispatcher))))
    (if doc
        (values (getf doc :content) doc)
        (let ((dynamic-doc (match-protocol-dynamic-routes dispatcher path)))
          (if dynamic-doc
              (values (getf dynamic-doc :content) dynamic-doc)
              (let ((slash-pos (position #\/ path :from-end t)))
                (when slash-pos
                  (let ((parent-path (subseq path 0 slash-pos))
                        (section-name (subseq path (1+ slash-pos))))
                    (multiple-value-bind (parent-content parent-doc) (find-protocol-content dispatcher parent-path)
                      (declare (ignore parent-content))
                      (when parent-doc
                        (let* ((sections (getf parent-doc :sections))
                               (match (find section-name sections
                                            :test (lambda (name sec)
                                                    (or (string-equal name (getf sec :title))
                                                        (string-equal name (slugify (getf sec :title))))))))
                          (if match
                              (values (getf match :content)
                                      (list :uri (format nil "~A:~A" (protocol-dispatcher-name dispatcher) path)
                                            :title (getf match :title)
                                            :parent-uri (getf parent-doc :uri)
                                            :tags (getf parent-doc :tags)
                                            :covers (getf parent-doc :covers)))
                              nil))))))))))))

(defun find-protocol-by-prefix (dispatcher prefix)
  "Return all static docs matching prefix path."
  (let ((results '())
        (clean-prefix (string-downcase prefix)))
    (maphash (lambda (path doc)
               (when (or (string= path clean-prefix)
                         (str:starts-with? (format nil "~A/" clean-prefix) path))
                 (push doc results)))
             (protocol-dispatcher-registry dispatcher))
    (sort results #'string< :key (lambda (d) (getf d :uri)))))

(defun resolve-hypertext-uri (uri-string)
  "Dispatch and resolve hypertext content."
  (multiple-value-bind (protocol full-path) (parse-uri-protocol uri-string)
    (let* ((ext-pos (position #\. full-path :from-end t))
           (ext (when ext-pos (subseq full-path (1+ ext-pos))))
           (format (cond
                     ((equal ext "md") :markdown)
                     ((equal ext "org") :org-mode)
                     ((equal ext "json") :json)
                     ((equal ext "txt") :text)
                     (t nil)))
           (path (if format (subseq full-path 0 ext-pos) full-path))
           (dispatcher (gethash (string-downcase protocol) *protocol-dispatchers*)))
      (when dispatcher
        (multiple-value-bind (doc-content doc-meta) (find-protocol-content dispatcher path)
          (if doc-content
              (values (format-hypertext-content doc-content (or format :markdown))
                      (case (or format :markdown)
                        (:json "application/json")
                        (:org-mode "text/org")
                        (:text "text/plain")
                        (t "text/markdown"))
                      doc-meta)
              (let ((matches (find-protocol-by-prefix dispatcher path)))
                (if matches
                    (values (format-hypertext-directory matches (or format :markdown))
                            (case (or format :markdown)
                              (:json "application/json")
                              (:org-mode "text/org")
                              (t "text/markdown"))
                            `((:type . :directory) (:count . ,(length matches))))
                    nil))))))))

;;* Filesystem Interfaces (Two-way)
(defun render-protocol-as-org (protocol)
  "Serialize dispatcher registry to an aggregate Org document."
  (let ((dispatcher (gethash (string-downcase protocol) *protocol-dispatchers*)))
    (with-output-to-string (s)
      (format s "#+TITLE: ~A Documentation~%~%" protocol)
      (when dispatcher
        (let ((keys (sort (loop for k being the hash-keys of (protocol-dispatcher-registry dispatcher) collect k)
                          #'string<)))
          (dolist (k keys)
            (let* ((doc (gethash k (protocol-dispatcher-registry dispatcher)))
                   (uri (getf doc :uri))
                   (title (getf doc :title))
                   (content (getf doc :content))
                   (tags (getf doc :tags))
                   (covers (getf doc :covers)))
              (format s "* ~A~%" title)
              (format s ":PROPERTIES:~%")
              (format s ":URI: ~A~%" uri)
              (when tags (format s ":TAGS: ~{~A~^ ~}~%" tags))
              (when covers (format s ":COVERS: ~{~A~^ ~}~%" covers))
              (format s ":END:~%")
              (format s "~A~%~%" (string-trim '(#\Newline #\Space #\Tab) content)))))))))

(defun parse-protocol-from-org (protocol text)
  "Parse top-level headings and update dispatcher."
  (let* ((doc (org-mode-parser:parse-org-string (or text "")))
         (headings (org-mode-parser:collect-all-headings doc))
         (dispatcher (get-or-create-dispatcher protocol)))
    (clrhash (protocol-dispatcher-registry dispatcher))
    (dolist (h headings)
      (when (= (org-mode-parser:heading-level h) 1)
        (let* ((title (org-mode-parser:heading-title h))
               (uri (or (org-mode-parser:heading-property h :URI)
                        (format nil "~A:~A" protocol (slugify title))))
               (tags-str (org-mode-parser:heading-property h :TAGS))
               (tags (when tags-str (str:words tags-str)))
               (covers-str (org-mode-parser:heading-property h :COVERS))
               (covers (when covers-str (str:words covers-str)))
               (content (%heading-body-string h)))
          (register-hypertext uri (list :title title
                                        :content content
                                        :tags tags
                                        :covers covers)))))))

(definterface :hypertext-docs "docs"
  :format :org
  :render (lambda () (render-protocol-as-org "docs"))
  :parse (lambda (text) (parse-protocol-from-org "docs" text)))

(definterface :hypertext-apidoc ":apidoc"
  :format :org
  :render (lambda () (render-protocol-as-org "apidoc"))
  :parse (lambda (text) (parse-protocol-from-org "apidoc" text)))

;;* Autoloading

(defun load-hypertext-files ()
  "Load all `.lisp` files in repo-root `wiki/` and global config `wiki/` directories."
  (let ((global-dir (uiop:subpathname *hactar-config-path* "wiki/"))
        (project-dir (merge-pathnames #P"wiki/" *repo-root*)))
    (when (uiop:directory-exists-p global-dir)
      (dolist (file (uiop:directory-files global-dir))
        (when (string-equal (pathname-type file) "lisp")
          (load file :if-does-not-exist nil))))
    (when (uiop:directory-exists-p project-dir)
      (dolist (file (uiop:directory-files project-dir))
        (when (string-equal (pathname-type file) "lisp")
          (load file :if-does-not-exist nil))))))

;;* Route Installation
(defun %install-hypertext-routes ()
  "Install route handler for URI protocols."
  (unregister-route 'hypertext-route)
  (let* ((protocols (loop for k being the hash-keys of *protocol-dispatchers* collect k))
         (protocols (or protocols (list "docs" "apidoc" "wiki")))
         (pattern (format nil "^(~{~A~^|~}):(.+)$" (mapcar #'string-downcase protocols))))
    (register-route 'hypertext-route
                    pattern
                    '(:protocol :path)
                    60
                    (lambda (protocol path)
                      (let ((result (resolve-hypertext-uri (format nil "~A:~A" protocol path))))
                        (if result
                            result
                            (format nil "URI not found: ~A:~A" protocol path)))))))

(push #'%install-hypertext-routes *route-reinit-hooks*)

(%install-hypertext-routes)
