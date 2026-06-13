;; Documentation and Guides Handling
(in-package :hactar)

(defun add-doc (title source &key (uri 'source) tags covers meta related id)
  "Add a documentation entry to the global hypertext registry."
  (let* ((is-symbol (some (lambda (m) (and (consp m) (eq (car m) :symbol))) meta))
         (symbol-name (when is-symbol (cdr (assoc :symbol meta))))
         (final-uri (cond
                      ((and (eq uri 'source) is-symbol) (format nil "apidoc:~A" symbol-name))
                      ((eq uri 'source) (format nil "docs:~A" (slugify title)))
                      (t uri)))
         (final-meta (append meta (list (cons :id id) (cons :related related)))))
    (register-hypertext final-uri
                        (list :title title
                              :content (if (str:starts-with? "raw:" source)
                                           (subseq source 4)
                                           source)
                              :tags tags
                              :covers covers
                              :metadata final-meta))))

(defmacro defdocforsymbol (symbol-spec source &key (uri ''source) (type :function) (version "latest") (format :markdown) related covers tags meta)
  "Define documentation for a specific symbol, creating graph connections."
  (let* ((symbol-name (string-downcase (string symbol-spec)))
         (title (format nil "~A (~A)" symbol-name type)))
    `(add-doc ,title ,source
              :uri ,uri
              :covers (cons ,symbol-name ,covers)
              :tags (append (list "symbol" (string-downcase (symbol-name ,type))) ,tags)
              :related (mapcar (lambda (r) (list :target (string-downcase (string r)) :type :related)) (uiop:ensure-list ,related))
              :meta (append ,meta
                            `((:symbol . ,,symbol-name)
                              (:type . ,,type)
                              (:version . ,,version)
                              (:format . ,,format))))))

(defmacro defdoc (title &rest args)
  "Define a documentation entry."
  (let ((source (getf args :source))
        (version (getf args :version))
        (covers (getf args :covers)))
    (unless (or version covers)
      (error "Defdoc for ~S requires either :version or :covers." title))
    (let ((covers-expr (if covers
                           covers
                           `(uiop:ensure-list ,version))))
      `(add-doc ,title ,source
                :uri ,(getf args :uri ''source)
                :tags ,(getf args :tags)
                :covers ,covers-expr
                :meta ,(getf args :meta)
                :related ,(getf args :related)
                :id ,(getf args :id)))))

(defun get-all-registered-docs ()
  "Returns all registered documents in the docs and apidoc dispatchers as alists."
  (let ((results '()))
    (dolist (protocol '("docs" "apidoc"))
      (let ((dispatcher (get-or-create-dispatcher protocol)))
        (maphash (lambda (path doc-plist)
                   (declare (ignore path))
                   (push (hypertext-to-alist doc-plist) results))
                 (protocol-dispatcher-registry dispatcher))))
    results))

;;* Guides
(defun register-guide (name &key title content source tags covers sections)
  "Register a guide plist into guide dispatcher, keyed by downcased NAME."
  (register-hypertext (format nil "guide:~A" (string-downcase (string name)))
                      (list :title (or title name)
                            :content (or content "")
                            :source source
                            :tags tags
                            :covers covers
                            :sections sections)))

(defmacro defguide (name &rest args)
  "Define a guide registered under 'guide:' protocol."
  (let ((title (getf args :title))
        (content (getf args :content))
        (source (getf args :source))
        (tags (getf args :tags))
        (covers (getf args :covers))
        (sections (getf args :sections)))
    `(register-guide ,(string-downcase (string name))
                     :title ,(or title (string name))
                     :content ,(or content "")
                     :source ,source
                     :tags ,tags
                     :covers ,covers
                     :sections ,sections)))

(defun get-guide (name)
  "Look up a registered guide by name."
  (let ((dispatcher (get-or-create-dispatcher "guide")))
    (gethash (string-downcase (string name)) (protocol-dispatcher-registry dispatcher))))

(defun guide-activate (name)
  "Add a registered guide to the documentation context."
  (let ((guide (get-guide name)))
    (when guide
      (add-doc-to-context (hypertext-to-alist guide))
      guide)))

(defun guide-add-section (name section-title section-content)
  "Append a section to a registered guide."
  (let ((guide (get-guide name)))
    (when guide
      (let* ((sections (getf guide :sections))
             (updated-sections (append sections (list (list :title section-title :content section-content)))))
        (setf (getf guide :sections) updated-sections)
        (let ((dispatcher (get-or-create-dispatcher "guide")))
          (setf (gethash (string-downcase (string name)) (protocol-dispatcher-registry dispatcher)) guide)))
      guide)))

;;* Helpers and Commands
(define-command docs-context (args)
  "Show documentation currently added to the context."
  (declare (ignore args))
  (if *docs-context*
      (progn
        (format t "Documentation in context:~%")
        (dolist (doc *docs-context*)
          (format t "  - ~A (ID: ~A, Source: ~A)~%"
                  (cdr (assoc :title doc))
                  (cdr (assoc :id doc))
                  (cdr (assoc :source doc)))))
      (format t "No documentation in context.~%")))

(define-command docs-add-to-context (args)
  "Add a known document to the context by its URI or Source."
  (let ((uri (format nil "~{~A~^ ~}" args)))
    (if (string= uri "")
        (format t "Please provide a URI or Source to add.~%")
        (let ((doc (find uri *docs* :key (lambda (d) (cdr (assoc :uri d))) :test #'string=)))
          (unless doc
            (setf doc (find uri *docs* :key (lambda (d) (cdr (assoc :source d))) :test #'string=)))
          (if doc
              (add-doc-to-context doc)
              (format t "No document found matching URI/Source: ~A~%" uri))))))

(define-command docs-add (args)
  "Add/import documentation. Alias of /import."
  (import args))

(define-command docs (args)
  "Find and select documentation. Fuzzy selects from loaded stack-specific docs."
  (declare (ignore args))
  (let ((results (get-all-registered-docs)))
    (if results
        (let ((selected-doc (fuzzy-select-doc results)))
          (when selected-doc
            (add-doc-to-context selected-doc)))
        (format t "No documentation loaded.~%")))
  :json (lambda (args)
          (declare (ignore args))
          (to-json (coerce (or (get-all-registered-docs) '()) 'vector))))

(define-command guides (args)
  "List available registered guides and select one to activate."
  (declare (ignore args))
  (let ((results '()))
    (let ((dispatcher (get-or-create-dispatcher "guide")))
      (maphash (lambda (path doc-plist)
                 (declare (ignore path))
                 (push (hypertext-to-alist doc-plist) results))
               (protocol-dispatcher-registry dispatcher)))
    (if results
        (let ((selected-doc (fuzzy-select-doc results)))
          (when selected-doc
            (add-doc-to-context selected-doc)))
        (format t "No guides defined. Define guides using defguide.~%"))))

(define-command guide (args)
  "Print the content of a registered guide."
  (if (null args)
      (format t "Usage: /guide <name>~%")
      (let ((guide (get-guide (first args))))
        (if guide
            (progn
              (format t "--- Guide: ~A ---~%" (getf guide :title))
              (format t "~A~%" (getf guide :content))
              (format t "--- End of Guide ---~%"))
            (format t "Guide '~A' not found.~%" (first args))))))

(defun %lookup-parse-symbols (args)
  "Helper for /lookup: parse the raw arg list into a list of symbol strings."
  (let* ((symbols-str (format nil "~{~A~^ ~}"
                              (remove-if (lambda (a)
                                           (and (stringp a)
                                                (or (string= a "--format")
                                                    (and (>= (length a) 9)
                                                         (string= a "--format=" :end1 9))
                                                     (assoc a *shorthand-format-flags* :test #'string=))))
                                         args)))
         (symbols (mapcar (lambda (s) (string-trim '(#\Space) s))
                          (uiop:split-string symbols-str :separator ","))))
    (remove "" symbols :test #'string=)))

(define-command lookup (args)
  "Lookup symbols in documentation.
Usage: /lookup symbol1,symbol2"
  (let ((symbols (%lookup-parse-symbols args)))
    (if (null symbols)
        (format t "Usage: /lookup symbol1,symbol2~%")
        (let ((results '()))
          (dolist (proto '("apidoc" "docs"))
            (let ((dispatcher (get-or-create-dispatcher proto)))
              (maphash (lambda (path doc-plist)
                         (declare (ignore path))
                         (let ((covers (getf doc-plist :covers))
                               (title (getf doc-plist :title)))
                           (when (some (lambda (sym)
                                         (or (member sym covers :test #'string-equal)
                                             (search sym title :test #'string-equal)))
                                       symbols)
                             (push (hypertext-to-alist doc-plist) results))))
                       (protocol-dispatcher-registry dispatcher))))
          (if results
              (let ((selected (fuzzy-select-doc results)))
                (when selected
                  (add-doc-to-context selected)))
              (format t "No documentation found matching: ~{~A~^, ~}~%" symbols))))))

(define-command lookup-in-files (args)
  "Lookup symbols in files using rg.
Usage: /lookup-in-files symbol1,symbol2 [--format=json]"
  (let* ((format-opt (getf args :format))
         (pos-args (append (uiop:ensure-list (getf args :subcommand))
                           (uiop:ensure-list (getf args :args))))
         (symbols-str (format nil "~{~A~^ ~}" pos-args))
         (symbols (mapcar (lambda (s) (string-trim '(#\Space) s))
                          (uiop:split-string symbols-str :separator ","))))
    (setf symbols (remove "" symbols :test #'string=))
    (if (null symbols)
        (format t "Usage: /lookup-in-files symbol1,symbol2~%")
        (if (find-executable "rg")
            (let ((files (search-files-with-rg symbols *repo-root*)))
              (if (string-equal format-opt "json")
                  (let ((json-str (format nil "~A~%" (to-json (coerce files 'vector)))))
                    (editor-output json-str :type "json" :success "true"))
                  (if files
                      (let* ((rel-files (mapcar (lambda (f) (uiop:native-namestring (uiop:enough-pathname f *repo-root*))) files))
                             (selected-rel-file (fuzzy-select rel-files)))
                        (when selected-rel-file
                          (let ((full-path (merge-pathnames selected-rel-file *repo-root*)))
                            (format t "Selected: ~A~%" selected-rel-file)
                            (format t "~A~%" (get-file-content full-path)))))
                      (format t "No files found matching: ~{~A~^, ~}~%" symbols))))
            (format t "Error: 'rg' (ripgrep) is not installed or not in PATH.~%"))))
  :cli-options ((:long "format" :description "Output format (json)")))

(define-command search-add-files (args)
  "Search for files containing a text pattern using rg, and add them to context."
  (let ((query (format nil "~{~A~^ ~}" args)))
    (if (string= query "")
        (format t "Please provide a search query.~%")
        (if (find-executable "rg")
            (let ((files (search-files-with-rg query *repo-root*)))
              (if files
                  (let* ((rel-files (mapcar (lambda (f) (uiop:native-namestring (uiop:enough-pathname f *repo-root*))) files))
                         (selected-rel-files (fuzzy-select rel-files :multi t)))
                    (if selected-rel-files
                        (progn
                          (dolist (f selected-rel-files)
                            (let ((full-path (merge-pathnames f *repo-root*)))
                              (pushnew (uiop:native-namestring full-path) *files* :test #'string=)
                              (format t "Added: ~A~%" f)))
                          (format t "Added ~A files to context.~%" (length selected-rel-files)))
                        (format t "No files selected.~%")))
                  (format t "No files found matching '~A'.~%" query)))
            (format t "Error: 'rg' (ripgrep) is not installed or not in PATH.~%")))))
