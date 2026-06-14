;; Starters kits handling
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defstarter defstart add-starter get-all-registered-starters)))

(defun coerce-to-list (val)
  "Helper to coerce vector/string/single values into a list."
  (cond
    ((null val) nil)
    ((listp val) val)
    ((vectorp val) (coerce val 'list))
    ((stringp val) (uiop:split-string val :separator ","))
    (t (list val))))

(defun add-starter (title source &key stack language version uri tags covers meta related id)
  "Add a starter template to the global starters registry."
  (let* ((final-uri (cond
                      ((eq uri 'source) (format nil "starters:~A" (slugify title)))
                      (t uri)))
         (final-meta (append meta
                             (list (cons :stack (coerce-to-list stack))
                                   (cons :language language)
                                   (cons :version version)
                                   (cons :id id)
                                   (cons :related related)))))
    (register-hypertext final-uri
                        (list :title title
                              :content (if (str:starts-with? "raw:" source)
                                           (subseq source 4)
                                           source)
                              :tags (append '("starter") (coerce-to-list tags))
                              :covers (coerce-to-list covers)
                              :metadata final-meta))))

(defmacro defstarter (title &rest args)
  "Define a starter template."
  (let ((source (getf args :source))
        (stack (getf args :stack))
        (language (getf args :language))
        (version (getf args :version))
        (uri (getf args :uri ''source))
        (tags (getf args :tags))
        (covers (getf args :covers))
        (meta (getf args :meta))
        (related (getf args :related))
        (id (getf args :id)))
    `(add-starter ,title ,source
                  :stack ,stack
                  :language ,language
                  :version ,version
                  :uri ,uri
                  :tags ,tags
                  :covers ,covers
                  :meta ,meta
                  :related ,related
                  :id ,id)))

(defmacro defstart (title &rest args)
  "Alias for defstarter."
  `(defstarter ,title ,@args))

(defun get-all-registered-starters ()
  "Returns all registered starter documents in the starters dispatcher as alists."
  (let ((results '())
        (dispatcher (gethash "starters" *protocol-dispatchers*)))
    (when dispatcher
      (maphash (lambda (path doc-plist)
                 (declare (ignore path))
                 (push (hypertext-to-alist doc-plist) results))
               (protocol-dispatcher-registry dispatcher)))
    results))

(defun starter->lisp (uri out-file &key stack language version tags covers meta)
  "Fetch URI content and emit a (defstarter ...) form to OUT-FILE."
  (multiple-value-bind (content title source-meta)
      (fetch-import-content uri)
    (let* ((title (or title (format nil "Import from ~A" uri)))
           (final-meta (or meta source-meta))
           (tags-list (coerce-to-list tags))
           (covers-list (or (coerce-to-list covers)
                            (list title)))
           (stack-list (coerce-to-list stack))
           (form `(defstarter ,title
                      :source ,(concatenate 'string "raw:" content)
                      :uri ,uri
                      :stack ',stack-list
                      :language ,language
                      :version ,version
                      :tags ',tags-list
                      :covers ',covers-list
                      :meta ',final-meta)))
      (ensure-directories-exist out-file)
      (with-open-file (s out-file :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
        (format s ";;; Imported Starter from ~A~%(in-package :hactar)~%~%" uri)
        (let ((*print-case* :downcase)
              (*print-pretty* t)
              (*print-right-margin* 10000)
              (*print-circle* nil)
              (*print-readably* nil)
              (*package* (find-package :hactar)))
          (prin1 form s))
        (terpri s))
      out-file)))

(define-command starters (args)
  "Find and select a starter template. Fuzzy selects from loaded starters."
  (declare (ignore args))
  (let ((results (get-all-registered-starters)))
    (if results
        (let ((selected-starter (fuzzy-select-doc results)))
          (when selected-starter
            (add-doc-to-context selected-starter)
            (format t "Selected starter: ~A~%" (cdr (assoc :title selected-starter)))))
        (format t "No starters loaded. Define starters using defstarter or import them.~%")))
  :json (lambda (args)
          (declare (ignore args))
          (to-json (coerce (or (get-all-registered-starters) '()) 'vector))))

(define-command starters.import (args)
  "Import a starter template.
   Usage: /starters.import <uri> [out-file] -stack=... -language=... -version=..."
  (if (null args)
      (format t "Usage: /starters.import <uri> [out-file] -stack=... -language=... -version=...~%")
      (let* ((uri (first args))
             (rest-args (rest args))
             (parsed-args (parse-cli-args-s (join-strings " " rest-args)))
             (pos-args (getf parsed-args :args))
             (out-file-arg (first pos-args))
             (stack (getf parsed-args :stack))
             (language (getf parsed-args :language))
             (version (getf parsed-args :version))
             (tags (getf parsed-args :tags))
             (covers (getf parsed-args :covers))
             (tags-list (coerce-to-list tags))
             (covers-list (coerce-to-list covers))
             (import-dir (uiop:ensure-directory-pathname
                          (or *starters-import-path* *docs-import-path* "./hactar/docs/")))
             (resolved-dir (if (uiop:absolute-pathname-p import-dir)
                               import-dir
                               (merge-pathnames import-dir (or *repo-root* (uiop:getcwd)))))
             (out-file (or out-file-arg
                           (uiop:native-namestring
                            (merge-pathnames
                             (format nil "~A.lisp"
                                     (cl-ppcre:regex-replace-all "[^A-Za-z0-9_.-]" uri "_"))
                             resolved-dir)))))
        (handler-case
            (let ((file (starter->lisp uri out-file
                                       :stack stack
                                       :language language
                                       :version version
                                       :tags tags-list
                                       :covers covers-list)))
              (load file)
              (format t "Imported starter ~A -> ~A (emitted defstarter lisp and loaded)~%" uri file))
          (error (e)
            (format t "Import failed for ~A: ~A~%" uri e)))))
  :sub t)
