;; error references and fixes
(in-package :hactar)

(defun format-error-for-preview (err)
  (format nil "Title: ~A~%Code: ~A~%Stack: ~A~%Slug: ~A~%Message: ~A~%~%Cause: ~A~%~%Solution: ~A"
          (cdr (assoc :title err))
          (cdr (assoc :code err))
          (cdr (assoc :stack err))
          (cdr (assoc :slug err))
          (cdr (assoc :message err))
          (cdr (assoc :cause err))
          (cdr (assoc :solution err))))

(defun fuzzy-select-error (errors)
  "Select an error from a list using fuzzy-select."
  (let* ((items (loop for err in errors
                      collect `((:item . ,(format nil "~A (Code: ~A)" (cdr (assoc :title err)) (cdr (assoc :code err))))
                                (:preview . ,(format-error-for-preview err)))))
         (selected (fuzzy-select items)))
    (when selected
      (let* ((title-code (cdr (assoc :item selected)))
             (code (second (cl-ppcre:split "\\(Code: " (string-trim ")" title-code)))))
        (find code errors :key (lambda (e) (cdr (assoc :code e))) :test #'string=)))))

(defun extract-org-content-from-node (node)
  "Extract content from a node, removing property drawers."
  (with-output-to-string (s)
    (dolist (child (org-mode-parser:node-children node))
      (unless (typep child 'org-mode-parser:org-properties-drawer)
        (princ (org-mode-parser:org-to-string child) s)))))

(defun parse-errors-from-org-file (filename)
  "Parse errors from an Org file. Returns a list of error plists."
  (let* ((doc (org-mode-parser:parse-org-file filename))
         (errors '()))
    (org-mode-parser:walk-tree
     doc
     (lambda (node)
       (when (and (typep node 'org-mode-parser:org-heading)
                   (member "error" (org-mode-parser:heading-tags node) :test #'string-equal))
         (let ((title (org-mode-parser:heading-title node))
               (tags (remove "error" (org-mode-parser:heading-tags node) :test #'string-equal))
               (children (org-mode-parser:node-children node))
               code stack slug message cause solution)

           ;; Extract Code, Stack, Slug
           (let ((props-drawer (find-if (lambda (c) (typep c 'org-mode-parser:org-properties-drawer)) children)))
             (when props-drawer
               (let ((props (org-mode-parser:node-properties props-drawer)))
                 (setf code (cdr (assoc :CODE props)))
                 (setf stack (cdr (assoc :STACK props)))
                 (setf slug (cdr (assoc :SLUG props))))))

           ;; Extract Message (from #+begin_src error)
           (let ((msg-node (find-if (lambda (c)
                                      (and (typep c 'org-mode-parser:org-src-block)
                                           (string-equal (org-mode-parser:src-block-language c) "error")))
                                    children)))
             (when msg-node
               (setf message (string-trim '(#\Space #\Tab #\Newline #\Return) (org-mode-parser:src-block-content msg-node)))))

           ;; Extract Cause and Solution
           (dolist (child children)
             (when (typep child 'org-mode-parser:org-heading)
               (cond
                 ((string-equal (org-mode-parser:heading-title child) "Cause")
                  (setf cause (string-trim '(#\Space #\Tab #\Newline #\Return) (extract-org-content-from-node child))))
                 ((string-equal (org-mode-parser:heading-title child) "Solution")
                  (setf solution (string-trim '(#\Space #\Tab #\Newline #\Return) (extract-org-content-from-node child)))))))

           (when (and title code message cause solution)
             (push (list (cons :title title)
                         (cons :code code)
                         (cons :stack stack)
                         (cons :slug slug)
                         (cons :message message)
                         (cons :cause cause)
                         (cons :solution solution)
                         (cons :tags tags))
                   errors))))))
    (nreverse errors)))

(defmacro deferror (title &rest args)
  "Define an error document registered under 'errors:' protocol."
  (let ((code (getf args :code))
        (stack (getf args :stack))
        (slug (getf args :slug))
        (message (getf args :message))
        (cause (getf args :cause))
        (solution (getf args :solution))
        (tags (getf args :tags)))
    (unless (and title code stack message cause solution)
      (error "deferror: title, code, stack, message, cause, and solution are required."))
    (let* ((final-slug (or slug (if stack (format nil "~A:~A" stack code) code)))
           (uri (format nil "errors:~A" final-slug))
           (content (format nil "Message: ~A~%~%Cause: ~A~%~%Solution: ~A" message cause solution)))
      `(register-hypertext ,uri
                           (list :title ,title
                                 :content ,content
                                 :tags ,tags
                                 :covers (uiop:ensure-list ,stack)
                                 :metadata (list (cons :code ,code)
                                                 (cons :stack ,stack)
                                                 (cons :slug ,final-slug)
                                                 (cons :message ,message)
                                                 (cons :cause ,cause)
                                                 (cons :solution ,solution)))))))

(defun get-all-registered-errors ()
  "Get all registered errors as alists."
  (let ((results '()))
    (dolist (proto '("errors" "error"))
      (let ((dispatcher (gethash proto *protocol-dispatchers*)))
        (when dispatcher
          (maphash (lambda (path err-plist)
                     (declare (ignore path))
                     (push (hypertext-to-error-alist err-plist) results))
                   (protocol-dispatcher-registry dispatcher)))))
    results))

(define-command errors (args)
  "Find and select errors relevant to the current project/context."
  (declare (ignore args))
  (let ((results (get-all-registered-errors)))
    (if results
        (let ((selected (fuzzy-select-error results)))
          (when selected
            (add-error-to-context selected)))
        (format t "No errors found.~%"))))

(define-command errors-add (args)
  "Add a new error entry by emitting a (deferror ...) lisp file under errors/ and loading it.
   Usage: /errors-add --code CODE --title TITLE --message MSG --cause CAUSE --solution SOL --stack STACK --slug SLUG"
  (let* ((cli-string (format nil "errors-add ~{~A~^ ~}" args))
         (parsed (parse-cli-args-s cli-string)))
    (let ((code (getf parsed :code))
          (stack (getf parsed :stack))
          (slug (getf parsed :slug))
          (title (getf parsed :title))
          (message (getf parsed :message))
          (cause (getf parsed :cause))
          (solution (getf parsed :solution))
          (tags (uiop:ensure-list (getf parsed :tags))))
      (if (and code title message cause solution)
          (let* ((final-slug (or slug (if stack (format nil "~A:~A" stack code) code)))
                 (out-file (uiop:native-namestring
                            (merge-pathnames
                             (format nil "errors/~A.lisp" final-slug)
                             (or *repo-root* (uiop:getcwd)))))
                 (form `(deferror ,title
                            :code ,code
                            :stack ,stack
                            :slug ,final-slug
                            :message ,message
                            :cause ,cause
                            :solution ,solution
                            :tags ',tags)))
            (ensure-directories-exist out-file)
            (with-open-file (s out-file :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create
                                        :external-format :utf-8)
              (format s ";;; Created error doc~%(in-package :hactar)~%~%")
              (let ((*print-case* :downcase))
                (prin1 form s))
              (terpri s))
            (load out-file)
            (format t "Error '~A' added and written to ~A~%" title out-file))
          (format t "Missing required arguments. Need --code, --title, --message, --cause, --solution.~%")))))

(define-command errors-add-to-context (args)
  "Select and add a known error to context (from *errors*)."
  (declare (ignore args))
  (if *errors*
      (let ((selected (fuzzy-select-error *errors*)))
        (when selected
          (add-error-to-context selected)))
      (format t "No errors in *errors* list.~%")))

(defun errors-import->lisp (errors out-file source)
  "Emit (deferror ...) forms for ERRORS to OUT-FILE and return the path."
  (ensure-directories-exist out-file)
  (with-open-file (s out-file :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
    (format s ";;; Imported errors from ~A~%(in-package :hactar)~%~%" source)
    (let ((*print-case* :downcase))
      (dolist (err errors)
        (prin1 `(deferror ,(cdr (assoc :title err))
                    :code ,(cdr (assoc :code err))
                    :stack ,(cdr (assoc :stack err))
                    :slug ,(cdr (assoc :slug err))
                    :message ,(cdr (assoc :message err))
                    :cause ,(cdr (assoc :cause err))
                    :solution ,(cdr (assoc :solution err))
                    :tags ',(cdr (assoc :tags err)))
               s)
        (terpri s) (terpri s))))
  out-file)

(defun errors-import (args)
  (let ((filename (if (keywordp (first args))
                      (first (getf args :args))
                      (first args))))
    (if (and filename (probe-file filename))
        (let ((errors (parse-errors-from-org-file filename)))
          (if errors
              (let ((out-file (uiop:native-namestring
                               (merge-pathnames
                                (format nil "errors/~A.lisp" (pathname-name filename))
                                (or *repo-root* (uiop:getcwd))))))
                (errors-import->lisp errors out-file filename)
                (load out-file)
                (format t "Imported ~A errors from ~A -> ~A~%"
                        (length errors) filename out-file))
              (format t "No valid errors found in ~A~%" filename)))
        (format t "File not found: ~A~%" filename))))

(define-command errors.import (args)
  "Import an error document (Org format).
   Usage: hactar errors.import <file.org>"
  (errors-import args))
