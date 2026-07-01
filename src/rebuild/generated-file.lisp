(in-package #:hactar-rebuild)

(defun read-forms (path)
  "Read all forms from PATH with reader eval disabled.

Honor an IN-PACKAGE form as it is encountered so subsequent unqualified DSL
symbols are interned in the same package they will be evaluated in."
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((*read-eval* nil)
          (*package* (find-package "HACTAR-REBUILD"))
          (forms '()))
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (progn
                 (push form forms)
                 (when (and (consp form)
                            (symbolp (car form))
                            (string= (string-downcase (symbol-name (car form)))
                                     "in-package"))
                   (let ((package (find-package (second form))))
                     (unless package
                       (error "Unknown package in IN-PACKAGE: ~A" (second form)))
                     (setf *package* package)))))
      (nreverse forms))))

(defun load-hactar-file (path)
  "Validate and load PATH into the hactar-user DSL package."
  (let ((forms (read-forms path)))
    (multiple-value-bind (ok errors) (validate-forms forms)
      (unless ok
        (error "Invalid .hactar.lisp: ~{~A~^; ~}" errors)))
    (reset-registries)
    (let ((*package* (find-package "HACTAR-USER")))
      (dolist (form forms)
        (unless (and (consp form)
                     (symbolp (car form))
                     (string= (string-downcase (symbol-name (car form)))
                              "in-package"))
          (eval form)))))
  t)
