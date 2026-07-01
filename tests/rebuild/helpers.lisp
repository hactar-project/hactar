(in-package #:hactar-rebuild-tests)

(defmacro with-temp-project ((root &key files) &body body)
  "Bind ROOT to a fresh temp dir (with a .git marker), create FILES
((relpath . contents) ...), bind *repo-root*, run BODY, then clean up."
  (let ((dir (gensym)))
    `(let* ((,dir (format nil "/tmp/hactar-rt-~A-~A/"
                          (get-universal-time) (random 100000)))
            (,root (uiop:ensure-directory-pathname ,dir)))
       (unwind-protect
            (progn
              (ensure-directories-exist ,root)
              (ensure-directories-exist (merge-pathnames ".git/" ,root))
              (dolist (f ,files)
                (let ((p (merge-pathnames (car f) ,root)))
                  (ensure-directories-exist p)
                  (with-open-file (s p :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                    (write-string (cdr f) s))))
              (let ((hactar-rebuild:*repo-root* ,root))
                ,@body))
         (ignore-errors (uiop:delete-directory-tree
                         ,root :validate t :if-does-not-exist :ignore))))))

(defun engine-forms (source)
  "Read SOURCE (a string) into forms interned in the hactar-rebuild package,
mirroring how generate-harness and read-forms produce DSL forms."
  (let ((*package* (find-package :hactar-rebuild))
        (*read-eval* nil))
    (with-input-from-string (s source)
      (loop for form = (read s nil :eof)
            until (eq form :eof)
            collect form))))

(defun eval-in-user (source)
  "Evaluate DSL forms in the hactar-user package, like loading .hactar.lisp.
SOURCE may be a string (read in hactar-user so symbols intern there) or an
already-read form/list of forms (evaluated as-is)."
  (let ((*package* (find-package :hactar-user)))
    (if (stringp source)
        (with-input-from-string (s source)
          (loop with *read-eval* = nil
                for form = (read s nil :eof)
                until (eq form :eof)
                for val = (let ((*read-eval* t)) (eval form))
                finally (return val)))
        (eval source))))
