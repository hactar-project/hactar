(require :asdf)
(let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file q) (load q)))
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :hactar :silent t)

(defun build-hactar ()
  "Build the hactar executable using ASDF's program-op."
  (format t "~&Building hactar executable using asdf:make...~%")
  (ensure-directories-exist "hactar/build/")
  (handler-case
      (asdf:make :hactar)
    (error (e)
      (format *error-output* "~&Error during build: ~A~%" e)
      (uiop:quit 1)))
  (format t "~&Build complete. Executable should be at 'hactar/build/hactar'.~%"))
