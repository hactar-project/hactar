(require :asdf)
(declaim (optimize (speed 3) (safety 1) (debug 0)))
;; Load the ASDF system definition from the current directory
(push (uiop:getcwd) asdf:*central-registry*)
;; Load the systems
(asdf:load-system :hactar-migrations)
(asdf:load-system :hactar)
(asdf:load-system :llm)

(defun build-hactar ()
  "Build the hactar executable using ASDF's program-op."
  (format t "~&Building hactar executable using asdf:make...~%")
  (handler-case
      (asdf:make :hactar)
    (error (e)
      (format *error-output* "~&Error during build: ~A~%" e)
      (uiop:quit 1)))
  (format t "~&Build complete. Executable should be at 'hactar'.~%"))
