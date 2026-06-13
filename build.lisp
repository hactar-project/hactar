(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
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
