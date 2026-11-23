;; a wrapper lisp file used in repl workflows when running tests
;; Load necessary systems using ASDF for dependencies
(ql:quickload :hactar)
(ql:quickload :hactar-tests)

;; Run Hactar tests
(format t "~&--- Running Hactar + Split Library Tests ---~%")
(unless (hactar-rebuild-tests:run-tests)
  (uiop:quit 1))

(format t "~&--- All Tests Completed ---~%")
