;; a wrapper lisp file used in repl workflows when running tests
;; Load necessary systems using ASDF for dependencies
(ql:quickload :llm)
(ql:quickload :llm-tests)
(ql:quickload :hactar)
(ql:quickload :hactar-tests)

;; Run LLM tests
(format t "~&--- Running LLM Tests ---~%")
;; (llm-tests:run-tests)

;; Run Hactar tests
(format t "~&--- Running Hactar Tests ---~%")
(hactar-tests:run-tests)

(format t "~&--- All Tests Completed ---~%")
