;; dev.lisp - dev repl environment
(require :asdf)
(require :ql)

;; Load the ASDF system definition from the current directory
(push (uiop:getcwd) asdf:*central-registry*)

(ql:quickload :hactar-migrations)
(ql:quickload :hactar)
(ql:quickload :llm)

;; Start the main function, which includes Slynk server
(hactar:main)
