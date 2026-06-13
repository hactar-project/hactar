;; dev.lisp - dev repl environment
(require :asdf)
(require :ql)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :hactar)
(ql:quickload :llm)
(hactar:main)
