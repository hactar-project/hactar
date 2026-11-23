(asdf:defsystem #:hactar-tests
  :description "Network-free tests for the Hactar greenfield rebuild engine."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:hactar-llm-live #:hactar-jsonrpc #:hactar-vfs #:hactar-watch
               #:hactar-sandbox #:hactar-tui #:hactar-codegen
               #:hactar-acp #:hactar-mcp #:hactar-http #:hactar-proxy
               #:hactar-rag #:hactar-knowledge
               #:hactar-modes-web #:hactar-modes-ruby #:hactar-modes-systems
               #:hactar-modes-lisp #:hactar-modes-shell #:hactar-modes-services
               #:hactar-starters
               #:hactarsh #:ruhe #:hyperfractal #:hactar-pro
               #:fiveam #:str #:uiop)
  :pathname "tests/rebuild/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "helpers")
               (:file "dsl.test")
               (:file "route.test")
               (:file "hook.test")
               (:file "sandbox.test")
               (:file "mode.test")
               (:file "generated-file.test")
               (:file "bootstrap.test")
               (:file "self-update.test")
               (:file "interface.test")
               (:file "session.test")
               (:file "adapters.test")
               (:file "knowledge.test")
               (:file "satellites.test")
               (:file "libraries.test")
               (:file "deps.test"))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :hactar-rebuild-tests :run-tests)))
