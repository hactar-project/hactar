(asdf:defsystem #:hactar-rebuild-tests
  :description "Network-free tests for the Hactar greenfield rebuild engine."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-rebuild #:fiveam #:str #:uiop)
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
               (:file "self-update.test"))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :hactar-rebuild-tests :run-tests)))
