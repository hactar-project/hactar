(asdf:defsystem #:hactar-core
  :description "Hactar engine + closed DSL surface (the reusable library)."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:cl-ppcre #:str #:uiop
               #:hactar-llm #:hactar-org #:hactar-md #:hactar-fzf #:hactar-fern)
  :serial t
  :pathname "src/rebuild/"
  :components ((:file "packages")
               (:file "result")
               (:file "state")
               (:file "hooks")
               (:file "surface")
               (:file "llm")
               (:file "ast")
               (:file "runners")
               (:file "generated-file")
               (:file "modes")
               (:file "self-update")
               (:file "interfaces")
               (:file "bootstrap")
               (:file "dispatcher")
               (:file "main")))
