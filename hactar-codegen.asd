(asdf:defsystem #:hactar-codegen
  :description "Deterministic code generator targets for Hactar"
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :serial t
  :components ((:file "vfs")
               (:file "compiler")
               (:file "checker")
               (:file "javascript-target")
               (:file "typescript-target")
               (:file "worker-target")
               (:file "json-target")
               (:file "toml-target")
               (:file "css-target")
               (:file "redwood-target")
               (:file "lsp")
               (:file "convert")))
