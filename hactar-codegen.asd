(asdf:defsystem #:hactar-codegen
  :description "Deterministic code generator targets for Hactar"
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-fern #:hactar-vfs #:cl-ppcre #:str #:shasht #:uiop)
  :serial t
  :pathname "src/codegen/"
  :components ((:file "packages")
               (:file "vfs-buffer")
               (:file "compiler")
               (:file "checker")
               (:file "javascript-target")
               (:file "typescript-target")
               (:file "worker-target")
               (:file "json-target")
               (:file "toml-target")
               (:file "css-target")
               (:file "redwood-target")
               (:file "gen-helpers")
               (:file "convert")
               #+hactar-codegen-lsp (:file "lsp")))
