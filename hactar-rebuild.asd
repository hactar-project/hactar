(asdf:defsystem #:hactar-rebuild
  :description "Greenfield Hactar rebuild: engine + closed DSL surface."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:cl-ppcre #:str #:uiop)
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
               (:file "bootstrap")
               (:file "dispatcher")
               (:file "main"))
  :build-operation "program-op"
  :build-pathname "../../hactar/build/hactar"
  :entry-point "hactar-rebuild:main")
