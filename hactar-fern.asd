(asdf:defsystem #:hactar-fern
  :description "Fern: Hactar's native AST/structural-editing backend (Lisp backend)."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:uiop)
  :serial t
  :pathname "src/fern/"
  :components ((:file "packages")
               (:file "fern")
               (:file "code-value")
               (:file "symbols")))
