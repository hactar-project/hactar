(asdf:defsystem #:hactarsh
  :description "A simple interactive shell composing hactar-tui and hactar-core."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:hactar-tui #:uiop)
  :serial t
  :pathname "src/hactarsh/"
  :components ((:file "packages")
               (:file "hactarsh")))
