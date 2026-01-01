(asdf:defsystem #:hactarsh
  :description "A simple shell for Hactar"
  :author "K-2052"
  :license "MIT"
  :depends-on (#:cl-readline)
  :components ((:file "hactarsh")))
