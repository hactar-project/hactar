(asdf:defsystem #:hactar-sandbox
  :description "Sandbox policy data and explicit subprocess enforcement."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:uiop)
  :serial t
  :pathname "src/sandbox/"
  :components ((:file "packages")
               (:file "sandbox")))
