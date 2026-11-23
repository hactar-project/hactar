(asdf:defsystem #:hactar-pro
  :description "Optional Hactar Pro content-repository client."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:uiop)
  :serial t
  :pathname "src/pro/"
  :components ((:file "packages")
               (:file "pro")))
