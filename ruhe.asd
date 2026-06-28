(asdf:defsystem #:ruhe
  :description "Ruhe: A webfeed generator for Hactar"
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :serial t
  :components ((:file "ruhe")))
