(asdf:defsystem #:hactar-watch
  :description "Portable pull-based filesystem events with callback composition."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:uiop)
  :serial t
  :pathname "src/watch/"
  :components ((:file "packages")
               (:file "watch")))
