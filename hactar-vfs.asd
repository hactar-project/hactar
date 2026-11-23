(asdf:defsystem #:hactar-vfs
  :description "Inspect-first staged filesystem plans."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:uiop #+sbcl #:sb-posix)
  :serial t
  :pathname "src/vfs/"
  :components ((:file "packages")
               (:file "vfs")))
