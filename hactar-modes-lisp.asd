(asdf:defsystem #:hactar-modes-lisp
  :description "Common Lisp and self-hosting Hactar content pack."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :pathname "src/packs/modes-lisp/"
  :components ((:file "pack")))
