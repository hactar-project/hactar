(asdf:defsystem #:hactar
  :description "Hactar executable: loads hactar-core, bootstraps, dispatches."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:hactar-tui
               #:hactar-modes-web #:hactar-modes-ruby #:hactar-modes-systems
               #:hactar-modes-lisp #:hactar-modes-shell #:hactar-modes-services
               #:hactar-starters)
  :serial t
  :pathname "src/cli/"
  :components ((:file "packages")
               (:file "main"))
  :build-operation "program-op"
  :build-pathname "hactar/build/hactar"
  :entry-point "hactar:main")
