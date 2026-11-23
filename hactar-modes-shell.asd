(asdf:defsystem #:hactar-modes-shell
  :description "Shell and agent-shell content pack."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :pathname "src/packs/modes-shell/"
  :components ((:file "pack")))
