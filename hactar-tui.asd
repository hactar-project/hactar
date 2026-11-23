(asdf:defsystem #:hactar-tui
  :description "Presentation-only terminal primitives for Hactar programs."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-md #:hactar-fzf #:cl-readline #:uiop)
  :serial t
  :pathname "src/tui/"
  :components ((:file "packages")
               (:file "themes")
               (:file "ansi")
               (:file "tui")))
