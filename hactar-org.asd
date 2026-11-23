(asdf:defsystem #:hactar-org
  :description "Minimal org-mode parsing and rendering for Hactar."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:uiop)
  :serial t
  :pathname "src/org/"
  :components ((:file "packages")
               (:file "org")
               (:file "document")))
