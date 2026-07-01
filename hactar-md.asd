(asdf:defsystem #:hactar-md
  :description "Minimal Markdown parsing and terminal rendering for Hactar."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:cl-ppcre #:uiop)
  :serial t
  :pathname "src/md/"
  :components ((:file "packages") (:file "md")))
