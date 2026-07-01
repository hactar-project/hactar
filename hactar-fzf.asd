(asdf:defsystem #:hactar-fzf
  :description "Pure fuzzy ranking and non-interactive selection for Hactar."
  :author "K-2052"
  :license "MIT"
  :serial t
  :pathname "src/fzf/"
  :components ((:file "packages") (:file "fzf")))
