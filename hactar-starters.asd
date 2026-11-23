(asdf:defsystem #:hactar-starters
  :description "Project starters and template prompts content pack."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :pathname "src/packs/starters/"
  :components ((:file "pack")))
