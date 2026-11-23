(asdf:defsystem #:hactar-modes-ruby
  :description "Ruby ecosystem content pack (Rails, Sinatra)."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :pathname "src/packs/modes-ruby/"
  :components ((:file "pack")))
