(asdf:defsystem #:hactar-modes-services
  :description "Services & skills content pack (Stripe, Claude skills, GPT, npm, GitHub)."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :pathname "src/packs/modes-services/"
  :components ((:file "pack")))
