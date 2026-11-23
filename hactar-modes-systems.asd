(asdf:defsystem #:hactar-modes-systems
  :description "Systems/backend language content pack (Python, Go, Rust)."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :pathname "src/packs/modes-systems/"
  :components ((:file "pack")))
