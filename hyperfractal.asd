(asdf:defsystem #:hyperfractal
  :description "Hyperfractal: Plaintext web browser wrapper for Hactar"
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :serial t
  :components ((:file "hyperfractal")))
