(asdf:defsystem #:hyperfractal
  :description "Hyperfractal: Plaintext web browser wrapper for Hactar."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:hactar-llm #:hactar-md #:hactar-org
               #:cl-ppcre #:str #:uiop)
  :serial t
  :pathname "src/hyperfractal/"
  :components ((:file "packages")
               (:file "hyperfractal")))
