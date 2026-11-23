(asdf:defsystem #:hactar-llm
  :description "Provider-agnostic LLM wrapper for Hactar (with offline mock client)."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:uiop)
  :serial t
  :pathname "src/llm/"
  :components ((:file "packages")
               (:file "llm")
               (:file "models")
               (:file "oauth")))
