(asdf:defsystem #:hactar-rag
  :description "In-memory embedding index and retrieval over hactar-llm."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-llm #:uiop)
  :serial t
  :pathname "src/rag/"
  :components ((:file "packages")
               (:file "rag")))
