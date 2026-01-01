(asdf:defsystem #:llm-tests
  :description "Tests for the llm library"
  :author "K-2052"
  :license "MIT"
  :version "0.0.1"
  :depends-on (
               #:fiveam
               #:babel
               #:str
	       #:dexador
	       #:drakma
	       #:mockingbird)
  :components ((:file "llm-tests")))
