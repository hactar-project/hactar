(asdf:defsystem #:llm
  :description "A Common Lisp LLM backend library"
  :author "K-2052"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "llm"))
  :depends-on (:dexador :drakma :cl-json :shasht :flexi-streams :babel :str :fiveam :mockingbird :cl-readline :cl-ppcre :chunga :alexandria)
  :in-order-to ((test-op (test-op "llm-tests"))))
