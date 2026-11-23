(asdf:defsystem #:hactar-knowledge
  :description "Hypertext docs, guides, error references, wiki-as-lisp, and doc import."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-org #:hactar-md #:hactar-rag #:str #:cl-ppcre #:uiop)
  :serial t
  :pathname "src/knowledge/"
  :components ((:file "packages")
               (:file "knowledge")
               (:file "wiki")))
