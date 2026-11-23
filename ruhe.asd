(asdf:defsystem #:ruhe
  :description "Ruhe: A webfeed generator composed from hactar-core, hactar-llm, and hactar-org."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:hactar-llm #:hactar-org #:hactar-md
               #:cl-ppcre #:str #:uiop)
  :serial t
  :pathname "src/ruhe/"
  :components ((:file "packages")
               (:file "ruhe")))
