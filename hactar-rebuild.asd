(asdf:defsystem #:hactar-rebuild
  :description "Greenfield Hactar rebuild foundation."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:cl-ppcre #:str #:uiop)
  :serial t
  :components ((:file "src/rebuild/packages")
               (:file "src/rebuild/result")
               (:file "src/rebuild/state")
               (:file "src/rebuild/surface")
               (:file "src/rebuild/generated-file")))
