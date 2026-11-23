(asdf:defsystem #:hactar-acp
  :description "Agent Client Protocol (ACP) adapter over hactar-jsonrpc and hactar-core."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:hactar-jsonrpc #:uiop)
  :serial t
  :pathname "src/acp/"
  :components ((:file "packages")
               (:file "acp")))
