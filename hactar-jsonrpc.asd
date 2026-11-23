(asdf:defsystem #:hactar-jsonrpc
  :description "Composable JSON-RPC 2.0 framing, dispatch, and clients."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:shasht #:babel #:uiop)
  :serial t
  :pathname "src/jsonrpc/"
  :components ((:file "packages")
               (:file "jsonrpc")))
