(asdf:defsystem #:hactar-mcp
  :description "Model Context Protocol (MCP) server and client over hactar-jsonrpc."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:hactar-jsonrpc #:uiop)
  :serial t
  :pathname "src/mcp/"
  :components ((:file "packages")
               (:file "mcp")))
