(defpackage #:hactar-mcp
  (:use #:cl)
  (:export #:make-mcp-server #:serve-mcp
           #:mcp-client #:connect-mcp #:disconnect-mcp
           #:mcp-list-tools #:mcp-call-tool #:mcp-tools->deftools
           #:tool->mcp-schema))
