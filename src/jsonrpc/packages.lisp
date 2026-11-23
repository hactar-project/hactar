(defpackage #:hactar-jsonrpc
  (:use #:cl)
  (:export #:+parse-error+ #:+invalid-request+ #:+method-not-found+
           #:+invalid-params+ #:+internal-error+
           #:rpc-error #:rpc-error-code #:rpc-error-message #:rpc-error-data
           #:server #:make-server #:register-method #:dispatch-message #:serve
           #:client #:make-client #:call #:notify
           #:make-request #:make-response #:make-error-response
           #:make-notification #:encode-message #:decode-message
           #:write-message #:read-message))
