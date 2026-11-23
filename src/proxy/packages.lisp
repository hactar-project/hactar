(defpackage #:hactar-proxy
  (:use #:cl)
  (:export #:*proxy-server* #:*proxy-port* #:*proxy-upstream-url*
           #:*proxy-forward-fn* #:*proxy-request-hooks* #:*proxy-response-hooks*
           #:add-proxy-request-hook #:add-proxy-response-hook
           #:extract-system-prompt #:inject-context #:reassemble-messages
           #:forward-proxy-request #:make-proxy-app
           #:start-proxy #:stop-proxy))
