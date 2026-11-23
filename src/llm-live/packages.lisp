(defpackage #:hactar-llm-live
  (:use #:cl)
  (:export #:*http-request-fn* #:*default-model* #:*ollama-endpoint*
           #:*provider-configs* #:payload->json #:json->alist
           #:build-provider-request #:extract-provider-response
           #:parse-stream-line #:llm-stream-reader #:make-stream-reader
           #:llm-stream-reader-closed-p #:read-next-chunk #:close-reader
           #:provider-complete #:ollama-embed
           #:get-copilot-token #:fetch-copilot-token #:save-copilot-token
           #:make-live-client #:enable-live-client!))
