(asdf:defsystem #:hactar-llm-live
  :description "Live HTTP/SSE provider clients and OAuth transport for hactar-llm."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-llm #:dexador #:shasht #:flexi-streams #:babel #:uiop)
  :serial t
  :pathname "src/llm-live/"
  :components ((:file "packages")
               (:file "live")))
