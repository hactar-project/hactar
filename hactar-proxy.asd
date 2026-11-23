(asdf:defsystem #:hactar-proxy
  :description "OpenAI/OpenRouter-compatible LLM proxy injecting Hactar context."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:hactar-llm #:clack #:clack-handler-hunchentoot
               #:dexador #:shasht #:babel #:uiop)
  :serial t
  :pathname "src/proxy/"
  :components ((:file "packages")
               (:file "proxy")))
