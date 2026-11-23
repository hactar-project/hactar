(asdf:defsystem #:hactar-http
  :description "HTTP API adapter (Clack) for editor extensions and remote control."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:clack #:clack-handler-hunchentoot #:shasht #:babel #:uiop)
  :serial t
  :pathname "src/http/"
  :components ((:file "packages")
               (:file "http")))
