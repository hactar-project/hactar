(asdf:defsystem #:hactar-modes-web
  :description "Web stack content pack (Next.js, React Router, React, Redwood, JS)."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :pathname "src/packs/modes-web/"
  :components ((:file "pack")))
