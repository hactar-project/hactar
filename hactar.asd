(asdf:defsystem #:hactar
  :description "Hactar is an AI pair programmer."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:cl-ppcre
	       #:cl-csv
	       #:cxml
	       #:uuid
	       #:sqlite
               #:hactar-migrations
               #:llm
	       #:chunga
               #:cl-readline
               #:cl-yaml
	       #:serapeum
               #:uiop
	       #:cl-mustache
	       #:fuzzy-match
	       #:cl-toml
	       #:cl-async
               #:str
               #:usocket
               #:lparallel
               #:cffi
	       #:cl-json
	       #:cl-fad
               #:shasht
               #:local-time
	       #:flexi-streams
               #:alexandria
               #:cl-base64
               #:slynk
               #:clingon
               #:ningle
               #:woo
               #:clack
               #:lack
               #:shasht
               #:imago
               #:cl-base64
               #:alexandria
               #:str
	       #:cl-charms
               #:cl-ppcre
	       #:cmark)
  :components ((:file "packages")
	       ;; core extras
	       (:file "hooks")
	       ;; state
	       (:file "state")
	       (:file "config")
	       ;; utils and backing libs
	       (:file "utils")
	       (:file "org-mode-parser")
	       (:file "org-mode")
	       (:file "markdown")
	       ;; core
	       (:file "tui")
	       (:file "db")
	       (:file "rag")
	       (:file "commands")
	       (:file "permissions")
	       (:file "tools")
	       (:file "docs")
	       (:file "errors")
	       (:file "watchers")
	       (:file "processors")
	       (:file "analyzers")
	       (:file "rules")
	       (:file "agents")
	       (:file "check")
	       (:file "dots")
	       (:file "context")
               (:file "code-value")
               (:file "entity")
               (:file "entity-macros")
               (:file "entity-storage")
               (:file "entity-types")
               (:file "gen")
	       ;; interfaces and apis
	       (:file "api-http")
	       (:file "api-complete")
	       ;; routing system - must come before web commands
	       (:file "router")
	       ;; ctags and code indexing
	       (:file "ctags")
	       ;; web commands
	       (:file "web")
	       (:file "import")
	       (:file "copilot")
               (:file "feature")
	       ;; modes
	       (:file "npm")
	       (:file "sh-mode")
	       (:file "js-mode")
	       (:file "react-mode")
	       (:file "react-router")
	       (:file "hactar-mode")
	       (:file "stripe-mode")
	       (:file "sinatra-mode")
	       (:file "assistant")
	       (:file "pro")
	       (:file "create")
	       (:file "preset")
	       (:file "skills-mode")
	       (:file "lisp-mode")
	       (:file "gpt-mode")
	       (:file "mcp")
	       (:file "acp")
	       (:file "to-org")
	       (:file "lisp-rpc-protocol")
	       (:file "lisp-rpc")
	       (:file "lisp-rpc-api")
	       (:file "hactar-monolith")
	       (:file "session")
	       (:file "litmode")
	       (:file "hyperfractal")
	       (:file "ruhe")
	       (:file "hactar"))         ; Compile main hactar file last
  :build-operation "program-op"
  :build-pathname "hactar"
  :entry-point "hactar:main")
