(asdf:defsystem #:hactar
  :description "Hactar is an AI pair programmer."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:cl-ppcre
	       #:cl-csv
	       #:cxml
	       #:uuid
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
               #:lack-request
               #:shasht
               #:imago
               #:cl-base64
               #:alexandria
               #:str
	       #:cl-charms
               #:cl-ppcre
	       #:cmark)
  :serial t
  :components ((:file "packages")
	       ;; core extras
	       (:file "hooks")
	       ;; state
	       (:file "state")
	       (:file "config")
	       ;; utils and backing libs
	       (:file "git")
	       (:file "utils")
	       (:file "formats")
	       (:file "prompts")
	       (:file "org-mode-parser")
	       (:file "org-mode")
	       (:file "markdown")
	       (:file "md-render")
	       ;; core
	       (:file "commands")
	       (:file "models")
	       (:file "debug")
	       (:file "tui")
	       (:file "tui-theme")
	       (:file "fzf")
	       (:file "hydra")
	       (:file "llm-utils")
	       (:file "rag")
	       (:file "permissions")
	       (:file "tools")
	       (:file "docs")
	       (:file "errors")
	       (:file "watchers")
	       (:file "processors")
	       (:file "analyzers")
	       (:file "rules")
	       (:file "gen-helpers")
	       (:file "mode")
	       (:file "modes/rails")
	       (:file "agents")
	       (:file "check")
	       (:file "dots")
	       (:file "context")
	       (:file "interface")
	       (:file "history")
	       (:file "persist")
               (:file "code-value")
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
	       (:file "oauth")
	       (:file "proxy")
	       ;; compiler
	       (:file "vfs")
	       (:file "compiler")
	       (:file "checker")
	       (:file "javascript-target")
	       (:file "typescript-target")
	       (:file "worker-target")
	       (:file "json-target")
	       (:file "toml-target")
	       (:file "css-target")
	       (:file "redwood-target")
	       (:file "lsp")
	       (:file "convert")
	       ;; modes
	       (:file "npm")
	       (:file "sh-mode")
	       (:file "js-mode")
	       (:file "hypertext")
	       (:file "react-mode")
	       (:file "react-router")
	       (:file "hactar-mode")
	       (:file "stripe-mode")
	       (:file "sinatra-mode")
	       (:file "assistant")
	       (:file "pro")
	       (:file "create")
	       (:file "starters")
	       (:file "preset")
	       (:file "skills-mode")
	       (:file "lisp-mode")
	       (:file "gpt-mode")
	       (:file "mcp")
	       (:file "acp")
	       (:file "molds")
	       (:file "to-org")
	       (:file "lisp-rpc-protocol")
	       (:file "lisp-rpc")
	       (:file "lisp-rpc-api")
	       (:file "hactar-monolith")
	       (:file "session")
	       (:file "litmode")
	       (:file "hyperfractal")
	       (:file "ruhe")
	       (:file "slynk")
	       (:file "wiki")
	       (:file "hactar"))
  :build-operation "program-op"
  :build-pathname "hactar"
  :entry-point "hactar:main")
