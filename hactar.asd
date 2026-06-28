(asdf:defsystem #:hactar-core
  :description "Hactar core engine and library."
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
	       (:file "diffusion-prompts")
	       (:file "lisp-form-parser")
	       (:file "fern")
	       (:file "synth")
	       (:file "org-mode-parser")
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
	       ;; modes
	       (:file "npm")
	       (:file "sh-mode")
	       (:file "js-mode")
	       (:file "hypertext")
	       (:file "react-mode")
	       (:file "redwood-mode")
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
	       (:file "to-org")
	       (:file "lisp-rpc-protocol")
	       (:file "lisp-rpc")
	       (:file "lisp-rpc-api")
	       (:file "session")
	       (:file "slynk")
	       (:file "wiki")
	       (:file "project-cli")))

(asdf:defsystem #:hactar
  :description "Hactar is an AI pair programmer."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core #:hactar-codegen #:ruhe #:hyperfractal)
  :serial t
  :components ((:file "hactar"))
  :build-operation "program-op"
  :build-pathname "hactar/build/hactar"
  :entry-point "hactar:main")
