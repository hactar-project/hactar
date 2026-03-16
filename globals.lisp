;; holds all the state for a hactar instance
;; mutable globals are ideal because we want to scope state to each hactar instance anyway
(in-package :hactar)

;;* General State
(defvar *hactar-version* "0.1.0" "The version of the Hactar application.")

(defvar *debug-stream* nil
                       "Stream for debug output.")

(defvar *debug* nil
                "Enable debug output.")

(defvar *silent* nil
                 "Suppress non-essential output when T.")

(defvar *in-repl* nil "True if currently inside the interactive REPL.")

(defvar *in-editor* nil "True if hactar is being run inside an editor (e.g. Emacs, Vim). Set via --in-editor flag or HACTAR_IN_EDITOR env var.")

(defvar *editor-log-file* nil "Pathname to the .hactar.{pid}.log file used for structured output in editor mode.")
(defvar *sqlite-vec-path* (uiop:getenv "SQLITE_VEC_PATH") "Path to the sqlite vec dynamic lib e.g ~/.local/share/sqlite-vec/vec0.so")

;;* Project Metadata
(defvar *name* nil "The name of the project.")
(defvar *author* (uiop:getenv "HACTAR_AUTHOR") "The author of the project.")

(defvar *repo-root* nil "The root directory of the repository being watched.")

(defvar *stack* '()
                "A list of technologies used in the stack")

(defvar *stack-changed-hook* (make-instance 'nhooks:hook)
  "Hook run when the stack changes. Handlers receive the new stack as argument.")

;; Install a handler to refresh generators/patterns when stack changes
(nhooks:add-hook *stack-changed-hook*
                 (make-instance 'nhooks:handler
                                :fn (lambda (stack)
                                      (declare (ignore stack))
                                      ;; Only refresh if gen.lisp has been loaded
                                      (when (fboundp 'refresh-generators)
                                        (funcall 'refresh-generators))
                                      (when (fboundp 'refresh-patterns)
                                        (funcall 'refresh-patterns)))
                                :name 'refresh-generators-on-stack-change))

(defvar *shell* (or (uiop:getenv "HACTAR_SHELL")
		    (uiop:getenv "SHELL")
		    "bash")
                "The shell being used on the system")

(defvar *language* "unknown"
                   "The main language being used in the project")

(defvar *repo-map* nil
                   "A map of all the symbols and tags in the current project.
Generated using tree-sitter")

;;* Models
(nhooks:define-hook-type model-changed (function (t t) t)
  "Hook run when the current model changes. Handler is called with (new-model old-model).")
(defvar *model-changed-hook* (make-instance 'hook-model-changed)
  "Hook run when *current-model* changes.")

(defvar *current-model* nil
  "The current model being used.")

(defvar *embedding-model* "nomic-embed-text"
  "Model to use for generating embeddings.")

(defvar *completion-model* (or (uiop:getenv "HACTAR_COMPLETION_MODEL")
			       "ollama/minimax-m2.5:cloud")
                           "Model configuration specifically for the /complete command and endpoint.")

(defvar *chunking-llm* (or (uiop:getenv "HACTAR_CHUNKING_MODEL")
			   "ollama/minimax-m2.5:cloud")
  "Model configuration specifically for chunk processing tasks.")

(defvar *cheap-model* (or (uiop:getenv "HACTAR_CHEAP_MODEL")
			  "ollama/minimax-m2.5:cloud")
  "The cheap model to use when cost is more of a concern.")

(defvar *docs-meta-model* (or (uiop:getenv "HACTAR_DOCS_META_MODEL")
			      (uiop:getenv "HACTAR_CHEAP_MODEL")
			      *cheap-model*)
  "Model to use for generating documentation metadata. Defaults to ollama/minimax-m2.5:cloud.")

(defvar *available-models* nil
                           "List of available models.")

;;* Chat & Context
(defvar *chat-history* '()
  "The chat history.")

(defvar *chat-history-limit* 8000
                             "Maximum character limit for chat history.")

(defvar *multiline-mode* nil
                         "Whether multiline mode is enabled.")

(defvar *transcript-file* ".hactar.transcript.json"
                          "File to save chat transcript to.")

(defvar *file-watcher* nil "The global file watcher instance.")

(defvar *files* nil
                "Files currently in the context window. Synchronized with context file.")

(defvar *images* '()
                 "List of images currently in the context. Each element is a plist (:path :text :mime-type :base64-data).")

(defvar *docs-context* '()
                       "List of documentation plists currently added to the context.")

(defvar *docs* nil
        "Global list of defined documentation.")

(defvar *docs-folder* "docs/"
                      "Default folder to look for documentation files.")

(defvar *errors-context* '()
                       "List of error plists currently added to the context.")

(defvar *errors* nil
        "Global list of defined errors.")

;;* Paths & Configuration
(defun get-xdg-config-dir ()
  "Returns the user's XDG configuration directory pathname (e.g., ~/.config/)."
  (uiop:xdg-config-home))

(defun get-xdg-data-dir ()
  "Returns the user's XDG data directory pathname (e.g., ~/.local/share/)."
  (uiop:xdg-data-home))

(defvar *hactar-repo-url* (or (uiop:getenv "HACTAR_REPO_URL")
			      "git@github.com:hactar-project/hactar.git"))

(defvar *hactar-repo-dir* (or (uiop:getenv "HACTAR_REPO_DIR")
			      (uiop:subpathname (get-xdg-data-dir) "hactar-repo/")))

(defvar *hactar-data-path* (or (uiop:getenv "HACTAR_DATA_PATH")
			       (uiop:subpathname (get-xdg-data-dir) "hactar/"))
  "Path to Hactar's data dir")

(defvar *hactar-config-path* (or (uiop:getenv "HACTAR_CONFIG_PATH")
				 (uiop:subpathname (get-xdg-config-dir) "hactar/"))
  "Path to Hactar's configuration directory.")
(defvar *db-path* (or (uiop:getenv "HACTAR_DB_PATH")
                      (uiop:subpathname *hactar-data-path*  "hactar.db"))
  
  "Path to the SQLite database file.")


(defvar *hactar-pro-path* (or (uiop:getenv "HACTAR_PRO_PATH")
                              (uiop:subpathname *hactar-data-path* "pro/"))
                          "Path to the Hactar Pro content repository.")

(defvar *hactar-starters-agent* (or (uiop:getenv "HACTAR_STARTERS_AGENT_PATH")
                                    (uiop:subpathname *hactar-data-path* "starters/AgentStarter.org"))
  "Path to the default Agent starter template (AgentStarter.org).")

;;* Limits & Constraints
(defvar *max-content-chars* 30000
                            "Maximum character length for content before splitting.")

(defvar *image-max-size-mb* 1
                            "Maximum size for an image file in megabytes before warning.")

(defvar *guide-warn-chars* 30000
                           "Character limit for guide content before warning.")

(defvar *guide-max-chars* 100000
                          "Maximum character limit for guide content.")

;;* Git & File System
(defvar *git-autocommit* t
                         "Enable automatic git commits after applying changes.")

(defvar *hactar-ignored-paths* '("^\\./straight/repos/.*")
                                "List of regex patterns for paths to ignore (treated as git-ignored). Paths are relative to repo root.")

;;* Ctags
(defvar *exclude-from-ctags* '(".git" "node_modules" "dist" "build" "straight" "*.min.js" "*.map")
                             "List of patterns to exclude from ctags indexing.")

(defvar *ctags-file* ".tags"
                     "Path to the ctags file relative to repo root.")

(defvar *tags-cache* nil
                     "Cache of loaded tags.")

;;* Guides & Rules
(defvar *active-rules* (make-hash-table :test 'equal)
                       "Hash table storing the text of currently active rules, keyed by rule name.")

(defvar *active-guide-file* nil
                            "Pathname of the currently active guide file.")

(defvar *guide-file-extension* "org"
                               "Default file extension for generated guides (e.g., 'org', 'md').")

(defvar *guide-exclude-tags* '("nocontext")
                             "List of tags to exclude headlines from the active guide file.")

(defvar *hactar-guide-path* (uiop:getenv "HACTAR_GUIDE_PATH")
                            "Path to the guide file, set via env var or config.")

;;* Tools
(defvar *defined-tools* (make-hash-table :test 'equal)
                        "Hash table storing defined tools. Key: tool name (string). Value: plist (:name :schema :fn :permissions).")

(defvar *tool-use-enabled* t
  "When T, tools are available for use by the LLM.")

(defvar *tools-in-system-prompt* (let ((env-val (uiop:getenv "HACTAR_TOOLS_IN_SYSTEM_PROMPT")))
                                   (if env-val
                                       (not (string= env-val "false"))
                                       t))
  "When T, include tool definitions in the system prompt (XML format).
   When NIL, pass tools to the LLM API using native tool calling.
   Default is T to use system prompt method.")

(defvar *hactar-skills-path* (or (uiop:getenv "HACTAR_SKILLS_PATH")
                                 (uiop:subpathname *hactar-data-path* "skills/"))
  "Path to the skills directory.")

(defvar *skills-file-ext* "md"
  "Default file extension for skill files.")

;;* Permissions
(defvar *permission-rules* '()
  "Ordered list of permission-rule structs, checked highest priority first.")

(defvar *session-overrides* '()
  "List of session-override structs created from interactive confirmations.")

(defvar *permission-log* '()
  "Log of recent permission decisions for debugging.")

(defvar *safe-command-patterns* '()
  "List of regex patterns for commands considered safe (read-only).")

(defvar *permission-log-max* 100
  "Maximum number of entries to keep in the permission log.")

;;** Agent State
(defvar *agent-definitions* (make-hash-table :test 'equal)
                            "Hash table storing agent definitions keyed by name.")
(defvar *running-agents* (make-hash-table :test 'equal)
                         "Hash table storing active agent instances, keyed by a unique ID.")
(defvar *agent-retry-limit* 10
                            "Default retry limit for agents.")
(defvar *live-dangerously* nil
                           "Set to T to disable safety checks and allow agents to run without a safe environment.")
(defvar *agent-safe-env* nil
                          "Set to T if running in a container or other safe environment, allowing agents to run.")

;; Auto-features state
(defvar *auto-lint* nil "Enable/disable automatic linting agent.")
(defvar *auto-test* nil "Enable/disable automatic testing agent.")
(defvar *auto-typecheck* nil "Enable/disable automatic type checking agent.")
(defvar *auto-docs* nil "Enable/disable automatic documentation features.")
(defvar *auto-suggest-commands* nil "Enable/disable automatic command suggestion.")
(defvar *auto-cmds* nil "Enable/disable automatic execution of shell commands.")

;;** Assistant Mode State
(defvar *assistant-mode-active* nil "Is the assistant mode currently active?")
(defvar *assistant-extraction* nil "The last text extracted by the assistant mode LLM.")
(defvar *assistant-last-screenshot-path* nil "Pathname of the last screenshot taken by the assistant.")
(defvar *assistant-output-file* nil "Pathname to write assistant extractions to (if --output is used).")
(defvar *assistant-audio-enabled* nil "Is TTS audio generation enabled for assistant mode?")
(defvar *assistant-audio-muted* nil "Is assistant mode audio output currently muted?")
(defvar *assistant-last-audio-file* nil "Pathname of the last TTS audio file generated.")
(defvar *piper-model-path* (or (uiop:getenv "PIPER_MODEL_PATH")
                              (uiop:subpathname (get-xdg-config-dir) "hactar/speech/models/en_US-amy-low.onnx"))
                           "Path to the Piper TTS model. Defaults to ~/.config/hactar/speech/models/en_US-amy-low.onnx.")
(defvar *assistant-initial-delay-done* nil "Has the initial 30s delay for assistant mode passed?")
(defvar *assistant-previous-image-description* "Screenshot of the currently focused window." "Default description for assistant screenshots.")


;;** AI Comment Analyzer State
(defvar *ai-comment-queue* '()
                           "Queue of files with AI! comments to process.")
(defvar *ai-comment-processor-lock* (bt:make-lock "ai-comment-processor-lock")
                                    "Lock to ensure single-threaded AI! comment processing.")

;;** HTTP Server State
(defvar *http-port* 4269
                    "Port for the HTTP server.")
(defvar *http-server* nil
                      "Instance of the running Clack server.")

;;** Slynk Server State
(defvar *slynk-port* 4005
  "Port for the Slynk server.")
(defvar *slynk-started* nil
  "T if Slynk server has been started.")



;;** Watcher configuration and state
(defvar *watcher-definitions* (make-hash-table :test 'equal)
                              "Hash table storing watcher definitions keyed by name.")
(defvar *active-watchers* (make-hash-table :test 'equalp) ; Keyed by process-info object
                          "Hash table storing active watcher instances.")
(defvar *test-command* "make test" "Default command to run for the test watcher.")
(defvar *lint-command* nil "Lint command to run for the lint agent (from config or derived from stack).")
(defvar *typecheck-command* nil "Typecheck command to run for the typecheck agent (from config or derived from stack).")
(defvar *test-agent-command* nil "Test command to run for the test agent (from config or derived from stack).")

(defvar *exposed-context-file* nil
  "Pathname of the exposed context file (hactar.{pid}.context.org).")

(defvar *context-expose-hooks-installed* nil
  "Whether context.expose hooks are installed.")

(defvar *litmode-enabled* nil
  "When T, literate single-file mode is active and provides context.")

;;** Session Management State
(defvar *sessions-dir* nil
  "Directory for storing session files. Set per-project to .hactar/sessions/ under *repo-root*.")

(defvar *global-sessions-dir* (uiop:subpathname (get-xdg-data-dir) "hactar/sessions/")
  "Global directory for storing session files (not project-specific).")

(defvar *current-session-name* nil
  "Name of the currently loaded session, or NIL if no session is active.")

(defvar *auto-save-session* nil
  "When T, automatically save session on REPL exit and restore on next start.")

(defvar *session-auto-save-name* ".autosave"
  "Name used for the auto-saved session.")

;;** Preset System State
(defvar *presets* (make-hash-table :test 'equal)
  "Hash table of defined presets, keyed by name.")

(defvar *active-presets* '()
  "List of currently active preset names, in order of activation.")

(defvar *snapshots* (make-hash-table :test 'equal)
  "Hash table of saved context snapshots, keyed by name.")

(defvar *preset-search-paths* '()
  "List of directories to search for preset files.")

(defvar *snapshots-dir* (uiop:subpathname (get-xdg-data-dir) "hactar/snapshots/")
  "Directory for storing context snapshots.")

;;** Code-Value System State
(defvar *code-values* (make-hash-table :test 'equal)
  "Registry of all code-values by ID or name.")

(defvar *code-value-counter* 0
  "Counter for generating unique code-value IDs.")

(defvar *staged-code-values* '()
  "List of code-values staged for application.")

(defvar *code-value-history* '()
  "History of applied code-value operations for undo support.")

(defvar *code-value-history-limit* 100
  "Maximum number of operations to keep in history.")

(defvar *transforms* (make-hash-table :test 'equal)
  "Registry of defined transforms by name.")

(defvar *framework-analyzers* (make-hash-table :test 'equal)
  "Registry of framework-specific analyzers.")

(defvar *recipes* (make-hash-table :test 'equal)
  "Registry of defined recipes by name.")

(defvar *template-search-paths* (let ((env-paths (uiop:getenv "HACTAR_TEMPLATE_SEARCH_PATHS")))
                                  (if env-paths
                                      (mapcar #'uiop:ensure-directory-pathname (uiop:split-string env-paths :separator " "))
                                      '()))
  "List of directories to search for templates, in priority order.")

;;** Lisp-RPC State
(defvar *lisp-rpc-mode* nil "T when running in Lisp-RPC mode (--lisp flag). LLM returns Lisp forms for evaluation.")
(defvar *lisp-rpc-pending-permission* nil "Plist of the current pending permission request, or nil.")
(defvar *lisp-rpc-permission-lock* nil "Lock for permission condition variable.")
(defvar *lisp-rpc-permission-cv* nil "Condition variable for permission response signalling.")
(defvar *lisp-rpc-permission-timeout* 120 "Seconds to wait for a permission response before auto-denying.")

;;** Process History Hook
(defvar *process-history-hook*
  (make-instance 'nhooks:hook)
  "Hook run after chat history is updated with an LLM response. Handlers receive the full history list.")

;;** AgentShell State
(defvar *agentshell-mode* nil "T when running in AgentShell (ACP client) mode.")

;;** MCP (Model Context Protocol) State
(defvar *mcp-mode* nil "T when running as an MCP server over stdio.")
(defvar *mcp-initialized* nil "T after the MCP initialize handshake is complete.")

;;** ACP (Agent Client Protocol) State
(defvar *acp-mode* nil "T when running in ACP stdio mode.")
(defvar *acp-session-id* nil "The current ACP session ID.")
(defvar *acp-client-capabilities* nil "Alist of client capabilities from initialize.")
(defvar *acp-pending-requests* (make-hash-table :test 'equal)
  "Hash table of pending outbound requests to Client, keyed by request ID string.")
(defvar *acp-request-counter* 0 "Counter for generating unique outbound request IDs.")
(defvar *acp-cancelled* nil "T when the current prompt turn has been cancelled.")
(defvar *acp-initialized* nil "T after the initialize handshake is complete.")
(defvar *lisp-mode-enabled* nil "T when lisp mode enabled")
