(in-package :hactar-user)

(defmode shell
  :when t
  :rules "Prefer POSIX sh compatibility, quote variables, and fail on unset/errors."
  :skills (shell))

(defmode agentshell
  :when t
  :rules "Emit clean structured shell summaries suitable for editor/agent-shell parsing.")

(defskill shell :doc "Shell scripting and CLI composition conventions.")
(defprompt generate-shell-command (query)
  "Generate a safe, portable POSIX shell command for: {{query}}")
