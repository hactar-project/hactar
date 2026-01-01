(in-package :hactar)
;;** Model configuration
(defstruct model-config
  name
  provider
  model-name
  edit-format
  use-repo-map
  examples-as-sys-msg
  extra-params
  max-output-tokens
  max-input-tokens
  cache-control
  input-cost-per-token
  output-cost-per-token
  supports) ; List of supported features like "vision"

(defun load-project-config (project-dir)
  "Loads project-specific configuration from .hactar.toml in the given directory."
  (let ((config-file (merge-pathnames #P".hactar.toml" project-dir)))
    (when (probe-file config-file)
      (unless *silent* (format t "~&Loading project config from: ~A~%" config-file))
      (handler-case
          (let* ((config-content (uiop:read-file-string config-file))
                 (config-data (cl-toml:parse config-content))
                 (project-table (gethash "project" config-data)))
            (when project-table
              (let* ((lang (gethash "language" project-table))
                     (stack (gethash "stack" project-table))
                     (author (gethash "author" project-table))
                     (test-cmd (gethash "test_command" project-table))
                     (guide-ext (gethash "guide_extension" project-table))
                     (guide-exclude (gethash "guide_exclude_tags" project-table))
                     (guide-table (gethash "guide" project-table))
                     (guide-path (when guide-table (gethash "path" guide-table)))
                     (commands-table (gethash "commands" project-table))
                     (lint-cmd (when commands-table (gethash "lint" commands-table)))
                     (typecheck-cmd (when commands-table (gethash "typecheck" commands-table)))
                     (test-agent-cmd (when commands-table (gethash "test" commands-table))))
                (when lang
                  (setf *language* lang)
                  (unless *silent* (format t "  Set language from config: ~A~%" *language*)))
                (when author
                  (setf *author* author)
                  (unless *silent* (format t "  Set author from config: ~A~%" *author*)))
                (when stack ;; Convert stack from vector (if parsed as such) to list
                  (setf *stack* (if (vectorp stack) (coerce stack 'list) stack))
                  (unless *silent* (format t "  Set stack from config: ~{~A~^, ~}~%" *stack*)))
                (when test-cmd
                  (setf *test-command* test-cmd) ; Set the test command variable
                  (unless *silent* (format t "  Set test command from config: ~A~%" *test-command*))
                  ;; Update the existing test-watcher definition if it exists
                  (let ((test-watcher-def (gethash 'test-watcher *watcher-definitions*)))
                    (when test-watcher-def
                      (setf (watcher-definition-command test-watcher-def)
                            (if (stringp test-cmd) (uiop:split-string test-cmd :separator " ") test-cmd))
                      (debug-log "Updated test-watcher command."))))
                (when lint-cmd
                  (setf *lint-command* lint-cmd)
                  (unless *silent* (format t "  Set lint command from config: ~A~%" *lint-command*)))
                (when typecheck-cmd
                  (setf *typecheck-command* typecheck-cmd)
                  (unless *silent* (format t "  Set typecheck command from config: ~A~%" *typecheck-command*)))
                (when test-agent-cmd
                  (setf *test-agent-command* test-agent-cmd)
                  (unless *silent* (format t "  Set test command from config: ~A~%" *test-agent-command*)))
                ;; Load guide extension
                (when (and guide-ext (stringp guide-ext) (not (string= guide-ext "")))
                  (setf *guide-file-extension* (string-downcase guide-ext))
                  (unless *silent* (format t "  Set guide file extension from config: ~A~%" *guide-file-extension*)))
                ;; Load guide exclude tags
                (when (and guide-exclude (vectorp guide-exclude)) ; TOML arrays are often parsed as vectors
                  (setf *guide-exclude-tags* (coerce guide-exclude 'list))
                  (unless *silent* (format t "  Set guide exclude tags from config: ~A~%" *guide-exclude-tags*)))

                ;; Load guide path
                (when guide-path
                  (setf *hactar-guide-path* guide-path)
                  (unless *silent* (format t "  Set guide path from config: ~A~%" *hactar-guide-path*)))))
            ;; Load auto settings from [project.auto]
            (let ((auto-table (gethash "auto" project-table)))
              (when auto-table
                (let ((lint (gethash "lint" auto-table))
                      (test (gethash "test" auto-table))
                      (typecheck (gethash "typecheck" auto-table))
                      (docs (gethash "docs" auto-table))
                      (suggest-commands (gethash "suggest_commands" auto-table))
                      (cmds (gethash "cmds" auto-table))
                      (all (gethash "all" auto-table))
                      (limits (gethash "limits" auto-table))
                      (tools-in-prompt (gethash "tools_in_system_prompt" auto-table)))
                  (when all
                    (setf *auto-lint* t *auto-test* t *auto-typecheck* t *auto-docs* t *auto-suggest-commands* t *auto-cmds* t)
                    (unless *silent* (format t "  Set all auto features from config: true~%")))
                  (unless (null tools-in-prompt)
                    (setf *tools-in-system-prompt* tools-in-prompt)
                    (unless *silent* (format t "  Set tools-in-system-prompt from config: ~A~%" tools-in-prompt)))
                  (unless (null lint) (setf *auto-lint* lint) (unless *silent* (format t "  Set auto-lint from config: ~A~%" lint)))
                  (unless (null test) (setf *auto-test* test) (unless *silent* (format t "  Set auto-test from config: ~A~%" test)))
                  (unless (null typecheck) (setf *auto-typecheck* typecheck) (unless *silent* (format t "  Set auto-typecheck from config: ~A~%" typecheck)))
                  (unless (null docs) (setf *auto-docs* docs) (unless *silent* (format t "  Set auto-docs from config: ~A~%" docs)))
                  (unless (null suggest-commands) (setf *auto-suggest-commands* suggest-commands) (unless *silent* (format t "  Set auto-suggest-commands from config: ~A~%" suggest-commands)))
                  (unless (null cmds) (setf *auto-cmds* cmds) (unless *silent* (format t "  Set auto-cmds from config: ~A~%" cmds)))
                  (when limits (setf *agent-retry-limit* limits) (unless *silent* (format t "  Set agent retry limit from config: ~A~%" limits))))))
            ;; Load embedding model configuration
            (let ((embedding-table (gethash "embedding" config-data)))
              (when embedding-table
                (let ((model (gethash "model" embedding-table)))
                  (when model
                    (setf *embedding-model* model)
                    (unless *silent* (format t "  Set embedding model from config: ~A~%" *embedding-model*))))))
            ;; Load paths
            (let ((paths-table (gethash "paths" config-data)))
              (when paths-table
                (let ((pro (gethash "pro" paths-table))
		      (hactar-config (gethash "hactar_config" paths-table))
                      (hactar-data (gethash "hactar_data" paths-table))
		      (hactar-repo-url (gethash "hactar_repo_url" paths-table))
		      (hactar-repo-dir (gethash "hactar_repo_dir" paths-table))
                      (database (gethash "database" paths-table))
                      (piper-model (gethash "piper_model" paths-table))
                      (templates (gethash "templates" paths-table)))
                  (when pro (setf *hactar-pro-path* pro))
		  (when hactar-config (setf *hactar-config-path* hactar-config))
                  (when hactar-data (setf *hactar-data-path* hactar-data))
		  (when hactar-repo-url (setf *hactar-repo-url* hactar-repo-url))
		  (when hactar-repo-dir (setf *hactar-repo-dir* hactar-repo-dir))
                  (when database (setf *db-path* database))
                  (when piper-model (setf *piper-model-path* piper-model))
                  (when templates
                    (setf *template-search-paths*
                          (mapcar #'uiop:ensure-directory-pathname
                                  (if (vectorp templates)
                                      (coerce templates 'list)
                                      (list templates))))))))
            ;; Load API keys
            (let ((api-keys-table (gethash "api_keys" config-data)))
              (when api-keys-table
                (let ((openai (gethash "openai" api-keys-table))
                      (anthropic (gethash "anthropic" api-keys-table))
                      (gemini (gethash "gemini" api-keys-table))
                      (openrouter (gethash "openrouter" api-keys-table)))
                  (when openai (setf llm:*openai-api-key* openai))
                  (when anthropic (setf llm:*anthropic-api-key* anthropic))
                  (when gemini (setf llm:*gemini-api-key* gemini))
                  (when openrouter (setf llm:*openrouter-api-key* openrouter)))))
            ;; Load analyzer settings
            (let ((analyzer-settings (gethash "analyzers" config-data)))
              (when (and analyzer-settings (listp analyzer-settings)) ; Ensure it's a TOML array (parsed as list)
                (dolist (analyzer-spec analyzer-settings)
                  (when (hash-table-p analyzer-spec) ; Ensure it's a table
                    (let ((name-str (gethash "name" analyzer-spec))
                          (enable (gethash "enable" analyzer-spec)))
                      (when (and name-str (stringp name-str) (not (null enable))) ; Check type and presence
                        (let ((analyzer-sym (ignore-errors (intern (string-upcase name-str) :hactar))))
                          (if (and analyzer-sym (gethash analyzer-sym *analyzer-registry*))
                              (progn
				(set-analyzer-enabled analyzer-sym enable)
				(unless *silent* (format t "  Set analyzer '~A' enabled status from config: ~A~%" name-str enable)))
                              (warn "Unknown analyzer specified in project config: ~A" name-str))))))))))
        (error (e)
          (format t "~&Error loading project config file ~A: ~A~%" config-file e)))))
  (resolve-active-guide-file project-dir))

(defun validate-model-config (model-data)
  "Validate model configuration and apply defaults."
  (let ((name (gethash "name" model-data)))
    (unless name
      (error "Model configuration missing required 'name' field"))

    (unless (gethash "model_name" model-data)
      (error "Model configuration missing required 'model_name' field for ~A" name))

    ;; Apply defaults if not specified
    (unless (gethash "edit_format" model-data)
      (setf (gethash "edit_format" model-data) "diff"))

    (unless (gethash "use_repo_map" model-data)
      (setf (gethash "use_repo_map" model-data) t))

    (unless (gethash "examples_as_sys_msg" model-data)
      (setf (gethash "examples_as_sys_msg" model-data) t))

    (unless (gethash "max_output_tokens" model-data)
      (setf (gethash "max_output_tokens" model-data) 8192))

    (unless (gethash "max_input_tokens" model-data)
      (setf (gethash "max_input_tokens" model-data) 32000))

    (unless (gethash "cache_control" model-data)
      (setf (gethash "cache_control" model-data) t))

    ;; Apply cost defaults if not specified
    (unless (gethash "input_cost_per_token" model-data)
      (setf (gethash "input_cost_per_token" model-data) 0.0000011)) ; Default input cost

    (unless (gethash "output_cost_per_token" model-data)
      (setf (gethash "output_cost_per_token" model-data) 0.0000044)) ; Default output cost

    ;; Ensure 'supports' is a list (default to empty)
    (unless (gethash "supports" model-data)
      (setf (gethash "supports" model-data) '()))
    (unless (listp (gethash "supports" model-data))
      (warn "Model ~A 'supports' field is not a list, ignoring." name)
      (setf (gethash "supports" model-data) '()))

    ;; Validate edit_format
    (let ((edit-format (gethash "edit_format" model-data)))
      (unless (member edit-format '("diff" "file") :test #'string=)
        (format t "Warning: Invalid edit_format '~A' for model ~A, defaulting to 'diff'~%"
                edit-format name)
        (setf (gethash "edit_format" model-data) "diff")))

    model-data))

(defun load-models-config (config-path)
  "Load model configurations from the specified path."
  (when (probe-file config-path)
    (with-open-file (stream config-path)
      (let* ((file-content (uiop:read-file-string stream))
             (config (cl-yaml:parse file-content)))
        (setf *available-models*
              (loop for model-data in config
                    for validated-data = (handler-case
                                             (validate-model-config model-data)
                                           (error (e)
                                             (format t "Error in model config: ~A~%" e)
                                            nil))
                    when validated-data
                    collect (make-model-config
                              :name (gethash "name" validated-data)
                              :provider (first (cl-ppcre:split "/" (gethash "name" validated-data)))
                              :model-name (gethash "model_name" validated-data)
                              :edit-format (gethash "edit_format" validated-data)
                              :use-repo-map (gethash "use_repo_map" validated-data)
                              :examples-as-sys-msg (gethash "examples_as_sys_msg" validated-data)
                              :extra-params (gethash "extra_params" validated-data)
                              :max-output-tokens (gethash "max_output_tokens" validated-data)
                              :max-input-tokens (gethash "max_input_tokens" validated-data)
                              :cache-control (gethash "cache_control" validated-data)
			      :input-cost-per-token (gethash "input_cost_per_token" validated-data)
                              :output-cost-per-token (gethash "output_cost_per_token" validated-data)
                              :supports (gethash "supports" validated-data))))))))
(defun %toml-escape (s)
  "Escape a string for TOML (quotes and backslashes)."
  (when s
    (let ((str (format nil "~A" s)))
      (setf str (cl-ppcre:regex-replace-all "\\\\" str "\\\\"))
      (setf str (cl-ppcre:regex-replace-all "\"" str "\\\\\""))
      str)))

(defun %toml-quote (s)
  "Quote a value as TOML string."
  (format nil "\"~A\"" (%toml-escape (or s ""))))

(defun %toml-array-of-strings (list-of-strings)
  "Render a TOML array of strings from a Lisp list of strings."
  (format nil "[~{~A~^, ~}]" (mapcar #'%toml-quote (or list-of-strings '()))))

(defun %native-path (p)
  "Convert a path (string or pathname) to a native namestring."
  (uiop:native-namestring (%to-pathname p)))

(defun %derive-language ()
  "Derive a fallback language based on the current stack if *language* is not set."
  (or *language*
      (cond
        ((member "typescript" *stack* :test #'string=) "typescript")
        ((member "javascript" *stack* :test #'string=) "javascript")
        ((member "rust" *stack* :test #'string=) "rust")
        ((member "python" *stack* :test #'string=) "python")
        (t "unknown"))))

(defun reload-config ()
  (load-models-config (get-models-config-path)))

(defun resolve-active-guide-file (project-dir)
  "Resolves and sets the *active-guide-file* based on configuration and fallbacks."
  (let ((candidate (or *hactar-guide-path*
                       (let ((org-guide (merge-pathnames ".hactar.guide.org" project-dir))
                             (md-guide (merge-pathnames ".hactar.guide.md" project-dir))
                             (agents-md (merge-pathnames "AGENTS.md" project-dir)))
                         (cond
                           ((probe-file org-guide) org-guide)
                           ((probe-file md-guide) md-guide)
                           ((probe-file agents-md) agents-md)
                           (t nil))))))
    (when candidate
      (let ((full-path (if (uiop:absolute-pathname-p candidate)
                           candidate
                           (merge-pathnames candidate project-dir))))
        (when (probe-file full-path)
          ;; Use set-active-guide if available (defined in docs.lisp), otherwise set directly
          (if (fboundp 'set-active-guide)
              (funcall 'set-active-guide full-path)
              (progn
                (setf *active-guide-file* full-path)
                (unless *silent* (format t "Activated guide: ~A~%" (uiop:native-namestring full-path))))))))))
