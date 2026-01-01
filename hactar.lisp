;;* Core commands and chat/repl handling
(in-package :hactar)

(defvar *file-watcher-stopped* nil
  "Track whether STOP-FILE-WATCHER has already been requested, to prevent
   double-stopping underlying libuv watchers.")

(defun get-tools-section ()
  "Generate the tools section for the system prompt.
   Only generates output when *tools-in-system-prompt* is T."
  (when (and *tools-in-system-prompt* *tool-use-enabled*)
    (generate-all-tools-xml)))

(defun get-litmode-context-section ()
  "Generate the litmode context section for messages, if litmode is active."
  (when (and (fboundp 'litmode-active-p)
             (funcall 'litmode-active-p))
    (funcall 'generate-litmode-context)))

(defun get-entity-rules-section ()
  "Generate the entity rules section for the system prompt from BOT active rules."
  (get-active-entity-rules))

(define-command debug (args)
                "Toggle debug mode for both hactar and llm packages."
                (declare (ignore args))
                (setf *debug* (not *debug*))
                ;; Also toggle the debug flag in the llm package
                (setf llm:*debug* *debug*)
                (format t "Debug mode is now ~A for hactar and llm.~%" (if *debug* "enabled" "disabled"))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (setf *debug* (not *debug*))
                       (setf llm:*debug* *debug*)
                       `(("text" . ,(format nil "Debug mode is now ~A." (if *debug* "enabled" "disabled")))
                         ("data" . (("debug" . ,(if *debug* t :false)))))))

(define-command ask (args)
                "Ask questions about the code base without editing any files."
                (let ((question (format nil "~{~A~^ ~}" args)))
		  (get-llm-response question))
                :acp t)

;; TODO: Add automatic skills lookup and insertion
;; the main purpose of a seperate /code command is so we can have smart prompts
(define-command code (args)
                "Make changes and refactors to code."
                (let ((changes-prompt (format nil "~{~A~^ ~}" args)))
                  (get-llm-response changes-prompt))
                :acp t)

(define-command clear (args)
                "Clear the chat history."
                (declare (ignore args))
                (clear-chat-history)
                (format t "Chat history cleared.~%")
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (clear-chat-history)
                       `(("text" . "Chat history cleared."))))

(define-command compress (args)
                "Manually trigger chat history compression."
                (declare (ignore args))
                (perform-history-compression)
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (perform-history-compression)
                       `(("text" . "Chat history compression completed."))))

(define-command help (args)
                "Display available commands and their descriptions."
                (declare (ignore args))
                (format t "Available commands:~%")
                ;; Sort commands alphabetically for better readability
                (let ((sorted-commands (sort (alexandria:hash-table-keys *commands*) #'string<)))
                  (dolist (cmd sorted-commands)
                    (let* ((command-info (gethash cmd *commands*))
                           (description (second command-info)))
                      (format t "  ~A - ~A~%" cmd description))))
                (format t "~%Session commands:~%")
                (format t "  /session           - List sessions or load by name~%")
                (format t "  /session.save      - Save current state as a session~%")
                (format t "  /session.load      - Load and restore a session~%")
                (format t "  /session.list      - List all available sessions~%")
                (format t "  /session.show      - Show session details~%")
                (format t "  /session.delete    - Delete a session~%")
                (format t "  /session.auto      - Toggle auto-save on exit~%")
                (format t "~%Generation commands (/gen):~%")
                (format t "  /gen <type> <name> - Generate code (component, route, model, etc.)~%")
                (format t "  /gen/add <pattern> --to <glob> - Add a pattern to files~%")
                (format t "  /gen/rm <type> <name> - Remove generated code~%")
                (format t "  /gen/undo - Undo last generation~%")
                (format t "  /gen/list - List available generators and patterns~%")
                (format t "  /gen/history - Show generation history~%")
                (format t "~%Available sub-commands:~%")
                (let ((sorted-subcommands (sort (alexandria:hash-table-keys *sub-commands*) #'string<)))
                  (dolist (subcmd sorted-subcommands)
                    (let* ((subcmd-info (gethash subcmd *sub-commands*))
                           (description (second subcmd-info))
                           (cli-options (third subcmd-info)))
                      (format t "  ~A~%" subcmd)
                      (format t "      ~A~%" description)
                      (when cli-options
                        (format t "      Options:~%")
                        (dolist (opt cli-options)
                          (let ((opt-short (getf opt :short))
                                (opt-long (getf opt :long))
                                (opt-desc (getf opt :description)))
                            (format t "        ")
                            (when opt-short (format t "-~A" opt-short))
                            (when (and opt-short opt-long) (format t ", "))
                            (when opt-long (format t "--~A" opt-long))
                            (format t " : ~A~%" opt-desc))))
                      ;; Check if this is a web command and print its routes
                      (let ((web-cmd (gethash subcmd *web-commands*)))
                        (when web-cmd
                          (dolist (route (web-command-routes web-cmd))
                            (format t "    ~A - ~A~%"
                                    (first (web-route-pattern route))
                                    (web-route-description route))))))))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (let ((commands '()))
                         (maphash (lambda (cmd-name handler)
                                    (declare (ignore handler))
                                    (let* ((cmd-info (gethash cmd-name *commands*))
                                           (description (if cmd-info (second cmd-info) "")))
                                      (push `(("name" . ,(string-trim "/" cmd-name))
                                              ("description" . ,description))
                                            commands)))
                                  *acp-commands*)
                         `(("text" . ,(format nil "~A commands available." (length commands)))
                           ("data" . ,(coerce (sort commands #'string< :key (lambda (c) (cdr (assoc "name" c :test #'string=)))) 'vector))))))

(define-command model (args)
                "Switch to a new LLM. Uses fuzzy-select if no model name is provided."
                (if args
                  (let ((model-name (first args)))
                    (set-current-model model-name))
                  (cond
                    ;; In ACP mode or non-interactive mode, list models instead of fuzzy-select
                    ((or *acp-mode* (not *in-repl*))
                     (if *available-models*
                       (progn
                         (format t "Current model: ~A~%" (if *current-model* (model-config-name *current-model*) "None"))
                         (format t "Available models (use /model <name> to switch):~%")
                         (dolist (model *available-models*)
                           (format t "  ~A~%" (model-config-name model))))
                       (format t "No models available.~%")))
                    ;; Interactive mode with fuzzy-select
                    (*available-models*
                     (let* ((items (loop for model in *available-models*
                                         collect `((:item . ,(model-config-name model))
                                                   (:preview . ,(format nil "Provider: ~A~%Model: ~A~%Max Tokens: ~A In / ~A Out~%Edit Format: ~A~%Cost (In/Out): $~8F / $~8F: Supports: ~A"
                                                                        (model-config-provider model)
                                                                        (model-config-model-name model)
                                                                        (model-config-max-input-tokens model)
                                                                        (model-config-max-output-tokens model)
                                                                        (model-config-edit-format model)
                                                                        (model-config-input-cost-per-token model)
                                                                        (model-config-output-cost-per-token model)
                                                                        (format nil "~{~A~^, ~}" (model-config-supports model)))))))
                            (selected-item (fuzzy-select items)))
                       (if selected-item
                         (let ((selected-model-name (cdr (assoc :item selected-item))))
                           (set-current-model selected-model-name))
                         (format t "Model selection cancelled.~%"))))
                    (t (format t "No models available to select.~%"))))
                :acp (lambda (cmd-args)
                       (if cmd-args
                           (let ((model-name (first cmd-args)))
                             (set-current-model model-name)
                             `(("text" . ,(format nil "Model set to: ~A" model-name))
                               ("data" . (("model" . ,model-name)))))
                           `(("text" . ,(format nil "Current model: ~A"
                                                (if *current-model* (model-config-name *current-model*) "None")))
                             ("data" . (("currentModel" . ,(if *current-model* (model-config-name *current-model*) :null))
                                        ("availableModels" . ,(coerce
                                                               (mapcar (lambda (m)
                                                                         `(("name" . ,(model-config-name m))
                                                                           ("provider" . ,(model-config-provider m))
                                                                           ("modelName" . ,(model-config-model-name m))))
                                                                       *available-models*)
                                                               'vector))))))))

(define-command models (args)
                "Search the list of available models."
                (let ((search-term (if args (first args) "")))
                  (format t "Available models:~%")
                  (dolist (model *available-models*)
                    (when (or (string= search-term "")
                              (search search-term (model-config-name model) :test #'char-equal))
                      (format t "  ~A~%" (model-config-name model))
                      (format t "    Model name: ~A~%" (model-config-model-name model))
                      (format t "    Edit format: ~A~%" (model-config-edit-format model))
                      (format t "    Max tokens: ~A input / ~A output~%"
                              (model-config-max-input-tokens model)
                              (model-config-max-output-tokens model))
                      (format t "    Supports: ~{~A~^, ~}~%" (model-config-supports model)))))
                :acp (lambda (cmd-args)
                       (let ((search-term (if cmd-args (first cmd-args) "")))
                         (let ((matching (loop for model in *available-models*
                                               when (or (string= search-term "")
                                                        (search search-term (model-config-name model) :test #'char-equal))
                                               collect `(("name" . ,(model-config-name model))
                                                         ("modelName" . ,(model-config-model-name model))
                                                         ("provider" . ,(model-config-provider model))
                                                         ("editFormat" . ,(model-config-edit-format model))
                                                         ("maxInputTokens" . ,(model-config-max-input-tokens model))
                                                         ("maxOutputTokens" . ,(model-config-max-output-tokens model))
                                                         ("supports" . ,(coerce (model-config-supports model) 'vector))))))
                           `(("text" . ,(format nil "~A model(s) found." (length matching)))
                             ("data" . ,(coerce matching 'vector)))))))

(define-command cheap-model (args)
                "Set the cheap model to use when cost is a concern."
                (if args
                  (let ((model-name (first args)))
                    (setf *cheap-model* model-name)
                    (format t "Cheap model set to: ~A~%" model-name))
                  (cond
                    ((or *acp-mode* (not *in-repl*))
                     (if *available-models*
                       (progn
                         (format t "Current cheap model: ~A~%" (or *cheap-model* "None"))
                         (format t "Available models (use /cheap-model <name> to switch):~%")
                         (dolist (model *available-models*)
                           (format t "  ~A~%" (model-config-name model))))
                       (format t "No models available.~%")))
                    (*available-models*
                     (let* ((items (loop for model in *available-models*
                                         collect `((:item . ,(model-config-name model))
                                                   (:preview . ,(format nil "Provider: ~A~%Model: ~A~%Max Tokens: ~A In / ~A Out~%Edit Format: ~A~%Cost (In/Out): $~8F / $~8F: Supports: ~A"
                                                                        (model-config-provider model)
                                                                        (model-config-model-name model)
                                                                        (model-config-max-input-tokens model)
                                                                        (model-config-max-output-tokens model)
                                                                        (model-config-edit-format model)
                                                                        (model-config-input-cost-per-token model)
                                                                        (model-config-output-cost-per-token model)
                                                                        (format nil "~{~A~^, ~}" (model-config-supports model)))))))
                            (selected-item (fuzzy-select items)))
                       (if selected-item
                         (let ((selected-model-name (cdr (assoc :item selected-item))))
                           (setf *cheap-model* selected-model-name)
                           (format t "Cheap model set to: ~A~%" selected-model-name))
                         (format t "Model selection cancelled.~%"))))
                    (t (format t "No models available to select.~%")))))

(define-command docs-meta-model (args)
                "Set the model to use for generating documentation metadata."
                (if args
                  (let ((model-name (first args)))
                    (setf *docs-meta-model* model-name)
                    (format t "Docs meta model set to: ~A~%" model-name))
                  (cond
                    ((or *acp-mode* (not *in-repl*))
                     (if *available-models*
                       (progn
                         (format t "Current docs meta model: ~A~%" (or *docs-meta-model* "None"))
                         (format t "Available models (use /docs-meta-model <name> to switch):~%")
                         (dolist (model *available-models*)
                           (format t "  ~A~%" (model-config-name model))))
                       (format t "No models available.~%")))
                    (*available-models*
                     (let* ((items (loop for model in *available-models*
                                         collect `((:item . ,(model-config-name model))
                                                   (:preview . ,(format nil "Provider: ~A~%Model: ~A~%Max Tokens: ~A In / ~A Out~%Edit Format: ~A~%Cost (In/Out): $~8F / $~8F: Supports: ~A"
                                                                        (model-config-provider model)
                                                                        (model-config-model-name model)
                                                                        (model-config-max-input-tokens model)
                                                                        (model-config-max-output-tokens model)
                                                                        (model-config-edit-format model)
                                                                        (model-config-input-cost-per-token model)
                                                                        (model-config-output-cost-per-token model)
                                                                        (format nil "~{~A~^, ~}" (model-config-supports model)))))))
                            (selected-item (fuzzy-select items)))
                       (if selected-item
                         (let ((selected-model-name (cdr (assoc :item selected-item))))
                           (setf *docs-meta-model* selected-model-name)
                           (format t "Docs meta model set to: ~A~%" selected-model-name))
                         (format t "Model selection cancelled.~%"))))
                    (t (format t "No models available to select.~%")))))

(define-command tokens (args)
                "Report on the number of tokens used by the current chat context."
                (declare (ignore args))
                (let* ((messages (prepare-messages ""))
                       (total-chars (reduce #'+ messages
                                            :key (lambda (msg)
                                                   (length (cdr (assoc :content msg))))))
                       (estimated-tokens (round total-chars 4)))
                  (format t "Estimated token usage:~%")
                  (format t "  Total characters: ~A~%" total-chars)
                  (format t "  Estimated tokens: ~A~%" estimated-tokens)
                  (when *current-model*
                    (let ((input-cost (model-config-input-cost-per-token *current-model*)))
                      (format t "  Estimated cost for next input: $~8F~%" (* estimated-tokens input-cost)))))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (let* ((messages (prepare-messages ""))
                              (total-chars (reduce #'+ messages
                                                   :key (lambda (msg)
                                                          (length (cdr (assoc :content msg))))))
                              (estimated-tokens (round total-chars 4))
                              (cost (when *current-model*
                                      (* estimated-tokens (model-config-input-cost-per-token *current-model*)))))
                         `(("text" . ,(format nil "Estimated tokens: ~A, chars: ~A~@[, cost: $~8F~]"
                                              estimated-tokens total-chars cost))
                           ("data" . (("totalCharacters" . ,total-chars)
                                      ("estimatedTokens" . ,estimated-tokens)
                                      ("estimatedCost" . ,(or cost :null))
                                      ("model" . ,(if *current-model* (model-config-name *current-model*) :null))))))))


(define-command quit (args)
                "Exit the application."
                (declare (ignore args))
                (if *lisp-rpc-mode*
                    (progn
                      (rpc-exit "quit")
                      ;; Signal the main loop to exit by disabling the mode
                      (setf *lisp-rpc-mode* nil))
                    (progn
                      (format t "Exiting hactar...~%")
                      ;; Let toplevel UNWIND-PROTECT handle cleanup; it is more robust and
                      ;; already handles errors. Avoid double-stopping watchers here.
                      (uiop:quit))))
(defmacro with-suppressed-output-if-rpc (&body body)
  "In lisp-rpc-mode, suppress stdout/stderr during BODY to avoid polluting the s-expression protocol stream."
  `(if *lisp-rpc-mode*
       (let ((*standard-output* (make-broadcast-stream))
             (*error-output*   (make-broadcast-stream)))
         ,@body)
       (progn ,@body)))

(defun dump-settings ()
  (format t "Current settings:~%")
  (format t "  Model: ~A~%" (if *current-model* (model-config-name *current-model*) "None"))
  (format t "  Cheap Model: ~A~%" *cheap-model*)
  (format t "  Docs Meta Model: ~A~%" *docs-meta-model*)
  (format t "  Embedding Model: ~A~%" *embedding-model*)
  (when *current-model*
    (format t "  Model details:~%")
    (format t "    Provider: ~A~%" (model-config-provider *current-model*))
    (format t "    Model name: ~A~%" (model-config-model-name *current-model*))
    (format t "    Edit format: ~A~%" (model-config-edit-format *current-model*))
    (format t "    Use repo map: ~A~%" (model-config-use-repo-map *current-model*))
    (format t "    Max tokens: ~A input / ~A output~%"
            (model-config-max-input-tokens *current-model*)
            (model-config-max-output-tokens *current-model*))
    (format t "    Cache control: ~A~%" (model-config-cache-control *current-model*))
    (format t "    Supports: ~{~A~^, ~}~%" (model-config-supports *current-model*)))
  (format t "  Repo Root ~A~%" *repo-root*)
  (format t "  Git autocommit: ~A~%" (if *git-autocommit* "Enabled" "Disabled"))
  (format t "  Multiline mode: ~A~%" (if *multiline-mode* "Enabled" "Disabled"))
  (format t "  Chat history limit: ~A characters~%" *chat-history-limit*)
  (format t "  Files in context: ~A~%" (length *files*))
  (format t "  Images in context: ~A~%" (length *images*))
  (format t "  Guide exclude tags: ~A~%" *guide-exclude-tags*)
  (format t "  Transcript file: ~A~%" *transcript-file*)
  (format t "  Current session: ~A~%" (or *current-session-name* "(none)"))
  (format t "  Session auto-save: ~A~%" (if *auto-save-session* "Enabled" "Disabled"))
  (format t "  System Prompt: ~A~%" (system-prompt))
  (format t " Dot System Prompt ~A~%" (dot-system-prompt))
  (format t "  Repository root: ~A~%" (if *repo-root* (namestring *repo-root*) "Not set")))

(defun dump-dot-system-prompt ()
  (format t "Dot System Prompt:~%")
  (format t "  ~A~%" (dot-system-prompt)))

(define-command settings (args)
                "Print out the current settings."
                (declare (ignore args))
                (dump-settings)
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       `(("text" . ,(with-output-to-string (*standard-output*) (dump-settings)))
                         ("data" . (("model" . ,(if *current-model* (model-config-name *current-model*) :null))
                                    ("cheapModel" . ,(or *cheap-model* :null))
                                    ("docsMetaModel" . ,(or *docs-meta-model* :null))
                                    ("embeddingModel" . ,(or *embedding-model* :null))
                                    ("repoRoot" . ,(if *repo-root* (namestring *repo-root*) :null))
                                    ("gitAutocommit" . ,(if *git-autocommit* t :false))
                                    ("multilineMode" . ,(if *multiline-mode* t :false))
                                    ("chatHistoryLimit" . ,*chat-history-limit*)
                                    ("filesInContext" . ,(length *files*))
                                    ("imagesInContext" . ,(length *images*))
                                    ("currentSession" . ,(or *current-session-name* :null))
                                    ("sessionAutoSave" . ,(if *auto-save-session* t :false)))))))

(define-command dump-api-keys (args)
                "Print out the API keys for each platform."
                (declare (ignore args))
                (format t "~A" (llm:dump-api-keys))
                :acp t)

(define-command reload (args)
		"Reload hactar. Clear chat history, empty context, and reload the config."
                (declare (ignore args))
		(clear-chat-history)
		(empty-context)
                (clear-session-overrides)
                (reload-config)
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (clear-chat-history)
                       (empty-context)
                       (clear-session-overrides)
                       (reload-config)
                       `(("text" . "Reload complete. Chat history cleared, context emptied, config reloaded."))))

(define-command dump (args)
		"Dump settings, context, and debug info"
		(declare (ignore args))
		(dump-settings)
                :acp t)

(define-command reset (args)
                "Drop all files and clear the chat history." (declare (ignore args))
                (setf *files* nil)
                (clear-chat-history)
                (clear-session-overrides)
                (format t "Reset complete. All files dropped, chat history cleared, and session overrides cleared.~%")
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (setf *files* nil)
                       (clear-chat-history)
                       (clear-session-overrides)
                       `(("text" . "Reset complete. All files dropped, chat history cleared, and session overrides cleared."))))

(define-command undo (args)
                "Revert the last git commit made by hactar."
                (declare (ignore args))
                (if (confirm-action "Are you sure you want to undo the last commit (git reset --hard HEAD~1)?")
                  (progn
                    (format t "Reverting last commit...~%")
                    (git-reset-hard "HEAD~1")
                    (format t "Last commit reverted.~%"))
                  (format t "Undo cancelled.~%"))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       ;; In ACP mode, skip confirmation (the client should handle confirmation)
                       (git-reset-hard "HEAD~1")
                       `(("text" . "Last commit reverted (git reset --hard HEAD~1).")
                         ("data" . (("action" . "git-reset-hard")
                                    ("target" . "HEAD~1"))))))

(define-command version (args)
                "Print the version information."
                (declare (ignore args))
                (format t "hactar version ~a~%" *hactar-version*)
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       `(("text" . ,(format nil "hactar version ~A" *hactar-version*))
                         ("data" . (("version" . ,*hactar-version*))))))

(define-command autocommit (args)
                "Toggle git autocommit on/off."
                (declare (ignore args))
                (setf *git-autocommit* (not *git-autocommit*))
                (format t "Git autocommit is now ~A.~%" (if *git-autocommit* "enabled" "disabled"))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (setf *git-autocommit* (not *git-autocommit*))
                       `(("text" . ,(format nil "Git autocommit is now ~A." (if *git-autocommit* "enabled" "disabled")))
                         ("data" . (("autocommit" . ,(if *git-autocommit* t :false)))))))

(define-command cost (args)
                "Estimate the cost of sending the current chat history as input."
                (declare (ignore args))
                (if *current-model*
                  (let* ((messages (prepare-messages ""))
                         (total-chars (reduce #'+ messages
                                              :key (lambda (msg)
                                                     (length (cdr (assoc :content msg))))))
                         (estimated-tokens (round total-chars 4))
                         (input-cost (model-config-input-cost-per-token *current-model*))
                         (estimated-cost (* estimated-tokens input-cost)))
                    (format t "Current Model: ~A~%" (model-config-name *current-model*))
                    (format t "Estimated Input Tokens (Current History): ~A~%" estimated-tokens)
                    (format t "Input Cost per Token: $~8F~%" input-cost)
                    (format t "Estimated Cost for this Input: $~8F~%" estimated-cost))
                  (format t "No model selected. Cannot estimate cost.~%"))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (if *current-model*
                           (let* ((messages (prepare-messages ""))
                                  (total-chars (reduce #'+ messages
                                                       :key (lambda (msg)
                                                              (length (cdr (assoc :content msg))))))
                                  (estimated-tokens (round total-chars 4))
                                  (input-cost (model-config-input-cost-per-token *current-model*))
                                  (estimated-cost (* estimated-tokens input-cost)))
                             `(("text" . ,(format nil "Model: ~A, Tokens: ~A, Cost: $~8F"
                                                  (model-config-name *current-model*)
                                                  estimated-tokens estimated-cost))
                               ("data" . (("model" . ,(model-config-name *current-model*))
                                          ("estimatedTokens" . ,estimated-tokens)
                                          ("inputCostPerToken" . ,input-cost)
                                          ("estimatedCost" . ,estimated-cost)))))
                           `(("text" . "No model selected. Cannot estimate cost.")))))

(define-command transcript (args)
  "View or manage the chat transcript."
  (cond
    ((null args)
     (if (probe-file *transcript-file*)
         (uiop:run-program (list "less" *transcript-file*)
                           :output :interactive
                           :error-output :interactive
                           :input :interactive)
         (format t "No transcript file found.~%")))
    ((string= (first args) "clear")
     (with-open-file (stream *transcript-file*
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
       (format stream ""))
     (format t "Transcript cleared.~%"))
    ((string= (first args) "save")
     (if (second args)
         (progn
           (uiop:run-program (list "cp" *transcript-file* (second args))
                             :output :interactive
                             :error-output :interactive)
           (format t "Transcript saved to ~A~%" (second args)))
         (format t "Please specify a filename.~%"))))
  :acp (lambda (cmd-args)
         (cond
           ((null cmd-args)
            (if (probe-file *transcript-file*)
                `(("text" . ,(uiop:read-file-string *transcript-file*))
                  ("data" . (("file" . ,*transcript-file*))))
                `(("text" . "No transcript file found."))))
           ((string= (first cmd-args) "clear")
            (with-open-file (stream *transcript-file*
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (format stream ""))
            `(("text" . "Transcript cleared.")))
           ((string= (first cmd-args) "save")
            (if (second cmd-args)
                (progn
                  (uiop:run-program (list "cp" *transcript-file* (second cmd-args))
                                    :output :string :error-output :string :ignore-error-status t)
                  `(("text" . ,(format nil "Transcript saved to ~A" (second cmd-args)))))
                `(("text" . "Please specify a filename."))))
           (t `(("text" . ,(format nil "Unknown transcript subcommand: ~A" (first cmd-args))))))))

(defun find-model-by-name (name)
  "Find a model configuration by name."
  (find name *available-models* :key #'model-config-name :test #'string=))

(defun set-current-model (model-name)
  "Set the current model by name."
  (let ((model (find-model-by-name model-name)))
    (if model
      (let ((old-model *current-model*))
        (setf *current-model* model)
        (nhooks:run-hook *model-changed-hook* model old-model)
        (unless *silent*
          (if *lisp-rpc-mode*
              (rpc-model-changed model-name)
              (format t "Switched to model: ~A~%" model-name))))
      (unless *silent*
        (if *lisp-rpc-mode*
            (rpc-model-not-found model-name)
            (format t "Model not found: ~A~%" model-name))))))

(defvar *waiting-for-llm* nil
  "T when we are waiting for an LLM response.")
(defvar *current-stream-reader* nil
  "The current active stream reader, if any. Used for cancellation.")

;;* Chat history
(defun estimate-tokens (text)
  "Estimate the number of tokens in a string (approx. 1.3 tokens/word)."
  (round (* (length (str:words text)) 1.3)))

(defun get-current-token-count ()
  "Calculate the estimated total token count for the current chat history."
  (reduce #'+ *chat-history* :key (lambda (msg) (estimate-tokens (cdr (assoc :content msg))))))

(defun perform-history-compression ()
  "Summarize the current chat history and replace it with the summary."
  (unless *current-model*
    (format t "~&Cannot compress history: No model selected.~%")
    (return-from perform-history-compression nil))

  (if *lisp-rpc-mode*
      (rpc-status "compressing-history")
      (format t "~&Compressing chat history...~%"))
  (let* ((summary-prompt-text (handler-case (uiop:read-file-string (get-prompt-path "summary.org"))
                                (error (e)
                                  (format t "~&Error reading summary prompt: ~A~%" e)
                                  (return-from perform-history-compression nil))))
         (messages-for-summary (append *chat-history*
                                       (list `((:role . "user") (:content . ,summary-prompt-text)))))
         (provider-type (intern (string-upcase (model-config-provider *current-model*)) :keyword)))

    (handler-case
        (progn
          (debug-log "Sending history for compression:" *chat-history*)
          (multiple-value-bind (summary-text full-response-history)
                               (llm:complete
                                 provider-type
                                 messages-for-summary
                                 :model (model-config-model-name *current-model*)
                                 :max-tokens (min 1024 (model-config-max-output-tokens *current-model*))
                                 :system-prompt ""
                                 :stream nil)
            (declare (ignore full-response-history))

            (if (and summary-text (> (length summary-text) 0))
              (progn
                (setf *chat-history* `(((role . "user") (:content . ,summary-text))))
                (save-transcript)
                (if *lisp-rpc-mode*
                    (rpc-history-compressed)
                    (format t "~&History compressed successfully.~%")))
              (if *lisp-rpc-mode*
                  (rpc-error "History compression failed: LLM returned empty summary.")
                  (format t "~&History compression failed: LLM returned empty summary.~%")))))
      (error (e)
        (if *lisp-rpc-mode*
            (rpc-error (format nil "History compression API error: ~A" e))
            (format t "~&Error during history compression API call: ~A~%" e))))))

(defun compress-chat-history-if-needed ()
  "Check if chat history needs compression based on token count and perform it."
  (when *current-model*
    (let* ((current-tokens (get-current-token-count))
           (max-input (model-config-max-input-tokens *current-model*))
           (threshold (* 0.8 max-input))) ; Compress at 80% capacity
      (when (> current-tokens threshold)
        (perform-history-compression)))))

(defun save-transcript ()
  "Save the entire chat history to the transcript file as a single JSON array using UTF-8."
  (with-open-file (stream *transcript-file*
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (cl-json:encode-json *chat-history* stream)))

(defun add-to-chat-history (role content &key tool-calls tool_call_id name)
  "Add a message to the chat history and save the updated transcript."
  (let ((message `((:role . ,role) (:content . ,(or content "")))))
    (when tool-calls (setf message (append message `((:tool_calls . ,tool-calls)))))
    (when tool_call_id (setf message (append message `((:tool_call_id . ,tool_call_id)))))
    (when name (setf message (append message `((:name . ,name)))))

    (setf *chat-history* (push-end message *chat-history*))
    (compress-chat-history-if-needed)
    (save-transcript)))

(defun clear-chat-history ()
  "Clear the chat history."
  (setf *chat-history* '())
  (save-transcript))

(defun drop-files ()
  "Drop files from context"
  (setf *files* nil))

(defun empty-context ()
  "Drop everything from context"
  (drop-files))

(defun load-chat-history ()
  "Load chat history from transcript file (JSON format) if it exists."
  (when (probe-file *transcript-file*)
    (with-open-file (stream *transcript-file* :direction :input :if-does-not-exist nil)
		    (when stream
		      (let* ((messages '()))
			(handler-case
			    (let* (
				   (file-content (make-string (file-length stream)))
				   (_ (read-sequence file-content stream))
				   (json-obj (cl-json:decode-json-from-string file-content)))
			      (declare (ignore _)) ;; Assuming the JSON structure is an array of messages
			      (dolist (message json-obj)
				(let ((role (cdr (assoc :role message)))
				      (content (cdr (assoc :content message))))
				  (push `((:role . ,role)
					  (:content . ,content))
					messages))))
			  (error (e)
				 (format t "~&Warning: Error parsing transcript file: ~A~%" e)))
			;; Reverse the history to maintain chronological order
			(setf *chat-history* (nreverse messages)))))))

;;* LLM interaction
(defun get-llm-response (prompt &key (stream t) custom-system-prompt (add-to-history t) dot-command-p is-continuation model)
  "Get a response from the current LLM, handling streaming and history compression.
   :stream - If T, stream the response to stdout.
   :custom-system-prompt - A string to override the default system prompt.
   :add-to-history - If T, add the prompt and response to the chat history.
   :dot-command-p - If T, use the special dot-command system prompt.
   :model - The model to use for this request, overriding *current-model*."
  (declare (ignore is-continuation))
  (let ((*current-model* (cond
                           ((stringp model) (find-model-by-name model))
                           (model model)
                           (t *current-model*))))
    (unless *current-model*
      (return-from get-llm-response
        (if (stringp model)
            (format nil "Error: Model '~A' not found." model)
            (format nil "Error: No model selected. Use /model to select a model."))))
    (setf *waiting-for-llm* t)
  (setf *current-stream-reader* nil)
  (when *in-repl*
    (rl:deprep-terminal)
    (disable-terminal-echo))
  (let* ((messages-for-api (prepare-messages prompt))
	 (provider-name (model-config-provider *current-model*))
	 (provider-type (intern (string-upcase provider-name) :keyword))
	 (system-prompt (cond
                          (custom-system-prompt custom-system-prompt)
                          (dot-command-p (dot-system-prompt))
                          (t (system-prompt))))
	 (full-response-text (make-string-output-stream))
	 (reader-instance nil)
	 (supports-vision (member "vision" (model-config-supports *current-model*) :test #'string=))
	 (images-to-send nil))

    (when (and supports-vision *images*)
      (format t "~&Preparing images for vision model...~%")
      (setf images-to-send
	    (loop for img-plist in *images*
		  for path = (getf img-plist :path)
		  for text = (getf img-plist :text)
		  collect (multiple-value-bind (base64-data mime-type)
			      (resize-and-encode-image path)
			    (when base64-data
			      `((:base64-data . ,base64-data)
				(:mime-type . ,mime-type)
				(:text . ,(or text prompt)))))))) ; Use image text or main prompt if no specific text
    ;; Filter out any images that failed processing
    (setf images-to-send (remove-if #'null images-to-send))

    (when (and *images* (not supports-vision))
      (format t "~&Warning: Images are in context, but the current model (~A) does not support vision. Images will be ignored.~%"
	      (model-config-name *current-model*)))

    #+sbcl
    (let ((previous-sigint-handler nil))
      (setf previous-sigint-handler
            (sb-sys:enable-interrupt
             sb-unix:sigint
             (lambda (signal info context)
               (declare (ignore signal info context))
               (setf *waiting-for-llm* nil)
               (when (and *current-stream-reader*
                          (not (llm:llm-stream-reader-closed-p *current-stream-reader*)))
                 (ignore-errors (llm:close-reader *current-stream-reader*)))
               (error 'hactar-interrupt))))
      (unwind-protect
           (handler-case
               (progn
                 (multiple-value-bind (response-result initial-messages)
                     (apply #'llm:complete
                            provider-type
                            messages-for-api
                            :model (model-config-model-name *current-model*)
                            :max-tokens (model-config-max-output-tokens *current-model*)
                            :max-context (model-config-max-input-tokens *current-model*)
                            :system-prompt system-prompt
                            :extra-headers (normalize-http-extra-headers
                                            (when (model-config-extra-params *current-model*)
                                              (gethash "extra_headers" (model-config-extra-params *current-model*))))
                            :stream stream
                            ;; Conditionally add :images argument
                            (when images-to-send `(:images ,images-to-send)))

                   (declare (ignore initial-messages))

                   (if stream
                       (progn
                         (setf reader-instance response-result)
                         (setf *current-stream-reader* reader-instance)
                         (unless (typep reader-instance 'llm:llm-stream-reader)
                           (error "LLM complete did not return a stream reader when stream=t"))

                         (unless *lisp-rpc-mode*
                           (format t "Assistant: ")
                           (force-output))

                         (loop for chunk = (llm:read-next-chunk reader-instance)
                               while chunk
                               do (progn
                                    (unless *lisp-rpc-mode*
                                      (stream-chunk chunk))
                                    (write-string chunk full-response-text)))
                         (unless *lisp-rpc-mode*
                           (format t "~%")))
                       (write-string response-result full-response-text))

                   (let ((assistant-response (get-output-stream-string full-response-text)))
                     (maybe-log-llm-response assistant-response)
                     (when *lisp-rpc-mode*
                       (rpc-response assistant-response))
                     (when add-to-history
                       (add-to-chat-history "user" prompt)
                       (add-to-chat-history "assistant" assistant-response)
                       (nhooks:run-hook *process-history-hook* *chat-history*))
                     assistant-response)))
             (hactar-interrupt ()
			        ;; User cancelled with C-c during LLM request - close stream and return quietly
			       (when (and reader-instance (not (llm:llm-stream-reader-closed-p reader-instance)))
				 (ignore-errors (llm:close-reader reader-instance)))
			       (if *lisp-rpc-mode*
				   (rpc-cancelled)
				 (format t "~&Cancelled.~%"))
			       (force-output)
			       nil)
             (error (e)
               (if *lisp-rpc-mode*
                   (rpc-error (format nil "~A" e))
                   (format t "~&Error during LLM interaction: ~A~%" e))
               (when (and reader-instance (not (llm:llm-stream-reader-closed-p reader-instance)))
                 (llm:close-reader reader-instance))
               nil))
        (if previous-sigint-handler
            (sb-sys:enable-interrupt sb-unix:sigint previous-sigint-handler)
            (sb-sys:enable-interrupt sb-unix:sigint :default))))
    #-sbcl
    (unwind-protect
         (handler-case
             (progn
               (multiple-value-bind (response-result initial-messages)
                   (apply #'llm:complete
                          provider-type
                          messages-for-api
                          :model (model-config-model-name *current-model*)
                          :max-tokens (model-config-max-output-tokens *current-model*)
                          :max-context (model-config-max-input-tokens *current-model*)
                          :system-prompt system-prompt
                          :extra-headers (normalize-http-extra-headers
                                          (when (model-config-extra-params *current-model*)
                                            (gethash "extra_headers" (model-config-extra-params *current-model*))))
                          :stream stream
                          (when images-to-send `(:images ,images-to-send)))

                 (declare (ignore initial-messages))

                 (if stream
                     (progn
                       (setf reader-instance response-result)
                       (setf *current-stream-reader* reader-instance)
                       (unless (typep reader-instance 'llm:llm-stream-reader)
                         (error "LLM complete did not return a stream reader when stream=t"))

                       (unless *lisp-rpc-mode*
                         (format t "Assistant: ")
                         (force-output))

                       (loop for chunk = (llm:read-next-chunk reader-instance)
                             while chunk
                             do (progn
                                  (unless *lisp-rpc-mode*
                                    (stream-chunk chunk))
                                  (write-string chunk full-response-text)))
                       (unless *lisp-rpc-mode*
                         (format t "~%")))
                     (write-string response-result full-response-text))

                 (let ((assistant-response (get-output-stream-string full-response-text)))
                   (maybe-log-llm-response assistant-response)
                   (when *lisp-rpc-mode*
                     (rpc-response assistant-response))
                   (when add-to-history
                     (add-to-chat-history "user" prompt)
                     (add-to-chat-history "assistant" assistant-response)
                     (nhooks:run-hook *process-history-hook* *chat-history*))
                   assistant-response)))
           (hactar-interrupt ()
             (when (and reader-instance (not (llm:llm-stream-reader-closed-p reader-instance)))
               (ignore-errors (llm:close-reader reader-instance)))
             (if *lisp-rpc-mode*
                 (rpc-cancelled)
                 (format t "~&Cancelled.~%"))
             (force-output)
             nil)
           (error (e)
             (if *lisp-rpc-mode*
                 (rpc-error (format nil "~A" e))
                 (format t "~&Error during LLM interaction: ~A~%" e))
             (when (and reader-instance (not (llm:llm-stream-reader-closed-p reader-instance)))
               (llm:close-reader reader-instance))
             nil)))
      (setf *waiting-for-llm* nil)
      (setf *current-stream-reader* nil)
      (when *in-repl*
        (enable-terminal-echo)
        (flush-terminal-input)
        (rl:prep-terminal t)))))
(defun stream-chunk (chunk)
  "Handle displaying streaming response chunks."
  (when chunk
    (format t "~A" chunk)
    (force-output *standard-output*)))

(defun maybe-log-llm-response (response)
  "If in editor mode, log the full LLM response to the editor log."
  (when (and *in-editor* response (> (length response) 0))
    (editor-log-write response :type "llm-response")))

(defun prepare-messages (prompt &key images (for-llm-api nil))
  "Prepare messages for the LLM.
   IMAGES: Handled by llm:complete.
   FOR-LLM-API: If true, includes full history with tool calls/results. Otherwise, just text for context/history display."
  (declare (ignore images for-llm-api))
  (let ((api-messages '()))
    (let ((litmode-ctx (get-litmode-context-section)))
      (when (and litmode-ctx (> (length litmode-ctx) 0))
        (push `((:role . "user")
                (:content . ,(format nil "[Project Context from .hactar.org]~%~%~A" litmode-ctx)))
              api-messages)
        (push `((:role . "assistant")
                (:content . "I've reviewed the project context from .hactar.org. I can see the headline index and active context. How can I help?"))
              api-messages)))
    (dolist (msg *chat-history*)
      (let* ((role (cdr (assoc :role msg)))
            (content (cdr (assoc :content msg)))
            (tool-calls (or (cdr (assoc :tool_calls msg)) (cdr (assoc 'cl-json::tool--calls msg))))
            (tool-call-id (or (cdr (assoc :tool_call_id msg)) (cdr (assoc 'cl-json::tool--call--id msg))))
            (name (or (cdr (assoc :name msg)) (cdr (assoc 'cl-json::name msg))))
            (api-msg `((:role . ,role))))

        (if (or (and content (not (string= content "")))
                (and (string= role "assistant") (not tool-calls))
                (string= role "tool"))
            (setf api-msg (append api-msg `((:content . ,(or content ""))))))

        (when tool-calls (setf api-msg (append api-msg `((:tool_calls . ,tool-calls)))))
        (when tool-call-id (setf api-msg (append api-msg `((:tool_call_id . ,tool-call-id)))))
        (when (and name (string= role "tool")) (setf api-msg (append api-msg `((:name . ,name)))))
        (push api-msg api-messages)))

    (when (and prompt (not (string= prompt "")))
      (push `((:role . "user") (:content . ,prompt)) api-messages))

    (nreverse api-messages)))

(defun stream-response (chunk)
  "Handle streaming response chunks."
  (when chunk
    (format t "~A" chunk)
    (force-output)))

(defun novelty-check (current previous)
  "Check if the current input is different from the previous one."
  (string/= (string-trim " " current)
            (string-trim " " previous)))

(define-condition hactar-interrupt (error) ())

(defun handle-interrupt (condition)
  "Handle interrupt signal. If waiting for LLM, cancel the request.
This is installed via HANDLER-BIND in the REPL and must accept exactly one argument."
  (declare (ignore condition))
  (cond
    (*waiting-for-llm*
     ;; Cancel the current LLM request
     (if *lisp-rpc-mode*
         (rpc-cancelled)
         (format t "~&Cancelled.~%"))
     (when (and *current-stream-reader*
                (not (llm:llm-stream-reader-closed-p *current-stream-reader*)))
       (llm:close-reader *current-stream-reader*))
     ;; We rely on unwind-protect in get-llm-response to reset globals and terminal
     (error 'hactar-interrupt))
    (t
     (if *lisp-rpc-mode*
         (rpc-interrupted)
         (format t "~&Interrupted. Type /quit to exit.~%"))
     (error 'hactar-interrupt))))

;;** REPL
(defun readline-do-nothing (count key)
  "A cl-readline command function that does nothing.
   Used to unbind keys like PageUp/PageDown from history navigation
   to allow terminal scrolling via mouse wheel."
  (declare (ignore count key))
  0) ; Return value for readline commands, typically 0 for success.

(defun repl ()
  "Run the REPL with readline support, including {hactar...tag} multi-line input."
  (let ((*in-repl* t))
    (if *lisp-rpc-mode*
        (rpc-hello *hactar-version*
                   (if *current-model* (model-config-name *current-model*) nil))
        (progn
          (format t "Hactar AI Pair Programmer~%")
          (format t "Type /help for available commands~%")
          (format t "Use {hactar to start multi-line input, end with tag} on a new line.~%")))

    (rl:register-function :complete #'command-completer)
    (rl:initialize)
    (let ((history-file (merge-pathnames ".hactar_history" *default-pathname-defaults*)))
      (when (probe-file history-file)
	(rl:read-history (namestring history-file)))

      (let ((in-hactar-tag-mode nil)
            (hactar-tag-buffer (make-string-output-stream)))

	(handler-bind
            ((#+sbcl sb-sys:interactive-interrupt
              #+ccl  ccl:interrupt-signal-condition
              #+clisp system::simple-interrupt-condition
              #+ecl ext:interactive-interrupt
              #+allegro excl:interrupt-signal
              #'handle-interrupt))
          (unwind-protect
               (loop
                 (handler-case
                     (let* ((prompt (cond
                                      (*lisp-rpc-mode*
                                       (rpc-ready)
                                       "")
                                      (in-hactar-tag-mode "... ")
                                      (t "hactar> ")))
                            (input (rl:readline :prompt prompt
                                                :add-history (not in-hactar-tag-mode) ; Only add history for initial lines or single lines
                                                :novelty-check #'novelty-check)))

                       (unless input
                         (if *lisp-rpc-mode*
                             (rpc-exit "eof")
                             (format t "~&Exiting due to EOF.~%"))
                         (return))

                       (if in-hactar-tag-mode
                           (if (string= (string-trim '(#\Space #\Tab #\Newline #\Return) input) "hactar}")
                               (let ((full-input (get-output-stream-string hactar-tag-buffer)))
                                 (setf in-hactar-tag-mode nil)
                                 (setf hactar-tag-buffer (make-string-output-stream))
                                 (when (string/= (string-trim '(#\Space #\Tab #\Newline #\Return) full-input) "")
                                   (multiple-value-bind (cmd args) (parse-command full-input)
                                     (cond
                                       (cmd
                                        (if *lisp-rpc-mode*
                                            (let ((output (with-output-to-string (*standard-output*)
                                                            (execute-command cmd args))))
                                              (when (and output (> (length output) 0))
                                                (rpc-command-output output cmd)))
                                            (execute-command cmd args)))
                                       ((and *lisp-mode-enabled* (lisp-mode-intercept full-input)) nil)
                                       (t
                                        (if *lisp-rpc-mode*
                                            (rpc-status "waiting" :model (if *current-model* (model-config-name *current-model*) nil))
                                            (progn
                                              (format t "~&⏳ Waiting for ~A...~%" (if *current-model* (model-config-name *current-model*) "LLM"))
                                              (force-output)))
                                        (get-llm-response full-input))))))
                               (progn
                                 (write-string input hactar-tag-buffer)
                                 (write-char #\Newline hactar-tag-buffer)))
                           (let ((trimmed-input (string-trim '(#\Space #\Tab) input))
                                 (hactar-tag-start "{hactar"))
                             (cond
                               ((str:starts-with? hactar-tag-start trimmed-input)
                                (setf in-hactar-tag-mode t)
                                (let ((rest-of-line (subseq trimmed-input (length hactar-tag-start))))
                                  (when (string/= (string-trim '(#\Space #\Tab #\Newline #\Return) rest-of-line) "")
                                    (write-string (string-trim '(#\Space #\Tab) rest-of-line) hactar-tag-buffer)
                                    (write-char #\Newline hactar-tag-buffer))))
                               (t
                                (multiple-value-bind (cmd args) (parse-command input)
                                  (cond
                                    (cmd
                                     (if *lisp-rpc-mode*
                                         (let ((output (with-output-to-string (*standard-output*)
                                                         (execute-command cmd args))))
                                           (when (and output (> (length output) 0))
                                             (rpc-command-output output cmd)))
                                         (execute-command cmd args)))
                                    ((string= (string-trim '(#\Space #\Tab #\Newline #\Return) input) "") nil)
                                    ((and *lisp-mode-enabled* (lisp-mode-intercept input)) nil)
                                    (t
                                     (if *lisp-rpc-mode*
                                         (rpc-status "waiting" :model (if *current-model* (model-config-name *current-model*) nil))
                                         (progn
                                           (format t "~&⏳ Waiting for ~A...~%" (if *current-model* (model-config-name *current-model*) "LLM"))
                                           (force-output)))
                                     (get-llm-response input)))))))))
                   (hactar-interrupt ()
                     nil)))
          (ignore-errors (rl:write-history (namestring history-file)))))))))

;;* User customizations
(defun load-user-customizations ()
  "Loads user customization file from .hactar.user.lisp or ~/.config/hactar/user.lisp."
  (let ((project-custom-file (merge-pathnames #P".hactar.user.lisp" *repo-root*))
        (global-custom-file (uiop:subpathname *hactar-config-path* "user.lisp")))
    (cond
      ((probe-file project-custom-file)
        (unless *silent* (format t "~&Loading user customizations from: ~A~%" project-custom-file))
        (load project-custom-file :if-does-not-exist nil))
      ((probe-file global-custom-file)
        (unless *silent* (format t "~&Loading user customizations from: ~A~%" global-custom-file))
        (load global-custom-file :if-does-not-exist nil))
      (t (debug-log "No user customization file found.")))))

(defun hactar-session-new (path)
  "Initialize a new Hactar session for the given path."
  (let ((repo-dir (ignore-errors (find-git-repo-root (uiop:ensure-directory-pathname (uiop:parse-native-namestring path))))))
    (unless repo-dir
      (format t "~&Error: Could not find git repository at ~A~%" path)
      (return-from hactar-session-new nil))

    (setf *repo-root* repo-dir)
    (debug-log (format nil "Repository root set to: ~A~%" *repo-root*))

    (when *in-editor*
      (setf *editor-log-file* (editor-log-path))
      (editor-log-init)
      (unless *silent* (format t "Editor log: ~A~%" (uiop:native-namestring *editor-log-file*))))

    (load-project-config *repo-root*)

    (unless *name*
      (setf *name* (car (last (pathname-directory *repo-root*)))))

    (load-user-customizations)

    (setf *file-watcher-stopped* nil)
    (start-file-watcher *repo-root*)

    (let ((context-path (context-expose-file-path)))
      (setf *exposed-context-file* context-path)
      (context-expose-install-hooks-if-needed)
      (context-expose-upsert-project-details)
      (context-expose-upsert-files-section)
      (unless *silent* (format t "Context file initialized: ~A~%" (uiop:native-namestring context-path))))

    (when *auto-lint*
      (let ((agent-def (gethash (intern "LINT" :hactar) *agent-definitions*)))
        (when agent-def
          (unless *silent* (format t "Auto-lint enabled. Starting lint agent...~%"))
          (start-agent agent-def nil))))
    (when *auto-test*
      (let ((agent-def (gethash (intern "TEST" :hactar) *agent-definitions*)))
        (when agent-def
          (unless *silent* (format t "Auto-test enabled. Starting test agent...~%"))
          (start-agent agent-def nil))))
    (when *auto-typecheck*
      (let ((agent-def (gethash (intern "TYPECHECK" :hactar) *agent-definitions*)))
        (when agent-def
          (unless *silent* (format t "Auto-typecheck enabled. Starting typecheck agent...~%"))
          (start-agent agent-def nil))))

    (unless *slynk-started*
      (let ((actual-slynk-port (find-free-port *slynk-port*)))
        (handler-case
            (with-suppressed-output-if-rpc
              (unless *silent*
                (format t "~&Starting Slynk server on port ~A...~%" actual-slynk-port))
              (slynk:create-server :port actual-slynk-port :dont-close t))
          (error (e)
            (if *lisp-rpc-mode*
                (rpc-error (format nil "Failed to start Slynk: ~A" e))
                (progn
                  (format *error-output* "~&Error starting Slynk server: ~A~%" e)
                  (format *error-output* "~&Ensure Slynk is installed (e.g., via Quicklisp) and the port ~A is free.~%" actual-slynk-port)))))
        (write-slynk-port-file actual-slynk-port)
        (setf *slynk-port* actual-slynk-port)
        (setf *slynk-started* t)
        (when *lisp-rpc-mode*
          (rpc-log :info "Slynk started" :port *slynk-port*))))

    (unless *http-server*
      (with-suppressed-output-if-rpc
        (start-http-server :port *http-port*))
      (when *lisp-rpc-mode*
        (rpc-log :info "HTTP server started" :port *http-port*))
      (unless *silent*
        (format t "HTTP server potentially running on port ~A for API access.~%" *http-port*)))

    (setf *sessions-dir* (merge-pathnames ".hactar/sessions/" *repo-root*))
    (session/auto-restore)

    (unless *silent* (format t "~&Session initialized for ~A~%" *repo-root*))
    t))

(define-command session.new (args)
  "Initialize a new session with a path to a git repository."
  (if args
      (hactar-session-new (first args))
      (format t "Usage: /session.new <path>~%"))
  :acp (lambda (cmd-args)
         (if cmd-args
             (if (hactar-session-new (first cmd-args))
                 `(("text" . ,(format nil "Session initialized for ~A" *repo-root*)))
                 `(("text" . "Failed to initialize session.")))
             `(("text" . "Usage: /session.new <path>")))))

(defun load-custom-modes ()
  "Load custom modes from *hactar-data-path*/modes/."
  (let ((modes-dir (uiop:subpathname *hactar-data-path* "modes/")))
    (when (uiop:directory-exists-p modes-dir)
      (let ((files (uiop:directory-files modes-dir)))
        (dolist (file files)
          (let ((ext (pathname-type file)))
            (when (and (stringp ext) (string-equal ext "lisp"))
              (unless *silent* (format t "~&Loading custom mode: ~A~%" file))
              (load file))))))))

;;* CLI
(defun help--print (cmd)
  "Print comprehensive help information for Hactar, including options and subcommands."
  (format t "~A - ~A~%~%"
          (clingon:command-name cmd)
          (clingon:command-description cmd))

  (format t "Version: ~A~%" (clingon:command-version cmd))
  (format t "Authors: ~{~A~^, ~}~%" (clingon:command-authors cmd))
  (format t "License: ~A~%~%" (clingon:command-license cmd))

  (format t "USAGE:~%")
  (format t "  ~A [OPTIONS] [SUBCOMMAND]~%~%" (clingon:command-name cmd))

  (format t "OPTIONS:~%")
  (let ((options (clingon:command-options cmd)))
    (dolist (opt options)
      (let* ((long-name (clingon:option-long-name opt))
             (short-name (clingon:option-short-name opt))
             (description (clingon:option-description opt))
             (initial-value (clingon:option-initial-value opt))
             (option-type (type-of opt)))
        (format t "  ")
        (when short-name
          (format t "-~A, " short-name))
        (format t "--~A" long-name)
        (unless (or (search "FLAG" (symbol-name option-type))
                    (search "BOOLEAN" (symbol-name option-type)))
          (format t " <value>"))
        (format t "~%")
        (format t "      ~A~%" description)
        (when (and initial-value
                   (not (or (search "FLAG" (symbol-name option-type))
                            (search "BOOLEAN" (symbol-name option-type)))))
          (format t "      (default: ~A)~%" initial-value))
        (format t "~%"))))

  (format t "SUBCOMMANDS:~%")
  (let ((sorted-subcommands (sort (alexandria:hash-table-keys *sub-commands*) #'string<)))
    (dolist (subcmd-name sorted-subcommands)
      (let* ((subcmd-info (gethash subcmd-name *sub-commands*))
             (description (second subcmd-info))
             (cli-options (third subcmd-info)))
        (format t "  ~A~%" subcmd-name)
        (format t "      ~A~%" description)
        (when cli-options
          (format t "      Options:~%")
          (dolist (opt cli-options)
            (let ((opt-short (getf opt :short))
                  (opt-long (getf opt :long))
                  (opt-desc (getf opt :description)))
              (format t "        ")
              (when opt-short (format t "-~A" opt-short))
              (when (and opt-short opt-long) (format t ", "))
              (when opt-long (format t "--~A" opt-long))
              (format t " : ~A~%" opt-desc))))
        ;; Check if this is a web command and print its routes
        (let ((web-cmd (gethash subcmd-name *web-commands*)))
          (when web-cmd
            (dolist (route (web-command-routes web-cmd))
              (format t "    ~A - ~A~%"
                      (first (web-route-pattern route))
                      (web-route-description route)))))
        (format t "~%"))))

  (format t "EXAMPLES:~%")
  (format t "  # Start interactive REPL with default model~%")
  (format t "  ~A~%~%" (clingon:command-name cmd))
  (format t "  # Use a specific model~%")
  (format t "  ~A --model anthropic/claude-3-7-sonnet-20250219~%~%" (clingon:command-name cmd))
  (format t "  # Quick query without entering REPL~%")
  (format t "  ~A -q \"Explain this code\"~%~%" (clingon:command-name cmd))
  (format t "  # Generate and copy shell command~%")
  (format t "  ~A -e \"list all files modified today\"~%~%" (clingon:command-name cmd))
  (format t "  # Initialize Hactar configuration~%")
  (format t "  ~A hactar.init~%~%" (clingon:command-name cmd))
  (format t "  # Start AgentShell (ACP client TUI)~%")
  (format t "  ~A --agentshell~%~%" (clingon:command-name cmd))
  (format t "  # AgentShell with custom agent command~%")
  (format t "  ~A --agentshell --agent-command \"my-agent --acp\"~%~%" (clingon:command-name cmd)))

(defun toplevel/options ()
  "Returns the options for the main Hactar command."
  (list
   (clingon:make-option
    :string
    :description "LLM model to use (e.g., ollama/qwen3:14b)"
    :short-name #\m
    :long-name "model"
    :initial-value nil
    :key :model)
   (clingon:make-option
    :string
    :description "Project name. Defaults to current directory name."
    :long-name "name"
    :initial-value nil
    :key :name)
   (clingon:make-option
    :string
    :description "Project path."
    :long-name "path"
    :initial-value nil
    :key :path)
   (clingon:make-option
    :string
    :description "Author name. Defaults to HACTAR_AUTHOR environment variable."
    :long-name "author"
    :initial-value nil
    :key :author)
   (clingon:make-option
    :string
    :description "Path to the models configuration file (models.yaml)"
    :short-name #\c
    :long-name "config-path"
    :initial-value nil
    :key :config-path)
   (clingon:make-option
    :string
    :description "Model to use for generating embeddings"
    :long-name "embedding-model"
    :initial-value "nomic-embed-text"
    :key :embedding-model)
   (clingon:make-option
    :integer
    :description "Port for the Slynk server"
    :short-name #\p
    :long-name "slynk-port"
    :initial-value 4005
    :key :slynk-port)
   (clingon:make-option
    :string				; Expecting space-separated list
    :description "List of analyzers to disable (space-separated)."
    :long-name "disable-analyzers"
    :initial-value ""
    :key :disable-analyzers)
   (clingon:make-option
    :string				; Expecting space-separated list
    :description "List of analyzers to enable (space-separated)."
    :long-name "enable-analyzers"
    :initial-value ""
    :key :enable-analyzers)
   (clingon:make-option
    :integer
    :description "Port for the HTTP API server"
    :long-name "http-port"
    :initial-value 4269
    :key :http-port)
   (clingon:make-option
    :flag
    :description "Use Anthropic Sonnet model (anthropic/claude-3-7-sonnet-20250219)"
    :long-name "sonnet"
    :key :sonnet)
   (clingon:make-option
    :flag
    :description "Use Gemini Pro Experimental model (gemini/gemini-2.5-pro-exp-03-25)"
    :long-name "gemini"
    :key :gemini)
   (clingon:make-option
    :flag
    :description "Use free Gemini Pro Experimental via OpenRouter (openrouter/google/gemini-2.5-pro-exp-03-25:free)"
    :long-name "gemini-free"
    :key :gemini-free)
   (clingon:make-option
    :flag
    :description "Use Deepseek Chat model via OpenRouter (openrouter/deepseek/deepseek-chat-v3-0324)"
    :long-name "deepseek"
    :key :deepseek)
   (clingon:make-option
    :flag
    :description "Use free Deepseek Base model via OpenRouter (openrouter/deepseek/deepseek-v3-base:free)"
    :long-name "deepseek-free"
    :key :deepseek-free)
   (clingon:make-option
    :flag
    :description "Use OpenAI GPT-4o Mini model (openai/o4-mini)"
    :long-name "o4"
    :key :o4)
   (clingon:make-option
    :string
    :description "Send a query to the LLM, print the result, and exit."
    :short-name #\q
    :long-name "query"
    :key :immediate-query)
   (clingon:make-option
    :string
    :description "Generate a shell command from the query, print it, and copy it to clipboard."
    :short-name #\e
    :long-name "execute"
    :key :execute)
   (clingon:make-option
    :string
    :description "Generate a shell command from the query, execute it, and print its output."
    :long-name "execute-immediately"
    :key :execute-immediately)
   (clingon:make-option
    :flag
    :description "Enable assistant mode for visual interaction."
    :long-name "assistant"
    :key :assistant)
   (clingon:make-option
    :string
    :description "File path to write assistant's text extractions (used with --assistant)."
    :long-name "output"
    :key :assistant-output-file)
   (clingon:make-option
    :flag
    :description "Enable TTS audio output for assistant's extractions (used with --assistant)."
    :long-name "audio"
    :key :assistant-audio-enabled)
   (clingon:make-option
    :flag
    :description "Allow agents to run without a safe environment (e.g., container)."
    :long-name "live-dangerously"
    :key :live-dangerously)
   (clingon:make-option
    :flag
    :description "Enable automatic lint agent."
    :long-name "auto-lint"
    :key :auto-lint)
   (clingon:make-option
    :flag
    :description "Enable automatic test agent."
    :long-name "auto-test"
    :key :auto-test)
   (clingon:make-option
    :flag
    :description "Enable automatic typecheck agent."
    :long-name "auto-typecheck"
    :key :auto-typecheck)
   (clingon:make-option
    :flag
    :description "Enable AI comment handling on file changes (AI!/AI?)."
    :long-name "watch"
    :key :watch)
   (clingon:make-option
    :flag
    :description "Enable Lisp-only mode. LLM returns executable Lisp code for eval/reject."
    :long-name "lisp-mode"
    :key :lisp-mode)
   (clingon:make-option
    :flag
    :description "Enable all automation (lint, test, typecheck)."
    :long-name "auto-all"
    :key :auto-all)
   (clingon:make-option
    :flag
    :description "Indicate hactar is running inside an editor (Emacs, Vim, etc.). Disables TUI, redirects structured output to .hactar.{pid}.log."
    :long-name "in-editor"
    :key :in-editor)
   (clingon:make-option
    :flag
    :description "Start in Agent Client Protocol (ACP) mode over stdio."
    :long-name "acp"
    :key :acp)
   (clingon:make-option
    :flag
    :description "Start as a Model Context Protocol (MCP) server over stdio."
    :long-name "mcp"
    :key :mcp)
   (clingon:make-option
    :flag
    :description "Enable literate single-file mode (litmode) at startup."
    :long-name "lit"
    :key :lit)
   (clingon:make-option
    :flag
    :description "Enable Lisp-RPC mode. LLM returns Lisp s-expressions for sandboxed evaluation."
    :long-name "lisp"
    :key :lisp-rpc)
   (clingon:make-option
    :flag
    :description "Start AgentShell, an ACP client TUI for interacting with an agent."
    :long-name "agentshell"
    :key :agentshell)
   (clingon:make-option
    :string
    :description "Command to launch the ACP agent subprocess (for --agentshell)."
    :long-name "agent-command"
    :initial-value nil
    :key :agent-command)))

(defun handle-execute-flag (query run-immediately-p)
  "Handles --execute and --execute-immediately flags."
  (when (and *current-model* query (not (string= query "")))
    (let* ((system-prompt-text (uiop:read-file-string (get-prompt-path "generate-shell-command.mustache")))
           (llm-response (get-llm-response query
                                           :custom-system-prompt system-prompt-text
                                           :stream nil
                                           :add-to-history nil)))
      (if llm-response
          (let* ((extracted-block (extract-md-fenced-code-block llm-response))
                 (shell-command (when extracted-block (cdr (assoc :contents extracted-block)))))
            (if (and shell-command (not (string= shell-command "")))
                (let ((trimmed-command (string-trim '(#\Space #\Tab #\Newline #\Return) shell-command)))
                  (if run-immediately-p
                      (multiple-value-bind (output error-output exit-code)
                                           (uiop:run-program trimmed-command :output :string :error-output :string :ignore-error-status t)
					   (format t "~A" output)
					   (when (and error-output (not (string= error-output ""))) (format t "~A" error-output)) ; Print command's stderr
					   (uiop:quit exit-code))
                    (progn
                      (format t "~A~%" trimmed-command)
                      (copy-to-clipboard trimmed-command)
                      (uiop:quit 0))))
              (progn
                (format t "Error: Could not extract a valid shell command from LLM response.~%")
                (debug-log "LLM Response for shell command generation:" llm-response)
                (uiop:quit 1))))
        (progn
          (format t "Error: No response from LLM for shell command generation.~%")
          (uiop:quit 1)
          ;; Adding return-from to satisfy compiler's block analysis,
          ;; though uiop:quit means this is technically unreachable.
          (return-from handle-execute-flag nil)))))
  ;; Fallthrough conditions if model/query are not set
  (unless *current-model* (format t "Error: No model selected. Use /model to select a model.~%"))
  (unless (and query (not (string= query ""))) (format t "Error: Query for execution cannot be empty.~%"))
  (uiop:quit 1))

(defun toplevel/handler (cmd)
  "The handler function for the main Hactar command."
  (let* ((free-args (clingon:command-arguments cmd))
         (model-from-opt (clingon:getopt cmd :model))
         (name-from-opt (clingon:getopt cmd :name))
         (author-from-opt (clingon:getopt cmd :author))
         (config-path (clingon:getopt cmd :config-path))
         (embedding-model-from-opt (clingon:getopt cmd :embedding-model))
         (slynk-port (clingon:getopt cmd :slynk-port))
         (http-port (clingon:getopt cmd :http-port))
         (disable-analyzers-str (clingon:getopt cmd :disable-analyzers))
         (enable-analyzers-str (clingon:getopt cmd :enable-analyzers))
         (use-sonnet (clingon:getopt cmd :sonnet))
         (use-gemini (clingon:getopt cmd :gemini))
         (use-gemini-free (clingon:getopt cmd :gemini-free))
         (use-deepseek (clingon:getopt cmd :deepseek))
         (use-deepseek-free (clingon:getopt cmd :deepseek-free))
         (use-o4 (clingon:getopt cmd :o4))
         (immediate-query (clingon:getopt cmd :immediate-query))
         (execute-query (clingon:getopt cmd :execute))
         (execute-immediately-query (clingon:getopt cmd :execute-immediately))
         (assistant-mode (clingon:getopt cmd :assistant))
         (assistant-output (clingon:getopt cmd :assistant-output-file))
         (assistant-audio (clingon:getopt cmd :assistant-audio-enabled))
         (lit-flag (clingon:getopt cmd :lit))
         (live-dangerously (clingon:getopt cmd :live-dangerously))
         (auto-lint (clingon:getopt cmd :auto-lint))
         (auto-test (clingon:getopt cmd :auto-test))
         (auto-typecheck (clingon:getopt cmd :auto-typecheck))
         (auto-all (clingon:getopt cmd :auto-all))
         (watch-flag (clingon:getopt cmd :watch))
         (lisp-mode-flag (clingon:getopt cmd :lisp-mode))
         (acp-flag (clingon:getopt cmd :acp))
         (mcp-flag (clingon:getopt cmd :mcp))
         (lisp-rpc-flag (clingon:getopt cmd :lisp-rpc))
         (agentshell-flag (clingon:getopt cmd :agentshell))
         (agent-command-str (clingon:getopt cmd :agent-command))
         (in-editor-flag (or (clingon:getopt cmd :in-editor)
                             (let ((env-val (uiop:getenv "HACTAR_IN_EDITOR")))
                               (and env-val (not (string= env-val "")) (not (string-equal env-val "false"))))))
         (path (or (clingon:getopt cmd :path)
                   (let ((args-str (format nil "~{~A~^ ~}" free-args)))
                     (multiple-value-bind (match regs)
                         (cl-ppcre:scan-to-strings "--path\\s+([^\\s]+)" args-str)
                       (declare (ignore match))
                       (when (and regs (> (length regs) 0))
                         (aref regs 0))))))
	 (model model-from-opt))

    (when slynk-port (setf *slynk-port* slynk-port))
    (when http-port (setf *http-port* http-port))

    (when live-dangerously
      (setf *live-dangerously* t)
      (unless *silent* (format t "Running in live-dangerously mode. Agents can execute commands.~%")))

    (when in-editor-flag
      (setf *in-editor* t)
      (unless *silent* (format t "Running in editor mode. Structured output will be logged.~%")))

    (when auto-all
      (setf *auto-lint* t)
      (setf *auto-test* t)
      (setf *auto-typecheck* t))
    (when auto-lint (setf *auto-lint* t))
    (when auto-test (setf *auto-test* t))
    (when auto-typecheck (setf *auto-typecheck* t))

    (when (or immediate-query execute-query execute-immediately-query free-args acp-flag mcp-flag agentshell-flag lisp-rpc-flag)
      (setf *silent* t))	  ; Enable silent mode for non-interactive modes

    (when use-sonnet (setf model "anthropic/claude-3-7-sonnet-20250219"))
    (when use-gemini (setf model "gemini/gemini-2.5-pro-exp-03-25"))
    (when use-gemini-free (setf model "openrouter/google/gemini-2.5-pro-exp-03-25:free"))
    (when use-deepseek (setf model "openrouter/deepseek/deepseek-chat-v3-0324"))
    (when use-deepseek-free (setf model "openrouter/deepseek/deepseek-v3-base:free"))
    (when use-o4 (setf model "openai/o4-mini"))

    (ensure-directories-exist (directory-namestring (get-models-config-path)))
    (ensure-directories-exist (merge-pathnames "hactar/" (get-xdg-config-dir)))

    (load-models-config (or config-path (get-models-config-path)))
    (set-current-model (or model
                           (uiop:getenv "HACTAR_MODEL")
                           "ollama/minimax-m2.5:cloud"))
    (when embedding-model-from-opt
      (setf *embedding-model* embedding-model-from-opt))
    (setf *completion-model* *current-model*)
    ;; (setf *repo-root*
    ;;       (handler-case
    ;; 	      (find-git-repo-root (if path
    ;; 				      (uiop:ensure-directory-pathname (uiop:parse-native-namestring path))
    ;; 				    (uiop:getcwd)))
    ;;         (error (e)
    ;; 		   (log-warning "Failed to detect git repository root (continuing without repo): ~A" e)
    ;; 		   (when path
    ;;                  (uiop:ensure-directory-pathname (uiop:parse-native-namestring path))))))

    (debug-log (format nil "Repository root set to: ~A~%" *repo-root*))

    (when name-from-opt
      (setf *name* name-from-opt))

    (when author-from-opt
      (setf *author* author-from-opt))

    (dolist (name-str (uiop:split-string disable-analyzers-str :separator " "))
      (when (string/= name-str "")
        (let ((analyzer-sym (ignore-errors (intern (string-upcase name-str) :hactar))))
          (if (and analyzer-sym (gethash analyzer-sym *analyzer-registry*))
              (progn
		(set-analyzer-enabled analyzer-sym nil)
		(unless *silent* (format t "  Disabled analyzer via CLI: ~A~%" name-str)))
              (warn "Unknown analyzer specified in --disable-analyzers: ~A" name-str)))))
    (dolist (name-str (uiop:split-string enable-analyzers-str :separator " "))
      (when (string/= name-str "")
        (let ((analyzer-sym (ignore-errors (intern (string-upcase name-str) :hactar))))
          (if (and analyzer-sym (gethash analyzer-sym *analyzer-registry*))
              (progn
		(set-analyzer-enabled analyzer-sym t)
		(unless *silent* (format t "  Enabled analyzer via CLI: ~A~%" name-str)))
              (warn "Unknown analyzer specified in --enable-analyzers: ~A" name-str)))))

    (when watch-flag
      (set-analyzer-enabled 'ai-comment-edit t)
      (set-analyzer-enabled 'ai-comment-question t)
      (unless *silent* (format t "  AI comment handling enabled (AI!/AI?).~%")))
    
    (when lisp-mode-flag
      (setf *lisp-mode-enabled* t)
      (unless *silent* (format t "  Lisp-only mode enabled.~%")))

    (when lisp-rpc-flag
      (setf *lisp-rpc-mode* t)
      (unless *silent* (format t "  Lisp-RPC mode enabled. LLM will return Lisp forms.~%")))
    
    (load-custom-modes)

    (let ((detected-root (ignore-errors (find-git-repo-root (if path
                                                                (uiop:ensure-directory-pathname (uiop:parse-native-namestring path))
                                                                (uiop:getcwd))))))
      (if detected-root
          (hactar-session-new (namestring detected-root))
          (unless *silent*
            (format t "~&Warning: No git repository detected. Some features will be disabled.~%")
            (format t "Use /session.new <path> to initialize a session later.~%"))))

    (when free-args
      (let* ((command-name (first free-args))
             (command-info (gethash command-name *sub-commands*)))
        (when command-info
          (let ((command-fn (first command-info))
                (command-args (rest free-args)))
            (funcall command-fn command-args)
            (uiop:quit 0)))))

    (when assistant-mode
      (setf *assistant-mode-active* t)
      (setf *assistant-output-file* assistant-output)
      (setf *assistant-audio-enabled* assistant-audio)
      (unless *silent* (format t "~&Entering Assistant Mode...~%"))
      (when *assistant-output-file*
        (unless *silent* (format t "Assistant extractions will be written to: ~A~%" *assistant-output-file*)))
      (when *assistant-audio-enabled*
        (unless *silent* (format t "Assistant TTS audio output enabled.~%"))))

    (cond
      (mcp-flag
       (start-mcp)
       (uiop:quit 0))
      (acp-flag
       (start-acp)
       (uiop:quit 0))
      (agentshell-flag
       (let ((agent-cmd (when agent-command-str
                          (str:split #\Space agent-command-str))))
         (start-agentshell :agent-command agent-cmd))
       (uiop:quit 0))
      (execute-query
       (handle-execute-flag execute-query nil))
      (execute-immediately-query
       (handle-execute-flag execute-immediately-query t))
      (immediate-query
       (progn
         (when (and *current-model* immediate-query)
           (let ((response (get-llm-response immediate-query :stream nil :add-to-history nil)))
             (format t "~A~%" response)))
         (uiop:quit 0)))
      (lisp-rpc-flag
       (progn
         (load-chat-history)
         (when lit-flag
           (init-litmode))
         ;; Enter the blocking main loop (no REPL)
         (unwind-protect
              (lisp-rpc-main-loop)
           (ignore-errors (session/auto-save))
           (delete-slynk-port-file)
           (when *editor-log-file*
             (ignore-errors (delete-file *editor-log-file*)))
           (ignore-errors (stop-http-server))
           (unless *file-watcher-stopped*
             (setf *file-watcher-stopped* t)
             (handler-case (stop-file-watcher)
               (serious-condition (e) (format *error-output* "~&Cleanup error: ~A~%" e))))
           (handler-case
               (maphash (lambda (k v) (declare (ignore k))
                          (handler-case (stop-watcher v)
                            (serious-condition (e) (format *error-output* "~&Cleanup error: ~A~%" e))))
                        *active-watchers*)
             (serious-condition (e) (format *error-output* "~&Cleanup error: ~A~%" e))))))
      (t
       (progn
       (load-chat-history)
       (when lit-flag
         (init-litmode))

       ;; rl:initialize is now called earlier, before variable-bind
       (rl:bind-keyseq "\\C-l"
                       (lambda (count key)
                         (declare (ignore count key))
                         (format t "~c[2J~c[H" #\Escape #\Escape)
                         (rl:redisplay)))
       (unwind-protect
            (repl)
         (ignore-errors (session/auto-save))
         (delete-slynk-port-file)
         (when *assistant-last-screenshot-path*
           (ignore-errors (delete-file *assistant-last-screenshot-path*)))
         (when *assistant-last-audio-file*
           (ignore-errors (delete-file *assistant-last-audio-file*)))
         (when *editor-log-file*
           (ignore-errors (delete-file *editor-log-file*)))
         (ignore-errors (stop-http-server))
         (unless *file-watcher-stopped*
           (setf *file-watcher-stopped* t)
           (handler-case (stop-file-watcher)
             (serious-condition (e) (format *error-output* "~&Cleanup error (stop-file-watcher): ~A~%" e))))
         (handler-case
           (maphash (lambda (k v) (declare (ignore k))
                      (handler-case (stop-watcher v)
                        (serious-condition (e) (format *error-output* "~&Cleanup error (stop-watcher ~A): ~A~%" k e))))
                    *active-watchers*)
           (serious-condition (e) (format *error-output* "~&Cleanup error (maphash watchers): ~A~%" e)))
         (handler-case (rl:deprep-terminal)
           (serious-condition (e) (format *error-output* "~&Cleanup error (deprep-terminal): ~A~%" e)))))))))

(defun hactar-init ()
  "Initialize Hactar by cloning the repo and copying default prompts and models.yaml."
  (let* ((repo-url *hactar-repo-url*)
         (repo-dir (%to-pathname *hactar-repo-dir*))
         (repo-dir-native (uiop:native-namestring (uiop:ensure-directory-pathname repo-dir)))
	 (data-dir (%to-pathname *hactar-data-path*))
         (config-dir (%to-pathname *hactar-config-path*))
         (config-dir-native (uiop:native-namestring (uiop:ensure-directory-pathname config-dir)))
         (prompts-src (uiop:subpathname (uiop:ensure-directory-pathname repo-dir) "prompts/"))
         (prompts-dst (uiop:subpathname (uiop:ensure-directory-pathname config-dir) "prompts/"))
         (models-src (uiop:subpathname (uiop:ensure-directory-pathname repo-dir) "data/models.yaml"))
         (models-dst (get-models-config-path))
         (ok t))

    (handler-case
        (progn
          (if (uiop:directory-exists-p (uiop:ensure-directory-pathname repo-dir))
              (multiple-value-bind (out err code)
                  (uiop:run-program (list "git" "-C" repo-dir-native "rev-parse" "--is-inside-work-tree")
                                    :output :string :error-output :string :ignore-error-status t)
                (declare (ignore out))
                (if (zerop code)
                    (progn
                      (format t "Updating Hactar repo at ~A...~%" repo-dir-native)
                      (multiple-value-bind (o e c)
                          (uiop:run-program (list "git" "-C" repo-dir-native "pull" "--ff-only")
                                            :output :string :error-output :string :ignore-error-status t)
                        (declare (ignore o))
                        (unless (zerop c)
                          (setf ok t)
                          (log-warning "Failed to update repo (~A)" e))))
                    (progn
                      (setf ok t)
                      (log-warning "Target directory exists but is not a git repo: ~A" repo-dir-native))))
              (progn
                (ensure-directories-exist (uiop:pathname-directory-pathname (uiop:ensure-directory-pathname repo-dir)))
                (format t "Cloning Hactar repo from ~A to ~A...~%" repo-url repo-dir-native)
                (multiple-value-bind (out err code)
                    (uiop:run-program (list "git" "clone" repo-url repo-dir-native)
                                      :output :string :error-output :string :ignore-error-status t)
                  (declare (ignore out))
                  (if (zerop code)
                      (log-good "Cloned Hactar repo to ~A" repo-dir-native)
                      (progn
                        (setf ok t)
                        (log-warning "Failed to clone Hactar repo: ~A" err)))))))
      (error (e)
        (setf ok t)
        (log-warning "Error cloning/updating Hactar repo: ~A" e)))

    (when ok
      (handler-case
          (progn
            (ensure-directories-exist (uiop:ensure-directory-pathname data-dir))
            (multiple-value-bind (out err code)
                (uiop:run-program (list "cp" "-r"
                                        (uiop:native-namestring (uiop:ensure-directory-pathname prompts-src))
                                        (uiop:native-namestring (uiop:ensure-directory-pathname data-dir)))
                                  :output :string :error-output :string :ignore-error-status t)
              (declare (ignore out))
              (if (zerop code)
                  (log-good "Copied prompts to ~A" (uiop:native-namestring (uiop:ensure-directory-pathname prompts-dst)))
                  (progn
                    (setf ok nil)
                    (log-warning "Failed to copy prompts: ~A" err)))))
        (error (e)
          (setf ok nil)
          (log-warning "Error copying prompts: ~A" e))))

    (when ok
      (handler-case
          (progn
            (ensure-directories-exist (uiop:ensure-directory-pathname (uiop:pathname-directory-pathname (%to-pathname models-dst))))
            (multiple-value-bind (out err code)
                (uiop:run-program (list "cp"
                                        (uiop:native-namestring models-src)
                                        (uiop:native-namestring (%to-pathname models-dst)))
                                  :output :string :error-output :string :ignore-error-status t)
              (declare (ignore out))
              (if (zerop code)
                  (log-good "Copied models.yaml to ~A" (uiop:native-namestring (%to-pathname models-dst)))
                  (progn
                    (setf ok nil)
                    (log-warning "Failed to copy models.yaml: ~A" err)))))
        (error (e)
          (setf ok nil)
          (log-warning "Error copying models.yaml: ~A" e))))

    (when ok
      (unless (hactar-migrations:run-migrations)
	(format *error-output* "Failed to run database migrations. Aborting.~%")
	(uiop:quit 1)))
    ok))

(define-sub-command hactar.init (args)
  "Initialize Hactar: clone repo and install default prompts and models."
  (declare (ignore args))
  (let ((ok (hactar-init)))
    (uiop:quit (if ok 0 1))))

(define-sub-command help (args)
		    "Display comprehensive help information about Hactar."
		    (declare (ignore args))
		    (help--print (toplevel/command))
		    (uiop:quit 0))

(defun toplevel/command ()
  "Creates the main Hactar command object with subcommands."
  (clingon:make-command
   :name "hactar"
   :description "Hactar AI Pair Programmer"
   :version *hactar-version*
   :authors '("K-2052")
   :license "MIT"
   :options (toplevel/options)
   :handler #'toplevel/handler))

;;* Main
(defun option-takes-argument-p (opt)
  "Check if a clingon option takes an argument."
  (let ((option-type (type-of opt)))
    (not (or (search "FLAG" (symbol-name option-type))
             (search "BOOLEAN" (symbol-name option-type))))))

(defun get-arg-consuming-flags ()
  "Get a list of flags that consume the next argument."
  (let ((opts (toplevel/options))
        (flags '()))
    (dolist (opt opts)
      (when (option-takes-argument-p opt)
        (when (clingon:option-short-name opt)
          (push (format nil "-~A" (clingon:option-short-name opt)) flags))
        (when (clingon:option-long-name opt)
          (push (format nil "--~A" (clingon:option-long-name opt)) flags))))
    flags))

(defun preprocess-arguments (args)
  "Injects '--' before the first subcommand found in args to prevent clingon from parsing subcommand flags."
  (let ((new-args '())
        (consuming-arg nil)
        (arg-flags (get-arg-consuming-flags))
        (found-subcommand nil))
    (dolist (arg args)
      (cond
        (found-subcommand
         (push arg new-args))
        (consuming-arg
         (push arg new-args)
         (setf consuming-arg nil))
        ((member arg arg-flags :test #'string=)
         (push arg new-args)
         (setf consuming-arg t))
        ((and (not (str:starts-with? "-" arg))
              (gethash arg *sub-commands*))
         (setf found-subcommand t)
         (push "--" new-args)
         (push arg new-args))
        (t
         (push arg new-args))))
    (nreverse new-args)))

(defun main (&optional provided-args)
  "Main entry point for the Hactar application executable."
  (let ((app (toplevel/command))
        (args (preprocess-arguments (or provided-args (uiop:command-line-arguments)))))
    (clingon:run app args)))
