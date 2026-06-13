;;* Core commands and chat/repl handling
(in-package :hactar)

(declaim (ftype function lisp-mode-intercept start-agentshell))

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

(defun get-mode-rules-section ()
  "Generate the rules section for the system prompt from active modes (Refactor Plan §7.6)."
  (get-active-mode-rules))

(defun get-entity-rules-section ()
  "Deprecated shim: the BOT entity system is being replaced by modes.
   Returns the active mode rules so existing callers keep working."
  (get-mode-rules-section))

(defun get-feature-rules-section ()
  "Deprecated shim: the feature system is being replaced by modes.
   Returns \"\" so mode rules are not duplicated (the entity shim already emits them)."
  "")

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

(define-command help.flag (args)
                "Print details and specifications for a registered global CLI flag."
                (if args
                    (let* ((flag-name (first args))
                           (flag (gethash flag-name *flags*)))
                      (unless flag
                        (setf flag (or (gethash (format nil "-~A" flag-name) *flags*)
                                       (gethash (format nil "--~A" flag-name) *flags*))))
                      (if flag
                          (progn
                            (format t "Flag: ~{~A~^, ~}~%" (append (flag-long-names flag) (flag-short-names flag)))
                            (format t "  Description: ~A~%" (or (flag-description flag) "No description available."))
                            (format t "  Takes value: ~A~%" (if (flag-takes-value flag) "Yes" "No"))
                            (when (flag-examples flag)
                              (format t "  Examples:~%")
                              (dolist (ex (flag-examples flag))
                                (format t "    ~A~%" ex))))
                          (format t "Flag not found: ~A~%" flag-name)))
                    (format t "Usage: /help.flag <flag-name>~%"))
                :acp t)

(define-command help (args)
                "Display available commands and their descriptions. Use --spec or --spec-lisp for machine-readable output."
                (cond
                  ((member "--spec-lisp" args :test #'string=)
                   (emit-cli-spec :lisp-p t))
                  ((member "--spec" args :test #'string=)
                   (emit-cli-spec))
                  (t
                  (format t "~A~%" (colorize "Available commands:" :bold-cyan))
                 ;; Sort commands alphabetically for better readability
                 (let ((sorted-commands (sort (alexandria:hash-table-keys *commands*) #'string<)))
                   (dolist (cmd sorted-commands)
                     (unless (gethash cmd *hidden-commands*)
                       (let* ((command-info (gethash cmd *commands*))
                              (description (second command-info)))
                         (format t "  ~A ~A ~A~%" (colorize cmd :bold-green) (colorize "-" :dim) (colorize description :dim))))))
                 (format t "~%~A~%" (colorize "Available sub-commands:" :bold-cyan))
                 (let ((sorted-subcommands (sort (alexandria:hash-table-keys *sub-commands*) #'string<)))
                   (dolist (subcmd sorted-subcommands)
                     (unless (gethash (format nil "/~A" subcmd) *hidden-commands*)
                       (let* ((subcmd-info (gethash subcmd *sub-commands*))
                              (description (second subcmd-info))
                              (cli-options (third subcmd-info)))
                         (format t "  ~A~%" (colorize subcmd :bold-magenta))
                         (format t "      ~A~%" (colorize description :dim))
                         (when cli-options
                           (format t "      ~A~%" (colorize "Options:" :cyan))
                           (dolist (opt cli-options)
                             (let ((opt-short (getf opt :short))
                                   (opt-long (getf opt :long))
                                   (opt-desc (getf opt :description)))
                               (format t "        ")
                               (when opt-short (format t "~A" (colorize (format nil "-~A" opt-short) :yellow)))
                               (when (and opt-short opt-long) (format t "~A " (colorize "," :dim)))
                               (when opt-long (format t "~A" (colorize (format nil "--~A" opt-long) :yellow)))
                               (format t " ~A~%" (colorize (format nil ": ~A" opt-desc) :dim)))))
                         ;; Check if this is a web command and print its routes
                         (let ((web-cmd (gethash subcmd *web-commands*)))
                           (when web-cmd
                             (dolist (route (web-command-routes web-cmd))
                               (format t "    ~A ~A ~A~%"
                                       (colorize (first (web-route-pattern route)) :cyan)
                                       (colorize "-" :dim)
                                       (colorize (web-route-description route) :dim)))))))))))
                :acp (lambda (cmd-args)
                       (cond
                         ((member "--spec-lisp" cmd-args :test #'string=)
                          `(("text" . ,(with-output-to-string (s) (emit-cli-spec :lisp-p t :stream s)))))
                         ((member "--spec" cmd-args :test #'string=)
                          `(("text" . ,(with-output-to-string (s) (emit-cli-spec :stream s)))
                            ("data" . ,(build-cli-spec))))
                         (t
                        (let ((commands '()))
                          (maphash (lambda (cmd-name handler)
                                     (declare (ignore handler))
                                     (unless (gethash cmd-name *hidden-commands*)
                                       (let* ((cmd-info (gethash cmd-name *commands*))
                                              (description (if cmd-info (second cmd-info) "")))
                                         (push `(("name" . ,(string-trim "/" cmd-name))
                                                 ("description" . ,description))
                                               commands))))
                                   *acp-commands*)
                          `(("text" . ,(format nil "~A commands available." (length commands)))
                            ("data" . ,(coerce (sort commands #'string< :key (lambda (c) (cdr (assoc "name" c :test #'string=)))) 'vector))))))))

(defun token-cost-for-count (token-count)
  "Return estimated input cost for TOKEN-COUNT using the current model."
  (calculate-model-cost *current-model* :input-tokens token-count))

(defun token-breakdown-items ()
  "Return a list of token breakdown plists for system prompt, chat history, and files."
  (let* ((system-text (system-prompt))
         (system-tokens (estimate-tokens system-text))
         (history-text (with-output-to-string (s)
                         (dolist (msg *chat-history*)
                           (format s "~A~%~A~%~%"
                                   (or (cdr (assoc :role msg)) "")
                                   (or (cdr (assoc :content msg)) "")))))
         (history-tokens (estimate-tokens history-text))
         (items (list (list :label "system messages"
                            :tokens system-tokens
                            :cost (token-cost-for-count system-tokens)
                            :hint nil)
                      (list :label "chat history"
                            :tokens history-tokens
                            :cost (token-cost-for-count history-tokens)
                            :hint "use /clear to clear"))))
    (dolist (file *files*)
      (let* ((rel (if *repo-root*
                      (uiop:native-namestring (uiop:enough-pathname file *repo-root*))
                      (uiop:native-namestring file)))
             (content (or (ignore-errors (get-file-content file)) ""))
             (tokens (estimate-tokens content)))
        (push (list :label rel
                    :tokens tokens
                    :cost (token-cost-for-count tokens)
                    :hint "/drop to remove")
              items)))
    (nreverse items)))

(defun render-token-breakdown ()
  "Render a granular token breakdown string."
  (let* ((items (token-breakdown-items))
         (total (reduce #'+ items :key (lambda (item) (getf item :tokens)) :initial-value 0))
         (total-cost (reduce #'+ items :key (lambda (item) (getf item :cost)) :initial-value 0.0)))
    (with-output-to-string (s)
      (dolist (item items)
        (format s "$ ~,4F ~10:D ~A~@[ ~A~]~%"
                (getf item :cost)
                (getf item :tokens)
                (getf item :label)
                (getf item :hint)))
      (format s "==================~%")
      (format s "$ ~,4F ~10:D tokens total~%" total-cost total))))

(define-command tokens (args)
                "Report on the number of tokens used by the current chat context."
                (declare (ignore args))
                (format t "~A" (render-token-breakdown))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (let* ((items (token-breakdown-items))
                              (total (reduce #'+ items :key (lambda (item) (getf item :tokens)) :initial-value 0))
                              (total-cost (reduce #'+ items :key (lambda (item) (getf item :cost)) :initial-value 0.0)))
                         `(("text" . ,(render-token-breakdown))
                           ("data" . (("items" . ,(coerce
                                                   (mapcar (lambda (item)
                                                             `(("label" . ,(getf item :label))
                                                               ("tokens" . ,(getf item :tokens))
                                                               ("cost" . ,(getf item :cost))
                                                               ("hint" . ,(or (getf item :hint) :null))))
                                                           items)
                                                   'vector))
                                      ("estimatedTokens" . ,total)
                                      ("estimatedCost" . ,total-cost)
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


(define-command exit (args)
                "Exit the application. Alias for /quit."
                (declare (ignore args))
                (if *lisp-rpc-mode*
                    (progn
                      (rpc-exit "quit")
                      (setf *lisp-rpc-mode* nil))
                    (progn
                      (format t "Exiting hactar...~%")
                      (uiop:quit))))

(define-command state (args)
  "View and inspect Hactar state variables."
  (let* ((modified-only-p (member "--modified" args :test #'string=))
         (clean-args (remove "--modified" args :test #'string=))
         (var-name (first clean-args)))
    (if var-name
        (let* ((search-name (string-downcase (string-trim '(#\*) var-name)))
               (info (gethash (format nil "*~A*" search-name) *state-registry*))
               (info-exact (gethash (string-downcase var-name) *state-registry*))
               (found (or info-exact info)))
          (if (and found (or (not modified-only-p) (state-modified-p found)))
               (format t "~A~%" (format-state-xml found))
               (format t "</notfound>~%")))
        (if *in-editor*
            (progn
              (format t "<state>~%")
              (maphash (lambda (k info)
                         (declare (ignore k))
                         (when (or (not modified-only-p) (state-modified-p info))
                           (format t "~A~%" (format-state-xml info))))
                       *state-registry*)
              (format t "</state>~%"))
            (let* ((items (loop for k being the hash-keys of *state-registry*
                                 for info being the hash-values of *state-registry*
                                 when (or (not modified-only-p) (state-modified-p info))
                                 collect `((:item . ,k)
                                           (:preview . ,(format nil "Type: ~A~%~A" (getf info :type) (or (getf info :doc) ""))))))
                    (selected-item (when (fboundp 'fuzzy-select)
                                     (fuzzy-select items))))
              (if selected-item
                  (let* ((info (gethash (cdr (assoc :item selected-item)) *state-registry*))
                         (sym (getf info :name)))
                    (format t "Variable: ~A~%" sym)
                    (format t "Value: ~A~%" (if (and sym (boundp sym)) (symbol-value sym) "nil"))
                    (format t "Type: ~A~%" (getf info :type))
                    (format t "Doc: ~A~%" (or (getf info :doc) "None")))
                  (format t "Selection cancelled.~%"))))))
  :json (lambda (args)
          (let* ((modified-only-p (member "--modified" args :test #'string=))
                 (clean-args (remove "--modified" args :test #'string=))
                 (var-name (first (remove-if (lambda (arg) (str:starts-with-p "-" arg)) clean-args)))
                 (found (when var-name
                          (let* ((search-name (string-downcase (string-trim '(#\*) var-name)))
                                 (info (gethash (format nil "*~A*" search-name) *state-registry*))
                                 (info-exact (gethash (string-downcase var-name) *state-registry*)))
                            (or info-exact info)))))
            (if found
                (if (or (not modified-only-p) (state-modified-p found))
                    (to-json `(("name" . ,(symbol-name (getf found :name)))
                               ("type" . ,(getf found :type))
                               ("doc" . ,(or (getf found :doc) ""))
                               ("value" . ,(let ((sym (getf found :name)))
                                             (if (boundp sym)
                                                 (let ((v (symbol-value sym)))
                                                   (cond
                                                     ((eq v t) t)
                                                     ((eq v nil) :false)
                                                     ((or (stringp v) (numberp v)) v)
                                                     (t (princ-to-string v))))
                                                 :null)))))
                    (to-json nil))
                (let ((vars '()))
                  (maphash (lambda (k v)
                             (declare (ignore k))
                             (when (or (not modified-only-p) (state-modified-p v))
                               (push `(("name" . ,(symbol-name (getf v :name)))
                                       ("type" . ,(getf v :type))
                                       ("doc" . ,(or (getf v :doc) ""))
                                       ("value" . ,(let ((sym (getf v :name)))
                                                     (if (boundp sym)
                                                         (let ((val (symbol-value sym)))
                                                           (cond
                                                             ((eq val t) t)
                                                             ((eq val nil) :false)
                                                             ((or (stringp val) (numberp val)) val)
                                                             (t (princ-to-string val))))
                                                         :null))))
                                     vars)))
                           *state-registry*)
                  (to-json (coerce vars 'vector))))))
  :yaml (lambda (args)
          (let* ((modified-only-p (member "--modified" args :test #'string=))
                 (clean-args (remove "--modified" args :test #'string=))
                 (var-name (first (remove-if (lambda (arg) (str:starts-with-p "-" arg)) clean-args)))
                 (found (when var-name
                          (let* ((search-name (string-downcase (string-trim '(#\*) var-name)))
                                 (info (gethash (format nil "*~A*" search-name) *state-registry*))
                                 (info-exact (gethash (string-downcase var-name) *state-registry*)))
                            (or info-exact info)))))
            (with-output-to-string (s)
              (if found
                  (when (or (not modified-only-p) (state-modified-p found))
                    (let ((sym (getf found :name)))
                      (format s "name: ~A~%type: ~A~%doc: ~A~%value: ~A~%"
                              (symbol-name sym)
                              (getf found :type)
                              (or (getf found :doc) "")
                              (if (boundp sym) (symbol-value sym) "null"))))
                  (maphash (lambda (k v)
                             (declare (ignore k))
                             (when (or (not modified-only-p) (state-modified-p v))
                               (let ((sym (getf v :name)))
                                 (format s "- name: ~A~%  type: ~A~%  doc: ~A~%  value: ~A~%"
                                         (symbol-name sym)
                                         (getf v :type)
                                         (or (getf v :doc) "")
                                         (if (boundp sym) (symbol-value sym) "null")))))
                           *state-registry*)))))
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
                         (estimated-cost (calculate-model-cost *current-model* :input-tokens estimated-tokens)))
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
                                   (estimated-cost (calculate-model-cost *current-model* :input-tokens estimated-tokens)))
                             `(("text" . ,(format nil "Model: ~A, Tokens: ~A, Cost: $~8F"
                                                   (model-config-name *current-model*)
                                                   estimated-tokens estimated-cost))
                               ("data" . (("model" . ,(model-config-name *current-model*))
                                          ("estimatedTokens" . ,estimated-tokens)
                                          ("inputCostPerToken" . ,input-cost)
                                          ("estimatedCost" . ,estimated-cost)))))
                            `(("text" . "No model selected. Cannot estimate cost.")))))

(define-command history (args)
  "View or manage the chat history. Supports format flags: --json (-j, json), --lisp (-l, lisp), --yaml (-y, yaml), --toml (-t, toml)."
  (let* ((format (cond
                   ((or (member "--json" args :test #'string-equal)
                        (member "-j" args :test #'string-equal)
                        (member "json" args :test #'string-equal))
                    :json)
                   ((or (member "--lisp" args :test #'string-equal)
                        (member "-l" args :test #'string-equal)
                        (member "lisp" args :test #'string-equal))
                    :lisp)
                   ((or (member "--yaml" args :test #'string-equal)
                        (member "-y" args :test #'string-equal)
                        (member "yaml" args :test #'string-equal))
                    :yaml)
                   ((or (member "--toml" args :test #'string-equal)
                        (member "-t" args :test #'string-equal)
                        (member "toml" args :test #'string-equal))
                    :toml)))
         (clean-args (remove-if (lambda (x)
                                  (member x '("--json" "-j" "json" "--lisp" "-l" "lisp"
                                              "--yaml" "-y" "yaml" "--toml" "-t" "toml")
                                          :test #'string-equal))
                                args))
         (iface (gethash :history *interfaces*))
         (path (and iface (interface-abs-path iface))))
    (cond
      ((null clean-args)
       (cond
         ((eq format :json)
          (format t "~A~%" (cl-json:encode-json-to-string *chat-history*)))
         ((eq format :lisp)
          (format t ";;; Chat history~%(in-package :hactar)~%~%(setf *chat-history* '~S)~%" *chat-history*))
         ((eq format :yaml)
          (format t "~A" (serialize-history-to-yaml *chat-history*)))
         ((eq format :toml)
          (format t "~A" (serialize-to-toml *chat-history*)))
         (t
          (if (and path (probe-file path))
              (uiop:run-program (list "less" (namestring path))
                                :output :interactive
                                :error-output :interactive
                                :input :interactive)
              (format t "No history file found.~%")))))
      ((string= (first clean-args) "clear")
       (clear-chat-history)
       (format t "History cleared.~%"))
      ((string= (first clean-args) "save")
       (if (second clean-args)
           (cond
             ((eq format :json)
              (with-open-file (stream (second clean-args) :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
                (cl-json:encode-json *chat-history* stream))
              (format t "History saved as JSON to ~A~%" (second clean-args)))
             ((eq format :lisp)
              (with-open-file (stream (second clean-args) :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
                (format stream ";;; Chat history~%(in-package :hactar)~%~%(setf *chat-history* '~S)~%" *chat-history*))
              (format t "History saved as Lisp to ~A~%" (second clean-args)))
             ((eq format :yaml)
              (with-open-file (stream (second clean-args) :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
                (write-string (serialize-history-to-yaml *chat-history*) stream))
              (format t "History saved as YAML to ~A~%" (second clean-args)))
             ((eq format :toml)
              (with-open-file (stream (second clean-args) :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
                (write-string (serialize-to-toml *chat-history*) stream))
              (format t "History saved as TOML to ~A~%" (second clean-args)))
             (t
              (if (and path (probe-file path))
                  (progn
                    (uiop:run-program (list "cp" (namestring path) (second clean-args))
                                      :output :interactive
                                      :error-output :interactive)
                    (format t "History saved to ~A~%" (second clean-args)))
                  (format t "No history file found to save.~%"))))
           (format t "Please specify a filename.~%")))))
  :acp (lambda (cmd-args)
         (let* ((format (cond
                          ((or (member "--json" cmd-args :test #'string-equal)
                               (member "-j" cmd-args :test #'string-equal)
                               (member "json" cmd-args :test #'string-equal))
                           :json)
                          ((or (member "--lisp" cmd-args :test #'string-equal)
                               (member "-l" cmd-args :test #'string-equal)
                               (member "lisp" cmd-args :test #'string-equal))
                           :lisp)
                          ((or (member "--yaml" cmd-args :test #'string-equal)
                               (member "-y" cmd-args :test #'string-equal)
                               (member "yaml" cmd-args :test #'string-equal))
                           :yaml)
                          ((or (member "--toml" cmd-args :test #'string-equal)
                               (member "-t" cmd-args :test #'string-equal)
                               (member "toml" cmd-args :test #'string-equal))
                           :toml)))
                (clean-args (remove-if (lambda (x)
                                         (member x '("--json" "-j" "json" "--lisp" "-l" "lisp"
                                                     "--yaml" "-y" "yaml" "--toml" "-t" "toml")
                                                 :test #'string-equal))
                                       cmd-args))
                (iface (gethash :history *interfaces*))
                (path (and iface (interface-abs-path iface))))
           (cond
             ((null clean-args)
              (cond
                ((eq format :json)
                 `(("text" . ,(cl-json:encode-json-to-string *chat-history*))
                   ("data" . (("history" . ,*chat-history*)))))
                ((eq format :lisp)
                 `(("text" . ,(format nil ";;; Chat history~%(in-package :hactar)~%~%(setf *chat-history* '~S)~%" *chat-history*))
                   ("data" . (("history" . ,*chat-history*)))))
                ((eq format :yaml)
                 `(("text" . ,(serialize-history-to-yaml *chat-history*))
                   ("data" . (("history" . ,*chat-history*)))))
                ((eq format :toml)
                 `(("text" . ,(serialize-to-toml *chat-history*))
                   ("data" . (("history" . ,*chat-history*)))))
                (t
                 (if (and path (probe-file path))
                     `(("text" . ,(uiop:read-file-string path))
                       ("data" . (("file" . ,(namestring path)))))
                     `(("text" . "No history file found."))))) )
             ((string= (first clean-args) "clear")
              (clear-chat-history)
              `(("text" . "History cleared.")))
             ((string= (first clean-args) "save")
              (if (second clean-args)
                  (cond
                    ((eq format :json)
                     (with-open-file (stream (second clean-args) :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
                       (cl-json:encode-json *chat-history* stream))
                     `(("text" . ,(format nil "History saved as JSON to ~A" (second clean-args)))))
                    ((eq format :lisp)
                     (with-open-file (stream (second clean-args) :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
                       (format stream ";;; Chat history~%(in-package :hactar)~%~%(setf *chat-history* '~S)~%" *chat-history*))
                     `(("text" . ,(format nil "History saved as Lisp to ~A" (second clean-args)))))
                    ((eq format :yaml)
                     (with-open-file (stream (second clean-args) :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
                       (write-string (serialize-history-to-yaml *chat-history*) stream))
                     `(("text" . ,(format nil "History saved as YAML to ~A" (second clean-args)))))
                    ((eq format :toml)
                     (with-open-file (stream (second clean-args) :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
                       (write-string (serialize-to-toml *chat-history*) stream))
                     `(("text" . ,(format nil "History saved as TOML to ~A" (second clean-args)))))
                    (t
                     (if (and path (probe-file path))
                         (progn
                           (uiop:run-program (list "cp" (namestring path) (second clean-args))
                                             :output :string :error-output :string :ignore-error-status t)
                           `(("text" . ,(format nil "History saved to ~A" (second clean-args)))))
                         `(("text" . "No history file found to save.")))))
                  `(("text" . "Please specify a filename."))))
             (t `(("text" . ,(format nil "Unknown history subcommand: ~A" (first clean-args))))))))
  :json (lambda (args)
          (declare (ignore args))
          (cl-json:encode-json-to-string *chat-history*))
  :yaml (lambda (args)
          (declare (ignore args))
          (serialize-history-to-yaml *chat-history*))
  :toml (lambda (args)
          (declare (ignore args))
          (serialize-to-toml *chat-history*)))

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
  (let* ((summary-prompt-text (handler-case (get-prompt 'summary "summary.org")
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
                (nhooks:run-hook *history-changed-hook*)
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

(defun add-to-chat-history (role content &key tool-calls tool_call_id name)
  "Add a message to the chat history and save the updated history interface."
  (let ((message `((:role . ,role) (:content . ,(or content "")))))
    (when tool-calls (setf message (append message `((:tool_calls . ,tool-calls)))))
    (when tool_call_id (setf message (append message `((:tool_call_id . ,tool_call_id)))))
    (when name (setf message (append message `((:name . ,name)))))

    (setf *chat-history* (push-end message *chat-history*))
    (compress-chat-history-if-needed)
    (nhooks:run-hook *history-changed-hook*)))

(defun clear-chat-history ()
  "Clear the chat history."
  (setf *chat-history* '())
  (nhooks:run-hook *history-changed-hook*))

(defun drop-files ()
  "Drop files from context"
  (setf *files* nil))

(defun empty-context ()
  "Drop everything from context"
  (drop-files))

(defun load-chat-history ()
  "Load chat history from the history interface file if it exists."
  (let* ((iface (gethash :history *interfaces*))
         (path (and iface (interface-abs-path iface))))
    (when (and path (probe-file path))
      (handler-case
          (load path)
        (error (e)
          (format t "~&Warning: Error loading history file: ~A~%" e))))))

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
    (unwind-protect
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
                              (setf *last-user-prompt* prompt)
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
                         nil))))
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

(defmacro with-command-interrupt-protection (() &body body)
  "Wrap BODY with direct handler-case for the implementation-specific interrupt
   conditions (e.g. sb-sys:interactive-interrupt on SBCL). This catches Ctrl+C
   *before* it reaches the outer handler-bind in the REPL, which cannot dispatch
   to the inner handler-case (per CL spec, handler-bind handlers that signal
   search above themselves). Without this, Ctrl+C during interactive commands
   like /state drops into the SBCL debugger."
  `(handler-case
       (progn ,@body)
     (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      ()
      (if *lisp-rpc-mode*
          (rpc-interrupted)
          (format t "~&Interrupted.~%"))
      (values))
     (hactar-interrupt ()
      (values))))

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
		       ;; tag mode is when we are doing multiline input
		       ;; TODO: maybe remove this? or reduce the complexity somehow?. this code seems messy
                       (if in-hactar-tag-mode
                           (if (string= (string-trim '(#\Space #\Tab #\Newline #\Return) input) "hactar}")
                               (let ((full-input (get-output-stream-string hactar-tag-buffer)))
                                 (setf in-hactar-tag-mode nil)
                                 (setf hactar-tag-buffer (make-string-output-stream))
                                 (when (string/= (string-trim '(#\Space #\Tab #\Newline #\Return) full-input) "")
                                   (multiple-value-bind (cmd args) (parse-command full-input)
							(cond
							 (cmd
							  (with-command-interrupt-protection ()
											     (if *lisp-rpc-mode*
												 (let ((output (with-output-to-string (*standard-output*)
																      (execute-command cmd args))))
												   (when (and output (> (length output) 0))
												     (rpc-command-output output cmd)))
											       (execute-command cmd args))))
							 ((and (or *response-mode* *lisp-mode-enabled* *lisp-response-mode-enabled* *json-response-mode-enabled*) (response-mode-intercept full-input)) nil)
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
						    (with-command-interrupt-protection ()
										       (if *lisp-rpc-mode*
											   (let ((output (with-output-to-string (*standard-output*)
																(execute-command cmd args))))
											     (when (and output (> (length output) 0))
											       (rpc-command-output output cmd)))
											 (execute-command cmd args))))
						   ((string= (string-trim '(#\Space #\Tab #\Newline #\Return) input) "") nil)
						   ((and (or *response-mode* *lisp-mode-enabled* *lisp-response-mode-enabled* *json-response-mode-enabled*) (response-mode-intercept input)) nil)
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
    (load-hypertext-files)

    (setf *file-watcher-stopped* nil)
    (start-file-watcher *repo-root*)

    (let ((context-path (context-expose-file-path)))
      (setf *exposed-context-file* context-path)
      (context-expose-install-hooks-if-needed)
      (context-expose-upsert-project-details)
      (context-expose-upsert-files-section)
      (unless *silent* (format t "Context file initialized: ~A~%" (uiop:native-namestring context-path))))

    ;; Refactor Plan §6.1: materialize two-way file interfaces under ./.hactar/<id>/
    (handler-case
        (when (fboundp 'materialize-all-interfaces)
          (funcall 'materialize-all-interfaces)
          (unless *silent*
            (format t "Interfaces materialized: ~A~%"
                    (uiop:native-namestring (funcall 'instance-dir)))))
      (error (e) (debug-log "Failed to materialize interfaces:" e)))

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

    (when (and *slynk-auto-start* (not *slynk-started*))
      (start-slynk))

    (when (and *http-auto-start* (not *http-server*))
      (start-http))

    ;; Start proxy if auto-start is enabled
    (when *proxy-auto-start*
      (with-suppressed-output-if-rpc
        (start-proxy :silent t))
      (when *lisp-rpc-mode*
        (rpc-log :info "LLM Proxy started" :port *http-port*))
      (unless *silent*
        (format t "LLM Proxy running at http://localhost:~A/v1/chat/completions~%" *http-port*)))

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
(defun help--print (&optional cmd)
  "Print comprehensive help for Hactar using the unified flag/route registries.
CMD argument is ignored (kept for backward compatibility)."
  (declare (ignore cmd))
  (format t "~A~%" (colorize "hactar - Hactar AI Pair Programmer" :bold))
  (format t "~%")
  (format t "~A ~A~%" (colorize "Version:" :dim) (colorize *hactar-version* :cyan))
  (format t "~A ~A~%" (colorize "Authors:" :dim) "K-2052")
  (format t "~A ~A~%~%" (colorize "License:" :dim) "MIT")

  (format t "~A~%" (colorize "USAGE:" :bold-cyan))
  (format t "  hactar [OPTIONS] [SUBCOMMAND|URI] [ARGS...]~%~%")

  (format t "~A~%" (colorize "OPTIONS:" :bold-cyan))
  (let ((flags (list-registered-flags)))
    (dolist (flag (sort flags #'string<
                        :key (lambda (f) (or (first (flag-long-names f))
                                             (first (flag-short-names f))
                                             ""))))
      (format t "  ")
      (when (flag-short-names flag)
        (format t "~A~A " (colorize (first (flag-short-names flag)) :yellow) (colorize "," :dim)))
      (when (flag-long-names flag)
        (format t "~A" (colorize (first (flag-long-names flag)) :yellow)))
      (when (flag-takes-value flag)
        (format t " ~A" (colorize "<value>" :dim)))
      (format t "~%      ~A~%~%" (colorize (flag-description flag) :dim))))

  (format t "~A~%" (colorize "SUBCOMMANDS:" :bold-cyan))
  (let ((sorted-subcommands (sort (alexandria:hash-table-keys *sub-commands*) #'string<)))
    (dolist (subcmd-name sorted-subcommands)
      (unless (gethash (format nil "/~A" subcmd-name) *hidden-commands*)
        (let* ((subcmd-info (gethash subcmd-name *sub-commands*))
               (description (second subcmd-info))
               (cli-options (third subcmd-info)))
          (format t "  ~A~%" (colorize subcmd-name :bold-green))
          (format t "      ~A~%" (colorize description :dim))
          (when cli-options
            (format t "      ~A~%" (colorize "Options:" :cyan))
            (dolist (opt cli-options)
              (let ((opt-short (getf opt :short))
                    (opt-long (getf opt :long))
                    (opt-desc (getf opt :description)))
                (format t "        ")
                (when opt-short (format t "~A" (colorize (format nil "-~A" opt-short) :yellow)))
                (when (and opt-short opt-long) (format t "~A " (colorize "," :dim)))
                (when opt-long (format t "~A" (colorize (format nil "--~A" opt-long) :yellow)))
                (format t " ~A~%" (colorize (format nil ": ~A" opt-desc) :dim)))))
          (let ((web-cmd (gethash subcmd-name *web-commands*)))
            (when web-cmd
              (dolist (route (web-command-routes web-cmd))
                (format t "    ~A ~A ~A~%"
                        (colorize (first (web-route-pattern route)) :magenta)
                        (colorize "-" :dim)
                        (colorize (web-route-description route) :dim)))))
          (format t "~%")))))

  (format t "~A~%" (colorize "URIs (Infinite CLI):" :bold-cyan))
  (format t "  ~A~%" (colorize "Any unmatched argument is routed via the unified router." :dim))
  (format t "  ~A~%" (colorize "Wiki queries are routed automatically:" :dim))
  (format t "    ~A~%" (colorize "hactar redwood.docs/auth.types" :white))
  (format t "    ~A~%~%" (colorize "hactar wiki:redwood.docs/auth.example" :white))

  (format t "~A~%" (colorize "EXAMPLES:" :bold-cyan))
  (format t "  ~A~%" (colorize "hactar" :white))
  (format t "  ~A~%" (colorize "hactar --model anthropic/claude-sonnet-4.6" :white))
  (format t "  ~A~%" (colorize "hactar -q \"Explain this code\"" :white))
  (format t "  ~A~%" (colorize "hactar -e \"list all files modified today\"" :white))
  (format t "  ~A~%" (colorize "hactar hactar.init" :white))
  (format t "  ~A~%" (colorize "hactar --agentshell" :white)))

(defvar *cli-opts* (make-hash-table :test 'eq)
  "Parsed CLI options, populated by defflag handlers during PARSE-CLI-INPUT.")

(defun cli-opt (key &optional default)
  "Read a parsed CLI option."
  (multiple-value-bind (v p) (gethash key *cli-opts*) (if p v default)))

(defun (setf cli-opt) (value key)
  (setf (gethash key *cli-opts*) value))

(defun register-hactar-flags ()
  "Register all Hactar global flags. Called once at startup."
  (clrhash *flags*)
  (defflag :model ("--model" "-m") (v)
    :description "LLM model to use (e.g., ollama/qwen3:14b)"
    :examples ("--model ollama/qwen3:14b" "-m anthropic/claude-sonnet-4.6")
    (setf (cli-opt :model) v))
  (defflag :provider ("--provider") (v)
    :description "Provider to use with model aliases (anthropic, openrouter, ...)"
    :examples ("--provider openrouter" "--provider anthropic")
    (setf (cli-opt :provider) v))
  (defflag :name ("--project-name") (v)
    :description "Project name (defaults to current directory name)"
    (setf (cli-opt :name) v))
  (defflag :instance-name ("--name") (v)
    :description "Name of the instance (places files under .hactar/$instance-name)"
    (setf (cli-opt :instance-name) v))
  (defflag :path ("--project-path") (v)
    :description "Project path"
    (setf (cli-opt :path) v))
  (defflag :author ("--project-author") (v)
    :description "Author name"
    (setf (cli-opt :author) v))
  (defflag :config-path ("--config-path" "--config" "-c") (v)
    :description "Path to the models configuration file (models.yaml)"
    (setf (cli-opt :config-path) v))
  (defflag :embedding-model ("--embedding-model") (v)
    :description "Model to use for generating embeddings"
    :validator (lambda (value)
                 (when (or (string= value "")
                           (search "/" value))
                   (error "embedding model must be an Ollama model name without provider prefix"))
                 value)
    :error-level :warning
    :initializer (lambda ()
                   (let ((embedding-model-from-opt (cli-opt :embedding-model)))
                     (when embedding-model-from-opt
                       (if (or (string= embedding-model-from-opt "")
                               (search "/" embedding-model-from-opt))
                           (log-warning "Embedding model ~S is invalid. Embedding model is Ollama only; do not prefix the model." embedding-model-from-opt)
                           (progn
                             (setf *embedding-model* embedding-model-from-opt)
                             (let ((found (or (find-model-by-name embedding-model-from-opt)
                                              (find-model-by-name (format nil "ollama/~A" embedding-model-from-opt)))))
                               (unless found
                                 (log-warning "Embedding model ~S does not exist in the models configuration." embedding-model-from-opt))))))))
    (setf (cli-opt :embedding-model) v))
  (defflag :slynk-port ("--slynk-port" "-P") (v)
    :description "Port for the Slynk server"
    (setf (cli-opt :slynk-port) (parse-integer v :junk-allowed t)))
  (defflag :disable-analyzers ("--disable-analyzers") (v)
    :description "Space-separated list of analyzers to disable"
    :examples ("--disable-analyzers \"ai-comment-edit ai-comment-question\""
               "--disable-analyzers react-router-dependency")
    (setf (cli-opt :disable-analyzers) v))
  (defflag :enable-analyzers ("--enable-analyzers") (v)
    :description "Space-separated list of analyzers to enable"
    :examples ("--enable-analyzers \"ai-comment-edit ai-comment-question\""
               "--enable-analyzers react-router-dependency")
    (setf (cli-opt :enable-analyzers) v))
  (defflag :http-port ("--http-port") (v)
    :description "Port for the HTTP API server"
    (setf (cli-opt :http-port) (parse-integer v :junk-allowed t)))
  (defflag :sonnet ("--sonnet") ()
    :description "Use Claude Sonnet 4.6 model"
    (setf (cli-opt :sonnet) t))
  (defflag :gemini ("--gemini") ()
    :description "Use Gemini 3.5. Flash"
    (setf (cli-opt :gemini) t)
    (setf (cli-opt :provider) "gemini"))
  (defflag :gpt ("--gpt") ()
    :description "Use GPT 5.4 model"
    (setf (cli-opt :gpt) t))
  (defflag :opus ("--opus") ()
    :description "Use Claude Opus 4.8 model"
    (setf (cli-opt :opus) t))
  (defflag :gemini-free ("--gemini-free") ()
    :description "Use free Gemini 2.5r"
    (setf (cli-opt :gemini-free) t))
  (defflag :deepseek ("--deepseek") ()
    :description "Use DeepSeek V4 Pro model"
    (setf (cli-opt :deepseek) t)
    (setf (cli-opt :provider) "deepseek"))
  (defflag :openrouter ("--openrouter") ()
    :description "Use OpenRouter as the provider"
    (setf (cli-opt :provider) "openrouter"))
  (defflag :copilot ("--copilot") ()
    :description "Use Copilot as the provider"
    (setf (cli-opt :provider) "copilot"))
  (defflag :anthropic ("--anthropic") ()
    :description "Use Anthropic as the provider"
    (setf (cli-opt :provider) "anthropic"))
  (defflag :openai ("--openai") ()
    :description "Use OpenAI as the provider"
    (setf (cli-opt :provider) "openai"))
  (defflag :deepseek-flash ("--deepseek-flash") ()
    :description "Use DeepSeek V4 Flash model"
    (setf (cli-opt :deepseek-flash) t))
  (defflag :deep ("--deep") ()
    :description "Shortcut for --deepseek"
    (setf (cli-opt :deepseek) t))
  (defflag :deep-flash ("--deep-flash") ()
    :description "Shortcut for --deepseek-flash"
    (setf (cli-opt :deepseek-flash) t))
  (defflag :kimi ("--kimi") ()
    :description "Use Kimi model"
    (setf (cli-opt :kimi) t))
  (defflag :glm ("--glm") ()
    :description "Use GLM 5.7 model"
    (setf (cli-opt :glm) t))
  (defflag :tiny ("--tiny") ()
    :description "Use the tiny model (default: ollama/gemma4:e2b)"
    (setf (cli-opt :tiny) t))
  (defflag :query ("--query" "-q") (v)
    :description "Send a query to the LLM, print the result, and exit"
    (setf (cli-opt :immediate-query) v))
  (defflag :execute ("--execute" "-e") (v)
    :description "Generate a shell command from the query and copy it"
    (setf (cli-opt :execute) v))
  (defflag :execute-immediately ("--execute-immediately") (v)
    :description "Generate a shell command and execute it immediately"
    (setf (cli-opt :execute-immediately) v))
  (defflag :assistant ("--assistant") ()
    :description "Enable assistant mode"
    (setf (cli-opt :assistant) t))
  (defflag :output ("--assistant-output") (v)
    :description "Output file (for --assistant)"
    (setf (cli-opt :assistant-output-file) v))
  (defflag :audio ("--audio") ()
    :description "Enable TTS audio output (assistant mode)"
    :initializer (lambda ()
                   (when (cli-opt :assistant-audio-enabled)
                     (setf *assistant-audio-enabled* t)
                     (unless *silent* (format t "Assistant TTS audio output enabled.~%"))))
    (setf (cli-opt :assistant-audio-enabled) t))
  (defflag :live-dangerously ("--live-dangerously") ()
    :description "Allow agents to run without a safe environment"
    (setf (cli-opt :live-dangerously) t))
  (defflag :auto-lint ("--auto-lint") ()
    :description "Enable automatic lint agent"
    (setf (cli-opt :auto-lint) t))
  (defflag :auto-test ("--auto-test") ()
    :description "Enable automatic test agent"
    (setf (cli-opt :auto-test) t))
  (defflag :auto-typecheck ("--auto-typecheck") ()
    :description "Enable automatic typecheck agent"
    (setf (cli-opt :auto-typecheck) t))
  (defflag :auto-all ("--auto-all") ()
    :description "Enable all automation"
    (setf (cli-opt :auto-all) t))
  (defflag :lisp-response-mode ("--lisp-response-mode") ()
    :description "Enable Lisp-only response mode (LLM returns executable Lisp)"
    (setf (cli-opt :lisp-response-mode) t))
  (defflag :json-response-mode ("--json-response-mode") ()
    :description "Enable JSON response mode (LLM returns JSON)"
    (setf (cli-opt :json-response-mode) t))
  (defflag :response-mode ("--response-mode") (v)
    :description "Conform LLM response to format: xml, json, yaml, markdown, or org-mode"
    (setf (cli-opt :response-mode) v))
  (defflag :in-editor ("--in-editor") ()
    :description "Running inside an editor (Emacs, Vim, etc.)"
    (setf (cli-opt :in-editor) t))
  (defflag :acp ("--acp") ()
    :description "Start in Agent Client Protocol (ACP) mode over stdio"
    (setf (cli-opt :acp) t))
  (defflag :mcp ("--mcp") ()
    :description "Start as a Model Context Protocol (MCP) server"
    (setf (cli-opt :mcp) t))
  (defflag :lit ("--lit") ()
    :description "Enable literate single-file mode at startup"
    (setf (cli-opt :lit) t))
  (defflag :lisp-rpc ("--lisp") ()
    :description "Enable Lisp-RPC mode"
    (setf (cli-opt :lisp-rpc) t))
  (defflag :agentshell ("--agentshell") ()
    :description "Start AgentShell, an ACP client TUI"
    (setf (cli-opt :agentshell) t))
  (defflag :agent-command ("--agent-command") (v)
    :description "Command to launch the ACP agent subprocess"
    (setf (cli-opt :agent-command) v))
  (defflag :tui ("--tui") ()
    :description "Start the 3-column TUI interface"
    (setf (cli-opt :tui) t))
  (defflag :theme ("--theme") (v)
    :description "TUI color theme name or path"
    :validator (lambda (value)
                 (unless (or (find-theme-by-name value)
                             (probe-file value))
                   (error "theme not found: ~A" value))
                 value)
    :error-level :warning
    :initializer (lambda ()
                   (let ((theme-from-opt (cli-opt :theme)))
                     (when theme-from-opt
                       (setf *tui-theme-name* theme-from-opt))
                     (when *tui-theme-name*
                       (let ((theme (find-theme-by-name *tui-theme-name*)))
                         (if theme
                             (progn
                               (setf *tui-theme* theme)
                               (unless *silent*
                                 (format t "Theme: ~A~%" (tui-theme-name theme))))
                             (log-warning "Theme ~S is invalid or missing" *tui-theme-name*))))))
    (setf (cli-opt :theme) v))
  (defflag :http ("--http") ()
    :description "Start the HTTP API server on startup"
    (setf (cli-opt :http) t))
  (defflag :slynk ("--slynk") ()
    :description "Start the Slynk server on startup"
    (setf (cli-opt :slynk) t))
  (defflag :proxy ("--proxy") ()
    :description "Start the OpenRouter-compatible LLM proxy on startup"
    (setf (cli-opt :proxy) t))
  (defflag :proxy-upstream-url ("--proxy-upstream-url" "--proxy-upstream") (v)
    :description "Upstream LLM API URL for the proxy"
    (setf (cli-opt :proxy-upstream-url) v))
  (defflag :help ("--help" "-h") ()
    :description "Show help"
    (setf (cli-opt :help) t))
  (defflag :version ("--version") ()
    :description "Print version and exit"
    (setf (cli-opt :version) t))
  t)

(defun handle-execute-flag (query run-immediately-p)
  "Handles --execute and --execute-immediately flags."
  (when (and *current-model* query (not (string= query "")))
    (let* ((system-prompt-text (get-prompt 'generate-shell-command "generate-shell-command.mustache"))
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

(defun setup-cli-model-environment ()
  "Initialize the configuration and model environment for CLI subcommands."
  (let* ((model-from-opt (cli-opt :model))
         (config-path (cli-opt :config-path))
         (provider-from-opt (cli-opt :provider))
         (use-sonnet (cli-opt :sonnet))
         (use-gemini (cli-opt :gemini))
         (use-gpt (cli-opt :gpt))
         (use-opus (cli-opt :opus))
         (use-gemini-free (cli-opt :gemini-free))
         (use-deepseek (cli-opt :deepseek))
         (use-deepseek-flash (cli-opt :deepseek-flash))
         (use-kimi (cli-opt :kimi))
         (use-glm (cli-opt :glm))
         (use-tiny (cli-opt :tiny))
         (model model-from-opt))

    (when use-sonnet (setf model (format nil "~A/claude-sonnet-4.6" (or provider-from-opt "anthropic"))))
    (when use-gemini (setf model (format nil "~A//gemini-3.5-flash" (or provider-from-opt "gemini"))))
    (when use-gpt (setf model (format nil "~A/gpt-5.4" (or provider-from-opt "openai"))))
    (when use-opus (setf model (format nil "~A/claude-opus-4.8" (or provider-from-opt "anthropic"))))
    (when use-gemini-free (setf model "openrouter/google/gemini-2.5-pro-exp-03-25:free"))
    (when use-deepseek (setf model (format nil "~A/deepseek-v4-pro:cloud" (or provider-from-opt "ollama"))))
    (when use-deepseek-flash (setf model (format nil "~A/deepseek-v4-flash:cloud" (or provider-from-opt "ollama"))))
    (when use-kimi (setf model (format nil "~A/kimi-k2.6:cloud" (or provider-from-opt "ollama"))))
    (when use-glm (setf model (format nil "~A/glm-5.7:cloud" (or provider-from-opt "ollama"))))
    (when use-tiny (setf model "ollama/gemma4:e2b"))

    (ensure-directories-exist (directory-namestring (get-models-config-path)))
    (ensure-directories-exist (merge-pathnames "hactar/" (get-xdg-config-dir)))

    (when config-path
      (unless (probe-file config-path)
        (format *error-output* "Error: Configuration file specified via --config-path does not exist: ~A~%" config-path)
        (uiop:quit 1))
      (handler-case
          (with-open-file (stream config-path)
            (let* ((file-content (uiop:read-file-string stream))
                   (config (cl-yaml:parse file-content)))
              (unless config
                (error "Configuration file is empty or invalid structure"))))
        ((or error yaml.error:parsing-error) (e)
          (format *error-output* "Error: Failed to load config from ~A: ~A~%" config-path e)
          (uiop:quit 1))))

    (load-models-config (or config-path (get-models-config-path)))
    (set-current-model (or model
                           (uiop:getenv "HACTAR_MODEL")
                           *default-llm*))

    (setf *completion-model* *current-model*)

    (run-flag-initializers)))

(defun toplevel/handler (free-args)
  "The core startup logic for Hactar, executing based on parsed flags."
  (let* ((model-from-opt (cli-opt :model))
         (name-from-opt (cli-opt :name))
         (instance-name-from-opt (cli-opt :instance-name))
         (author-from-opt (cli-opt :author))
         (config-path (cli-opt :config-path))
         (embedding-model-from-opt (cli-opt :embedding-model))
         (slynk-port (cli-opt :slynk-port))
         (http-port (cli-opt :http-port))
         (disable-analyzers-str (cli-opt :disable-analyzers))
         (enable-analyzers-str (cli-opt :enable-analyzers))
         (provider-from-opt (cli-opt :provider))
         (use-sonnet (cli-opt :sonnet))
         (use-gemini (cli-opt :gemini))
         (use-gpt (cli-opt :gpt))
         (use-opus (cli-opt :opus))
         (use-gemini-free (cli-opt :gemini-free))
         (use-deepseek (cli-opt :deepseek))
         (use-deepseek-flash (cli-opt :deepseek-flash))
         (use-kimi (cli-opt :kimi))
         (use-glm (cli-opt :glm))
         (use-tiny (cli-opt :tiny))
         (immediate-query (cli-opt :immediate-query))
         (execute-query (cli-opt :execute))
         (execute-immediately-query (cli-opt :execute-immediately))
         (assistant-mode (cli-opt :assistant))
         (assistant-output (cli-opt :assistant-output-file))
         (assistant-audio (cli-opt :assistant-audio-enabled))
         (lit-flag (cli-opt :lit))
         (live-dangerously (cli-opt :live-dangerously))
         (auto-lint (cli-opt :auto-lint))
         (auto-test (cli-opt :auto-test))
         (auto-typecheck (cli-opt :auto-typecheck))
         (auto-all (cli-opt :auto-all))
         (watch-flag (cli-opt :watch))
         (lisp-response-mode-flag (cli-opt :lisp-response-mode))
         (json-response-mode-flag (cli-opt :json-response-mode))
         (response-mode-opt (cli-opt :response-mode))
         (acp-flag (cli-opt :acp))
         (mcp-flag (cli-opt :mcp))
         (lisp-rpc-flag (cli-opt :lisp-rpc))
         (agentshell-flag (cli-opt :agentshell))
         (agent-command-str (cli-opt :agent-command))
         (tui-flag (cli-opt :tui))
         (theme-from-opt (cli-opt :theme))
         (http-flag (cli-opt :http))
         (slynk-flag (cli-opt :slynk))
         (proxy-flag (cli-opt :proxy))
         (proxy-upstream-opt (cli-opt :proxy-upstream-url))
         (in-editor-flag (or (cli-opt :in-editor)
                             (let ((env-val (uiop:getenv "HACTAR_IN_EDITOR")))
                               (and env-val (not (string= env-val ""))
                                    (not (string-equal env-val "false"))))))
         (path (or (cli-opt :path)
                   (let ((args-str (format nil "~{~A~^ ~}" free-args)))
                     (multiple-value-bind (match regs)
                         (cl-ppcre:scan-to-strings "--project-path\\s+([^\\s]+)" args-str)
                       (declare (ignore match))
                       (when (and regs (> (length regs) 0))
                         (aref regs 0))))))
         (model model-from-opt))

    (declare (ignorable use-sonnet use-gemini use-gpt use-opus use-gemini-free use-deepseek use-deepseek-flash use-kimi use-glm use-tiny model provider-from-opt config-path model-from-opt))

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

    (when (or (member "--execute" (uiop:command-line-arguments) :test #'string=)
              (member "-e" (uiop:command-line-arguments) :test #'string=))
      (unless (and execute-query (not (string= execute-query "")))
        (format *error-output* "Error: --execute requires a query argument.~%")
        (uiop:quit 1)))
    (when (member "--execute-immediately" (uiop:command-line-arguments) :test #'string=)
      (unless (and execute-immediately-query (not (string= execute-immediately-query "")))
        (format *error-output* "Error: --execute-immediately requires a query argument.~%")
        (uiop:quit 1)))

    (when (or immediate-query execute-query execute-immediately-query free-args acp-flag mcp-flag agentshell-flag lisp-rpc-flag)
      (setf *silent* t))          ; Enable silent mode for non-interactive modes

    (setup-cli-model-environment)


    (when name-from-opt
      (setf *name* name-from-opt))

    (when instance-name-from-opt
      (setf *instance-id* instance-name-from-opt))

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

    (when http-flag
      (setf *http-auto-start* t))
    (when slynk-flag
      (setf *slynk-auto-start* t))
    (when proxy-flag
      (setf *proxy-auto-start* t))
    (when proxy-upstream-opt
      (setf *proxy-upstream-url* proxy-upstream-opt))

    (when watch-flag
      (set-analyzer-enabled 'ai-comment-edit t)
      (set-analyzer-enabled 'ai-comment-question t)
      (unless *silent* (format t "  AI comment handling enabled (AI!/AI?).~%")))

    (when lisp-response-mode-flag
      (setf *lisp-mode-enabled* t)
      (setf *lisp-response-mode-enabled* t)
      (setf *response-mode* :lisp)
      (unless *silent* (format t "  Lisp-only response mode enabled.~%")))

    (when json-response-mode-flag
      (setf *json-response-mode-enabled* t)
      (setf *response-mode* :json)
      (unless *silent* (format t "  JSON response mode enabled.~%")))

    (when response-mode-opt
      (let ((mode (intern (string-upcase response-mode-opt) :keyword)))
        (setf *response-mode* mode)
        (setf *lisp-mode-enabled* (eq mode :lisp))
        (setf *lisp-response-mode-enabled* (eq mode :lisp))
        (setf *json-response-mode-enabled* (eq mode :json))
        (unless *silent* (format t "  Response conformance mode '~A' enabled.~%" response-mode-opt))))

    (when (or lisp-rpc-flag *lisp-mode-enabled* *lisp-response-mode-enabled*)
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
      (unless *silent* (format t "~&Entering Assistant Mode...~%"))
      (when *assistant-output-file*
        (unless *silent* (format t "Assistant extractions will be written to: ~A~%" *assistant-output-file*))))

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
           (global-cleanup))))
      (tui-flag
       (progn
         (load-chat-history)
         (when lit-flag
           (init-litmode))
         (unwind-protect
              (run-tui)
           (global-cleanup))))
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
           (global-cleanup)
           (handler-case (rl:deprep-terminal)
             (serious-condition (e) (format *error-output* "~&Cleanup error (deprep-terminal): ~A~%" e)))))))))

(defun hactar-init ()
  "Initialize Hactar by cloning the repo and copying default prompts and models.yaml."
  (let* ((repo-url *hactar-repo-url*)
         (repo-dir (%to-pathname *hactar-repo-dir*))
         (repo-dir-native (uiop:native-namestring (uiop:ensure-directory-pathname repo-dir)))
	 (data-dir (%to-pathname *hactar-data-path*))
         (config-dir (%to-pathname *hactar-config-path*))
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
				   (declare (ignore out err))
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

    ok))

(define-sub-command query (args)
  "Send a query to the LLM, print the result, and exit."
  (let ((query (format nil "~{~A~^ ~}" args)))
    (unless (and query (not (string= query "")))
      (format *error-output* "Error: query requires a query argument.~%")
      (uiop:quit 1))
    (unless *current-model*
      (format *error-output* "Error: No model selected. Use /model to select a model.~%")
      (uiop:quit 1))
    (let ((response (get-llm-response query :stream nil :add-to-history nil)))
      (if response
          (progn
            (format t "~A~%" response)
            (uiop:quit 0))
          (progn
            (format *error-output* "Error: No response from LLM.~%")
            (uiop:quit 1))))))

(define-sub-command execute (args)
  "Generate a shell command from a query. Copies it to clipboard by default; runs immediately with -i or --immediately."
  (let* ((run-immediately-p (or (member "-i" args :test #'string=)
                                (member "--immediately" args :test #'string=)
                                (member "--execute-immediately" args :test #'string=)))
         (clean-args (remove-if (lambda (x) (member x '("-i" "--immediately" "--execute-immediately") :test #'string=)) args))
         (query (format nil "~{~A~^ ~}" clean-args)))
    (unless (and query (not (string= query "")))
      (format *error-output* "Error: execute requires a query argument.~%")
      (uiop:quit 1))
    (handle-execute-flag query run-immediately-p)))

(define-sub-command hactar.init (args)
  "Initialize Hactar: clone repo and install default prompts and models."
  (declare (ignore args))
  (let ((ok (hactar-init)))
    (uiop:quit (if ok 0 1))))

(define-sub-command help (args)
		    "Display comprehensive help information about Hactar. Use --spec or --spec-lisp for machine-readable output."
		    (cond
		      ((member "--spec-lisp" args :test #'string=)
		       (emit-cli-spec :lisp-p t))
		      ((member "--spec" args :test #'string=)
		       (emit-cli-spec))
		      (t
		       (help--print nil)))
		    (uiop:quit 0))

(defun flag-help--print (flag)
  "Print help details for a single flag."
  (let ((names (append (flag-long-names flag) (flag-short-names flag))))
    (format t "Flag: ~A~%" (flag-name flag))
    (when names
      (format t "  Aliases: ~{~A~^, ~}~%" names))
    (format t "  Takes Value: ~A~%" (if (flag-takes-value flag) "Yes" "No"))
    (format t "  Description: ~A~%" (or (flag-description flag) "No description available."))
    (format t "  Examples:~%")
    (if (flag-examples flag)
        (dolist (ex (flag-examples flag))
          (format t "    ~A~%" ex))
        (let ((primary-name (or (first (flag-long-names flag))
                                (first (flag-short-names flag)))))
          (if (flag-takes-value flag)
              (format t "    hactar ~A <value>~%" primary-name)
              (format t "    hactar ~A~%" primary-name))))))


(define-sub-command help.flag (args)
  "Print the help details for a flag. Use --spec or --spec-lisp for machine-readable output."
  (let* ((spec-p (or (member "--spec" args :test #'string=)
                     (member "-s" args :test #'string=)))
         (spec-lisp-p (member "--spec-lisp" args :test #'string=))
         (clean-args (remove-if (lambda (x) (member x '("--spec" "-s" "--spec-lisp") :test #'string=)) args))
         (flag-query (first clean-args)))
    (unless flag-query
      (format t "Error: No flag specified.~%")
      (format t "Usage: hactar help.flag <flag-name> [--spec] [--spec-lisp]~%")
      (uiop:quit 1))
    (let ((flag (find-flag-by-string flag-query)))
      (unless flag
        (format t "Error: Flag '~A' not found.~%" flag-query)
        (uiop:quit 1))
      (cond
        (spec-lisp-p
         (format t "~S~%" (build-flag-spec flag)))
        (spec-p
         (format t "<json>~%~A~%</json>~%" (to-json (build-flag-spec flag))))
        (t
         (flag-help--print flag)))
      (uiop:quit 0))))

(defun find-symbol-by-name (name-str)
  "Find a symbol by name string, supporting package prefixes."
  (let* ((name-str (string-trim '(#\Space #\Tab) name-str))
         (colon-pos (position #\: name-str)))
    (if colon-pos
        (let* ((pkg-part (subseq name-str 0 colon-pos))
               (sym-part (subseq name-str (if (and (< (1+ colon-pos) (length name-str))
                                                   (char= (char name-str (1+ colon-pos)) #\:))
                                              (+ 2 colon-pos)
                                              (1+ colon-pos))))
               (pkg (find-package (string-upcase pkg-part))))
          (when pkg
            (find-symbol (string-upcase sym-part) pkg)))
        (or (find-symbol (string-upcase name-str) :hactar)
            (find-symbol (string-upcase name-str) :cl)
            (find-symbol (string-upcase name-str) :keyword)))))

(define-sub-command state (args)
  "Echo the value of a global variable to stdout. Usage: hactar state <variable-name>"
  (let ((var-name (first args)))
    (unless var-name
      (format t "Error: No variable specified.~%")
      (format t "Usage: hactar state <variable-name>~%")
      (uiop:quit 1))
    (let ((sym (find-symbol-by-name var-name)))
      (if (and sym (boundp sym))
          (format t "~A~%" (symbol-value sym))
          (progn
            (format t "Error: Variable '~A' is unbound or not found.~%" var-name)
            (uiop:quit 1)))
      (uiop:quit 0))))

(define-command retry (args)
  "Resend the last prompt with confirmation."
  (declare (ignore args))
  (if *last-user-prompt*
      (progn
        (format t "Resend last prompt: \"~A\"? [y/n]: " *last-user-prompt*)
        (force-output)
        (let ((response (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     (read-line *standard-input* nil ""))))
          (if (or (string-equal response "y") (string-equal response "yes"))
              (progn
                (format t "~&⏳ Retrying prompt...~%")
                (force-output)
                (get-llm-response *last-user-prompt*))
              (format t "Cancelled.~%"))))))

(define-command render-markdown (args)
  "Render a markdown file with Glow-inspired ANSI styling."
  (if (null args)
      (format t "Usage: /render <filename>~%")
      (let ((filename (first args)))
        (if (probe-file filename)
            (handler-case
                (let ((content (uiop:read-file-string filename)))
                  (format t "~A~%" (render-md-ansi content)))
              (error (e)
                (format t "Error reading file ~A: ~A~%" filename e)))
            (format t "File not found: ~A~%" filename))))
  :completions (lambda (text args)
                 (declare (ignore args))
                 (let* ((prefix (if (string= text "") "./" text))
                        (dir-part (or (directory-namestring prefix) ""))
                        (name-part (file-namestring prefix))
                        (base-dir (if (and dir-part (not (string= dir-part "")))
                                      (probe-file dir-part)
                                      (uiop:getcwd))))
                   (when (and base-dir (probe-file base-dir))
                     (loop for entry in (directory (merge-pathnames (make-pathname :name :wild :type :wild :defaults base-dir) base-dir))
                           for namestr = (uiop:native-namestring entry)
                           for rel = (enough-namestring namestr (uiop:native-namestring (uiop:getcwd)))
                           when (str:starts-with-p name-part (file-namestring rel) :ignore-case t)
                           collect (if (uiop:directory-pathname-p entry)
                                       (if (str:ends-with-p "/" rel) rel (format nil "~A/" rel))
                                       rel)))))
  :acp (lambda (cmd-args)
         (if (null cmd-args)
             `(("text" . "Usage: /render <filename>"))
             (let ((filename (first cmd-args)))
               (if (probe-file filename)
                   (handler-case
                       (let ((content (uiop:read-file-string filename)))
                         `(("text" . ,(render-md-ansi content))))
                     (error (e)
                       `(("text" . ,(format nil "Error reading file ~A: ~A" filename e)))))
                   `(("text" . ,(format nil "File not found: ~A" filename)))))))
  :slash t
  :sub t)


(defun global-cleanup ()
  "Clean up all temporary/port/context files upon exiting the application."
  (ignore-errors (session/auto-save))
  (ignore-errors (delete-slynk-port-file))
  (ignore-errors (context-expose-delete-file))
  (ignore-errors (stop-http-server))
  (when *repo-root*
    (let ((port-file (merge-pathnames ".hactar.port" *repo-root*)))
      (when (probe-file port-file)
        (ignore-errors (delete-file port-file)))))
  (when *editor-log-file*
    (ignore-errors (delete-file *editor-log-file*)))
  (when *assistant-last-screenshot-path*
    (ignore-errors (delete-file *assistant-last-screenshot-path*)))
  (when *assistant-last-audio-file*
    (ignore-errors (delete-file *assistant-last-audio-file*)))
  (unless *file-watcher-stopped*
    (setf *file-watcher-stopped* t)
    (ignore-errors (stop-file-watcher)))
  (ignore-errors
    (maphash (lambda (k v) (declare (ignore k))
               (ignore-errors (stop-watcher v)))
             *active-watchers*)))


;;* Main
(defun main (&optional provided-args)
  "Main entry point for the Hactar application executable.

Uses the unified defflag/defroute router system (the 'Infinite CLI').
Free arguments are dispatched as:
  1. Registered sub-commands (e.g. 'hactar.init')
  2. Routes via EXECUTE-ROUTE (e.g. wiki URIs)
  3. The interactive REPL (default)"
  (register-hactar-flags)
  (clrhash *cli-opts*)
  (let* ((raw-args (or provided-args (uiop:command-line-arguments)))
         (free-args (parse-cli-input raw-args)))
    (cond
      ((cli-opt :version)
       (format t "hactar version ~A~%" *hactar-version*)
       (uiop:quit 0)))
    ;; If a known sub-command is present, dispatch directly.
    (let* ((first-arg (first free-args))
           (sub-info (and first-arg (gethash first-arg *sub-commands*))))
      (cond
        (sub-info
         (let* ((cmd-args (rest free-args)))
           (cond
             ((member "--spec-lisp" cmd-args :test #'string=)
              (format t "~S~%" (build-subcommand-group-spec first-arg))
              (uiop:quit 0))
             ((member "--spec" cmd-args :test #'string=)
              (format t "<json>~%~A~%</json>~%" (to-json (build-subcommand-group-spec first-arg)))
              (uiop:quit 0)))
           (let* ((fmt-str (extract-format-string cmd-args))
                  (fmt (when fmt-str (parse-format-keyword fmt-str)))
                  (slash-cmd (format nil "/~A" first-arg)))
           (when (cli-opt :help)
             (setf cmd-args (append cmd-args (list "--help"))))
           (unless (or (string= first-arg "hactar.init")
                       (member "-h" cmd-args :test #'string=)
                       (member "--help" cmd-args :test #'string=)
                       (cli-opt :help))
             (setup-cli-model-environment))
           (cond
             ((and fmt-str (null fmt))
              (format *error-output*
                      "Unknown format: ~A. Supported: ~{~A~^, ~}~%"
                      fmt-str (supported-format-names)))
             ((and fmt (null (get-format-handler slash-cmd fmt)))
              (format *error-output*
                      "Format ~A is not supported by command ~A.~%"
                      fmt-str first-arg))
             (fmt
              (execute-format-command slash-cmd fmt cmd-args))
             (t
              (funcall (first sub-info) cmd-args)))))
         (uiop:quit 0))
        ((cli-opt :help)
         (help--print nil)
         (uiop:quit 0))
        ;; URI-style argument (contains '/' or matches a route): dispatch via router
        ((and first-arg
              (or (search "/" first-arg)
                  (str:starts-with? "wiki:" first-arg)
                  (cl-ppcre:scan "^[a-zA-Z0-9_.-]+:" first-arg)
                  (search ".docs" first-arg)))
         (let ((result (execute-route first-arg)))
           (when result (format t "~A~%" result)))
         (uiop:quit 0))
        ;; Fall back to executing the core logic using parsed free args
        (t
         (toplevel/handler free-args))))))
