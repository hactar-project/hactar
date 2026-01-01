(in-package :hactar)
;;* Agents
(defstruct agent-definition
  name
  args
  docstring
  stack
  init-form
  run-form
  stop-condition-form
  cleanup-form
  package)

(defstruct agent-instance
  (id (format nil "~A" (uuid:make-v4-uuid)))
  definition
  thread
  (state (make-hash-table :test 'equal))
  (status :stopped)
  last-command-status
  (retry-count 0))

(defmacro defagent (name args &body body)
  "Define an agent that can be run to perform tasks.
   The body should contain a docstring (optional) followed by keyword arguments:
   :stack (optional list of strings)
   :init (required form)
   :run (optional form)
   :stop-condition (optional form)
   :cleanup (optional form)"
  (let* ((docstring (when (stringp (first body)) (first body)))
         (body-without-doc (if docstring (rest body) body))
         (plist body-without-doc)
         (agent-name-sym (intern (string-upcase (symbol-name name)) :hactar)))
    `(progn
       (setf (gethash ',agent-name-sym *agent-definitions*)
             (make-agent-definition
              :name ',agent-name-sym
              :args ',args
              :docstring ,docstring
              :stack ',(getf plist :stack)
              :init-form ',(getf plist :init)
              :run-form ',(getf plist :run)
              :stop-condition-form ',(getf plist :stop-condition)
              :cleanup-form ',(getf plist :cleanup)
              :package ,(package-name *package*)))
       ',agent-name-sym)))

(defagent cmd (command-string &optional user-prompt-path)
  "Runs a command, and if it fails, asks an LLM for a fix and retries up to 3 times."
  :init (progn
          (setf (agent-instance-retry-count agent-instance) 0)
          (let ((state (agent-instance-state agent-instance)))
            (setf (gethash 'last-llm-response state) "")
            (let ((test-pkg (find-package :hactar-tests)))
              (when test-pkg
                (setf (gethash (intern "LAST-LLM-RESPONSE" test-pkg) state) ""))))
          (setf (agent-instance-last-command-status agent-instance) -1) ; Start with non-zero status
          (format t "Agent 'cmd' initialized to run '~A'.~%" command-string))
  :run (progn
         (incf (agent-instance-retry-count agent-instance))
         (format t "Agent 'cmd': Running command (Attempt ~A): ~A~%"
                 (agent-instance-retry-count agent-instance)
                 command-string)
         (multiple-value-bind (output error-output exit-code)
             (uiop:run-program command-string :output :string :error-output :string :ignore-error-status t)
           (setf (agent-instance-last-command-status agent-instance) exit-code)
           (if (zerop exit-code)
               (format t "Agent 'cmd': Command succeeded.~%")
               (progn
                 (format t "Agent 'cmd': Command failed with exit code ~A.~%" exit-code)
                 (let* ((combined-output (format nil "exit-code:~A~%STDOUT:~%~A~%STDERR:~%~A" exit-code output error-output))
                        (prompt (if user-prompt-path
                                    (let ((mustache:*escape-tokens* nil)
					  (user-prompt-template (uiop:read-file-string user-prompt-path)))
                                      (mustache:render* user-prompt-template `((:output . ,combined-output))))
                                    combined-output))
                        (system-prompt "Reply with the word stop as soon as the exit status is a zero. Otherwise reply with continue."))
                   ;; Only consult the LLM if we haven't reached the retry limit yet
                   (when (< (agent-instance-retry-count agent-instance) 3)
                     (let* ((llm-response (get-llm-response prompt :custom-system-prompt system-prompt :stream t :add-to-history t))
                            (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or llm-response "")))
                            (state (agent-instance-state agent-instance)))
                       (setf (gethash 'last-llm-response state) trimmed)
                       (let ((test-pkg (find-package :hactar-tests)))
                         (when test-pkg
                           (setf (gethash (intern "LAST-LLM-RESPONSE" test-pkg) state) trimmed))))))))))
  :stop-condition (or (zerop (agent-instance-last-command-status agent-instance))
                      (>= (agent-instance-retry-count agent-instance) 3)
                      (string-equal "stop" (gethash 'last-llm-response (agent-instance-state agent-instance) "")))
  :cleanup (let ((status (agent-instance-last-command-status agent-instance)))
             (if (zerop status)
                 (format t "Agent 'cmd' finished successfully.~%")
                 (format t "Agent 'cmd' finished with failure (last exit code: ~A).~%" status))))

(defun derive-lint-command-from-stack ()
  "Derive a sensible lint/build command from the current *stack*."
  (cond
    ((member "typescript" *stack* :test #'string=) "tsc")
    ((member "npm" *stack* :test #'string=) "npm run lint")
    ((member "rust" *stack* :test #'string=) "cargo build")
    (t "make typecheck")))

(defun get-lint-command ()
  "Return the lint command from config or a derived default."
  (or *lint-command*
      (derive-lint-command-from-stack)))

(defun get-lint-prompt (command output user-instructions)
  "Load a lint-fix prompt template and render it with details."
  (let* ((template-string (handler-case
                              (uiop:read-file-string (get-prompt-path "lint-fix.mustache"))
                            (error (e)
                              (declare (ignore e))
                              "Analyze the following lint/build errors from running '{{command}}'.
Propose precise code fixes as SEARCH/REPLACE blocks only. Do not include any other text.

Errors:
```
{{output}}
```
{{#instructions}}
Additional instructions:
{{instructions}}
{{/instructions}}")))
         (rendered (mustache:render* template-string
                                     `((:command . ,command)
                                       (:output . ,output)
                                       (:instructions . ,(or user-instructions ""))))))
    rendered))

(defagent lint (&optional user-instructions)
  "Runs a lint command in a loop and fixes code with lint errors using SEARCH/REPLACE blocks."
  :init (progn
          (setf (agent-instance-retry-count agent-instance) 0)
          (setf (agent-instance-last-command-status agent-instance) -1)
          (format t "Agent 'lint' initialized.~%"))
  :run (let* ((command (get-lint-command)))
         (incf (agent-instance-retry-count agent-instance))
         (format t "Agent 'lint': Running lint (Attempt ~A): ~A~%"
                 (agent-instance-retry-count agent-instance) command)
         (multiple-value-bind (output error-output exit-code)
             (uiop:run-program command :output :string :error-output :string :ignore-error-status t)
           (setf (agent-instance-last-command-status agent-instance) exit-code)
           (if (zerop exit-code)
               (format t "Agent 'lint': No lint errors detected.~%")
               (let* ((combined-output (format nil "exit-code:~A~%STDOUT:~%~A~%STDERR:~%~A"
                                               exit-code output error-output))
                      (prompt (get-lint-prompt command combined-output user-instructions)))
                 (format t "Agent 'lint': Lint failed. Asking LLM for fixes...~%")
                 ;; Non-streamed to ensure full S/R blocks land in history for processor
                 (get-llm-response prompt :stream nil :add-to-history t)))))
  :stop-condition (or (zerop (agent-instance-last-command-status agent-instance))
                      (>= (agent-instance-retry-count agent-instance) *agent-retry-limit*))
  :cleanup (let ((status (agent-instance-last-command-status agent-instance)))
             (if (zerop status)
                 (format t "Agent 'lint' finished successfully (no errors).~%")
                 (format t "Agent 'lint' finished after retries (last exit code: ~A).~%" status))))

(defun derive-typecheck-command-from-stack ()
  "Derive a sensible typecheck command from the current *stack*."
  (cond
    ((member "typescript" *stack* :test #'string=) "tsc --noEmit")
    ((member "npm" *stack* :test #'string=) "npm run typecheck")
    ((member "rust" *stack* :test #'string=) "cargo check")
    (t "make typecheck")))

(defun get-typecheck-command ()
  "Return the typecheck command from config or a derived default."
  (or *typecheck-command*
      (derive-typecheck-command-from-stack)))

(defun get-typecheck-prompt (command output user-instructions)
  "Load a typecheck-fix prompt template and render it with details."
  (let* ((template-string (handler-case
                              (uiop:read-file-string (get-prompt-path "typecheck-fix.mustache"))
                            (error (e)
                              (declare (ignore e))
                              "Analyze the following typecheck/build errors from running '{{command}}'.
Propose precise code fixes as SEARCH/REPLACE blocks only. Do not include any other text.

Errors:
```
{{output}}
```
{{#instructions}}
Additional instructions:
{{instructions}}
{{/instructions}}")))
         (rendered (mustache:render* template-string
                                     `((:command . ,command)
                                       (:output . ,output)
                                       (:instructions . ,(or user-instructions ""))))))
    rendered))

(defagent typecheck (&optional user-instructions)
  "Runs a typecheck command in a loop and fixes code with type errors using SEARCH/REPLACE blocks."
  :init (progn
          (setf (agent-instance-retry-count agent-instance) 0)
          (setf (agent-instance-last-command-status agent-instance) -1)
          (format t "Agent 'typecheck' initialized.~%"))
  :run (let* ((command (get-typecheck-command)))
         (incf (agent-instance-retry-count agent-instance))
         (format t "Agent 'typecheck': Running typecheck (Attempt ~A): ~A~%"
                 (agent-instance-retry-count agent-instance) command)
         (multiple-value-bind (output error-output exit-code)
             (uiop:run-program command :output :string :error-output :string :ignore-error-status t)
           (setf (agent-instance-last-command-status agent-instance) exit-code)
           (if (zerop exit-code)
               (format t "Agent 'typecheck': No type errors detected.~%")
               (let* ((combined-output (format nil "exit-code:~A~%STDOUT:~%~A~%STDERR:~%~A"
                                               exit-code output error-output))
                      (prompt (get-typecheck-prompt command combined-output user-instructions)))
                 (format t "Agent 'typecheck': Typecheck failed. Asking LLM for fixes...~%")
                 ;; Non-streamed to ensure full S/R blocks land in history for processor
                 (get-llm-response prompt :stream nil :add-to-history t)))))
  :stop-condition (or (zerop (agent-instance-last-command-status agent-instance))
                      (>= (agent-instance-retry-count agent-instance) *agent-retry-limit*))
  :cleanup (let ((status (agent-instance-last-command-status agent-instance)))
             (if (zerop status)
                 (format t "Agent 'typecheck' finished successfully (no errors).~%")
                 (format t "Agent 'typecheck' finished after retries (last exit code: ~A).~%" status))))

(defun derive-test-agent-command-from-stack ()
  "Derive a sensible test command from the current *stack*."
  (cond
    ((member "npm" *stack* :test #'string=) "npm test")
    ((member "rust" *stack* :test #'string=) "cargo test")
    (t "make test")))

(defun get-test-agent-command ()
  "Return the test command from config or a derived default."
  (or *test-agent-command*
      (derive-test-agent-command-from-stack)))

(defun get-test-agent-prompt (command output user-instructions)
  "Load a test-fix prompt template and render it with details."
  (let* ((template-string (handler-case
                              (uiop:read-file-string (get-prompt-path "test-fix.mustache"))
                            (error (e)
                              (declare (ignore e))
                              "Analyze the following test failures from running '{{command}}'.
Propose precise code fixes as SEARCH/REPLACE blocks only. Do not include any other text.

Errors:
```
{{output}}
```
{{#instructions}}
Additional instructions:
{{instructions}}
{{/instructions}}")))
         (rendered (mustache:render* template-string
                                     `((:command . ,command)
                                       (:output . ,output)
                                       (:instructions . ,(or user-instructions ""))))))
    rendered))

(defagent test (&optional user-instructions)
  "Runs a test command in a loop and fixes code with test failures using SEARCH/REPLACE blocks."
  :init (progn
          (setf (agent-instance-retry-count agent-instance) 0)
          (setf (agent-instance-last-command-status agent-instance) -1)
          (format t "Agent 'test' initialized.~%"))
  :run (let* ((command (get-test-agent-command)))
         (incf (agent-instance-retry-count agent-instance))
         (format t "Agent 'test': Running tests (Attempt ~A): ~A~%"
                 (agent-instance-retry-count agent-instance) command)
         (multiple-value-bind (output error-output exit-code)
             (uiop:run-program command :output :string :error-output :string :ignore-error-status t)
           (setf (agent-instance-last-command-status agent-instance) exit-code)
           (if (zerop exit-code)
               (format t "Agent 'test': All tests passed.~%")
               (let* ((combined-output (format nil "exit-code:~A~%STDOUT:~%~A~%STDERR:~%~A"
                                               exit-code output error-output))
                      (prompt (get-test-agent-prompt command combined-output user-instructions)))
                 (format t "Agent 'test': Tests failed. Asking LLM for fixes...~%")
                 ;; Non-streamed to ensure full S/R blocks land in history for processor
                 (get-llm-response prompt :stream nil :add-to-history t)))))
  :stop-condition (or (zerop (agent-instance-last-command-status agent-instance))
                      (>= (agent-instance-retry-count agent-instance) *agent-retry-limit*))
  :cleanup (let ((status (agent-instance-last-command-status agent-instance)))
             (if (zerop status)
                 (format t "Agent 'test' finished successfully (all tests passed).~%")
                 (format t "Agent 'test' finished after retries (last exit code: ~A).~%" status))))
;;* Agent Core Functions
(defun run-command-loop (agent-instance command-string prompt-name)
  "Runs a command in a loop, feeding output to an LLM until a stop condition is met.
   This function is intended to be called from within an agent's run logic."
  (loop
    (when (>= (agent-instance-retry-count agent-instance) *agent-retry-limit*)
      (format t "Agent [~A]: Reached retry limit (~A). Stopping.~%"
              (agent-definition-name (agent-instance-definition agent-instance))
              *agent-retry-limit*)
      (setf (agent-instance-last-command-status agent-instance) -1) ; Indicate limit reached
      (return nil))

    (incf (agent-instance-retry-count agent-instance))
    (format t "Agent [~A]: Running command (Attempt ~A): ~A~%"
            (agent-definition-name (agent-instance-definition agent-instance))
            (agent-instance-retry-count agent-instance)
            command-string)

    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program command-string :output :string :error-output :string :ignore-error-status t)

      (setf (agent-instance-last-command-status agent-instance) exit-code)

      (cond
        ((zerop exit-code)
         (format t "Agent [~A]: Command succeeded (exit code 0).~%" (agent-definition-name (agent-instance-definition agent-instance)))
         (return t)) ; Success
        (t
         (let* ((combined-output (format nil "STDOUT:~%~A~%STDERR:~%~A" output error-output))
                (prompt-template (uiop:read-file-string (get-prompt-path prompt-name)))
                (prompt (mustache:render* prompt-template `((:output . ,combined-output)))))
           (format t "Agent [~A]: Command failed (exit code ~A). Asking LLM for fixes...~%"
                   (agent-definition-name (agent-instance-definition agent-instance))
                   exit-code)
           (get-llm-response prompt :stream nil :add-to-history t)
           ;; The response will trigger the search-replace-processor to apply fixes.
           ;; The loop will then continue to the next iteration.
           ))))))

(defun default-stop-condition (agent-instance)
  "Default stop condition for an agent. Stops if the last command was successful (0) or failed terminally."
  (let ((status (agent-instance-last-command-status agent-instance)))
    (and status (or (zerop status) (< status 0)))))

(defun agent-can-run-p ()
  "Checks if the safety flags allow agents to run."
  (or *live-dangerously* *agent-safe-env*))

(defun start-agent (agent-def args)
  "Creates an agent instance and starts its execution loop in a new thread."
  (unless (agent-can-run-p)
    (format t "Agent execution is disabled. Set *live-dangerously* or *agent-safe-env* to true to enable.~%")
    (return-from start-agent nil))

  (when (and (agent-definition-stack agent-def)
             (not (subsetp (agent-definition-stack agent-def) *stack* :test #'string=)))
    (format t "Agent '~A' cannot run because project stack ~A does not match agent stack ~A.~%"
            (agent-definition-name agent-def) *stack* (agent-definition-stack agent-def))
    (return-from start-agent nil))

  (let ((instance (make-agent-instance :definition agent-def))
        (retry-limit *agent-retry-limit*)) ; Capture dynamic var for the new thread
    (setf (gethash (agent-instance-id instance) *running-agents*) instance)
    (setf (agent-instance-status instance) :running)

    (let ((thread (bt:make-thread
                   (lambda ()
                     (let ((*agent-retry-limit* retry-limit)) ; Re-bind in the new thread
                       (unwind-protect
                          (let* ((pkg (find-package (agent-definition-package agent-def)))
                                 (agent-instance-sym (if pkg (intern "AGENT-INSTANCE" pkg) 'agent-instance))
                                 (ignorable-args (remove-if (lambda (arg) (member arg lambda-list-keywords))
                                                            (agent-definition-args agent-def)))
                                 (init-fn (eval `(lambda (,agent-instance-sym ,@(agent-definition-args agent-def))
                                                   (declare (ignorable ,agent-instance-sym ,@ignorable-args))
                                                   ,(agent-definition-init-form agent-def))))
                                 (run-fn (when (agent-definition-run-form agent-def)
                                           (eval `(lambda (,agent-instance-sym ,@(agent-definition-args agent-def))
                                                    (declare (ignorable ,agent-instance-sym ,@ignorable-args))
                                                    ,(agent-definition-run-form agent-def)))))
                                 (stop-fn (if (agent-definition-stop-condition-form agent-def)
                                              (eval `(lambda (,agent-instance-sym ,@(agent-definition-args agent-def))
                                                       (declare (ignorable ,agent-instance-sym ,@ignorable-args))
                                                       ,(agent-definition-stop-condition-form agent-def)))
                                              (lambda (agent-instance &rest ignored-args)
                                                (declare (ignore ignored-args))
                                                (default-stop-condition agent-instance))))
                                 (cleanup-fn (when (agent-definition-cleanup-form agent-def)
                                               (eval `(lambda (,agent-instance-sym ,@(agent-definition-args agent-def))
                                                        (declare (ignorable ,agent-instance-sym ,@ignorable-args))
                                                        ,(agent-definition-cleanup-form agent-def))))))
                           (handler-case
                               (progn
                                 ;; Init
                                 (apply init-fn instance args)
                                 ;; Run loop
                                 (when run-fn
                                   (loop until (apply stop-fn instance args)
                                         do (apply run-fn instance args)))
                                 ;; Cleanup
                                 (when cleanup-fn
                                   (apply cleanup-fn instance args)))
                             (error (e)
                               (format t "Agent [~A] encountered an error: ~A~%"
                                       (agent-definition-name agent-def) e)
                               (setf (agent-instance-status instance) :error)))
                           (setf (agent-instance-status instance) :stopped)
                           (remhash (agent-instance-id instance) *running-agents*)
                           (format t "Agent [~A] has stopped.~%" (agent-definition-name agent-def)))))
                     "Agent Thread") ; End of lambda
                   :name (format nil "Agent [~A]" (agent-definition-name agent-def)))))
      (setf (agent-instance-thread instance) thread)
      (format t "Agent [~A] started with ID: ~A~%" (agent-definition-name agent-def) (agent-instance-id instance))
      instance)))

(defun stop-agent (agent-id)
  "Stops a running agent by its ID."
  (let ((instance (gethash agent-id *running-agents*)))
    (if (and instance (bt:thread-alive-p (agent-instance-thread instance)))
        (progn
          (format t "Stopping agent [~A] (ID: ~A)...~%"
                  (agent-definition-name (agent-instance-definition instance))
                  agent-id)
          (bt:destroy-thread (agent-instance-thread instance))
          (setf (agent-instance-status instance) :stopped)
          (remhash agent-id *running-agents*)
          (format t "Agent stopped.~%"))
        (format t "Agent with ID ~A not found or not running.~%" agent-id))))

;;** Agent Commands
(define-sub-command agents (&rest args)
  "List currently running agents."
  (declare (ignore args))
  (if (zerop (hash-table-count *running-agents*))
      (format t "No agents are currently running.~%")
      (progn
        (format t "Running agents:~%")
        (maphash (lambda (id instance)
                   (format t "  ID: ~A~%    Name: ~A~%    Status: ~A~%"
                           id
                           (agent-definition-name (agent-instance-definition instance))
                           (agent-instance-status instance)))
                 *running-agents*))))

(define-command agent-run (args)
  "Run an agent. With no arguments, it shows a selector. Otherwise, runs the agent named in the first argument."
  (if args
      (let* ((agent-name-str (first args))
             (agent-name (intern (string-upcase agent-name-str) :hactar))
             (agent-def (gethash agent-name *agent-definitions*))
             (agent-args (rest args)))
        (if agent-def
            (start-agent agent-def agent-args)
            (format t "Agent '~A' not found.~%" agent-name-str)))
      ;; Original fzf logic for interactive selection
      (let* ((definitions (alexandria:hash-table-values *agent-definitions*))
             (items (loop for d in definitions
                          when (or (null (agent-definition-stack d))
                                   (subsetp (agent-definition-stack d) *stack* :test #'string=))
                          collect `((:item . ,(string (agent-definition-name d)))
                                    (:preview . ,(or (agent-definition-docstring d) "No description."))))))
        (if items
            (let* ((selected-item (fuzzy-select items))
                   (selected-name-str (when selected-item (cdr (assoc :item selected-item))))
                   (selected-name (when selected-name-str (intern selected-name-str :hactar)))
                   (agent-def (when selected-name (gethash selected-name *agent-definitions*))))
              (if agent-def
                  (start-agent agent-def nil) ; No args can be passed in fzf mode
                  (format t "Invalid agent selected.~%")))
            (format t "No available agents found for the current project stack.~%")))))

(define-command agent-stop (args)
  "Select and stop a running agent."
  (declare (ignore args))
  (let* ((running-agents (alexandria:hash-table-values *running-agents*))
         (items (loop for instance in running-agents
                      collect `((:item . ,(agent-instance-id instance))
                                (:preview . ,(format nil "Name: ~A~%Status: ~A"
                                                     (agent-definition-name (agent-instance-definition instance))
                                                     (agent-instance-status instance)))))))
    (if items
        (let* ((selected-item (fuzzy-select items))
               (selected-id (when selected-item (cdr (assoc :item selected-item)))))
          (if selected-id
              (stop-agent selected-id)
              (format t "Agent selection cancelled.~%")))
        (format t "No agents are currently running.~%"))))

(define-sub-command agent.run (args)
  "Run an agent by name. Usage: hactar agent.run <agent_name> [agent_args...]"
  (if (null args)
      (format t "Usage: hactar agent.run <agent_name> [agent_args...]~%")
      (let* ((agent-name-str (first args))
             (agent-name (intern (string-upcase agent-name-str) :hactar))
             (agent-def (gethash agent-name *agent-definitions*))
             (agent-args (rest args)))
        (if agent-def
            (let ((instance (start-agent agent-def agent-args)))
              (when instance
                ;; Wait for the agent's thread to finish
                (bt:join-thread (agent-instance-thread instance))))
            (format t "Agent '~A' not found.~%" agent-name-str)))))

(define-sub-command agent.stop (args)
  "Stop a running agent by its ID. Usage: hactar agent.stop <agent_id>"
  (if (null args)
      (format t "Usage: hactar agent.stop <agent_id>~%")
      (stop-agent (first args))))

(defvar *watcher-test-watcher-output-hook* (make-instance 'hook-watcher-output)
  "Hook for output from the test-watcher.")

(defwatcher test-watcher *test-command* "Run the project's test suite (e.g., 'make test')." :daemon nil)

(def-analyzer analyze-test-results ((*watcher-test-watcher-output-hook*)) nil (active-watcher line)
              "Analyzes output from the test watcher for failures and asks LLM for fixes."
              (debug-log "Running analyze-test-results analyzer for line:" line)
              (when (or (search "FAIL" line :test #'string-equal)
                        (search "ERROR" line :test #'string-equal)
                        (search "FAILED" line :test #'string-equal))
                (format t "~&Detected test failure/error in output: ~A~%" line)
                (when (confirm-action "Ask AI for potential fixes based on recent output?")
                  (let* ((full-output (get-watcher-output active-watcher)) ; Get accumulated output
                         (prompt (format nil "The following test output indicates a failure. Please analyze the errors and suggest potential fixes as SEARCH/REPLACE blocks if applicable:\n\n```\n~A\n```" full-output)))
                    (format t "~&Asking LLM for analysis...~%")
                    (get-llm-response prompt :stream t :add-to-history t)))))
