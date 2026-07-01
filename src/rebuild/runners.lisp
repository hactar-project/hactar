(in-package #:hactar-rebuild)

;;; ---------------------------------------------------------------------------
;;; Sandbox policy enforcement
;;; ---------------------------------------------------------------------------

(defun sandbox-check (sandbox-name cmd)
  "Signal an error if CMD violates the named sandbox's allow/deny lists."
  (when sandbox-name
    (let* ((sb (gethash (key-string sandbox-name) *sandboxes*))
           (bin (first (str:split " " (str:trim cmd) :omit-nulls t))))
      (when sb
        (when (find-if (lambda (d) (search d cmd)) (sandbox-def-deny sb))
          (error "Sandbox ~A denies: ~A" sandbox-name cmd))
        (when (and (sandbox-def-allow sb)
                   (not (member bin (sandbox-def-allow sb) :test #'string=)))
          (error "Sandbox ~A does not allow binary: ~A" sandbox-name bin))))))

;;; ---------------------------------------------------------------------------
;;; Shell execution (only reachable through tools)
;;; ---------------------------------------------------------------------------

(defvar *dry-run-shell* nil
  "When true, SHELL-CMD echoes the command instead of executing it (tests).")

(defun shell-cmd (cmd &key sandbox)
  "Execute a shell command under sandbox policy and return a RESULT."
  (sandbox-check sandbox cmd)
  (when *dry-run-shell*
    (return-from shell-cmd (ok (format nil "[dry-run] ~A" cmd)
                               :data (list :exit-code 0 :cmd cmd))))
  (multiple-value-bind (out serr code)
      (uiop:run-program cmd :output :string :error-output :string
                            :ignore-error-status t
                            :directory (and *repo-root* (truename *repo-root*)))
    (make-result :status (if (zerop code) :ok :error)
                 :message (if (zerop code) out serr)
                 :data (list :exit-code code :stdout out :stderr serr))))

;;; ---------------------------------------------------------------------------
;;; Universal project tools -> resolved per detected stack
;;; ---------------------------------------------------------------------------

(defstate *stack-commands* (make-hash-table :test 'equal)
  "action keyword (:test :lint :typecheck :build) -> shell command string.")

(defun resolve-stack-cmd (action &optional arg)
  (let ((tmpl (gethash action *stack-commands*)))
    (when tmpl
      (if arg (format nil "~A ~A" tmpl arg) tmpl))))

(defun run-test-cmd (&optional file)
  (let ((cmd (resolve-stack-cmd :test file)))
    (if cmd (shell-cmd cmd :sandbox "project")
        (err "No test command resolved for this stack."))))

(defun run-lint-cmd ()
  (let ((cmd (resolve-stack-cmd :lint)))
    (if cmd (shell-cmd cmd :sandbox "project") (err "No lint command resolved."))))

(defun run-typecheck-cmd ()
  (let ((cmd (resolve-stack-cmd :typecheck)))
    (if cmd (shell-cmd cmd :sandbox "project") (err "No typecheck command resolved."))))

(defun run-build-cmd ()
  (let ((cmd (resolve-stack-cmd :build)))
    (if cmd (shell-cmd cmd :sandbox "project") (err "No build command resolved."))))

;;; ---------------------------------------------------------------------------
;;; Tool runner
;;; ---------------------------------------------------------------------------

(defun run-tool (name &rest args)
  "Run a registered tool by applying ARGS to its function. Returns a RESULT."
  (let ((tool (gethash (key-string name) *tools*)))
    (unless tool
      (return-from run-tool (err (format nil "No such tool: ~A" name))))
    (handler-case
        (let ((val (apply (tool-def-fn tool) args)))
          (if (result-p val) val (ok (format nil "~S" val))))
      (error (e) (err e)))))

;;; ---------------------------------------------------------------------------
;;; Prompt rendering ({{var}}, {{stack}}, {{doc:NAME}})
;;; ---------------------------------------------------------------------------

(defun lookup-doc (name)
  "Return the doc body/uri for NAME, or an empty string."
  (let ((doc (gethash (key-string name) *docs*)))
    (if doc (or (doc-def-text doc) (doc-def-uri doc) "") "")))

(defun render-prompt (prompt params)
  "PROMPT is a prompt name (symbol) or a literal template string.
Expands {{stack}}, {{doc:NAME}}, and {{key}} from the PARAMS plist."
  (let* ((named (and (symbolp prompt)
                     (gethash (key-string prompt) *prompts*)))
         (tmpl (if named (prompt-def-template named) (princ-to-string prompt))))
    (labels ((expand (s)
               (setf s (cl-ppcre:regex-replace-all "\\{\\{stack\\}\\}" s
                        (format nil "~{~A~^ ~}" *stack*)))
               (setf s (cl-ppcre:regex-replace-all "\\{\\{doc:([^}]+)\\}\\}" s
                        (lambda (m &rest g)
                          (declare (ignore g))
                          (lookup-doc (subseq m 6 (1- (length m)))))))
               (loop for (k v) on params by #'cddr
                     do (setf s (cl-ppcre:regex-replace-all
                                 (format nil "\\{\\{~A\\}\\}"
                                         (string-downcase (string k)))
                                 s (princ-to-string v))))
               s))
      (expand tmpl))))

;;; ---------------------------------------------------------------------------
;;; Action runner
;;; ---------------------------------------------------------------------------

(defun apply-strategy (strategy answer params)
  "Apply the LLM ANSWER according to STRATEGY. :create/:report are supported;
network-free by default (returns the answer as a result)."
  (declare (ignore params))
  (ecase strategy
    ((:create :edit :apply) (ok answer :artifacts nil))
    ((:report nil) (ok answer))))

(defun run-action (name params)
  "Render the action prompt, call the LLM, and apply the result."
  (let ((action (gethash (key-string name) *actions*)))
    (unless action
      (return-from run-action (err (format nil "No action: ~A" name))))
    (let* ((prompt (render-prompt (action-def-prompt action) params))
           (answer (ask prompt)))
      (apply-strategy (action-def-apply action) answer params))))

;;; ---------------------------------------------------------------------------
;;; Agent runner
;;; ---------------------------------------------------------------------------

(defun compose-system-prompt (agent)
  (with-output-to-string (s)
    (format s "~A~%~%" (or (agent-def-instructions agent) ""))
    (when (agent-def-skills agent)
      (format s "Skills: ~{~A~^, ~}~%" (agent-def-skills agent)))
    (when (agent-def-tools agent)
      (format s "Tools available: ~{~A~^, ~}~%" (agent-def-tools agent)))))

(defun run-agent (name user-request)
  "Compose model + tools + skills + sandbox + instructions and run a turn."
  (let ((agent (gethash (key-string name) *agents*)))
    (unless agent
      (return-from run-agent (err (format nil "No agent: ~A" name))))
    (let* ((system (compose-system-prompt agent))
           (answer (llm-chat (agent-def-model agent) system user-request
                             (agent-def-tools agent))))
      (ok answer))))

;;; ---------------------------------------------------------------------------
;;; Command runner
;;; ---------------------------------------------------------------------------

(defun run-command (name args)
  (let ((cmd (gethash (key-string name) *commands*)))
    (unless cmd
      (return-from run-command (err (format nil "No command: ~A" name))))
    (let ((handler (command-def-handler cmd)))
      (if (and handler (fboundp handler))
          (funcall (symbol-function handler) args)
          (err (format nil "Command ~A has no runnable handler." name))))))
