;;* AgentShell — ACP client using hactar + charms
(in-package :hactar)

;;** State

(defstruct agentshell-state
  "Holds all state for an agentshell session."
  (process nil)                    ; uiop process-info for the agent subprocess
  (stdin nil)                      ; output stream to agent's stdin
  (stdout nil)                     ; input stream from agent's stdout
  (stderr nil)                     ; input stream from agent's stderr
  (session-id nil :type (or null string))
  (agent-info nil)                 ; alist from initialize response
  (agent-capabilities nil)         ; alist from initialize response
  (config-options nil)             ; vector of config option alists
  (available-commands nil)         ; vector of command alists
  (current-model "unknown" :type string)
  (current-mode "code" :type string)
  (request-counter 0 :type integer)
  (pending-requests (make-hash-table :test 'equal))
  ;; Display state
  (output-lines (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (scroll-offset 0 :type integer)  ; lines scrolled up from bottom
  (input-buffer "" :type string)
  (input-cursor 0 :type integer)
  (status-message "" :type string)
  (running t :type boolean)
  (waiting nil :type boolean)      ; waiting for agent response
  ;; Tool call tracking
  (active-tool-calls (make-hash-table :test 'equal))
  ;; Pending agent→client requests needing user action
  (pending-permission nil)         ; plist of pending permission request or nil
  ;; Reader thread
  (reader-thread nil)
  (message-queue (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (message-lock (bt:make-lock "agentshell-msg-lock"))
  ;; Input history
  (history (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (history-index -1 :type integer))

(defvar *agentshell* nil "The current agentshell state instance.")

;;** JSON-RPC Transport

(defun agentshell-write-message (state message)
  "Serialize MESSAGE as JSON and write to agent's stdin as newline-delimited JSON."
  (let* ((shasht:*write-alist-as-object* t)
         (shasht:*write-plist-as-object* nil)
         (shasht:*symbol-name-function* (lambda (sym)
                                          (let ((name (symbol-name sym)))
                                            (cond
                                              ((string= name "JSONRPC") "jsonrpc")
                                              ((string= name "SESSION-ID") "sessionId")
                                              ((string= name "PROTOCOL-VERSION") "protocolVersion")
                                              ((string= name "CLIENT-CAPABILITIES") "clientCapabilities")
                                              ((string= name "CLIENT-INFO") "clientInfo")
                                              ((string= name "AGENT-CAPABILITIES") "agentCapabilities")
                                              ((string= name "AGENT-INFO") "agentInfo")
                                              ((string= name "CONFIG-ID") "configId")
                                              ((string= name "CONFIG-OPTIONS") "configOptions")
                                              ((string= name "MODE-ID") "modeId")
                                              ((string= name "OPTION-ID") "optionId")
                                              ((string= name "TOOL-CALL-ID") "toolCallId")
                                              ((string= name "STOP-REASON") "stopReason")
                                              ((string= name "AVAILABLE-COMMANDS") "availableCommands")
                                              ((string= name "SESSION-UPDATE") "sessionUpdate")
                                              ((string= name "CURRENT-VALUE") "currentValue")
                                              ((string= name "MIME-TYPE") "mimeType")
                                              ((string= name "EXIT-CODE") "exitCode")
                                              ((string= name "TERMINAL-ID") "terminalId")
                                              ((string= name "OUTPUT-BYTE-LIMIT") "outputByteLimit")
                                              ((string= name "WAIT-FOR-EXIT") "waitForExit")
                                              ((string= name "READ-TEXT-FILE") "readTextFile")
                                              ((string= name "WRITE-TEXT-FILE") "writeTextFile")
                                              (t (string-downcase name))))))
         (json-string (shasht:write-json message nil)))
    (let ((clean (cl-ppcre:regex-replace-all "\\n" json-string "")))
      (write-line clean (agentshell-state-stdin state))
      (force-output (agentshell-state-stdin state)))))

(defun agentshell-read-message (state)
  "Read a single newline-delimited JSON-RPC message from agent's stdout.
   Returns parsed alist or NIL on EOF."
  (let ((line (handler-case
                  (read-line (agentshell-state-stdout state) nil nil)
                (error () nil))))
    (when (and line (> (length (string-trim '(#\Space #\Tab #\Return) line)) 0))
      (handler-case
          (let ((shasht:*read-default-object-format* :alist))
            (shasht:read-json line))
        (error (e)
          (agentshell-append-output state
            (format nil "[Parse error: ~A]" e))
          nil)))))

(defun agentshell-make-request (state method params)
  "Create a JSON-RPC 2.0 request and return (values message id-string)."
  (let* ((id (incf (agentshell-state-request-counter state)))
         (id-str (format nil "client_~A" id))
         (request `(("jsonrpc" . "2.0")
                    ("id" . ,id-str)
                    ("method" . ,method)
                    ("params" . ,params))))
    (values request id-str)))

(defun agentshell-make-response (id result)
  "Create a JSON-RPC 2.0 success response (client → agent)."
  `(("jsonrpc" . "2.0")
    ("id" . ,id)
    ("result" . ,result)))

(defun agentshell-make-error-response (id code message)
  "Create a JSON-RPC 2.0 error response (client → agent)."
  `(("jsonrpc" . "2.0")
    ("id" . ,id)
    ("error" . (("code" . ,code) ("message" . ,message)))))

(defun agentshell-call (state method params &key (timeout 120))
  "Send a request and wait for the response, processing other messages meanwhile.
   Returns the result alist."
  (multiple-value-bind (request id-str) (agentshell-make-request state method params)
    (let ((result-box (list nil nil))) ; (result received-p)
      (setf (gethash id-str (agentshell-state-pending-requests state)) result-box)
      (agentshell-write-message state request)
      (let ((start (get-universal-time)))
        (loop
          (when (second result-box)
            (remhash id-str (agentshell-state-pending-requests state))
            (let ((res (first result-box)))
              ;; Check if it's an error
              (when (and (listp res) (assoc "error" res :test #'string=))
                (let ((err (cdr (assoc "error" res :test #'string=))))
                  (agentshell-append-output state
                    (format nil "[Error ~A: ~A]"
                            (cdr (assoc "code" err :test #'string=))
                            (cdr (assoc "message" err :test #'string=))))))
              (return res)))
          (when (> (- (get-universal-time) start) timeout)
            (remhash id-str (agentshell-state-pending-requests state))
            (agentshell-append-output state "[Request timed out]")
            (return nil))
          (agentshell-process-queued-messages state)
          (sleep 0.05))))))

;;** Output Buffer Management

(defun agentshell-append-output (state text)
  "Append text to the output buffer, splitting into lines."
  (let ((lines (str:lines text)))
    (dolist (line lines)
      (vector-push-extend line (agentshell-state-output-lines state)))))

(defun agentshell-append-output-inline (state text)
  "Append text to the last output line (for streaming chunks).
   If the text contains newlines, split appropriately."
  (let ((lines (str:lines text))
        (buf (agentshell-state-output-lines state)))
    (when lines
      (if (> (length buf) 0)
          ;; Append first fragment to last line
          (setf (aref buf (1- (length buf)))
                (concatenate 'string (aref buf (1- (length buf))) (first lines)))
          ;; No existing lines, just push
          (vector-push-extend (first lines) buf))
      ;; Push remaining lines
      (dolist (line (rest lines))
        (vector-push-extend line buf)))))

;;** Message Processing

(defun agentshell-process-queued-messages (state)
  "Process all messages in the queue."
  (let ((messages nil))
    (bt:with-lock-held ((agentshell-state-message-lock state))
      (when (> (fill-pointer (agentshell-state-message-queue state)) 0)
        (setf messages (coerce (agentshell-state-message-queue state) 'list))
        (setf (fill-pointer (agentshell-state-message-queue state)) 0)))
    (dolist (msg messages)
      (agentshell-dispatch state msg))))

(defun agentshell-dispatch (state msg)
  "Dispatch an incoming JSON-RPC message from the agent."
  (let* ((id (cdr (assoc "id" msg :test #'string=)))
         (method (cdr (assoc "method" msg :test #'string=)))
         (params (cdr (assoc "params" msg :test #'string=)))
         (result (cdr (assoc "result" msg :test #'string=)))
         (error-obj (cdr (assoc "error" msg :test #'string=))))
    (cond
      ;; Response to our outbound request
      ((and id (null method) (or result error-obj))
       (let* ((id-key (if (stringp id) id (format nil "~A" id)))
              (pending (gethash id-key (agentshell-state-pending-requests state))))
         (when pending
           (setf (first pending) (if error-obj msg result))
           (setf (second pending) t))))

      ;; Incoming request from agent (has id and method) — agent→client capability calls
      ((and id method)
       (agentshell-handle-agent-request state id method params))

      ;; Notification from agent (method, no id)
      ((and method (null id))
       (agentshell-handle-notification state method params))

      (t nil))))

;;** Notification Handlers

(defun agentshell-handle-notification (state method params)
  "Handle a notification from the agent."
  (cond
    ((string= method "session/update")
     (agentshell-handle-session-update state params))
    (t
     (agentshell-append-output state
       (format nil "[Notification: ~A]" method)))))

(defun agentshell-handle-session-update (state params)
  "Handle a session/update notification."
  (let* ((update (cdr (assoc "update" params :test #'string=)))
         (update-type (cdr (assoc "sessionUpdate" update :test #'string=))))
    (cond
      ((string= update-type "agent_message_chunk")
       (let* ((content (cdr (assoc "content" update :test #'string=)))
              (text (cdr (assoc "text" content :test #'string=))))
         (when text
           (agentshell-append-output-inline state text))))

      ((string= update-type "thought_message_chunk")
       (let* ((content (cdr (assoc "content" update :test #'string=)))
              (text (cdr (assoc "text" content :test #'string=))))
         (when text
           (agentshell-append-output-inline state
             (format nil "💭 ~A" text)))))

      ((string= update-type "tool_call")
       (let ((tc-id (cdr (assoc "toolCallId" update :test #'string=)))
             (title (cdr (assoc "title" update :test #'string=)))
             (status (cdr (assoc "status" update :test #'string=))))
         (setf (gethash tc-id (agentshell-state-active-tool-calls state)) update)
         (agentshell-append-output state
           (format nil "🔧 [~A] ~A (~A)" (or status "?") (or title tc-id) tc-id))))

      ((string= update-type "tool_call_update")
       (let ((tc-id (cdr (assoc "toolCallId" update :test #'string=)))
             (status (cdr (assoc "status" update :test #'string=)))
             (content (cdr (assoc "content" update :test #'string=))))
         (let ((existing (gethash tc-id (agentshell-state-active-tool-calls state))))
           (when existing
             (setf (cdr (assoc "status" existing :test #'string=)) status)))
         (let ((status-icon (cond
                              ((string= status "completed") "✅")
                              ((string= status "failed") "❌")
                              ((string= status "in_progress") "⏳")
                              (t "❓"))))
           (agentshell-append-output state
             (format nil "  ~A ~A" status-icon (or status ""))))
         ;; Show content if present
         (when (and content (vectorp content))
           (loop for block across content
                 for block-content = (cdr (assoc "content" block :test #'string=))
                 when block-content
                 do (let ((text (cdr (assoc "text" block-content :test #'string=))))
                      (when text
                        (agentshell-append-output state
                          (format nil "    ~A" text))))))))

      ((string= update-type "available_commands_update")
       (let ((cmds (cdr (assoc "availableCommands" update :test #'string=))))
         (setf (agentshell-state-available-commands state) cmds)))

      ((string= update-type "config_options_update")
       (let ((opts (cdr (assoc "configOptions" update :test #'string=))))
         (setf (agentshell-state-config-options state) opts)
         ;; Extract current model from config options
         (when (vectorp opts)
           (loop for opt across opts
                 when (string= (cdr (assoc "id" opt :test #'string=)) "model")
                 do (setf (agentshell-state-current-model state)
                          (or (cdr (assoc "currentValue" opt :test #'string=)) "unknown"))
                 when (string= (cdr (assoc "id" opt :test #'string=)) "mode")
                 do (setf (agentshell-state-current-mode state)
                          (or (cdr (assoc "currentValue" opt :test #'string=)) "code"))))))

      ((string= update-type "plan")
       (let ((entries (cdr (assoc "entries" update :test #'string=))))
         (agentshell-append-output state "📋 Plan:")
         (when (vectorp entries)
           (loop for entry across entries
                 for content = (cdr (assoc "content" entry :test #'string=))
                 for status = (cdr (assoc "status" entry :test #'string=))
                 do (agentshell-append-output state
                      (format nil "  [~A] ~A" (or status "?") (or content "")))))))

      ((string= update-type "user_message_chunk")
       ;; Usually from session/load replay; show it
       (let* ((content (cdr (assoc "content" update :test #'string=)))
              (text (cdr (assoc "text" content :test #'string=))))
         (when text
           (agentshell-append-output state (format nil "> ~A" text)))))

      (t
       (agentshell-append-output state
         (format nil "[Update: ~A]" (or update-type "unknown")))))))

;;** Agent→Client Request Handlers

(defun agentshell-handle-agent-request (state id method params)
  "Handle an incoming request from the agent (agent→client)."
  (cond
    ;; Permission request
    ((string= method "session/request_permission")
     (agentshell-handle-permission-request state id params))

    ;; Filesystem: read
    ((string= method "fs/read_text_file")
     (agentshell-handle-fs-read state id params))

    ;; Filesystem: write
    ((string= method "fs/write_text_file")
     (agentshell-handle-fs-write state id params))

    ;; Terminal operations
    ((string= method "terminal/create")
     (agentshell-handle-terminal-create state id params))
    ((string= method "terminal/output")
     (agentshell-handle-terminal-output state id params))
    ((string= method "terminal/wait_for_exit")
     (agentshell-handle-terminal-wait state id params))
    ((string= method "terminal/kill")
     (agentshell-handle-terminal-kill state id params))
    ((string= method "terminal/release")
     (agentshell-handle-terminal-release state id params))

    (t
     ;; Unknown method
     (agentshell-write-message state
       (agentshell-make-error-response id -32601
         (format nil "Method not supported: ~A" method))))))

(defun agentshell-handle-permission-request (state id params)
  "Handle a permission request from the agent. Queue it for user interaction."
  (let* ((tool-call (cdr (assoc "toolCall" params :test #'string=)))
         (title (cdr (assoc "title" tool-call :test #'string=)))
         (options (cdr (assoc "options" params :test #'string=))))
    (agentshell-append-output state
      (format nil "⚠️  Permission requested: ~A" (or title "unknown")))
    (when (and options (vectorp options))
      (loop for opt across options
            for i from 1
            do (agentshell-append-output state
                 (format nil "  ~A) ~A" i
                         (cdr (assoc "name" opt :test #'string=))))))
    (agentshell-append-output state "  Enter number to respond:")
    (setf (agentshell-state-pending-permission state)
          (list :id id :options options))))

(defun agentshell-resolve-permission (state choice-index)
  "Resolve a pending permission request with the user's choice."
  (let ((perm (agentshell-state-pending-permission state)))
    (when perm
      (let* ((id (getf perm :id))
             (options (getf perm :options))
             (chosen (when (and (vectorp options)
                                (> choice-index 0)
                                (<= choice-index (length options)))
                       (aref options (1- choice-index)))))
        (if chosen
            (let ((option-id (cdr (assoc "optionId" chosen :test #'string=)))
                  (kind (cdr (assoc "kind" chosen :test #'string=))))
              (agentshell-write-message state
                (agentshell-make-response id
                  `(("outcome" . (("outcome" . "selected")
                                  ("optionId" . ,option-id)
                                  ("kind" . ,kind))))))
              (agentshell-append-output state
                (format nil "  → Selected: ~A"
                        (cdr (assoc "name" chosen :test #'string=)))))
            (progn
              (agentshell-write-message state
                (agentshell-make-response id
                  `(("outcome" . (("outcome" . "cancelled"))))))
              (agentshell-append-output state "  → Cancelled")))
        (setf (agentshell-state-pending-permission state) nil)))))

;;*** Filesystem Handlers

(defvar *agentshell-terminals* (make-hash-table :test 'equal)
  "Client-side terminal tracking for agentshell.")

(defstruct agentshell-terminal
  (id "" :type string)
  (output "" :type string)
  (exit-code nil)
  (thread nil))

(defun agentshell-handle-fs-read (state id params)
  "Handle fs/read_text_file from agent."
  (let* ((path (cdr (assoc "path" params :test #'string=)))
         (line-start (cdr (assoc "line" params :test #'string=)))
         (limit (cdr (assoc "limit" params :test #'string=))))
    (handler-case
        (if (probe-file path)
            (let* ((content (uiop:read-file-string path :external-format :utf-8))
                   (lines (when (or line-start limit) (str:lines content)))
                   (start (or line-start 0))
                   (end (if limit (+ start limit) (when lines (length lines))))
                   (selected (if lines
                                 (str:join (string #\Newline)
                                           (subseq lines
                                                   (min start (length lines))
                                                   (min end (length lines))))
                                 content)))
              (agentshell-write-message state
                (agentshell-make-response id `(("content" . ,selected)))))
            (agentshell-write-message state
              (agentshell-make-error-response id -32602
                (format nil "File not found: ~A" path))))
      (error (e)
        (agentshell-write-message state
          (agentshell-make-error-response id -32603
            (format nil "Error reading: ~A" e)))))))

(defun agentshell-handle-fs-write (state id params)
  "Handle fs/write_text_file from agent."
  (let ((path (cdr (assoc "path" params :test #'string=)))
        (content (cdr (assoc "content" params :test #'string=))))
    (handler-case
        (progn
          (ensure-directories-exist (pathname path))
          (with-open-file (s path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
            (write-string content s))
          (agentshell-append-output state
            (format nil "📝 Wrote: ~A" path))
          (agentshell-write-message state
            (agentshell-make-response id `(("success" . t)))))
      (error (e)
        (agentshell-write-message state
          (agentshell-make-error-response id -32603
            (format nil "Error writing: ~A" e)))))))

;;*** Terminal Handlers

(defun agentshell-handle-terminal-create (state id params)
  "Handle terminal/create from agent."
  (let* ((command (cdr (assoc "command" params :test #'string=)))
         (args (cdr (assoc "args" params :test #'string=)))
         (cwd (cdr (assoc "cwd" params :test #'string=)))
         (term-id (format nil "cterm_~A" (uuid:make-v4-uuid)))
         (full-cmd (if (and args (vectorp args))
                       (format nil "~A ~{~A~^ ~}" command (coerce args 'list))
                       command))
         (shell-cmd (if cwd
                        (format nil "cd ~A && ~A" cwd full-cmd)
                        full-cmd))
         (term (make-agentshell-terminal :id term-id)))
    (setf (gethash term-id *agentshell-terminals*) term)
    (agentshell-append-output state
      (format nil "🖥️  Terminal ~A: ~A" term-id full-cmd))
    (setf (agentshell-terminal-thread term)
          (bt:make-thread
           (lambda ()
             (handler-case
                 (multiple-value-bind (stdout stderr exit-code)
                     (uiop:run-program (list *shell* "-c" shell-cmd)
                                       :output :string :error-output :string
                                       :ignore-error-status t)
                   (setf (agentshell-terminal-output term)
                         (format nil "~A~@[~%~A~]" stdout
                                 (when (and stderr (> (length stderr) 0)) stderr)))
                   (setf (agentshell-terminal-exit-code term) exit-code))
               (error (e)
                 (setf (agentshell-terminal-output term)
                       (format nil "Error: ~A" e))
                 (setf (agentshell-terminal-exit-code term) 1))))
           :name (format nil "agentshell-term-~A" term-id)))
    (agentshell-write-message state
      (agentshell-make-response id `(("terminalId" . ,term-id))))))

(defun agentshell-handle-terminal-output (state id params)
  "Handle terminal/output from agent."
  (let* ((term-id (cdr (assoc "terminalId" params :test #'string=)))
         (term (gethash term-id *agentshell-terminals*)))
    (if term
        (let ((result `(("output" . ,(agentshell-terminal-output term))
                        ("truncated" . :false))))
          (when (agentshell-terminal-exit-code term)
            (push (cons "exitStatus"
                        `(("exitCode" . ,(agentshell-terminal-exit-code term))
                          ("signal" . :null)))
                  result))
          (agentshell-write-message state
            (agentshell-make-response id result)))
        (agentshell-write-message state
          (agentshell-make-error-response id -32602
            (format nil "Unknown terminal: ~A" term-id))))))

(defun agentshell-handle-terminal-wait (state id params)
  "Handle terminal/wait_for_exit from agent."
  (let* ((term-id (cdr (assoc "terminalId" params :test #'string=)))
         (term (gethash term-id *agentshell-terminals*)))
    (if term
        (progn
          ;; Wait for the thread to finish
          (when (agentshell-terminal-thread term)
            (bt:join-thread (agentshell-terminal-thread term)))
          (agentshell-write-message state
            (agentshell-make-response id
              `(("exitCode" . ,(or (agentshell-terminal-exit-code term) -1))
                ("signal" . :null)))))
        (agentshell-write-message state
          (agentshell-make-error-response id -32602
            (format nil "Unknown terminal: ~A" term-id))))))

(defun agentshell-handle-terminal-kill (state id params)
  "Handle terminal/kill from agent."
  (let* ((term-id (cdr (assoc "terminalId" params :test #'string=)))
         (term (gethash term-id *agentshell-terminals*)))
    (if term
        (progn
          (when (and (agentshell-terminal-thread term)
                     (bt:thread-alive-p (agentshell-terminal-thread term)))
            (ignore-errors (bt:destroy-thread (agentshell-terminal-thread term))))
          (unless (agentshell-terminal-exit-code term)
            (setf (agentshell-terminal-exit-code term) -1))
          (agentshell-write-message state
            (agentshell-make-response id nil)))
        (agentshell-write-message state
          (agentshell-make-error-response id -32602
            (format nil "Unknown terminal: ~A" term-id))))))

(defun agentshell-handle-terminal-release (state id params)
  "Handle terminal/release from agent."
  (let* ((term-id (cdr (assoc "terminalId" params :test #'string=)))
         (term (gethash term-id *agentshell-terminals*)))
    (when term
      (when (and (agentshell-terminal-thread term)
                 (bt:thread-alive-p (agentshell-terminal-thread term)))
        (ignore-errors (bt:destroy-thread (agentshell-terminal-thread term))))
      (remhash term-id *agentshell-terminals*))
    (agentshell-write-message state
      (agentshell-make-response id nil))))

;;** ACP Handshake & Session Management

(defun agentshell-initialize (state)
  "Perform the ACP initialize handshake."
  (let ((result (agentshell-call state "initialize"
                  `(("protocolVersion" . 1)
                    ("clientCapabilities" .
                     (("fs" . (("readTextFile" . t)
                               ("writeTextFile" . t)))
                      ("terminal" . t)))
                    ("clientInfo" .
                     (("name" . "agentshell")
                      ("version" . ,*hactar-version*)))))))
    (when result
      (setf (agentshell-state-agent-info state)
            (cdr (assoc "agentInfo" result :test #'string=)))
      (setf (agentshell-state-agent-capabilities state)
            (cdr (assoc "agentCapabilities" result :test #'string=)))
      (let ((agent-name (cdr (assoc "name"
                               (agentshell-state-agent-info state) :test #'string=)))
            (agent-version (cdr (assoc "version"
                                  (agentshell-state-agent-info state) :test #'string=))))
        (agentshell-append-output state
          (format nil "Connected to ~A v~A" (or agent-name "agent") (or agent-version "?"))))
      t)))

(defun agentshell-new-session (state)
  "Create a new ACP session."
  (let ((result (agentshell-call state "session/new"
                  `(("cwd" . ,(namestring (or *repo-root* (uiop:getcwd))))))))
    (when result
      (setf (agentshell-state-session-id state)
            (cdr (assoc "sessionId" result :test #'string=)))
      (let ((config-opts (cdr (assoc "configOptions" result :test #'string=))))
        (when config-opts
          (setf (agentshell-state-config-options state) config-opts)
          (when (vectorp config-opts)
            (loop for opt across config-opts
                  when (string= (cdr (assoc "id" opt :test #'string=)) "model")
                  do (setf (agentshell-state-current-model state)
                           (or (cdr (assoc "currentValue" opt :test #'string=)) "unknown"))))))
      (agentshell-append-output state
        (format nil "Session: ~A" (agentshell-state-session-id state)))
      t)))

(defun agentshell-send-prompt (state text)
  "Send a prompt to the agent and process the response."
  (setf (agentshell-state-waiting state) t)
  ;; Start a new line for the response
  (agentshell-append-output state "")
  (let ((result (agentshell-call state "session/prompt"
                  `(("sessionId" . ,(agentshell-state-session-id state))
                    ("prompt" . ,(vector
                                  `(("type" . "text")
                                    ("text" . ,text))))))))
    (setf (agentshell-state-waiting state) nil)
    ;; Ensure output ends with newline
    (agentshell-append-output state "")
    (when result
      (let ((stop-reason (cdr (assoc "stopReason" result :test #'string=))))
        (when (and stop-reason (not (string= stop-reason "end_turn")))
          (agentshell-append-output state
            (format nil "[Stop: ~A]" stop-reason)))))))

(defun agentshell-send-cancel (state)
  "Send session/cancel notification."
  (when (agentshell-state-session-id state)
    (agentshell-write-message state
      `(("jsonrpc" . "2.0")
        ("method" . "session/cancel")
        ("params" . (("sessionId" . ,(agentshell-state-session-id state))))))))

;;** Background Reader Thread

(defun agentshell-start-reader (state)
  "Start a background thread that reads messages from the agent."
  (setf (agentshell-state-reader-thread state)
        (bt:make-thread
         (lambda ()
           (loop while (agentshell-state-running state)
                 do (handler-case
                        (let ((msg (agentshell-read-message state)))
                          (if msg
                              (bt:with-lock-held ((agentshell-state-message-lock state))
                                (vector-push-extend msg (agentshell-state-message-queue state)))
                              ;; EOF
                              (progn
                                (setf (agentshell-state-running state) nil)
                                (return))))
                      (error ()
                        (setf (agentshell-state-running state) nil)
                        (return)))))
         :name "agentshell-reader")))

;;** Process Management

(defun agentshell-spawn-agent (command-args)
  "Spawn an agent subprocess and return a state struct.
   COMMAND-ARGS is a list of strings for the command, e.g. (\"hactar\" \"--acp\")."
  (let* ((process (uiop:launch-program command-args
                    :input :stream
                    :output :stream
                    :error-output :stream))
         (state (make-agentshell-state
                 :process process
                 :stdin (uiop:process-info-input process)
                 :stdout (uiop:process-info-output process)
                 :stderr (uiop:process-info-error-output process))))
    state))

(defun agentshell-stop (state)
  "Stop the agent subprocess and clean up."
  (setf (agentshell-state-running state) nil)
  (when (and (agentshell-state-reader-thread state)
             (bt:thread-alive-p (agentshell-state-reader-thread state)))
    (ignore-errors (bt:destroy-thread (agentshell-state-reader-thread state))))
  (when (agentshell-state-process state)
    (ignore-errors (close (agentshell-state-stdin state)))
    (ignore-errors (uiop:terminate-process (agentshell-state-process state)))
    (ignore-errors (uiop:wait-process (agentshell-state-process state))))
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (and (agentshell-terminal-thread v)
                        (bt:thread-alive-p (agentshell-terminal-thread v)))
               (ignore-errors (bt:destroy-thread (agentshell-terminal-thread v)))))
           *agentshell-terminals*)
  (clrhash *agentshell-terminals*))

;;** TUI Rendering

(defun agentshell-render (state)
  "Render the agentshell TUI."
  (charms:clear-window charms:*standard-window* :force-repaint t)
  (multiple-value-bind (win-width win-height)
      (charms:window-dimensions charms:*standard-window*)
    (let* ((status-row 0)
           (separator-row 1)
           (output-start-row 2)
           (input-row (1- win-height))
           (input-prompt-row (- win-height 2))
           (output-height (max 1 (- input-prompt-row output-start-row)))
           (output-lines (agentshell-state-output-lines state))
           (total-lines (length output-lines))
           (scroll (agentshell-state-scroll-offset state))
           (visible-start (max 0 (- total-lines output-height scroll)))
           (visible-end (max 0 (- total-lines scroll))))

      (charms/ll:attron charms/ll:a_reverse)
      (let ((status (format nil " ~A | ~A | ~A ~A"
                            (or (let ((info (agentshell-state-agent-info state)))
                                  (cdr (assoc "name" info :test #'string=)))
                                "agentshell")
                            (agentshell-state-current-model state)
                            (agentshell-state-current-mode state)
                            (if (agentshell-state-waiting state) "⏳" ""))))
        (let ((padded (str:shorten (1- win-width) status :ellipsis "")))
          (charms:write-string-at-point charms:*standard-window* padded 0 status-row)
          ;; Pad to fill width
          (loop for col from (length padded) below (1- win-width)
                do (charms:write-char-at-point charms:*standard-window* #\Space col status-row))))
      (charms/ll:attroff charms/ll:a_reverse)

      (loop for col from 0 below (1- win-width)
            do (charms:write-char-at-point charms:*standard-window* #\─ col separator-row))

      (loop for row from 0 below output-height
            for line-idx = (+ visible-start row)
            when (and (>= line-idx 0) (< line-idx visible-end) (< line-idx total-lines))
            do (let ((line (aref output-lines line-idx))
                     (screen-row (+ output-start-row row)))
                 (charms:write-string-at-point charms:*standard-window*
                   (str:shorten (1- win-width) (or line "") :ellipsis "…")
                   0 screen-row)))

      (loop for col from 0 below (1- win-width)
            do (charms:write-char-at-point charms:*standard-window* #\─ col input-prompt-row))

      (let* ((prompt (if (agentshell-state-pending-permission state) "choice> " "› "))
             (display-input (concatenate 'string prompt (agentshell-state-input-buffer state)))
             (truncated (str:shorten (- win-width 2) display-input :ellipsis "")))
        (charms:write-string-at-point charms:*standard-window* truncated 0 input-row))

      (let ((cursor-col (+ (if (agentshell-state-pending-permission state) 8 2)
                           (agentshell-state-input-cursor state))))
        (charms/ll:move input-row (min cursor-col (- win-width 2))))))

  (charms:refresh-window charms:*standard-window*))

;;** Local Command Handling

(defun agentshell-handle-local-command (state input)
  "Handle local /commands in agentshell. Returns T if handled."
  (let ((trimmed (string-trim '(#\Space #\Tab) input)))
    (cond
      ((or (string= trimmed "/quit") (string= trimmed "/exit"))
       (setf (agentshell-state-running state) nil)
       t)

      ((string= trimmed "/clear")
       (setf (fill-pointer (agentshell-state-output-lines state)) 0)
       (setf (agentshell-state-scroll-offset state) 0)
       t)

      ((string= trimmed "/cancel")
       (agentshell-send-cancel state)
       (agentshell-append-output state "[Cancelled]")
       t)

      ((str:starts-with? "/model " trimmed)
       (let ((model-name (string-trim '(#\Space) (subseq trimmed 7))))
         (agentshell-call state "session/set_config_option"
           `(("sessionId" . ,(agentshell-state-session-id state))
             ("configId" . "model")
             ("value" . ,model-name)))
         (agentshell-append-output state (format nil "Model → ~A" model-name)))
       t)

      ((str:starts-with? "/mode " trimmed)
       (let ((mode-name (string-trim '(#\Space) (subseq trimmed 6))))
         (agentshell-call state "session/set_config_option"
           `(("sessionId" . ,(agentshell-state-session-id state))
             ("configId" . "mode")
             ("value" . ,mode-name)))
         (agentshell-append-output state (format nil "Mode → ~A" mode-name)))
       t)

      ((string= trimmed "/help")
       (agentshell-append-output state "AgentShell Commands:")
       (agentshell-append-output state "  /quit, /exit  - Exit agentshell")
       (agentshell-append-output state "  /clear        - Clear output")
       (agentshell-append-output state "  /cancel       - Cancel current request")
       (agentshell-append-output state "  /model <name> - Switch model")
       (agentshell-append-output state "  /mode <mode>  - Switch mode (ask/code/architect)")
       (agentshell-append-output state "  /help         - Show this help")
       (agentshell-append-output state "  PageUp/Down   - Scroll output")
       t)

      (t nil))))

;;** Input Handling

(defun agentshell-input-insert-char (state c)
  "Insert a character at the cursor position."
  (let* ((buf (agentshell-state-input-buffer state))
         (pos (agentshell-state-input-cursor state)))
    (setf (agentshell-state-input-buffer state)
          (concatenate 'string (subseq buf 0 pos) (string c) (subseq buf pos)))
    (incf (agentshell-state-input-cursor state))))

(defun agentshell-input-backspace (state)
  "Delete character before cursor."
  (let* ((buf (agentshell-state-input-buffer state))
         (pos (agentshell-state-input-cursor state)))
    (when (> pos 0)
      (setf (agentshell-state-input-buffer state)
            (concatenate 'string (subseq buf 0 (1- pos)) (subseq buf pos)))
      (decf (agentshell-state-input-cursor state)))))

(defun agentshell-input-delete (state)
  "Delete character at cursor."
  (let* ((buf (agentshell-state-input-buffer state))
         (pos (agentshell-state-input-cursor state)))
    (when (< pos (length buf))
      (setf (agentshell-state-input-buffer state)
            (concatenate 'string (subseq buf 0 pos) (subseq buf (1+ pos)))))))

(defun agentshell-input-submit (state)
  "Submit the current input buffer."
  (let ((input (agentshell-state-input-buffer state)))
    (setf (agentshell-state-input-buffer state) "")
    (setf (agentshell-state-input-cursor state) 0)
    (setf (agentshell-state-scroll-offset state) 0)
    ;; Add to history
    (when (and input (> (length input) 0))
      (vector-push-extend input (agentshell-state-history state)))
    (setf (agentshell-state-history-index state) -1)
    input))

(defun agentshell-history-prev (state)
  "Navigate to previous history entry."
  (let* ((hist (agentshell-state-history state))
         (idx (agentshell-state-history-index state))
         (new-idx (if (= idx -1)
                      (1- (length hist))
                      (max 0 (1- idx)))))
    (when (and (> (length hist) 0) (>= new-idx 0))
      (setf (agentshell-state-history-index state) new-idx)
      (setf (agentshell-state-input-buffer state) (aref hist new-idx))
      (setf (agentshell-state-input-cursor state)
            (length (agentshell-state-input-buffer state))))))

(defun agentshell-history-next (state)
  "Navigate to next history entry."
  (let* ((hist (agentshell-state-history state))
         (idx (agentshell-state-history-index state)))
    (cond
      ((= idx -1) nil)
      ((>= (1+ idx) (length hist))
       (setf (agentshell-state-history-index state) -1)
       (setf (agentshell-state-input-buffer state) "")
       (setf (agentshell-state-input-cursor state) 0))
      (t
       (incf (agentshell-state-history-index state))
       (setf (agentshell-state-input-buffer state)
             (aref hist (agentshell-state-history-index state)))
       (setf (agentshell-state-input-cursor state)
             (length (agentshell-state-input-buffer state)))))))

;;** Main TUI Loop

(defun agentshell-tui-loop (state)
  "Main TUI event loop for agentshell."
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-extra-keys charms:*standard-window*)
    (charms/ll:curs-set 1)
    (charms/ll:timeout 50) ; 50ms polling for non-blocking input

    (agentshell-append-output state "AgentShell - ACP Client")
    (agentshell-append-output state "Type /help for commands, /quit to exit")
    (agentshell-append-output state "")

    (agentshell-append-output state "Connecting to agent...")
    (agentshell-render state)

    (unless (agentshell-initialize state)
      (agentshell-append-output state "[Failed to initialize agent]")
      (agentshell-render state)
      (sleep 2)
      (return-from agentshell-tui-loop))

    (unless (agentshell-new-session state)
      (agentshell-append-output state "[Failed to create session]")
      (agentshell-render state)
      (sleep 2)
      (return-from agentshell-tui-loop))

    (agentshell-append-output state "")

    (loop while (agentshell-state-running state)
          do (progn
               (agentshell-process-queued-messages state)

               (agentshell-render state)

               (let ((c (charms:get-char charms:*standard-window* :ignore-error t)))
                 (when c
                   (cond
                     ((eql c (code-char 3))
                      (if (agentshell-state-waiting state)
                          (agentshell-send-cancel state)
                          (setf (agentshell-state-running state) nil)))

                     ((and (eql c (code-char 4))
                           (string= (agentshell-state-input-buffer state) ""))
                      (setf (agentshell-state-running state) nil))

                     ((or (eql c #\Return) (eql c #\Newline))
                      (let ((input (agentshell-input-submit state)))
                        (when (and input (> (length input) 0))
                          ;; Check for pending permission
                          (if (agentshell-state-pending-permission state)
                              (let ((n (ignore-errors
                                         (parse-integer (string-trim '(#\Space) input)))))
                                (if n
                                    (agentshell-resolve-permission state n)
                                    (agentshell-append-output state "Enter a number.")))
                              (progn
                                (agentshell-append-output state (format nil "› ~A" input))
                                (if (agentshell-handle-local-command state input)
                                    nil
                                    (bt:make-thread
                                     (lambda ()
                                       (agentshell-send-prompt state input))
                                     :name "agentshell-prompt")))))))

                     ((or (eql c #\Backspace) (eql c #\Rubout)
                          (eql c (code-char 127))
                          (eql c (code-char charms/ll:key_backspace)))
                      (agentshell-input-backspace state))

                     ((eql c (code-char charms/ll:key_dc))
                      (agentshell-input-delete state))

                     ((eql c (code-char charms/ll:key_left))
                      (when (> (agentshell-state-input-cursor state) 0)
                        (decf (agentshell-state-input-cursor state))))

                     ((eql c (code-char charms/ll:key_right))
                      (when (< (agentshell-state-input-cursor state)
                               (length (agentshell-state-input-buffer state)))
                        (incf (agentshell-state-input-cursor state))))

                     ((eql c (code-char charms/ll:key_up))
                      (agentshell-history-prev state))

                     ((eql c (code-char charms/ll:key_down))
                      (agentshell-history-next state))

                     ((eql c (code-char charms/ll:key_ppage))
                      (incf (agentshell-state-scroll-offset state) 10)
                      (let ((max-scroll (max 0 (- (length (agentshell-state-output-lines state))
                                                  5))))
                        (setf (agentshell-state-scroll-offset state)
                              (min (agentshell-state-scroll-offset state) max-scroll))))

                     ((eql c (code-char charms/ll:key_npage))
                      (decf (agentshell-state-scroll-offset state) 10)
                      (setf (agentshell-state-scroll-offset state)
                            (max 0 (agentshell-state-scroll-offset state))))

                     ((eql c (code-char charms/ll:key_home))
                      (setf (agentshell-state-input-cursor state) 0))

                     ((eql c (code-char charms/ll:key_end))
                      (setf (agentshell-state-input-cursor state)
                            (length (agentshell-state-input-buffer state))))

                     ((eql c (code-char 1))
                      (setf (agentshell-state-input-cursor state) 0))

                     ((eql c (code-char 5))
                      (setf (agentshell-state-input-cursor state)
                            (length (agentshell-state-input-buffer state))))

                     ((eql c (code-char 11))
                      (setf (agentshell-state-input-buffer state)
                            (subseq (agentshell-state-input-buffer state)
                                    0 (agentshell-state-input-cursor state))))

                     ((eql c (code-char 21))
                      (setf (agentshell-state-input-buffer state) "")
                      (setf (agentshell-state-input-cursor state) 0))

                     ((eql c (code-char 12))
                      (charms:clear-window charms:*standard-window* :force-repaint t))

                     ((and c (graphic-char-p c))
                      (agentshell-input-insert-char state c)))))))))

;;** Entry Points

(defun start-agentshell (&key (agent-command nil))
  "Start agentshell, connecting to an ACP agent.
   AGENT-COMMAND is a list of strings for the agent subprocess command.
   Defaults to launching hactar itself with --acp."
  (let* ((cmd (or agent-command
                  (list (namestring (truename (uiop:argv0)))
                        "--acp"
                        "--path" (namestring (or *repo-root* (uiop:getcwd))))))
         (state (agentshell-spawn-agent cmd)))
    (setf *agentshell* state)
    ;; Start background reader
    (agentshell-start-reader state)
    ;; Run TUI
    (unwind-protect
         (agentshell-tui-loop state)
      (agentshell-stop state)
      (setf *agentshell* nil))))

(define-command agentshell (args)
  "Start the AgentShell ACP client TUI."
  (declare (ignore args))
  (start-agentshell)
  :slash t :sub nil)

(define-sub-command agentshell (args)
  "Start the AgentShell ACP client TUI."
  (declare (ignore args))
  (start-agentshell)
  (uiop:quit 0))
