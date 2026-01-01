;;* Agent Client Protocol (ACP) — JSON-RPC 2.0 over stdio
(in-package :hactar)

;;* Capability registry
(defvar *acp-agent-capabilities* (make-hash-table :test 'equal)
  "Registry of agent capabilities. Keys are dotted path strings, values are capability definitions.")

(defstruct acp-capability
  "Structure representing an agent capability."
  (path nil :type list)           ; e.g. ("fs" "readTextFile")
  (description "" :type string)
  (handler nil :type (or null function))  ; Function implementing the capability
  (enabled t :type boolean)
  (advertise t :type boolean)    ; If NIL, not included in capabilities tree (sub-method)
  (permissions :confirm :type keyword)  ; :confirm, :auto, or :deny — fed into resolve-permission
  (tool-name nil :type (or null string))) ; Tool name for permission system lookups

(defmacro defcapability (name (&rest path) &body body)
  "Define an ACP agent capability.

NAME: Symbol naming the capability (used for the handler function).
PATH: List of strings forming the capability path, e.g. (\"fs\" \"readTextFile\").
BODY: Can start with keyword options:
      :description \"...\" - Capability description
      :enabled t/nil - Whether enabled by default (default t)
      :permissions :confirm/:auto/:deny - Permission level (default :confirm)
      Then the handler implementation. The handler receives (ID PARAMS) arguments.

Example:
  (defcapability fs-read-text-file (\"fs\" \"readTextFile\")
    :description \"Read a text file from the agent's filesystem\"
    :permissions :auto
    (let ((path (cdr (assoc \"path\" params :test #'string=))))
      (uiop:read-file-string path)))"
  (let* ((fn-name (intern (format nil "ACP-CAPABILITY-~A" (symbol-name name)) :hactar))
         (cap-key (format nil "~{~A~^.~}" path))
         (tool-name-str (string-downcase (substitute #\_ #\- (symbol-name name))))
         (description nil)
         (enabled t)
         (permissions :confirm)
         (impl-body body))
    ;; Extract keyword options
    (let ((advertise t))
      (loop while (and impl-body (keywordp (car impl-body)))
            do (case (car impl-body)
                 (:description (setf description (cadr impl-body)))
                 (:enabled (setf enabled (cadr impl-body)))
                 (:advertise (setf advertise (cadr impl-body)))
                 (:permissions (setf permissions (cadr impl-body))))
               (setf impl-body (cddr impl-body)))
    `(progn
       (defun ,fn-name (id params)
         "ACP capability handler. ID is the request id, PARAMS is the request params alist."
         (declare (ignorable id params))
         ,@impl-body)
       (setf (gethash ,cap-key *acp-agent-capabilities*)
             (make-acp-capability
              :path ',path
              :description ,(or description "")
              :handler #',fn-name
              :enabled ,enabled
              :advertise ,advertise
              :permissions ,permissions
              :tool-name ,tool-name-str))
       ',name))))

(defun acp-agent-capabilities-tree ()
  "Build a nested alist tree from registered agent capabilities for the initialize response.
   Returns an alist like ((\"fs\" . ((\"readTextFile\" . t) (\"writeTextFile\" . t))) (\"terminal\" . t))."
  (let ((tree nil))
    (maphash (lambda (key cap)
               (declare (ignore key))
               (when (and (acp-capability-enabled cap)
                          (acp-capability-advertise cap))
                 (let ((path (acp-capability-path cap)))
                   (if (= 1 (length path))
                       ;; Top-level capability like ("terminal")
                       (push (cons (first path) t) tree)
                       ;; Nested capability like ("fs" "readTextFile")
                       (let* ((group (first path))
                              (leaf (second path))
                              (existing (assoc group tree :test #'string=)))
                         (if existing
                             ;; Add to existing group
                             (push (cons leaf t) (cdr existing))
                             ;; Create new group
                             (push (cons group (list (cons leaf t))) tree)))))))
             *acp-agent-capabilities*)
    (nreverse tree)))

(defun acp-check-capability-permission (cap id params)
  "Check permissions for an ACP capability before executing it.
   Returns :allow, :deny, or :confirm resolved to :allow/:deny.
   Sends appropriate error responses for :deny.
   Capabilities with :permissions :auto are auto-allowed unless explicitly denied by a rule."
  (let* ((tool-name (or (acp-capability-tool-name cap)
                        (format nil "~{~A~^.~}" (acp-capability-path cap))))
         ;; Convert params alist to plist for the permissions system
         (args-plist (when params
                       (loop for (key . val) in params
                             append (list (intern (string-upcase key) :keyword) val))))
         (decision (resolve-permission tool-name args-plist)))
    ;; For :auto capabilities, treat :confirm as :allow (only explicit :deny blocks)
    (when (and (eq (acp-capability-permissions cap) :auto)
               (eq decision :confirm))
      (return-from acp-check-capability-permission :allow))
    (case decision
      (:deny
       (acp-write-message
        (acp-make-error-response id +jsonrpc-invalid-params+
                                 (format nil "Permission denied for capability ~A" tool-name)))
       :deny)
      (:confirm
       (let ((tool-call-id (format nil "call_cap_~A" (uuid:make-v4-uuid))))
         ;; In ACP mode, request permission from the Client
         (if *acp-mode*
             (let ((client-decision (acp-request-permission tool-name args-plist tool-call-id)))
               (when (eq client-decision :deny)
                 (acp-write-message
                  (acp-make-error-response id +jsonrpc-invalid-params+
                                           (format nil "Permission denied by user for capability ~A" tool-name))))
               client-decision)
             ;; Outside ACP mode (unlikely but safe), prompt interactively
             (let ((user-decision (prompt-tool-permission tool-name args-plist)))
               (when (eq user-decision :deny)
                 (acp-write-message
                  (acp-make-error-response id +jsonrpc-invalid-params+
                                           (format nil "Permission denied by user for capability ~A" tool-name))))
               user-decision))))
      (:allow :allow)
      (otherwise :allow))))

(defun acp-find-capability-handler (method)
  "Find a capability handler for the given JSON-RPC method string.
   METHOD is like \"fs/read_text_file\" or \"terminal/create\".
   Maps method names to capability keys."
  (let* ((camel-method
           (cl-ppcre:regex-replace-all "_([a-z])"
             method
             (lambda (target-string start end match-start match-end reg-starts reg-ends)
               (declare (ignore start end match-start match-end))
               (string-upcase (subseq target-string (aref reg-starts 0) (aref reg-ends 0))))))
         (cap-key (cl-ppcre:regex-replace-all "/" camel-method "."))
         ;; Also try direct dotted path conversion: fs/read_text_file -> fs.readTextFile
         (parts (cl-ppcre:split "/" method))
         (dotted-keys '()))
    ;; Build possible capability keys from the method name
    ;; e.g. "fs/read_text_file" -> try "fs.readTextFile", "fs.read_text_file"
    (when (= (length parts) 2)
      (let ((group (first parts))
            (action (second parts)))
        ;; Try camelCase conversion: read_text_file -> readTextFile
        (let ((camel (with-output-to-string (s)
                       (let ((upnext nil))
                         (loop for c across action
                               do (cond
                                    ((char= c #\_) (setf upnext t))
                                    (upnext
                                     (write-char (char-upcase c) s)
                                     (setf upnext nil))
                                    (t (write-char c s))))))))
          (push (format nil "~A.~A" group camel) dotted-keys))
        ;; Try direct
        (push (format nil "~A.~A" group action) dotted-keys)))
    ;; Also try the computed cap-key
    (push cap-key dotted-keys)
    ;; Find first matching capability
    (dolist (k dotted-keys)
      (let ((cap (gethash k *acp-agent-capabilities*)))
        (when (and cap (acp-capability-enabled cap))
          (return-from acp-find-capability-handler (acp-capability-handler cap)))))
    nil))

(defun acp-find-capability-for-method (method)
  "Find the full capability struct for the given JSON-RPC method string.
   Returns the acp-capability struct or NIL."
  (let* ((camel-method
           (cl-ppcre:regex-replace-all "_([a-z])"
             method
             (lambda (target-string start end match-start match-end reg-starts reg-ends)
               (declare (ignore start end match-start match-end))
               (string-upcase (subseq target-string (aref reg-starts 0) (aref reg-ends 0))))))
         (cap-key (cl-ppcre:regex-replace-all "/" camel-method "."))
         (parts (cl-ppcre:split "/" method))
         (dotted-keys '()))
    (when (= (length parts) 2)
      (let ((group (first parts))
            (action (second parts)))
        (let ((camel (with-output-to-string (s)
                       (let ((upnext nil))
                         (loop for c across action
                               do (cond
                                    ((char= c #\_) (setf upnext t))
                                    (upnext
                                     (write-char (char-upcase c) s)
                                     (setf upnext nil))
                                    (t (write-char c s))))))))
          (push (format nil "~A.~A" group camel) dotted-keys))
        (push (format nil "~A.~A" group action) dotted-keys)))
    (push cap-key dotted-keys)
    (dolist (k dotted-keys)
      (let ((cap (gethash k *acp-agent-capabilities*)))
        (when (and cap (acp-capability-enabled cap))
          (return-from acp-find-capability-for-method cap))))
    nil))

;;* Error codes
(defparameter +jsonrpc-parse-error+ -32700)
(defparameter +jsonrpc-invalid-request+ -32600)
(defparameter +jsonrpc-method-not-found+ -32601)
(defparameter +jsonrpc-invalid-params+ -32602)
(defparameter +jsonrpc-internal-error+ -32603)

;;* Transport
(defun acp-write-message (message)
  "Serialize MESSAGE as JSON and write to stdout as a single newline-delimited line.
   MESSAGE is an alist representing a JSON-RPC 2.0 object."
  (let* ((shasht:*write-alist-as-object* t)
         (shasht:*write-plist-as-object* nil)
         (shasht:*symbol-name-function* (lambda (sym)
                                          (let ((name (symbol-name sym)))
                                            (cond
                                              ;; Preserve exact casing for known camelCase keys
                                              ((string= name "JSONRPC") "jsonrpc")
                                              ((string= name "SESSION-ID") "sessionId")
                                              ((string= name "SESSION-UPDATE") "sessionUpdate")
                                              ((string= name "TOOL-CALL-ID") "toolCallId")
                                              ((string= name "STOP-REASON") "stopReason")
                                              ((string= name "PROTOCOL-VERSION") "protocolVersion")
                                              ((string= name "CLIENT-CAPABILITIES") "clientCapabilities")
                                              ((string= name "CLIENT-INFO") "clientInfo")
                                              ((string= name "AGENT-CAPABILITIES") "agentCapabilities")
                                              ((string= name "AGENT-INFO") "agentInfo")
                                              ((string= name "AUTH-METHODS") "authMethods")
                                              ((string= name "LOAD-SESSION") "loadSession")
                                              ((string= name "PROMPT-CAPABILITIES") "promptCapabilities")
                                              ((string= name "EMBEDDED-CONTEXT") "embeddedContext")
                                              ((string= name "CONFIG-OPTIONS") "configOptions")
                                              ((string= name "AVAILABLE-COMMANDS") "availableCommands")
                                              ((string= name "CURRENT-VALUE") "currentValue")
                                              ((string= name "CONFIG-ID") "configId")
                                              ((string= name "CURRENT-MODE-ID") "currentModeId")
                                              ((string= name "AVAILABLE-MODES") "availableModes")
                                              ((string= name "MODE-ID") "modeId")
                                              ((string= name "OPTION-ID") "optionId")
                                              ((string= name "RAW-INPUT") "rawInput")
                                              ((string= name "RAW-OUTPUT") "rawOutput")
                                              ((string= name "TOOL-CALL") "toolCall")
                                              ((string= name "MIME-TYPE") "mimeType")
                                              ((string= name "EXIT-CODE") "exitCode")
                                              ((string= name "EXIT-STATUS") "exitStatus")
                                              ((string= name "TERMINAL-ID") "terminalId")
                                              ((string= name "OUTPUT-BYTE-LIMIT") "outputByteLimit")
                                              ((string= name "WAIT-FOR-EXIT") "waitForExit")
                                              ((string= name "READ-TEXT-FILE") "readTextFile")
                                              ((string= name "WRITE-TEXT-FILE") "writeTextFile")
                                              ((string= name "EXIT-STATUS") "exitStatus")
                                              ((string= name "MCP-SERVERS") "mcpServers")
                                              (t (string-downcase name))))))
         (json-string (shasht:write-json message nil)))
    ;; Ensure no embedded newlines
    (let ((clean (cl-ppcre:regex-replace-all "\\n" json-string "")))
      (write-line clean *standard-output*)
      (force-output *standard-output*))))

(defun acp-read-message ()
  "Read a single newline-delimited JSON-RPC message from stdin.
   Returns the parsed alist or NIL on EOF."
  (let ((line (read-line *standard-input* nil nil)))
    (when (and line (> (length (string-trim '(#\Space #\Tab #\Return) line)) 0))
      (handler-case
          (let ((shasht:*read-default-object-format* :alist))
            (shasht:read-json line))
        (error (e)
          (acp-log "JSON parse error: ~A" e)
          ;; Return a parse error indicator
          :parse-error)))))

(defun acp-log (fmt &rest args)
  "Log a message to stderr for debugging in ACP mode."
  (let ((msg (apply #'format nil fmt args)))
    (format *error-output* "~A~%" msg)
    (force-output *error-output*)))

;;* Message construction
(defun acp-make-response (id result)
  "Create a JSON-RPC 2.0 success response."
  `(("jsonrpc" . "2.0")
    ("id" . ,id)
    ("result" . ,result)))

(defun acp-make-error-response (id code message &optional data)
  "Create a JSON-RPC 2.0 error response."
  (let ((error-obj `(("code" . ,code)
                     ("message" . ,message))))
    (when data
      (push (cons "data" data) (cdr (last error-obj))))
    `(("jsonrpc" . "2.0")
      ("id" . ,id)
      ("error" . ,error-obj))))

(defun acp-make-notification (method params)
  "Create a JSON-RPC 2.0 notification (no id)."
  `(("jsonrpc" . "2.0")
    ("method" . ,method)
    ("params" . ,params)))

(defun acp-make-request (method params)
  "Create a JSON-RPC 2.0 request to send to the Client.
   Returns the request message and allocates a pending request ID."
  (let* ((id (incf *acp-request-counter*))
         (id-str (format nil "agent_~A" id))
         (request `(("jsonrpc" . "2.0")
                    ("id" . ,id-str)
                    ("method" . ,method)
                    ("params" . ,params))))
    (values request id-str)))

;;* Outbound client requests
(defun acp-call-client (method params &key (timeout 60))
  "Send a JSON-RPC request to the Client and wait for the response.
   Returns the result or signals an error."
  (multiple-value-bind (request id-str) (acp-make-request method params)
    (let ((result-box (list nil nil))) ;; (result-value received-p)
      (setf (gethash id-str *acp-pending-requests*) result-box)
      (acp-write-message request)
      (let ((start-time (get-universal-time)))
        (loop
          (when (second result-box)
            (remhash id-str *acp-pending-requests*)
            (return (first result-box)))
          (when (> (- (get-universal-time) start-time) timeout)
            (remhash id-str *acp-pending-requests*)
            (error "ACP client request timed out: ~A" method))
          (let ((msg (acp-read-message)))
            (cond
              ((null msg) 
               (remhash id-str *acp-pending-requests*)
               (error "ACP: stdin closed while waiting for response"))
              ((eq msg :parse-error)
               nil)
              (t (acp-dispatch-message msg)))))))))

;;* Session update notifications
(defun acp-send-session-update (update)
  "Send a session/update notification to the Client."
  (when *acp-session-id*
    (acp-write-message
     (acp-make-notification "session/update"
                            `(("sessionId" . ,*acp-session-id*)
                              ("update" . ,update))))))

(defun acp-send-agent-message-chunk (text)
  "Send an agent_message_chunk session update with text content."
  (acp-send-session-update
   `(("sessionUpdate" . "agent_message_chunk")
     ("content" . (("type" . "text")
                   ("text" . ,text))))))

(defun acp-send-user-message-chunk (text)
  "Send a user_message_chunk session update (for session/load replay)."
  (acp-send-session-update
   `(("sessionUpdate" . "user_message_chunk")
     ("content" . (("type" . "text")
                   ("text" . ,text))))))

(defun acp-send-thought-chunk (text)
  "Send a thought_message_chunk session update."
  (acp-send-session-update
   `(("sessionUpdate" . "thought_message_chunk")
     ("content" . (("type" . "text")
                   ("text" . ,text))))))

(defun acp-send-tool-call (tool-call-id title kind status &key content locations raw-input)
  "Send a tool_call session update."
  (let ((update `(("sessionUpdate" . "tool_call")
                  ("toolCallId" . ,tool-call-id)
                  ("title" . ,title)
                  ("kind" . ,kind)
                  ("status" . ,status))))
    (when content (push (cons "content" content) update))
    (when locations (push (cons "locations" locations) update))
    (when raw-input (push (cons "rawInput" raw-input) update))
    (acp-send-session-update update)))

(defun acp-send-tool-call-update (tool-call-id status &key content)
  "Send a tool_call_update session update."
  (let ((update `(("sessionUpdate" . "tool_call_update")
                  ("toolCallId" . ,tool-call-id)
                  ("status" . ,status))))
    (when content (push (cons "content" content) update))
    (acp-send-session-update update)))

(defun acp-send-available-commands ()
  "Send available_commands_update with only ACP-compatible slash commands."
  (let ((commands '()))
    (maphash (lambda (cmd-name handler)
               (declare (ignore handler))
               ;; Look up description from *commands*
               (let* ((cmd-info (gethash cmd-name *commands*))
                      (description (if cmd-info (second cmd-info) "ACP command")))
                 (push `(("name" . ,(string-trim "/" cmd-name))
                         ("description" . ,description))
                       commands)))
             *acp-commands*)
    (acp-send-session-update
     `(("sessionUpdate" . "available_commands_update")
       ("availableCommands" . ,(coerce (nreverse commands) 'vector))))))

(defun acp-send-config-options ()
  "Send config_options_update with model and mode selectors."
  (let* ((model-options
           (loop for model in *available-models*
                 collect `(("value" . ,(model-config-name model))
                           ("name" . ,(model-config-name model))
                           ("description" . ,(format nil "~A (~A)" 
                                                     (model-config-model-name model)
                                                     (model-config-provider model))))))
         (current-model-name (if *current-model* (model-config-name *current-model*) "none"))
         (config-options
           (list
            `(("id" . "mode")
              ("name" . "Session Mode")
              ("description" . "Controls how the agent operates")
              ("category" . "mode")
              ("type" . "select")
              ("currentValue" . "code")
              ("options" . ,(vector
                             `(("value" . "ask")
                               ("name" . "Ask")
                               ("description" . "Ask questions about the codebase"))
                             `(("value" . "code")
                               ("name" . "Code")
                               ("description" . "Make changes and refactors to code"))
                             `(("value" . "architect")
                               ("name" . "Architect")
                               ("description" . "Design and plan without implementation")))))
            `(("id" . "model")
              ("name" . "Model")
              ("description" . "LLM model to use")
              ("category" . "model")
              ("type" . "select")
              ("currentValue" . ,current-model-name)
              ("options" . ,(coerce model-options 'vector))))))
    (acp-send-session-update
     `(("sessionUpdate" . "config_options_update")
       ("configOptions" . ,(coerce config-options 'vector))))))

;;* Permission requests
(defun acp-request-permission (tool-name args tool-call-id)
  "Request permission from the Client for a tool call.
   Returns :allow or :deny."
  (let* ((params `(("sessionId" . ,*acp-session-id*)
                   ("toolCall" . (("toolCallId" . ,tool-call-id)
                                  ("title" . ,(format nil "Execute ~A" tool-name))
                                  ("kind" . "other")
                                  ("status" . "pending")
                                  ("rawInput" . ,args)))
                   ("options" . ,(vector
                                  `(("optionId" . "allow-once")
                                    ("name" . "Allow once")
                                    ("kind" . "allow_once"))
                                  `(("optionId" . "allow-always")
                                    ("name" . "Always allow this tool")
                                    ("kind" . "allow_always"))
                                  `(("optionId" . "reject-once")
                                    ("name" . "Reject")
                                    ("kind" . "reject_once"))
                                  `(("optionId" . "reject-always")
                                    ("name" . "Always reject this tool")
                                    ("kind" . "reject_always"))))))
         (result (acp-call-client "session/request_permission" params)))
    (let* ((outcome (cdr (assoc "outcome" result :test #'string=)))
           (outcome-type (cdr (assoc "outcome" outcome :test #'string=)))
           (option-id (cdr (assoc "optionId" outcome :test #'string=))))
      (cond
        ((string= outcome-type "cancelled") :deny)
        ((string= outcome-type "selected")
         (cond
           ((string= option-id "allow-once") :allow)
           ((string= option-id "allow-always")
            ;; Create session override
            (push (make-session-override
                   :tool-name tool-name
                   :match-type :tool-always
                   :decision :allow)
                  *session-overrides*)
            :allow)
           ((string= option-id "reject-once") :deny)
           ((string= option-id "reject-always")
            (push (make-session-override
                   :tool-name tool-name
                   :match-type :tool-deny
                   :decision :deny)
                  *session-overrides*)
            :deny)
           (t :deny)))
        (t :deny)))))

;;* Client capability methods
(defun acp-client-has-capability? (path)
  "Check if the Client advertised a capability at the given dotted path.
   PATH is a list of strings, e.g. (\"fs\" \"readTextFile\")."
  (let ((caps *acp-client-capabilities*))
    (dolist (key path)
      (setf caps (cdr (assoc key caps :test #'string=)))
      (unless caps (return-from acp-client-has-capability? nil)))
    (if (eq caps t) t
        (and caps (not (eq caps nil))))))

(defun acp-fs-read-text-file (path &key line limit)
  "Read a text file via the Client's fs/read_text_file method."
  (when (acp-client-has-capability? '("fs" "readTextFile"))
    (let ((params `(("sessionId" . ,*acp-session-id*)
                    ("path" . ,(acp-absolute-path path)))))
      (when line (push (cons "line" line) params))
      (when limit (push (cons "limit" limit) params))
      (let ((result (acp-call-client "fs/read_text_file" params)))
        (cdr (assoc "content" result :test #'string=))))))

(defun acp-fs-write-text-file (path content)
  "Write a text file via the Client's fs/write_text_file method."
  (when (acp-client-has-capability? '("fs" "writeTextFile"))
    (acp-call-client "fs/write_text_file"
                     `(("sessionId" . ,*acp-session-id*)
                       ("path" . ,(acp-absolute-path path))
                       ("content" . ,content)))
    t))

(defun acp-terminal-create (command &key args env cwd output-byte-limit)
  "Create a terminal via the Client's terminal/create method.
   Returns the terminal ID."
  (when (acp-client-has-capability? '("terminal"))
    (let ((params `(("sessionId" . ,*acp-session-id*)
                    ("command" . ,command))))
      (when args (push (cons "args" (coerce args 'vector)) params))
      (when env (push (cons "env" (coerce env 'vector)) params))
      (when cwd (push (cons "cwd" cwd) params))
      (when output-byte-limit (push (cons "outputByteLimit" output-byte-limit) params))
      (let ((result (acp-call-client "terminal/create" params)))
        (cdr (assoc "terminalId" result :test #'string=))))))

(defun acp-terminal-output (terminal-id)
  "Get terminal output via the Client's terminal/output method."
  (when (acp-client-has-capability? '("terminal"))
    (acp-call-client "terminal/output"
                     `(("sessionId" . ,*acp-session-id*)
                       ("terminalId" . ,terminal-id)))))

(defun acp-terminal-wait-for-exit (terminal-id)
  "Wait for terminal exit via the Client's terminal/wait_for_exit method."
  (when (acp-client-has-capability? '("terminal"))
    (acp-call-client "terminal/wait_for_exit"
                     `(("sessionId" . ,*acp-session-id*)
                       ("terminalId" . ,terminal-id)))))

(defun acp-terminal-kill (terminal-id)
  "Kill a terminal via the Client's terminal/kill method."
  (when (acp-client-has-capability? '("terminal"))
    (acp-call-client "terminal/kill"
                     `(("sessionId" . ,*acp-session-id*)
                       ("terminalId" . ,terminal-id)))))

(defun acp-terminal-release (terminal-id)
  "Release a terminal via the Client's terminal/release method."
  (when (acp-client-has-capability? '("terminal"))
    (acp-call-client "terminal/release"
                     `(("sessionId" . ,*acp-session-id*)
                       ("terminalId" . ,terminal-id)))))

;;* Agent capability definitions
;;** Filesystem
(defcapability fs-read-text-file ("fs" "readTextFile")
  :description "Read the contents of a text file from the agent's filesystem."
  :permissions :auto
  (let* ((path-val (cdr (assoc "path" params :test #'string=)))
         (line (cdr (assoc "line" params :test #'string=)))
         (limit (cdr (assoc "limit" params :test #'string=)))
         (full-path (if (uiop:absolute-pathname-p (pathname path-val))
                        path-val
                        (namestring (merge-pathnames path-val *repo-root*)))))
    (handler-case
        (if (probe-file full-path)
            (let* ((content (uiop:read-file-string full-path :external-format :utf-8))
                   (lines (when (or line limit) (str:lines content)))
                   (start-line (or line 0))
                   (end-line (if limit (+ start-line limit) (length lines)))
                   (selected (if lines
                                 (str:join (string #\Newline)
                                           (subseq lines
                                                   (min start-line (length lines))
                                                   (min end-line (length lines))))
                                 content)))
              (add-file-to-context (uiop:native-namestring full-path))
              (acp-write-message
               (acp-make-response id
                                  `(("content" . ,selected)))))
            (acp-write-message
             (acp-make-error-response id +jsonrpc-invalid-params+
                                      (format nil "File not found: ~A" path-val))))
      (error (e)
        (acp-write-message
         (acp-make-error-response id +jsonrpc-internal-error+
                                  (format nil "Error reading file: ~A" e)))))))

(defcapability fs-write-text-file ("fs" "writeTextFile")
  :description "Write content to a text file on the agent's filesystem."
  :permissions :confirm
  (let* ((path-val (cdr (assoc "path" params :test #'string=)))
         (content (cdr (assoc "content" params :test #'string=)))
         (full-path (if (uiop:absolute-pathname-p (pathname path-val))
                        path-val
                        (namestring (merge-pathnames path-val *repo-root*)))))
    (handler-case
        (progn
          (ensure-directories-exist (pathname full-path))
          (with-open-file (stream full-path
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
            (write-string content stream))
          (acp-write-message
           (acp-make-response id `(("success" . t)))))
      (error (e)
        (acp-write-message
         (acp-make-error-response id +jsonrpc-internal-error+
                                  (format nil "Error writing file: ~A" e)))))))

;;** Terminal
(defvar *acp-terminals* (make-hash-table :test 'equal)
  "Active ACP terminals. Maps terminal ID to terminal info alist.")

(defstruct acp-terminal-info
  "Tracks state of an agent-managed terminal."
  (id "" :type string)
  (process nil)                  ; uiop process-info
  (output "" :type string)       ; captured stdout
  (error-output "" :type string) ; captured stderr
  (exit-code nil)                ; nil while running
  (signal nil)                   ; signal name if killed
  (truncated nil :type boolean)
  (output-byte-limit nil)        ; max bytes to retain
  (released nil :type boolean))

(defun acp-terminal-generate-id ()
  "Generate a unique terminal ID."
  (format nil "term_~A" (uuid:make-v4-uuid)))

(defun acp-terminal-run-async (command args env cwd output-byte-limit)
  "Launch a command asynchronously and return a terminal-info struct.
   ENV is a list of alists with \"name\" and \"value\" keys."
  (let* ((term-id (acp-terminal-generate-id))
         (full-command (if args
                          (append (list command) (coerce args 'list))
                          (list command)))
         ;; Build environment as key=value strings for the subprocess
         (env-strings (when env
                        (loop for entry across (if (vectorp env) env (coerce env 'vector))
                              for name = (cdr (assoc "name" entry :test #'string=))
                              for value = (cdr (assoc "value" entry :test #'string=))
                              when (and name value)
                              collect (format nil "~A=~A" name value))))
         ;; Wrap command to include env vars and cwd
         (shell-cmd (format nil "~@[cd ~A && ~]~@[~{~A ~}~]~{~A~^ ~}"
                            cwd
                            (when env-strings
                              (mapcar (lambda (e) (format nil "~A" e)) env-strings))
                            full-command))
         (info (make-acp-terminal-info
                :id term-id
                :output-byte-limit output-byte-limit)))
    ;; Launch in a thread so we don't block the ACP loop
    (let ((thread
            (bt:make-thread
             (lambda ()
               (handler-case
                   (multiple-value-bind (stdout stderr exit-code)
                       (uiop:run-program (list *shell* "-c" shell-cmd)
                                         :output :string
                                         :error-output :string
                                         :ignore-error-status t)
                     ;; Truncate output if needed
                     (let ((combined (format nil "~A~@[~A~]" stdout
                                            (when (and stderr (> (length stderr) 0))
                                              (format nil "~%~A" stderr)))))
                       (when (and output-byte-limit
                                  (> (length combined) output-byte-limit))
                         (setf combined (subseq combined (- (length combined) output-byte-limit)))
                         (setf (acp-terminal-info-truncated info) t))
                       (setf (acp-terminal-info-output info) combined))
                     (setf (acp-terminal-info-exit-code info) exit-code))
                 (error (e)
                   (setf (acp-terminal-info-output info)
                         (format nil "Error executing command: ~A" e))
                   (setf (acp-terminal-info-exit-code info) 1))))
             :name (format nil "acp-terminal-~A" term-id))))
      (declare (ignore thread))
      (setf (gethash term-id *acp-terminals*) info)
      info)))

(defcapability terminal ("terminal")
  :description "Terminal command execution capability."
  :permissions :confirm
  ;; Top-level capability marker for the capabilities tree.
  ;; Individual terminal/* methods are handled by their own capabilities below.
  ;; This handler is unlikely to be called directly, but respond gracefully.
  (acp-write-message
   (acp-make-error-response id +jsonrpc-method-not-found+
                            "Use terminal/create, terminal/output, etc.")))

(defcapability terminal-create ("terminal" "create")
  :description "Create a new terminal and execute a command."
  :advertise nil
  :permissions :confirm
  (let* ((command (cdr (assoc "command" params :test #'string=)))
         (args (cdr (assoc "args" params :test #'string=)))
         (env (cdr (assoc "env" params :test #'string=)))
         (cwd (cdr (assoc "cwd" params :test #'string=)))
         (output-byte-limit (cdr (assoc "outputByteLimit" params :test #'string=))))
    (unless command
      (acp-write-message
       (acp-make-error-response id +jsonrpc-invalid-params+ "Missing 'command' parameter"))
      (return-from acp-capability-terminal-create))
    (let ((info (acp-terminal-run-async command args env cwd output-byte-limit)))
      (acp-write-message
       (acp-make-response id `(("terminalId" . ,(acp-terminal-info-id info))))))))

(defcapability terminal-output ("terminal" "output")
  :description "Get the current output of a terminal."
  :advertise nil
  :permissions :auto
  (let* ((terminal-id (cdr (assoc "terminalId" params :test #'string=)))
         (info (gethash terminal-id *acp-terminals*)))
    (unless info
      (acp-write-message
       (acp-make-error-response id +jsonrpc-invalid-params+
                                (format nil "Unknown terminal: ~A" terminal-id)))
      (return-from acp-capability-terminal-output))
    (let ((result `(("output" . ,(acp-terminal-info-output info))
                    ("truncated" . ,(if (acp-terminal-info-truncated info) t :false)))))
      (when (acp-terminal-info-exit-code info)
        (push (cons "exitStatus"
                    `(("exitCode" . ,(acp-terminal-info-exit-code info))
                      ("signal" . ,(or (acp-terminal-info-signal info) :null))))
              result))
      (acp-write-message (acp-make-response id result)))))

(defcapability terminal-wait-for-exit ("terminal" "waitForExit")
  :description "Wait for a terminal command to complete and return exit status."
  :advertise nil
  :permissions :auto
  (let* ((terminal-id (cdr (assoc "terminalId" params :test #'string=)))
         (info (gethash terminal-id *acp-terminals*)))
    (unless info
      (acp-write-message
       (acp-make-error-response id +jsonrpc-invalid-params+
                                (format nil "Unknown terminal: ~A" terminal-id)))
      (return-from acp-capability-terminal-wait-for-exit))
    ;; Poll until the process finishes
    (loop until (acp-terminal-info-exit-code info)
          do (sleep 0.1))
    (acp-write-message
     (acp-make-response id
                        `(("exitCode" . ,(acp-terminal-info-exit-code info))
                          ("signal" . ,(or (acp-terminal-info-signal info) :null)))))))

(defcapability terminal-kill ("terminal" "kill")
  :description "Kill a running terminal command without releasing the terminal."
  :advertise nil
  :permissions :confirm
  (let* ((terminal-id (cdr (assoc "terminalId" params :test #'string=)))
         (info (gethash terminal-id *acp-terminals*)))
    (unless info
      (acp-write-message
       (acp-make-error-response id +jsonrpc-invalid-params+
                                (format nil "Unknown terminal: ~A" terminal-id)))
      (return-from acp-capability-terminal-kill))
    ;; If the process is still running, try to kill it
    (when (and (acp-terminal-info-process info)
               (null (acp-terminal-info-exit-code info)))
      (handler-case
          (uiop:terminate-process (acp-terminal-info-process info))
        (error (e)
          (acp-log "Error killing terminal ~A: ~A" terminal-id e)))
      (setf (acp-terminal-info-signal info) "SIGTERM")
      (unless (acp-terminal-info-exit-code info)
        (setf (acp-terminal-info-exit-code info) -1)))
    (acp-write-message (acp-make-response id nil))))

(defcapability terminal-release ("terminal" "release")
  :description "Release a terminal, killing it if still running, and free all resources."
  :advertise nil
  :permissions :auto
  (let* ((terminal-id (cdr (assoc "terminalId" params :test #'string=)))
         (info (gethash terminal-id *acp-terminals*)))
    (unless info
      (acp-write-message
       (acp-make-error-response id +jsonrpc-invalid-params+
                                (format nil "Unknown terminal: ~A" terminal-id)))
      (return-from acp-capability-terminal-release))
    ;; Kill if still running
    (when (and (acp-terminal-info-process info)
               (null (acp-terminal-info-exit-code info)))
      (handler-case
          (uiop:terminate-process (acp-terminal-info-process info))
        (error () nil)))
    (setf (acp-terminal-info-released info) t)
    (remhash terminal-id *acp-terminals*)
    (acp-write-message (acp-make-response id nil))))

;;* Diff & resource link helpers
(defun acp-make-diff-content (path old-text new-text)
  "Create a diff content block for tool call content."
  `(("type" . "diff")
    ("path" . ,(acp-absolute-path path))
    ("oldText" . ,old-text)
    ("newText" . ,new-text)))

(defun acp-make-resource-link (file-path)
  "Create a resource_link content block for a file in context."
  (let* ((abs-path (acp-absolute-path file-path))
         (name (file-namestring (pathname file-path)))
         (ext (pathname-type (pathname file-path)))
         (mime (cond
                 ((member ext '("lisp" "cl" "el") :test #'string-equal) "text/x-lisp")
                 ((string-equal ext "py") "text/x-python")
                 ((string-equal ext "js") "text/javascript")
                 ((string-equal ext "ts") "text/typescript")
                 ((string-equal ext "rs") "text/x-rust")
                 ((string-equal ext "json") "application/json")
                 ((string-equal ext "yaml") "text/yaml")
                 ((string-equal ext "yml") "text/yaml")
                 ((string-equal ext "md") "text/markdown")
                 ((string-equal ext "org") "text/org")
                 ((string-equal ext "html") "text/html")
                 ((string-equal ext "css") "text/css")
                 ((string-equal ext "sh") "text/x-shellscript")
                 (t "text/plain"))))
    `(("type" . "resource_link")
      ("uri" . ,(format nil "file://~A" abs-path))
      ("name" . ,(or name abs-path))
      ("mimeType" . ,mime))))

(defun acp-send-plan (entries)
  "Send a plan session update. ENTRIES is a list of plists with :content, :priority, :status."
  (acp-send-session-update
   `(("sessionUpdate" . "plan")
     ("entries" . ,(coerce
                    (mapcar (lambda (e)
                              `(("content" . ,(getf e :content))
                                ("priority" . ,(or (getf e :priority) "medium"))
                                ("status" . ,(or (getf e :status) "pending"))))
                            entries)
                    'vector)))))

(defun acp-send-resource-links-for-context ()
  "Send resource_link content blocks for all files currently in context."
  (when (and *acp-session-id* *files*)
    (dolist (file *files*)
      (acp-send-agent-message-chunk "")  ; Ensure the chunk stream is active
      ;; Resource links are typically sent as part of tool call content or as 
      ;; embedded context in session updates. We include them as agent messages.
      )))

;;* Path utilities
(defun acp-absolute-path (path)
  "Ensure PATH is absolute. If relative, merge with *repo-root*."
  (let ((p (pathname path)))
    (if (uiop:absolute-pathname-p p)
        (namestring p)
        (namestring (merge-pathnames p *repo-root*)))))

;;* Content block parsing
(defun acp-parse-content-blocks (prompt-array)
  "Parse an array of ACP ContentBlock objects into a user message string.
   Handles text, resource, resource_link, and image types.
   Returns (values text-string images-list)."
  (let ((text-parts '())
        (images '()))
    (loop for block across prompt-array
          for block-type = (cdr (assoc "type" block :test #'string=))
          do (cond
               ((string= block-type "text")
                (push (cdr (assoc "text" block :test #'string=)) text-parts))
               
               ((string= block-type "resource")
                (let* ((resource (cdr (assoc "resource" block :test #'string=)))
                       (uri (cdr (assoc "uri" resource :test #'string=)))
                       (text (cdr (assoc "text" resource :test #'string=)))
                       (mime (cdr (assoc "mimeType" resource :test #'string=))))
                  (when text
                    (push (format nil "~%[Resource: ~A (~A)]~%~A" 
                                  (or uri "unknown") (or mime "") text)
                          text-parts))
                  (when (and uri (str:starts-with? "file://" uri))
                    (let ((file-path (subseq uri 7)))
                      (when (probe-file file-path)
                        (add-file-to-context (uiop:native-namestring file-path)))))))
               
               ((string= block-type "resource_link")
                (let* ((uri (cdr (assoc "uri" block :test #'string=)))
                       (name (cdr (assoc "name" block :test #'string=))))
                  (when (and uri (str:starts-with? "file://" uri))
                    (let ((file-path (subseq uri 7)))
                      (when (probe-file file-path)
                        (add-file-to-context (uiop:native-namestring file-path))
                        (push (format nil "[Added file: ~A]" (or name file-path)) text-parts))))))
               
               ((string= block-type "image")
                (let ((data (cdr (assoc "data" block :test #'string=)))
                      (mime (cdr (assoc "mimeType" block :test #'string=))))
                  (when data
                    (push `((:base64-data . ,data)
                            (:mime-type . ,mime)
                            (:text . "User provided image"))
                          images))))))
    (values (format nil "~{~A~^~%~}" (nreverse text-parts))
            (nreverse images))))

;;* Method handlers
(defun acp-handle-initialize (id params)
  "Handle the initialize method."
  (let ((protocol-version (cdr (assoc "protocolVersion" params :test #'string=)))
        (client-caps (cdr (assoc "clientCapabilities" params :test #'string=)))
        (client-info (cdr (assoc "clientInfo" params :test #'string=))))
    (declare (ignore client-info))
    (setf *acp-client-capabilities* client-caps)
    (setf *acp-initialized* t)
    
    (let ((response-version (if (and protocol-version (<= protocol-version 1)) 1 1))
          (caps-tree (acp-agent-capabilities-tree)))
      (acp-write-message
       (acp-make-response id
                          `(("protocolVersion" . ,response-version)
                            ("agentCapabilities" .
                             (("loadSession" . :false)
                              ("promptCapabilities" .
                               (("image" . t)
                                ("embeddedContext" . t)
                                ("audio" . :false)))
                              ,@caps-tree))
                            ("agentInfo" .
                             (("name" . "hactar")
                              ("title" . "Hactar AI Pair Programmer")
                              ("version" . ,*hactar-version*)))
                            ("authMethods" . ,(vector))))))))

(defun acp-handle-authenticate (id params)
  "Handle the authenticate method. No auth required."
  (declare (ignore params))
  (acp-write-message (acp-make-response id `(("authenticated" . t)))))

(defun acp-handle-session-new (id params)
  "Handle session/new to create a new session."
  (let ((cwd (cdr (assoc "cwd" params :test #'string=))))
    (when cwd
      (handler-case
          (setf *repo-root* (find-git-repo-root 
                             (uiop:ensure-directory-pathname 
                              (uiop:parse-native-namestring cwd))))
        (error ()
          (setf *repo-root* (uiop:ensure-directory-pathname 
                             (uiop:parse-native-namestring cwd))))))
    
    (setf *acp-session-id* (format nil "sess_~A" (uuid:make-v4-uuid)))
    (clear-chat-history)
    (setf *files* nil)
    (setf *images* nil)
    (setf *docs-context* nil)
    (setf *errors-context* nil)
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (and (acp-terminal-info-process v)
                          (null (acp-terminal-info-exit-code v)))
                 (ignore-errors (uiop:terminate-process (acp-terminal-info-process v)))))
             *acp-terminals*)
    (clrhash *acp-terminals*)
    
    (when *repo-root*
      (hactar-session-new (namestring *repo-root*)))
    
    (let* ((model-options
             (loop for model in *available-models*
                   collect `(("value" . ,(model-config-name model))
                             ("name" . ,(model-config-name model))
                             ("description" . ,(format nil "~A (~A)"
                                                       (model-config-model-name model)
                                                       (model-config-provider model))))))
           (current-model-name (if *current-model* (model-config-name *current-model*) "none"))
           (config-options
             (vector
              `(("id" . "mode")
                ("name" . "Session Mode")
                ("description" . "Controls how the agent operates")
                ("category" . "mode")
                ("type" . "select")
                ("currentValue" . "code")
                ("options" . ,(vector
                               `(("value" . "ask")
                                 ("name" . "Ask")
                                 ("description" . "Ask questions about the codebase"))
                               `(("value" . "code")
                                 ("name" . "Code")
                                 ("description" . "Make changes and refactors to code"))
                               `(("value" . "architect")
                                 ("name" . "Architect")
                                 ("description" . "Design and plan without implementation")))))
              `(("id" . "model")
                ("name" . "Model")
                ("description" . "LLM model to use")
                ("category" . "model")
                ("type" . "select")
                ("currentValue" . ,current-model-name)
                ("options" . ,(coerce model-options 'vector))))))
      
      (acp-write-message
       (acp-make-response id
                          `(("sessionId" . ,*acp-session-id*)
                            ("configOptions" . ,config-options)))))
    
    (acp-send-available-commands)
    (dolist (file *files*)
      (acp-send-session-update
       `(("sessionUpdate" . "agent_message_chunk")
         ("content" . ,(acp-make-resource-link file)))))))

(defun acp-handle-session-prompt (id params)
  "Handle session/prompt to process a user message."
  (acp-log "Params: ~A~%" params)
  (let* ((prompt-blocks (cdr (assoc "prompt" params :test #'string=)))
         (*acp-cancelled* nil))
    
    (unless prompt-blocks
      (acp-write-message 
       (acp-make-error-response id +jsonrpc-invalid-params+ "Missing prompt"))
      (return-from acp-handle-session-prompt))
    
    (multiple-value-bind (user-text images) 
        (acp-parse-content-blocks prompt-blocks)
      
      (when (string= (string-trim '(#\Space #\Tab #\Newline #\Return) user-text) "")
        (acp-write-message (acp-make-response id `(("stopReason" . "end_turn"))))
        (return-from acp-handle-session-prompt))
      
      (multiple-value-bind (cmd args) (parse-command user-text)
        (when cmd
          (if (acp-command-p cmd)
              (handler-case
                  (let ((result (execute-acp-command cmd args)))
                    (let ((text-val (cdr (assoc "text" result :test #'string=))))
                      (when (and text-val (> (length text-val) 0))
                        (acp-send-agent-message-chunk text-val)))
                    (let ((data-val (cdr (assoc "data" result :test #'string=))))
                      (when data-val
                        (acp-send-agent-message-chunk
                         (format nil "~A" (to-json data-val))))))
                (error (e)
                  (acp-send-agent-message-chunk
                   (format nil "Error executing command ~A: ~A" cmd e))))
              (let ((output (with-output-to-string (*standard-output*)
                              (execute-command cmd args))))
                (when (and output (> (length output) 0))
                  (acp-send-agent-message-chunk output))))
          (acp-write-message (acp-make-response id `(("stopReason" . "end_turn"))))
          (return-from acp-handle-session-prompt)))
      
      (handler-case
          (acp-process-prompt id user-text images)
        (hactar-interrupt ()
          (acp-write-message (acp-make-response id `(("stopReason" . "cancelled")))))
        (error (e)
          (acp-log "Error processing prompt: ~A" e)
          (acp-write-message 
           (acp-make-error-response id +jsonrpc-internal-error+ 
                                    (format nil "~A" e))))))))

(defun acp-process-prompt (request-id user-text images)
  "Process a prompt through the LLM, streaming chunks via session/update."
  (unless *current-model*
    (acp-send-agent-message-chunk "Error: No model selected.")
    (acp-write-message (acp-make-response request-id `(("stopReason" . "end_turn"))))
    (return-from acp-process-prompt))
  
  (let* ((messages-for-api (prepare-messages user-text))
         (provider-name (model-config-provider *current-model*))
         (provider-type (intern (string-upcase provider-name) :keyword))
         (sys-prompt (system-prompt))
         (full-response-text (make-string-output-stream))
         (reader-instance nil))
    
    (setf *waiting-for-llm* t)
    
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
                          :system-prompt sys-prompt
                          :stream t
                          (when images `(:images ,images)))
                 (declare (ignore initial-messages))
                 
                 (setf reader-instance response-result)
                 (setf *current-stream-reader* reader-instance)
                 
                 (unless (typep reader-instance 'llm:llm-stream-reader)
                   (error "LLM complete did not return a stream reader"))
                 
                 (loop for chunk = (llm:read-next-chunk reader-instance)
                       while (and chunk (not *acp-cancelled*))
                       do (progn
                            (acp-send-agent-message-chunk chunk)
                            (write-string chunk full-response-text)))
                 
                 (let* ((assistant-response (get-output-stream-string full-response-text))
                        (finish-reason nil))
                   (add-to-chat-history "user" user-text)
                   (add-to-chat-history "assistant" assistant-response)
                   
                   (when (and *tool-use-enabled* *tools-in-system-prompt*)
                     (let ((tool-calls (parse-xml-tool-calls assistant-response)))
                       (when tool-calls
                         (acp-execute-tool-calls tool-calls))))
                   
                   (let ((stop-reason
                           (cond
                             (*acp-cancelled* "cancelled")
                             ((and (stringp finish-reason)
                                   (or (string-equal finish-reason "length")
                                       (string-equal finish-reason "max_tokens")))
                              "max_tokens")
                             ((and (stringp finish-reason)
                                   (or (string-equal finish-reason "content_filter")
                                       (string-equal finish-reason "refusal")))
                              "refusal")
                             ((string= assistant-response "") "end_turn")
                             (t "end_turn"))))
                     (acp-write-message 
                      (acp-make-response request-id 
                                         `(("stopReason" . ,stop-reason))))))))
           
           (hactar-interrupt ()
             (when (and reader-instance 
                        (not (llm:llm-stream-reader-closed-p reader-instance)))
               (ignore-errors (llm:close-reader reader-instance)))
             (acp-write-message 
              (acp-make-response request-id `(("stopReason" . "cancelled")))))
           
           (error (e)
             (when (and reader-instance 
                        (not (llm:llm-stream-reader-closed-p reader-instance)))
               (ignore-errors (llm:close-reader reader-instance)))
             (acp-log "LLM error: ~A" e)
             (acp-write-message 
              (acp-make-error-response request-id +jsonrpc-internal-error+ 
                                       (format nil "~A" e)))))
      
      (setf *waiting-for-llm* nil)
      (setf *current-stream-reader* nil))))

(defun acp-execute-tool-calls (tool-calls)
  "Execute parsed XML tool calls with ACP permission handling and status reporting."
  (dolist (tc tool-calls)
    (when *acp-cancelled* (return))
    (let* ((tool-name (car tc))
           (args (cdr tc))
           (tool-call-id (format nil "call_~A" (uuid:make-v4-uuid)))
           (tool (gethash tool-name *defined-tools*))
           (kind (if tool
                     (cond
                       ((string= tool-name "read_file") "read")
                       ((string= tool-name "write_to_file") "edit")
                       ((string= tool-name "replace_in_file") "edit")
                       ((string= tool-name "execute_command") "execute")
                       (t "other"))
                     "other")))
      
      (let ((locations
              (when *repo-root*
                (let ((path-val (or (cdr (assoc :path args))
                                    (cdr (assoc "path" args :test #'string=)))))
                  (when path-val
                    (vector `(("path" . ,(acp-absolute-path path-val)))))))))
        
        (acp-send-tool-call tool-call-id
                            (format nil "~A" tool-name)
                            kind "pending"
                            :raw-input args
                            :locations locations)
      
      (let ((args-plist (loop for (key . val) in args
                              append (list key val))))
        
        (let ((decision (resolve-permission tool-name args-plist)))
          (case decision
            (:deny
             (acp-send-tool-call-update tool-call-id "failed"
                                        :content (vector `(("type" . "content")
                                                           ("content" . (("type" . "text")
                                                                         ("text" . "Permission denied by rule"))))))
             (return))
            (:confirm
             (let ((client-decision (acp-request-permission tool-name args-plist tool-call-id)))
               (when (eq client-decision :deny)
                 (acp-send-tool-call-update tool-call-id "failed"
                                            :content (vector `(("type" . "content")
                                                               ("content" . (("type" . "text")
                                                                             ("text" . "Permission denied by user"))))))
                 (return))))
            (:allow nil)))
        
        (let ((old-content nil)
              (file-path-for-diff nil))
          (when (or (string= tool-name "write_to_file") (string= tool-name "replace_in_file"))
            (let ((path-val (or (getf args-plist :path)
                                (getf args-plist "path"))))
              (when (and path-val *repo-root*)
                (setf file-path-for-diff path-val)
                (let ((full-path (merge-pathnames path-val *repo-root*)))
                  (when (probe-file full-path)
                    (setf old-content (ignore-errors (uiop:read-file-string full-path))))))))
          
          (acp-send-tool-call-update tool-call-id "in_progress")
          
          (handler-case
              (let ((result (funcall (tool-definition-function tool) args-plist)))
                ;; Build content blocks for the result
                (let ((content-blocks
                        (if (and file-path-for-diff
                                 (or (string= tool-name "write_to_file")
                                     (string= tool-name "replace_in_file")))
                            (let* ((new-content
                                     (let ((full-path (merge-pathnames file-path-for-diff *repo-root*)))
                                       (when (probe-file full-path)
                                         (ignore-errors (uiop:read-file-string full-path)))))
                                   (diff-block (when new-content
                                                 (acp-make-diff-content file-path-for-diff
                                                                        (or old-content "")
                                                                        new-content))))
                              (if diff-block
                                  (vector diff-block
                                          `(("type" . "content")
                                            ("content" . (("type" . "text")
                                                          ("text" . ,(or result ""))))))
                                  (vector `(("type" . "content")
                                            ("content" . (("type" . "text")
                                                          ("text" . ,(or result ""))))))))
                            (vector `(("type" . "content")
                                      ("content" . (("type" . "text")
                                                    ("text" . ,(or result "")))))))))
                  (acp-send-tool-call-update tool-call-id "completed" :content content-blocks))
                (when result
                  (acp-send-agent-message-chunk 
                   (format nil "~%Tool ~A result: ~A~%" tool-name result))))
            (error (e)
              (acp-send-tool-call-update 
               tool-call-id "failed"
               :content (vector `(("type" . "content")
                                  ("content" . (("type" . "text")
                                                ("text" . ,(format nil "Error: ~A" e)))))))))))))))

(defun acp-handle-session-cancel (params)
  "Handle session/cancel notification."
  (declare (ignore params))
  (setf *acp-cancelled* t)
  (when (and *current-stream-reader*
             (not (llm:llm-stream-reader-closed-p *current-stream-reader*)))
    (ignore-errors (llm:close-reader *current-stream-reader*))))

(defun acp-handle-set-config-option (id params)
  "Handle session/set_config_option."
  (let ((config-id (cdr (assoc "configId" params :test #'string=)))
        (value (cdr (assoc "value" params :test #'string=))))
    (cond
      ((string= config-id "model")
       (set-current-model value))
      ((string= config-id "mode")
       ;; Mode changes could affect system prompt, rules, etc.
       ;; For now just acknowledge
       (acp-log "Mode set to: ~A" value)))
    
    (let* ((model-options
             (loop for model in *available-models*
                   collect `(("value" . ,(model-config-name model))
                             ("name" . ,(model-config-name model))
                             ("description" . ,(format nil "~A (~A)"
                                                       (model-config-model-name model)
                                                       (model-config-provider model))))))
           (current-model-name (if *current-model* (model-config-name *current-model*) "none")))
      (acp-write-message
       (acp-make-response id
                          `(("configOptions" . 
                             ,(vector
                               `(("id" . "mode")
                                 ("name" . "Session Mode")
                                 ("description" . "Controls how the agent operates")
                                 ("category" . "mode")
                                 ("type" . "select")
                                 ("currentValue" . ,(or value "code"))
                                 ("options" . ,(vector
                                                `(("value" . "ask")
                                                  ("name" . "Ask")
                                                  ("description" . "Ask questions about the codebase"))
                                                `(("value" . "code")
                                                  ("name" . "Code")
                                                  ("description" . "Make changes and refactors to code"))
                                                `(("value" . "architect")
                                                  ("name" . "Architect")
                                                  ("description" . "Design and plan without implementation")))))
                               `(("id" . "model")
                                 ("name" . "Model")
                                 ("description" . "LLM model to use")
                                 ("category" . "model")
                                 ("type" . "select")
                                 ("currentValue" . ,current-model-name)
                                 ("options" . ,(coerce model-options 'vector)))))))))))

(defun acp-handle-set-mode (id params)
  "Handle session/set_mode (backward compat)."
  (let ((mode-id (cdr (assoc "modeId" params :test #'string=))))
    (acp-log "Mode set to: ~A" mode-id)
    (acp-write-message (acp-make-response id nil))))

;;* Dispatcher
(defun acp-dispatch-message (msg)
  (acp-log "Msg: ~A~%" msg)
  "Dispatch an incoming JSON-RPC message to the appropriate handler."
  (let* ((id (cdr (assoc "id" msg :test #'string=)))
         (method (cdr (assoc "method" msg :test #'string=)))
         (params (or (cdr (assoc "params" msg :test #'string=)) nil))
         (result (cdr (assoc "result" msg :test #'string=)))
         (error-obj (cdr (assoc "error" msg :test #'string=))))
    
    (cond
      ((and id (null method) (or result error-obj))
       (let ((id-key (if (stringp id) id (format nil "~A" id))))
         (let ((pending (gethash id-key *acp-pending-requests*)))
           (when pending
             (setf (first pending) (or result error-obj))
             (setf (second pending) t)))))
      
      ((and id method)
       (cond
         ((string= method "initialize")
          (acp-handle-initialize id params))
         ((string= method "authenticate")
          (acp-handle-authenticate id params))
         ((string= method "session/new")
          (acp-handle-session-new id params))
         ((string= method "session/prompt")
          (acp-handle-session-prompt id params))
         ((string= method "session/set_config_option")
          (acp-handle-set-config-option id params))
         ((string= method "session/set_mode")
          (acp-handle-set-mode id params))
         (t
          (let ((cap (acp-find-capability-for-method method)))
            (if cap
                (let ((decision (acp-check-capability-permission cap id params)))
                  (when (eq decision :allow)
                    (handler-case
                        (funcall (acp-capability-handler cap) id params)
                      (error (e)
                        (acp-write-message
                         (acp-make-error-response id +jsonrpc-internal-error+
                                                  (format nil "Capability error: ~A" e)))))))
                (acp-write-message
                 (acp-make-error-response id +jsonrpc-method-not-found+
                                          (format nil "Method not found: ~A" method))))))))
      
      ((and method (null id))
       (cond
         ((string= method "session/cancel")
          (acp-handle-session-cancel params))
         (t
          (acp-log "Unknown notification: ~A" method))))
      
      (t
       (acp-log "Unrecognized message: ~S" msg)))))

;;* Main loop
(defun acp-main-loop ()
  "Main stdio loop for ACP mode. Reads JSON-RPC messages and dispatches them."
  (acp-log "Hactar ACP mode started (version ~A)" *hactar-version*)
  (loop
    (let ((msg (acp-read-message)))
      (cond
        ((null msg)
         ;; EOF on stdin — graceful shutdown
         (acp-log "stdin EOF, shutting down ACP")
         (return))
        ((eq msg :parse-error)
         (acp-write-message
          (acp-make-error-response nil +jsonrpc-parse-error+ "Parse error")))
        (t
         (handler-case
             (acp-dispatch-message msg)
           (error (e)
             (acp-log "Error dispatching message: ~A" e)
             (let ((id (cdr (assoc "id" msg :test #'string=))))
               (when id
                 (acp-write-message
                  (acp-make-error-response id +jsonrpc-internal-error+
                                           (format nil "Internal error: ~A" e))))))))))))

;;* Entry point
(defun acp-on-context-file-changed (file-path)
  "Hook handler: resend available commands when context files change (commands may depend on context)."
  (declare (ignore file-path))
  (when *acp-session-id*
    (acp-send-available-commands)))

(defun acp-on-model-changed (new-model old-model)
  "Hook handler: resend config options when model changes."
  (declare (ignore old-model))
  (when *acp-session-id*
    (acp-send-config-options)))

(defun acp-install-hooks ()
  "Install ACP-specific hooks for dynamic updates."
  (nhooks:add-hook *context-file-added-hook*
                   (make-instance 'nhooks:handler
                                  :fn #'acp-on-context-file-changed
                                  :name 'acp-on-context-file-added))
  (nhooks:add-hook *context-file-dropped-hook*
                   (make-instance 'nhooks:handler
                                  :fn #'acp-on-context-file-changed
                                  :name 'acp-on-context-file-dropped))
  (nhooks:add-hook *model-changed-hook*
                   (make-instance 'nhooks:handler
                                  :fn #'acp-on-model-changed
                                  :name 'acp-on-model-changed)))

(defun start-acp ()
  "Initialize and start the ACP stdio server."
  (let ((*acp-mode* t)
        (*silent* t)
        (*in-repl* nil)
        (*acp-initialized* nil)
        (*acp-session-id* nil)
        (*acp-client-capabilities* nil)
        (*acp-pending-requests* (make-hash-table :test 'equal))
        (*acp-request-counter* 0)
        (*acp-cancelled* nil)
        (*acp-terminals* (make-hash-table :test 'equal)))
    
    (load-models-config (get-models-config-path))
    (set-current-model (or (uiop:getenv "HACTAR_MODEL") "ollama/qwen3:14b"))
    
    (acp-install-hooks)
    (acp-main-loop)))

(define-sub-command acp (args)
  "Start Hactar in Agent Client Protocol (ACP) mode over stdio."
  (declare (ignore args))
  (start-acp)
  (uiop:quit 0))
