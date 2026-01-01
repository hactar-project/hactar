;;* Model Context Protocol (MCP) — JSON-RPC 2.0 over stdio
(in-package :hactar)

;;* Error codes
(defparameter +mcp-parse-error+ -32700)
(defparameter +mcp-invalid-request+ -32600)
(defparameter +mcp-method-not-found+ -32601)
(defparameter +mcp-invalid-params+ -32602)
(defparameter +mcp-internal-error+ -32603)

;;* Transport
(defun mcp-write-message (message)
  "Serialize MESSAGE as JSON and write to stdout as a single newline-delimited line."
  (let* ((shasht:*write-alist-as-object* t)
         (shasht:*write-plist-as-object* nil)
         (shasht:*symbol-name-function* (lambda (sym)
                                          (let ((name (symbol-name sym)))
                                            (cond
                                              ((string= name "JSONRPC") "jsonrpc")
                                              ((string= name "PROTOCOL-VERSION") "protocolVersion")
                                              ((string= name "SERVER-INFO") "serverInfo")
                                              ((string= name "CLIENT-INFO") "clientInfo")
                                              ((string= name "INPUT-SCHEMA") "inputSchema")
                                              ((string= name "IS-ERROR") "isError")
                                              ((string= name "LIST-CHANGED") "listChanged")
                                              (t (string-downcase name))))))
         (json-string (shasht:write-json message nil)))
    (let ((clean (cl-ppcre:regex-replace-all "\\n" json-string "")))
      (write-line clean *standard-output*)
      (force-output *standard-output*))))

(defun mcp-read-message ()
  "Read a single newline-delimited JSON-RPC message from stdin.
   Returns the parsed alist or NIL on EOF."
  (let ((line (read-line *standard-input* nil nil)))
    (when (and line (> (length (string-trim '(#\Space #\Tab #\Return) line)) 0))
      (handler-case
          (let ((shasht:*read-default-object-format* :alist))
            (shasht:read-json line))
        (error (e)
          (mcp-log "JSON parse error: ~A" e)
          :parse-error)))))

(defun mcp-log (fmt &rest args)
  "Log a message to stderr for debugging in MCP mode."
  (let ((msg (apply #'format nil fmt args)))
    (format *error-output* "[MCP] ~A~%" msg)
    (force-output *error-output*)))

;;* Message construction
(defun mcp-make-response (id result)
  "Create a JSON-RPC 2.0 success response."
  `(("jsonrpc" . "2.0")
    ("id" . ,id)
    ("result" . ,result)))

(defun mcp-make-error-response (id code message &optional data)
  "Create a JSON-RPC 2.0 error response."
  (let ((error-obj `(("code" . ,code)
                     ("message" . ,message))))
    (when data
      (push (cons "data" data) (cdr (last error-obj))))
    `(("jsonrpc" . "2.0")
      ("id" . ,id)
      ("error" . ,error-obj))))

(defun mcp-make-notification (method params)
  "Create a JSON-RPC 2.0 notification (no id)."
  `(("jsonrpc" . "2.0")
    ("method" . ,method)
    ("params" . ,params)))

;;* Tool schema conversion
(defun tool-parameter-to-mcp-schema (param)
  "Convert a tool-parameter to MCP/JSON Schema format."
  (let ((schema `(("type" . ,(case (tool-parameter-type param)
                               (:string "string")
                               (:boolean "boolean")
                               (:number "number")
                               (:array "array")
                               (t "string")))
                  ("description" . ,(tool-parameter-description param)))))
    (when (tool-parameter-enum param)
      (push (cons "enum" (coerce (tool-parameter-enum param) 'vector)) schema))
    schema))

(defun tool-definition-to-mcp-schema (tool)
  "Convert a tool-definition to MCP tools/list format."
  (let* ((params (tool-definition-parameters tool))
         (required-params (remove-if-not #'tool-parameter-required params))
         (properties '()))
    (dolist (param params)
      (push (cons (tool-parameter-name param)
                  (tool-parameter-to-mcp-schema param))
            properties))
    `(("name" . ,(tool-definition-name tool))
      ("description" . ,(tool-definition-description tool))
      ("inputSchema" . (("type" . "object")
                        ("properties" . ,(nreverse properties))
                        ("required" . ,(coerce (mapcar #'tool-parameter-name required-params) 'vector)))))))

;;* Method handlers
(defun mcp-handle-initialize (id params)
  "Handle the MCP initialize method."
  (let ((protocol-version (cdr (assoc "protocolVersion" params :test #'string=)))
        (client-info (cdr (assoc "clientInfo" params :test #'string=))))
    (declare (ignore protocol-version))
    (mcp-log "Initialize from client: ~A"
             (if client-info
                 (or (cdr (assoc "name" client-info :test #'string=)) "unknown")
                 "unknown"))
    (setf *mcp-initialized* t)
    (mcp-write-message
     (mcp-make-response id
                        `(("protocolVersion" . "2025-03-26")
                          ("capabilities" . (("tools" . (("listChanged" . t)))))
                          ("serverInfo" . (("name" . "hactar")
                                           ("version" . ,*hactar-version*))))))))

(defun mcp-handle-ping (id params)
  "Handle the MCP ping method."
  (declare (ignore params))
  (mcp-write-message (mcp-make-response id nil)))

(defun mcp-handle-tools-list (id params)
  "Handle tools/list — return all defined tools in MCP schema format."
  (declare (ignore params))
  (let ((tools '()))
    (maphash (lambda (name tool)
               (declare (ignore name))
               (push (tool-definition-to-mcp-schema tool) tools))
             *defined-tools*)
    (mcp-write-message
     (mcp-make-response id
                        `(("tools" . ,(coerce (nreverse tools) 'vector)))))))

(defun mcp-handle-tools-call (id params)
  "Handle tools/call — execute a tool and return the result."
  (let ((tool-name (cdr (assoc "name" params :test #'string=)))
        (arguments (cdr (assoc "arguments" params :test #'string=))))
    (unless tool-name
      (mcp-write-message
       (mcp-make-error-response id +mcp-invalid-params+ "Missing 'name' in tools/call"))
      (return-from mcp-handle-tools-call))

    (let ((tool (gethash tool-name *defined-tools*)))
      (unless tool
        (mcp-write-message
         (mcp-make-error-response id +mcp-invalid-params+
                                  (format nil "Tool not found: ~A" tool-name)))
        (return-from mcp-handle-tools-call))

      ;; Convert arguments alist to plist for execute-tool
      (let ((args-plist (when arguments
                          (loop for (key . val) in arguments
                                append (list (intern (string-upcase key) :keyword) val)))))
        (multiple-value-bind (result success-p error-msg)
            (execute-tool tool-name args-plist)
          (let ((content-text (cond
                                (success-p (if (stringp result) result (format nil "~S" result)))
                                (error-msg error-msg)
                                (t "Unknown error"))))
            (mcp-write-message
             (mcp-make-response id
                                `(("content" . ,(vector `(("type" . "text")
                                                          ("text" . ,content-text))))
                                  ("isError" . ,(if success-p :false t)))))))))))

;;* Notifications
(defun mcp-notify-tools-changed ()
  "Send a notifications/tools/list_changed notification to the client."
  (when *mcp-initialized*
    (mcp-write-message
     (mcp-make-notification "notifications/tools/list_changed" nil))))

;;* Dispatcher
(defun mcp-dispatch-message (msg)
  "Dispatch an incoming JSON-RPC message to the appropriate MCP handler."
  (let* ((id (cdr (assoc "id" msg :test #'string=)))
         (method (cdr (assoc "method" msg :test #'string=)))
         (params (or (cdr (assoc "params" msg :test #'string=)) nil)))
    (cond
      ;; Incoming request (has id and method)
      ((and id method)
       (cond
         ((string= method "initialize")
          (mcp-handle-initialize id params))
         ((string= method "ping")
          (mcp-handle-ping id params))
         ((string= method "tools/list")
          (mcp-handle-tools-list id params))
         ((string= method "tools/call")
          (mcp-handle-tools-call id params))
         (t
          (mcp-write-message
           (mcp-make-error-response id +mcp-method-not-found+
                                    (format nil "Method not found: ~A" method))))))

      ;; Notification (has method, no id)
      ((and method (null id))
       (cond
         ((string= method "notifications/initialized")
          (mcp-log "Client confirmed initialization"))
         ((string= method "notifications/cancelled")
          (mcp-log "Client cancelled request"))
         (t
          (mcp-log "Unknown notification: ~A" method))))

      (t
       (mcp-log "Unrecognized message: ~S" msg)))))

;;* Main loop
(defun mcp-main-loop ()
  "Main stdio loop for MCP mode. Reads JSON-RPC messages and dispatches them."
  (mcp-log "Hactar MCP server started (version ~A)" *hactar-version*)
  (loop
    (let ((msg (mcp-read-message)))
      (cond
        ((null msg)
         (mcp-log "stdin EOF, shutting down MCP server")
         (return))
        ((eq msg :parse-error)
         (mcp-write-message
          (mcp-make-error-response nil +mcp-parse-error+ "Parse error")))
        (t
         (handler-case
             (mcp-dispatch-message msg)
           (error (e)
             (mcp-log "Error dispatching message: ~A" e)
             (let ((id (cdr (assoc "id" msg :test #'string=))))
               (when id
                 (mcp-write-message
                  (mcp-make-error-response id +mcp-internal-error+
                                           (format nil "Internal error: ~A" e))))))))))))

;;* Entry point
(defun start-mcp ()
  "Initialize and start the MCP stdio server."
  (let ((*mcp-mode* t)
        (*silent* t)
        (*in-repl* nil)
        (*mcp-initialized* nil))

    (load-models-config (get-models-config-path))
    (set-current-model (or (uiop:getenv "HACTAR_MODEL") "ollama/minimax-m2.5:cloud"))

    (handler-case
        (setf *repo-root* (find-git-repo-root (uiop:getcwd)))
      (error ()
        (setf *repo-root* (uiop:getcwd))))

    (when *repo-root*
      (load-project-config *repo-root*))

    (load-user-customizations)
    (mcp-main-loop)))

(define-sub-command mcp (args)
  "Start Hactar as a Model Context Protocol (MCP) server over stdio."
  (declare (ignore args))
  (start-mcp)
  (uiop:quit 0))
