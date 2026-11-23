(in-package #:hactar-mcp)

;;; Model Context Protocol (MCP) server and client over hactar-jsonrpc.

(defparameter +mcp-protocol-version+ "2024-11-05")

(defun %field (name alist)
  (when (listp alist)
    (cdr (assoc name alist :test #'string-equal :key #'string))))

(defun %empty-object ()
  (make-hash-table :test 'equal))

(defun %lambda-list-props (args)
  "Convert a simple tool lambda-list into (values properties required)."
  (let ((props nil)
        (required nil)
        (optional-p nil))
    (dolist (item args)
      (cond
        ((eq item '&optional) (setf optional-p t))
        ((member item '(&rest &key &allow-other-keys &body)) (return))
        ((symbolp item)
         (let ((name (string-downcase (symbol-name item))))
           (push (cons name '(("type" . "string"))) props)
           (unless optional-p
             (push name required))))))
    (values (if props (nreverse props) (%empty-object))
            (coerce (nreverse required) 'vector))))

(defun tool->mcp-schema (tool-plist)
  "Convert a HACTAR-CORE:LIST-TOOLS entry into an MCP tool descriptor alist."
  (multiple-value-bind (props required)
      (%lambda-list-props (getf tool-plist :args))
    `(("name" . ,(getf tool-plist :name))
      ("description" . ,(or (getf tool-plist :doc) ""))
      ("inputSchema" . (("type" . "object")
                        ("properties" . ,props)
                        ("required" . ,required))))))

(defun %coerce-tool-args (tool-name args-alist)
  "Order ARGS-ALIST values according to the tool's declared parameter names."
  (let* ((tool (find tool-name (hactar-core:list-tools)
                     :key (lambda (x) (getf x :name)) :test #'string-equal))
         (ll (and tool (getf tool :args)))
         (params (loop for sym in ll
                       until (member sym '(&rest &key &allow-other-keys &body))
                       unless (eq sym '&optional)
                         collect (string-downcase (symbol-name sym)))))
    (if (and params (listp args-alist))
        (loop for pname in params
              for entry = (assoc pname args-alist :test #'string-equal :key #'string)
              when entry collect (cdr entry))
        (mapcar #'cdr args-alist))))

;;; ---- MCP Server -----------------------------------------------------------

(defun make-mcp-server (&key (input *standard-input*) (output *standard-output*)
                             (framing :newline))
  "Create a JSON-RPC server exposing HACTAR-CORE tools via MCP."
  (let ((server (hactar-jsonrpc:make-server :input input :output output :framing framing)))
    (hactar-jsonrpc:register-method
     server "initialize"
     (lambda (params)
       (declare (ignore params))
       `(("protocolVersion" . ,+mcp-protocol-version+)
         ("capabilities" . (("tools" . (("listChanged" . t)))))
         ("serverInfo" . (("name" . "hactar") ("version" . "0.2.0"))))))
    (hactar-jsonrpc:register-method
     server "notifications/initialized"
     (lambda (params) (declare (ignore params)) nil))
    (hactar-jsonrpc:register-method
     server "ping"
     (lambda (params) (declare (ignore params)) (%empty-object)))
    (hactar-jsonrpc:register-method
     server "tools/list"
     (lambda (params)
       (declare (ignore params))
       `(("tools" . ,(coerce (mapcar #'tool->mcp-schema (hactar-core:list-tools))
                             'vector)))))
    (hactar-jsonrpc:register-method
     server "tools/call"
     (lambda (params)
       (let* ((name (%field "name" params))
              (arguments (%field "arguments" params)))
         (unless name
           (error 'hactar-jsonrpc:rpc-error
                  :code hactar-jsonrpc:+invalid-params+
                  :message "Missing tool name"))
         (let* ((positional (%coerce-tool-args name arguments))
                (result (apply #'hactar-core:run-tool name positional))
                (ok-p (hactar-core:result-ok-p result))
                (text (or (hactar-core:result-message result) "")))
           `(("content" . #((("type" . "text") ("text" . ,text))))
             ("isError" . ,(not ok-p)))))))
    server))

(defun serve-mcp (&key (input *standard-input*) (output *standard-output*)
                       (framing :newline))
  "Serve MCP JSON-RPC requests until EOF."
  (hactar-jsonrpc:serve (make-mcp-server :input input :output output :framing framing)))

;;; ---- MCP Client -----------------------------------------------------------

(defstruct (mcp-client (:constructor %make-mcp-client))
  rpc process server-info tools)

(defun connect-mcp (&key command input output (framing :newline) (initialize t))
  "Connect to an external MCP server over streams (INPUT/OUTPUT) or COMMAND."
  (let* ((process (when command
                    (uiop:launch-program command :input :stream :output :stream)))
         (in-stream (or input (and process (uiop:process-info-output process))))
         (out-stream (or output (and process (uiop:process-info-input process))))
         (rpc (hactar-jsonrpc:make-client :input in-stream :output out-stream
                                          :framing framing))
         (client (%make-mcp-client :rpc rpc :process process)))
    (when initialize
      (let ((info (hactar-jsonrpc:call
                   rpc "initialize"
                   `(("protocolVersion" . ,+mcp-protocol-version+)
                     ("capabilities" . ,(%empty-object))
                     ("clientInfo" . (("name" . "hactar") ("version" . "0.2.0")))))))
        (setf (mcp-client-server-info client) info)
        (hactar-jsonrpc:notify rpc "notifications/initialized" (%empty-object))))
    client))

(defun disconnect-mcp (client)
  "Close subprocess resources associated with CLIENT, if any."
  (when (mcp-client-process client)
    (ignore-errors (uiop:close-streams (mcp-client-process client)))
    (ignore-errors (uiop:terminate-process (mcp-client-process client))))
  t)

(defun mcp-list-tools (client)
  "Query tools/list from the remote MCP server."
  (let* ((resp (hactar-jsonrpc:call (mcp-client-rpc client) "tools/list" (%empty-object)))
         (raw (%field "tools" resp))
         (tools (if (vectorp raw) (coerce raw 'list) raw)))
    (setf (mcp-client-tools client) tools)
    tools))

(defun mcp-call-tool (client name &optional arguments)
  "Invoke tools/call on the remote MCP server."
  (hactar-jsonrpc:call (mcp-client-rpc client) "tools/call"
                       `(("name" . ,name)
                         ("arguments" . ,(or arguments (%empty-object))))))

(defun mcp-tools->deftools (client &key prefix)
  "Register each remote MCP tool as a runnable tool in HACTAR-CORE."
  (let ((tools (or (mcp-client-tools client) (mcp-list-tools client)))
        (registered nil))
    (dolist (tool tools)
      (let* ((remote-name (%field "name" tool))
             (doc (or (%field "description" tool) "Remote MCP tool"))
             (local-name (if prefix
                             (format nil "~A.~A" prefix remote-name)
                             remote-name)))
        (hactar-core:register-tool-fn
         local-name doc
         (lambda (&rest args)
           (let* ((payload (if (and (= 1 (length args)) (listp (first args))
                                    (consp (first (first args))))
                               (first args)
                               (loop for (k v) on args by #'cddr
                                     collect (cons (string-downcase (string k)) v))))
                  (res (mcp-call-tool client remote-name payload))
                  (content (%field "content" res))
                  (blocks (if (vectorp content) (coerce content 'list) content))
                  (text (or (and blocks (%field "text" (first blocks)))
                            (format nil "~S" res))))
             (if (%field "isError" res)
                 (hactar-core:err text :data res)
                 (hactar-core:ok text :data res)))))
        (push local-name registered)))
    (nreverse registered)))
