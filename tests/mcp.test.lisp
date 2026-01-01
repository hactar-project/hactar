;;* MCP Tests
(in-package :hactar-tests)

(def-suite mcp-tests
  :description "Tests for Model Context Protocol (MCP) implementation")

(in-suite mcp-tests)

;;** JSON-RPC Message Construction

(test mcp-make-response-basic
  "Test basic MCP JSON-RPC response construction."
  (let ((response (hactar::mcp-make-response 1 '(("key" . "value")))))
    (is (string= "2.0" (cdr (assoc "jsonrpc" response :test #'string=))))
    (is (= 1 (cdr (assoc "id" response :test #'string=))))
    (let ((result (cdr (assoc "result" response :test #'string=))))
      (is (string= "value" (cdr (assoc "key" result :test #'string=)))))))

(test mcp-make-response-with-string-id
  "Test MCP JSON-RPC response with string ID."
  (let ((response (hactar::mcp-make-response "req_abc" '(("status" . "ok")))))
    (is (string= "req_abc" (cdr (assoc "id" response :test #'string=))))))

(test mcp-make-response-nil-result
  "Test MCP JSON-RPC response with nil result."
  (let ((response (hactar::mcp-make-response 1 nil)))
    (is (null (cdr (assoc "result" response :test #'string=))))))

(test mcp-make-error-response-basic
  "Test MCP JSON-RPC error response construction."
  (let ((response (hactar::mcp-make-error-response 1 -32600 "Invalid Request")))
    (is (string= "2.0" (cdr (assoc "jsonrpc" response :test #'string=))))
    (is (= 1 (cdr (assoc "id" response :test #'string=))))
    (let ((err (cdr (assoc "error" response :test #'string=))))
      (is (= -32600 (cdr (assoc "code" err :test #'string=))))
      (is (string= "Invalid Request" (cdr (assoc "message" err :test #'string=)))))))

(test mcp-make-error-response-with-data
  "Test MCP JSON-RPC error response with additional data."
  (let ((response (hactar::mcp-make-error-response 2 -32603 "Internal error" '(("detail" . "trace")))))
    (let ((err (cdr (assoc "error" response :test #'string=))))
      (is (= -32603 (cdr (assoc "code" err :test #'string=))))
      (is (string= "Internal error" (cdr (assoc "message" err :test #'string=)))))))

(test mcp-make-notification-basic
  "Test MCP JSON-RPC notification construction (no id)."
  (let ((notif (hactar::mcp-make-notification "notifications/tools/list_changed" nil)))
    (is (string= "2.0" (cdr (assoc "jsonrpc" notif :test #'string=))))
    (is (string= "notifications/tools/list_changed" (cdr (assoc "method" notif :test #'string=))))
    (is (null (assoc "id" notif :test #'string=)))))

;;** Error Code Constants

(test mcp-error-codes
  "Test that MCP JSON-RPC error codes are correctly defined."
  (is (= -32700 hactar::+mcp-parse-error+))
  (is (= -32600 hactar::+mcp-invalid-request+))
  (is (= -32601 hactar::+mcp-method-not-found+))
  (is (= -32602 hactar::+mcp-invalid-params+))
  (is (= -32603 hactar::+mcp-internal-error+)))

;;** MCP Global State

(test mcp-initial-state
  "Test MCP globals have correct initial values."
  (is (null hactar::*mcp-mode*))
  (is (null hactar::*mcp-initialized*)))

;;** Write Message Serialization

(test mcp-write-message-output
  "Test that mcp-write-message writes newline-delimited JSON to stdout."
  (let ((written-output
          (with-output-to-string (*standard-output*)
            (hactar::mcp-write-message '(("jsonrpc" . "2.0") ("id" . 1) ("result" . nil))))))
    (is (char= #\Newline (char written-output (1- (length written-output)))))
    (let ((lines (str:lines (string-trim '(#\Newline) written-output))))
      (is (= 1 (length lines))))
    (is (search "jsonrpc" written-output))
    (is (search "2.0" written-output))))

(test mcp-write-message-camel-case-keys
  "Test that mcp-write-message correctly serializes camelCase keys."
  (let ((written-output
          (with-output-to-string (*standard-output*)
            (hactar::mcp-write-message
             '(("jsonrpc" . "2.0")
               ("id" . 1)
               ("result" . (("protocolVersion" . "2025-03-26")
                            ("serverInfo" . (("name" . "hactar"))))))))))
    (is (search "protocolVersion" written-output))
    (is (search "serverInfo" written-output))))

;;** Read Message

(test mcp-read-message-valid-json
  "Test mcp-read-message parses valid JSON from stdin."
  (let* ((json-line "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}")
         (msg (with-input-from-string (*standard-input* (format nil "~A~%" json-line))
                (hactar::mcp-read-message))))
    (is (string= "2.0" (cdr (assoc "jsonrpc" msg :test #'string=))))
    (is (= 1 (cdr (assoc "id" msg :test #'string=))))
    (is (string= "ping" (cdr (assoc "method" msg :test #'string=))))))

(test mcp-read-message-eof
  "Test mcp-read-message returns nil on EOF."
  (let ((msg (with-input-from-string (*standard-input* "")
               (hactar::mcp-read-message))))
    (is (null msg))))

(test mcp-read-message-empty-line
  "Test mcp-read-message returns nil for whitespace-only lines."
  (let ((msg (with-input-from-string (*standard-input* (format nil "   ~%"))
               (hactar::mcp-read-message))))
    (is (null msg))))

(test mcp-read-message-invalid-json
  "Test mcp-read-message returns :parse-error for invalid JSON."
  (let ((msg (with-input-from-string (*standard-input* (format nil "not valid json~%"))
               (let ((*error-output* (make-string-output-stream))) ; suppress stderr
                 (hactar::mcp-read-message)))))
    (is (eq :parse-error msg))))

;;** Tool Schema Conversion

(test tool-parameter-to-mcp-schema-string
  "Test converting a string parameter to MCP schema."
  (let* ((param (hactar::make-tool-parameter
                 :name "path" :type :string
                 :description "File path" :required t))
         (schema (hactar::tool-parameter-to-mcp-schema param)))
    (is (string= "string" (cdr (assoc "type" schema :test #'string=))))
    (is (string= "File path" (cdr (assoc "description" schema :test #'string=))))))

(test tool-parameter-to-mcp-schema-number
  "Test converting a number parameter to MCP schema."
  (let* ((param (hactar::make-tool-parameter
                 :name "count" :type :number
                 :description "How many" :required t))
         (schema (hactar::tool-parameter-to-mcp-schema param)))
    (is (string= "number" (cdr (assoc "type" schema :test #'string=))))))

(test tool-parameter-to-mcp-schema-boolean
  "Test converting a boolean parameter to MCP schema."
  (let* ((param (hactar::make-tool-parameter
                 :name "flag" :type :boolean
                 :description "Toggle" :required nil))
         (schema (hactar::tool-parameter-to-mcp-schema param)))
    (is (string= "boolean" (cdr (assoc "type" schema :test #'string=))))))

(test tool-parameter-to-mcp-schema-with-enum
  "Test converting a parameter with enum to MCP schema."
  (let* ((param (hactar::make-tool-parameter
                 :name "mode" :type :string
                 :description "Mode" :required nil
                 :enum '("fast" "slow")))
         (schema (hactar::tool-parameter-to-mcp-schema param)))
    (is (string= "string" (cdr (assoc "type" schema :test #'string=))))
    (let ((enum-val (cdr (assoc "enum" schema :test #'string=))))
      (is (vectorp enum-val))
      (is (= 2 (length enum-val)))
      (is (string= "fast" (aref enum-val 0)))
      (is (string= "slow" (aref enum-val 1))))))

(test tool-definition-to-mcp-schema-basic
  "Test converting a full tool definition to MCP schema."
  (let* ((tool (hactar::make-tool-definition
                :name "test_tool"
                :description "A test tool"
                :parameters (list
                             (hactar::make-tool-parameter
                              :name "path" :type :string
                              :description "File path" :required t)
                             (hactar::make-tool-parameter
                              :name "verbose" :type :boolean
                              :description "Verbose output" :required nil))
                :permissions :auto))
         (schema (hactar::tool-definition-to-mcp-schema tool)))
    (is (string= "test_tool" (cdr (assoc "name" schema :test #'string=))))
    (is (string= "A test tool" (cdr (assoc "description" schema :test #'string=))))
    (let ((input-schema (cdr (assoc "inputSchema" schema :test #'string=))))
      (is (string= "object" (cdr (assoc "type" input-schema :test #'string=))))
      ;; Check properties
      (let ((props (cdr (assoc "properties" input-schema :test #'string=))))
        (is (= 2 (length props)))
        (is-true (assoc "path" props :test #'string=))
        (is-true (assoc "verbose" props :test #'string=)))
      ;; Check required array
      (let ((required (cdr (assoc "required" input-schema :test #'string=))))
        (is (vectorp required))
        (is (= 1 (length required)))
        (is (string= "path" (aref required 0)))))))

(test tool-definition-to-mcp-schema-no-required
  "Test MCP schema with no required parameters."
  (let* ((tool (hactar::make-tool-definition
                :name "optional_tool"
                :description "All optional"
                :parameters (list
                             (hactar::make-tool-parameter
                              :name "x" :type :string
                              :description "Optional" :required nil))
                :permissions :auto))
         (schema (hactar::tool-definition-to-mcp-schema tool)))
    (let* ((input-schema (cdr (assoc "inputSchema" schema :test #'string=)))
           (required (cdr (assoc "required" input-schema :test #'string=))))
      (is (vectorp required))
      (is (= 0 (length required))))))

(test tool-definition-to-mcp-schema-no-params
  "Test MCP schema with zero parameters."
  (let* ((tool (hactar::make-tool-definition
                :name "no_params_tool"
                :description "No params"
                :parameters nil
                :permissions :auto))
         (schema (hactar::tool-definition-to-mcp-schema tool)))
    (let* ((input-schema (cdr (assoc "inputSchema" schema :test #'string=)))
           (props (cdr (assoc "properties" input-schema :test #'string=)))
           (required (cdr (assoc "required" input-schema :test #'string=))))
      (is (null props))
      (is (vectorp required))
      (is (= 0 (length required))))))

;;** Initialize Handler

(test mcp-handle-initialize
  "Test the MCP initialize handshake."
  (let ((output-messages '())
        (hactar::*mcp-initialized* nil)
        (hactar::*hactar-version* "0.1.0-test"))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-initialize 1
                                            '(("protocolVersion" . "2025-03-26")
                                              ("clientInfo" . (("name" . "test-client")
                                                               ("version" . "1.0")))))
             (is-true hactar::*mcp-initialized*)
             (is (= 1 (length output-messages)))
             (let* ((resp (first output-messages))
                    (result (cdr (assoc "result" resp :test #'string=))))
               (is (= 1 (cdr (assoc "id" resp :test #'string=))))
               (is (string= "2025-03-26" (cdr (assoc "protocolVersion" result :test #'string=))))
               (let ((server-info (cdr (assoc "serverInfo" result :test #'string=))))
                 (is (string= "hactar" (cdr (assoc "name" server-info :test #'string=))))
                 (is (string= "0.1.0-test" (cdr (assoc "version" server-info :test #'string=)))))
               (let ((capabilities (cdr (assoc "capabilities" result :test #'string=))))
                 (is-true (assoc "tools" capabilities :test #'string=)))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-handle-initialize-unknown-client
  "Test initialize with missing client info."
  (let ((output-messages '())
        (hactar::*mcp-initialized* nil))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-initialize 2 '(("protocolVersion" . "2025-03-26")))
             (is-true hactar::*mcp-initialized*)
             (is (= 1 (length output-messages))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

;;** Ping Handler

(test mcp-handle-ping
  "Test the MCP ping handler."
  (let ((output-messages '()))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-ping 42 nil)
             (is (= 1 (length output-messages)))
             (let ((resp (first output-messages)))
               (is (= 42 (cdr (assoc "id" resp :test #'string=))))
               (is (null (cdr (assoc "result" resp :test #'string=))))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

;;** Tools List Handler

(test mcp-handle-tools-list-empty
  "Test tools/list with no tools defined."
  (let ((output-messages '())
        (hactar::*defined-tools* (make-hash-table :test 'equal)))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-tools-list 3 nil)
             (is (= 1 (length output-messages)))
             (let* ((resp (first output-messages))
                    (result (cdr (assoc "result" resp :test #'string=)))
                    (tools (cdr (assoc "tools" result :test #'string=))))
               (is (= 3 (cdr (assoc "id" resp :test #'string=))))
               (is (vectorp tools))
               (is (= 0 (length tools)))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-handle-tools-list-with-tools
  "Test tools/list returns all defined tools in MCP schema format."
  (let ((output-messages '())
        (hactar::*defined-tools* (make-hash-table :test 'equal)))
    (setf (gethash "echo_tool" hactar::*defined-tools*)
          (hactar::make-tool-definition
           :name "echo_tool"
           :description "Echo a message"
           :parameters (list (hactar::make-tool-parameter
                              :name "message" :type :string
                              :description "Message to echo" :required t))
           :permissions :auto))
    (setf (gethash "math_tool" hactar::*defined-tools*)
          (hactar::make-tool-definition
           :name "math_tool"
           :description "Do math"
           :parameters (list (hactar::make-tool-parameter
                              :name "a" :type :number
                              :description "First number" :required t)
                             (hactar::make-tool-parameter
                              :name "b" :type :number
                              :description "Second number" :required t))
           :permissions :auto))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-tools-list 4 nil)
             (is (= 1 (length output-messages)))
             (let* ((resp (first output-messages))
                    (result (cdr (assoc "result" resp :test #'string=)))
                    (tools (cdr (assoc "tools" result :test #'string=))))
               (is (vectorp tools))
               (is (= 2 (length tools)))
               (loop for i from 0 below (length tools)
                     for tool = (aref tools i)
                     do (is-true (assoc "name" tool :test #'string=))
                        (is-true (assoc "description" tool :test #'string=))
                        (is-true (assoc "inputSchema" tool :test #'string=))
                        (let ((input-schema (cdr (assoc "inputSchema" tool :test #'string=))))
                          (is (string= "object" (cdr (assoc "type" input-schema :test #'string=))))
                          (is-true (assoc "properties" input-schema :test #'string=))
                          (is-true (assoc "required" input-schema :test #'string=))))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

;;** Tools Call Handler

(test mcp-handle-tools-call-missing-name
  "Test tools/call with missing tool name returns error."
  (let ((output-messages '()))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-tools-call 5 '(("arguments" . (("x" . "1")))))
             (is (= 1 (length output-messages)))
             (let ((resp (first output-messages)))
               (is-true (assoc "error" resp :test #'string=))
               (let ((err (cdr (assoc "error" resp :test #'string=))))
                 (is (= hactar::+mcp-invalid-params+
                        (cdr (assoc "code" err :test #'string=)))))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-handle-tools-call-tool-not-found
  "Test tools/call with nonexistent tool returns error."
  (let ((output-messages '())
        (hactar::*defined-tools* (make-hash-table :test 'equal)))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-tools-call 6
                                            '(("name" . "nonexistent_tool")
                                              ("arguments" . (("x" . "1")))))
             (is (= 1 (length output-messages)))
             (let ((resp (first output-messages)))
               (is-true (assoc "error" resp :test #'string=))
               (let ((err (cdr (assoc "error" resp :test #'string=))))
                 (is (search "not found" (cdr (assoc "message" err :test #'string=)))))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-handle-tools-call-success
  "Test tools/call with a successful tool execution."
  (let ((output-messages '())
        (hactar::*defined-tools* (make-hash-table :test 'equal))
        (hactar::*silent* t))
    (eval '(hactar::deftool mcp-test-echo ((message :string :description "Msg" :required t))
             :description "Echo"
             :permissions :auto
             (getf hactar::args :message)))
    (let ((orig-perm #'hactar::resolve-permission)
          (orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::resolve-permission)
            (lambda (tool-name args) (declare (ignore tool-name args)) :allow))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-tools-call 7
                                            '(("name" . "mcp_test_echo")
                                              ("arguments" . (("message" . "hello world")))))
             (is (= 1 (length output-messages)))
             (let* ((resp (first output-messages))
                    (result (cdr (assoc "result" resp :test #'string=)))
                    (content (cdr (assoc "content" result :test #'string=)))
                    (is-error (cdr (assoc "isError" result :test #'string=))))
               (is (= 7 (cdr (assoc "id" resp :test #'string=))))
               (is (eq :false is-error))
               (is (vectorp content))
               (is (= 1 (length content)))
               (let ((block (aref content 0)))
                 (is (string= "text" (cdr (assoc "type" block :test #'string=))))
                 (is (string= "hello world" (cdr (assoc "text" block :test #'string=)))))))
        (setf (fdefinition 'hactar::resolve-permission) orig-perm)
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-handle-tools-call-execution-error
  "Test tools/call when tool execution raises an error."
  (let ((output-messages '())
        (hactar::*defined-tools* (make-hash-table :test 'equal))
        (hactar::*silent* t))
    (setf (gethash "error_tool" hactar::*defined-tools*)
          (hactar::make-tool-definition
           :name "error_tool"
           :description "Always errors"
           :parameters nil
           :permissions :auto
           :function (lambda (args) (declare (ignore args)) (error "boom"))))
    (let ((orig-perm #'hactar::resolve-permission)
          (orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::resolve-permission)
            (lambda (tool-name args) (declare (ignore tool-name args)) :allow))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-tools-call 8
                                            '(("name" . "error_tool")))
             (is (= 1 (length output-messages)))
             (let* ((resp (first output-messages))
                    (result (cdr (assoc "result" resp :test #'string=)))
                    (is-error (cdr (assoc "isError" result :test #'string=))))
               (is (eq t is-error))))
        (setf (fdefinition 'hactar::resolve-permission) orig-perm)
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-handle-tools-call-no-arguments
  "Test tools/call with nil arguments."
  (let ((output-messages '())
        (hactar::*defined-tools* (make-hash-table :test 'equal))
        (hactar::*silent* t))
    (setf (gethash "no_args_tool" hactar::*defined-tools*)
          (hactar::make-tool-definition
           :name "no_args_tool"
           :description "No args needed"
           :parameters nil
           :permissions :auto
           :function (lambda (args) (declare (ignore args)) "done")))
    (let ((orig-perm #'hactar::resolve-permission)
          (orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::resolve-permission)
            (lambda (tool-name args) (declare (ignore tool-name args)) :allow))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-handle-tools-call 9
                                            '(("name" . "no_args_tool")))
             (is (= 1 (length output-messages)))
             (let* ((resp (first output-messages))
                    (result (cdr (assoc "result" resp :test #'string=)))
                    (content (cdr (assoc "content" result :test #'string=))))
               (is (vectorp content))
               (is (= 1 (length content)))
               (is (string= "done" (cdr (assoc "text" (aref content 0) :test #'string=))))))
        (setf (fdefinition 'hactar::resolve-permission) orig-perm)
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

;;** Notifications

(test mcp-notify-tools-changed-when-initialized
  "Test that tools changed notification is sent when initialized."
  (let ((output-messages '())
        (hactar::*mcp-initialized* t))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-notify-tools-changed)
             (is (= 1 (length output-messages)))
             (let ((notif (first output-messages)))
               (is (string= "notifications/tools/list_changed"
                            (cdr (assoc "method" notif :test #'string=))))
               (is (null (assoc "id" notif :test #'string=)))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-notify-tools-changed-when-not-initialized
  "Test that tools changed notification is NOT sent before initialization."
  (let ((output-messages '())
        (hactar::*mcp-initialized* nil))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-notify-tools-changed)
             (is (= 0 (length output-messages))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

;;** Message Dispatcher

(test mcp-dispatch-initialize
  "Test dispatching an initialize request."
  (let ((output-messages '())
        (hactar::*mcp-initialized* nil))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("id" . 1)
                ("method" . "initialize")
                ("params" . (("protocolVersion" . "2025-03-26")))))
             (is-true hactar::*mcp-initialized*)
             (is (= 1 (length output-messages))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-dispatch-ping
  "Test dispatching a ping request."
  (let ((output-messages '()))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("id" . 10)
                ("method" . "ping")
                ("params" . nil)))
             (is (= 1 (length output-messages)))
             (is (= 10 (cdr (assoc "id" (first output-messages) :test #'string=)))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-dispatch-tools-list
  "Test dispatching a tools/list request."
  (let ((output-messages '())
        (hactar::*defined-tools* (make-hash-table :test 'equal)))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("id" . 11)
                ("method" . "tools/list")
                ("params" . nil)))
             (is (= 1 (length output-messages)))
             (let ((result (cdr (assoc "result" (first output-messages) :test #'string=))))
               (is-true (assoc "tools" result :test #'string=))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-dispatch-tools-call
  "Test dispatching a tools/call request."
  (let ((output-messages '())
        (hactar::*defined-tools* (make-hash-table :test 'equal))
        (hactar::*silent* t))
    (setf (gethash "dispatch_test" hactar::*defined-tools*)
          (hactar::make-tool-definition
           :name "dispatch_test"
           :description "Test"
           :parameters nil
           :permissions :auto
           :function (lambda (args) (declare (ignore args)) "dispatched")))
    (let ((orig-perm #'hactar::resolve-permission)
          (orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::resolve-permission)
            (lambda (tool-name args) (declare (ignore tool-name args)) :allow))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("id" . 12)
                ("method" . "tools/call")
                ("params" . (("name" . "dispatch_test")))))
             (is (= 1 (length output-messages))))
        (setf (fdefinition 'hactar::resolve-permission) orig-perm)
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-dispatch-unknown-method
  "Test dispatching an unknown method returns method-not-found error."
  (let ((output-messages '()))
    (let ((orig-fn #'hactar::mcp-write-message))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::mcp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("id" . 99)
                ("method" . "unknown/method")
                ("params" . nil)))
             (is (= 1 (length output-messages)))
             (let ((err (cdr (assoc "error" (first output-messages) :test #'string=))))
               (is (= hactar::+mcp-method-not-found+
                      (cdr (assoc "code" err :test #'string=))))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)))))

(test mcp-dispatch-notification-initialized
  "Test dispatching notifications/initialized notification."
  (let ((output-messages '()))
    (let ((orig-fn #'hactar::mcp-write-message)
          (orig-log #'hactar::mcp-log))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::mcp-log)
            (lambda (fmt &rest args) (declare (ignore fmt args)) nil))
      (unwind-protect
           (progn
             (hactar::mcp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("method" . "notifications/initialized")))
             (is (= 0 (length output-messages))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)
        (setf (fdefinition 'hactar::mcp-log) orig-log)))))

(test mcp-dispatch-notification-cancelled
  "Test dispatching notifications/cancelled notification."
  (let ((output-messages '()))
    (let ((orig-fn #'hactar::mcp-write-message)
          (orig-log #'hactar::mcp-log))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::mcp-log)
            (lambda (fmt &rest args) (declare (ignore fmt args)) nil))
      (unwind-protect
           (progn
             (hactar::mcp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("method" . "notifications/cancelled")))
             (is (= 0 (length output-messages))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)
        (setf (fdefinition 'hactar::mcp-log) orig-log)))))

(test mcp-dispatch-response-ignored
  "Test that response messages (no method) are logged but don't crash."
  (let ((output-messages '()))
    (let ((orig-fn #'hactar::mcp-write-message)
          (orig-log #'hactar::mcp-log))
      (setf (fdefinition 'hactar::mcp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::mcp-log)
            (lambda (fmt &rest args) (declare (ignore fmt args)) nil))
      (unwind-protect
           (progn
             (hactar::mcp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("id" . 50)
                ("result" . (("data" . "something")))))
             (is (= 0 (length output-messages))))
        (setf (fdefinition 'hactar::mcp-write-message) orig-fn)
        (setf (fdefinition 'hactar::mcp-log) orig-log)))))

;;** Main Loop

(test mcp-main-loop-eof-exits
  "Test that mcp-main-loop exits gracefully on EOF."
  (let ((*error-output* (make-string-output-stream)))
    (with-input-from-string (*standard-input* "")
      (with-output-to-string (*standard-output*)
        ;; Should return without error on EOF
        (hactar::mcp-main-loop)
        (pass)))))

(test mcp-main-loop-parse-error
  "Test that mcp-main-loop handles parse errors and continues."
  (let ((*error-output* (make-string-output-stream))
        (output ""))
    (with-input-from-string (*standard-input* (format nil "garbage~%"))
      (setf output
            (with-output-to-string (*standard-output*)
              (hactar::mcp-main-loop))))
    (is (search "Parse error" output))))

(test mcp-main-loop-initialize-then-eof
  "Test main loop processing an initialize message then EOF."
  (let ((*error-output* (make-string-output-stream))
        (hactar::*mcp-initialized* nil)
        (output ""))
    (let ((init-msg "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-03-26\"}}"))
      (with-input-from-string (*standard-input* (format nil "~A~%" init-msg))
        (setf output
              (with-output-to-string (*standard-output*)
                (hactar::mcp-main-loop)))))
    (is-true hactar::*mcp-initialized*)
    (is (search "protocolVersion" output))
    (is (search "hactar" output))))

(test mcp-main-loop-ping-response
  "Test main loop processes a ping and returns empty result."
  (let ((*error-output* (make-string-output-stream))
        (output ""))
    (let ((ping-msg "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"ping\"}"))
      (with-input-from-string (*standard-input* (format nil "~A~%" ping-msg))
        (setf output
              (with-output-to-string (*standard-output*)
                (hactar::mcp-main-loop)))))
    (is (search "42" output))))

;;** Integration: Full Initialize + Tools/List Sequence

(test mcp-full-init-and-tools-list
  "Test a full initialize -> notifications/initialized -> tools/list sequence."
  (let ((*error-output* (make-string-output-stream))
        (hactar::*mcp-initialized* nil)
        (hactar::*defined-tools* (make-hash-table :test 'equal))
        (hactar::*silent* t)
        (output ""))
    (setf (gethash "greet" hactar::*defined-tools*)
          (hactar::make-tool-definition
           :name "greet"
           :description "Say hello"
           :parameters (list (hactar::make-tool-parameter
                              :name "name" :type :string
                              :description "Name" :required t))
           :permissions :auto))
    (let ((init-msg "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-03-26\"}}")
          (initialized-msg "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
          (list-msg "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"))
      (with-input-from-string (*standard-input*
                                (format nil "~A~%~A~%~A~%" init-msg initialized-msg list-msg))
        (setf output
              (with-output-to-string (*standard-output*)
                (hactar::mcp-main-loop)))))
    (is-true hactar::*mcp-initialized*)
    (is (search "greet" output))
    (is (search "inputSchema" output))))
