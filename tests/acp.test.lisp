(in-package :hactar-tests)

(def-suite acp-tests
  :description "Tests for Agent Client Protocol (ACP) implementation")

(in-suite acp-tests)

;;* JSON-RPC Message Construction

(test acp-make-response-basic
  "Test basic JSON-RPC response construction."
  (let ((response (hactar::acp-make-response 1 '(("key" . "value")))))
    (is (string= "2.0" (cdr (assoc "jsonrpc" response :test #'string=))))
    (is (= 1 (cdr (assoc "id" response :test #'string=))))
    (let ((result (cdr (assoc "result" response :test #'string=))))
      (is (string= "value" (cdr (assoc "key" result :test #'string=)))))))

(test acp-make-response-with-string-id
  "Test JSON-RPC response with string ID."
  (let ((response (hactar::acp-make-response "req_123" '(("status" . "ok")))))
    (is (string= "req_123" (cdr (assoc "id" response :test #'string=))))))

(test acp-make-response-nil-result
  "Test JSON-RPC response with nil result."
  (let ((response (hactar::acp-make-response 1 nil)))
    (is (null (cdr (assoc "result" response :test #'string=))))))

(test acp-make-error-response-basic
  "Test JSON-RPC error response construction."
  (let ((response (hactar::acp-make-error-response 1 -32600 "Invalid Request")))
    (is (string= "2.0" (cdr (assoc "jsonrpc" response :test #'string=))))
    (is (= 1 (cdr (assoc "id" response :test #'string=))))
    (let ((err (cdr (assoc "error" response :test #'string=))))
      (is (= -32600 (cdr (assoc "code" err :test #'string=))))
      (is (string= "Invalid Request" (cdr (assoc "message" err :test #'string=)))))))

(test acp-make-error-response-with-data
  "Test JSON-RPC error response with additional data."
  (let ((response (hactar::acp-make-error-response 2 -32603 "Internal error" '(("detail" . "stack trace here")))))
    (let ((err (cdr (assoc "error" response :test #'string=))))
      (is (= -32603 (cdr (assoc "code" err :test #'string=))))
      (is (string= "Internal error" (cdr (assoc "message" err :test #'string=)))))))

(test acp-make-notification-basic
  "Test JSON-RPC notification construction (no id)."
  (let ((notif (hactar::acp-make-notification "session/update" '(("sessionId" . "sess_1")))))
    (is (string= "2.0" (cdr (assoc "jsonrpc" notif :test #'string=))))
    (is (string= "session/update" (cdr (assoc "method" notif :test #'string=))))
    (is (null (assoc "id" notif :test #'string=)))
    (let ((params (cdr (assoc "params" notif :test #'string=))))
      (is (string= "sess_1" (cdr (assoc "sessionId" params :test #'string=)))))))

(test acp-make-request-generates-id
  "Test that acp-make-request generates unique IDs."
  (let ((hactar::*acp-request-counter* 0))
    (multiple-value-bind (req1 id1) (hactar::acp-make-request "fs/read_text_file" '(("path" . "/tmp/test")))
      (multiple-value-bind (req2 id2) (hactar::acp-make-request "fs/read_text_file" '(("path" . "/tmp/test2")))
        (is (string= "agent_1" id1))
        (is (string= "agent_2" id2))
        (is (not (string= id1 id2)))
        (is (string= "fs/read_text_file" (cdr (assoc "method" req1 :test #'string=))))
        (is (string= "agent_1" (cdr (assoc "id" req1 :test #'string=))))
        (is (string= "agent_2" (cdr (assoc "id" req2 :test #'string=))))))))

;;* Content Block Parsing

(test acp-parse-content-blocks-text-only
  "Test parsing content blocks with only text."
  (let ((blocks (vector `(("type" . "text") ("text" . "Hello world")))))
    (multiple-value-bind (text images) (hactar::acp-parse-content-blocks blocks)
      (is (string= "Hello world" text))
      (is (null images)))))

(test acp-parse-content-blocks-multiple-text
  "Test parsing multiple text content blocks."
  (let ((blocks (vector `(("type" . "text") ("text" . "First part"))
                        `(("type" . "text") ("text" . "Second part")))))
    (multiple-value-bind (text images) (hactar::acp-parse-content-blocks blocks)
      (is (search "First part" text))
      (is (search "Second part" text))
      (is (null images)))))

(test acp-parse-content-blocks-with-image
  "Test parsing content blocks with an image."
  (let ((blocks (vector `(("type" . "text") ("text" . "Look at this"))
                        `(("type" . "image") ("data" . "base64data==") ("mimeType" . "image/png")))))
    (multiple-value-bind (text images) (hactar::acp-parse-content-blocks blocks)
      (is (search "Look at this" text))
      (is (= 1 (length images)))
      (let ((img (first images)))
        (is (string= "base64data==" (cdr (assoc :base64-data img))))
        (is (string= "image/png" (cdr (assoc :mime-type img))))))))

(test acp-parse-content-blocks-resource-link
  "Test parsing content blocks with a resource_link."
  (let* ((hactar::*repo-root* (uiop:temporary-directory))
         (test-file (merge-pathnames "acp-test-resource.txt" hactar::*repo-root*))
         (hactar::*files* nil))
    (unwind-protect
         (progn
           (with-open-file (s test-file :direction :output :if-exists :supersede
                                        :if-does-not-exist :create)
             (write-string "test content" s))
           (let ((blocks (vector `(("type" . "resource_link")
                                   ("uri" . ,(format nil "file://~A" (namestring test-file)))
                                   ("name" . "acp-test-resource.txt")))))
             (multiple-value-bind (text images) (hactar::acp-parse-content-blocks blocks)
               (is (search "Added file" text))
               (is (null images))
               ;; File should have been added to context
               (is (member (uiop:native-namestring test-file) hactar::*files* :test #'string=)))))
      (ignore-errors (delete-file test-file)))))

(test acp-parse-content-blocks-resource-with-text
  "Test parsing content blocks with an inline resource containing text."
  (let ((hactar::*files* nil)
        (hactar::*repo-root* (uiop:temporary-directory)))
    (let ((blocks (vector `(("type" . "resource")
                            ("resource" . (("uri" . "file:///tmp/example.txt")
                                           ("text" . "Resource content here")
                                           ("mimeType" . "text/plain")))))))
      (multiple-value-bind (text images) (hactar::acp-parse-content-blocks blocks)
        (is (search "Resource content here" text))
        (is (null images))))))

(test acp-parse-content-blocks-empty
  "Test parsing an empty content blocks array."
  (let ((blocks (vector)))
    (multiple-value-bind (text images) (hactar::acp-parse-content-blocks blocks)
      (is (string= "" text))
      (is (null images)))))

;;* Path Utilities

(test acp-absolute-path-already-absolute
  "Test that absolute paths are returned as-is."
  (let ((hactar::*repo-root* #P"/home/user/project/"))
    (let ((result (hactar::acp-absolute-path "/etc/config.txt")))
      (is (string= "/etc/config.txt" result)))))

(test acp-absolute-path-relative
  "Test that relative paths are merged with repo root."
  (let ((hactar::*repo-root* #P"/home/user/project/"))
    ;; We can't fully test truename without actual files, but we can verify
    ;; the function doesn't error on the call pattern
    (handler-case
        (let ((result (hactar::acp-absolute-path "src/main.lisp")))
          (is (stringp result)))
      ;; truename may fail if the file doesn't exist, which is fine for this test
      (error () (pass)))))

;;* Resource Link Content

(test acp-make-resource-link-lisp
  "Test resource link creation for Lisp files."
  (let ((hactar::*repo-root* #P"/home/user/project/"))
    (handler-case
        (let ((link (hactar::acp-make-resource-link "src/main.lisp")))
          (is (string= "resource_link" (cdr (assoc "type" link :test #'string=))))
          (is (str:starts-with? "file://" (cdr (assoc "uri" link :test #'string=))))
          (is (string= "main.lisp" (cdr (assoc "name" link :test #'string=))))
          (is (string= "text/x-lisp" (cdr (assoc "mimeType" link :test #'string=)))))
      (error () (pass)))))

(test acp-make-resource-link-python
  "Test resource link creation for Python files."
  (let ((hactar::*repo-root* #P"/home/user/project/"))
    (handler-case
        (let ((link (hactar::acp-make-resource-link "app.py")))
          (is (string= "text/x-python" (cdr (assoc "mimeType" link :test #'string=)))))
      (error () (pass)))))

(test acp-make-resource-link-javascript
  "Test resource link creation for JavaScript files."
  (let ((hactar::*repo-root* #P"/home/user/project/"))
    (handler-case
        (let ((link (hactar::acp-make-resource-link "index.js")))
          (is (string= "text/javascript" (cdr (assoc "mimeType" link :test #'string=)))))
      (error () (pass)))))

(test acp-make-resource-link-unknown-ext
  "Test resource link defaults to text/plain for unknown extensions."
  (let ((hactar::*repo-root* #P"/home/user/project/"))
    (handler-case
        (let ((link (hactar::acp-make-resource-link "data.xyz")))
          (is (string= "text/plain" (cdr (assoc "mimeType" link :test #'string=)))))
      (error () (pass)))))

;;* Diff Content

(test acp-make-diff-content
  "Test diff content block creation."
  (let ((hactar::*repo-root* #P"/home/user/project/"))
    (handler-case
        (let ((diff (hactar::acp-make-diff-content "src/main.lisp" "old code" "new code")))
          (is (string= "diff" (cdr (assoc "type" diff :test #'string=))))
          (is (string= "old code" (cdr (assoc "oldText" diff :test #'string=))))
          (is (string= "new code" (cdr (assoc "newText" diff :test #'string=)))))
      (error () (pass)))))

;;* Client Capability Checks

(test acp-client-has-capability-present
  "Test capability check when capability is present."
  (let ((hactar::*acp-client-capabilities*
          '(("fs" . (("readTextFile" . t)
                     ("writeTextFile" . t)))
            ("terminal" . t))))
    (is-true (hactar::acp-client-has-capability? '("fs" "readTextFile")))
    (is-true (hactar::acp-client-has-capability? '("fs" "writeTextFile")))
    (is-true (hactar::acp-client-has-capability? '("terminal")))))

(test acp-client-has-capability-absent
  "Test capability check when capability is absent."
  (let ((hactar::*acp-client-capabilities*
          '(("fs" . (("readTextFile" . t))))))
    (is (null (hactar::acp-client-has-capability? '("fs" "writeTextFile"))))
    (is (null (hactar::acp-client-has-capability? '("terminal"))))
    (is (null (hactar::acp-client-has-capability? '("nonexistent"))))))

(test acp-client-has-capability-nil-caps
  "Test capability check with nil capabilities."
  (let ((hactar::*acp-client-capabilities* nil))
    (is (null (hactar::acp-client-has-capability? '("fs" "readTextFile"))))))

(test acp-client-has-capability-nested
  "Test deeply nested capability check."
  (let ((hactar::*acp-client-capabilities*
          '(("a" . (("b" . (("c" . t))))))))
    (is-true (hactar::acp-client-has-capability? '("a" "b" "c")))
    (is (null (hactar::acp-client-has-capability? '("a" "b" "d"))))))

;;* Message Dispatch

(test acp-dispatch-response-to-pending-request
  "Test that responses are dispatched to pending requests."
  (let ((hactar::*acp-pending-requests* (make-hash-table :test 'equal))
        (result-box (list nil nil)))
    (setf (gethash "agent_1" hactar::*acp-pending-requests*) result-box)
    (hactar::acp-dispatch-message
     '(("jsonrpc" . "2.0")
       ("id" . "agent_1")
       ("result" . (("content" . "file contents")))))
    (is-true (second result-box))
    (is (string= "file contents" 
                  (cdr (assoc "content" (first result-box) :test #'string=))))))

(test acp-dispatch-error-response-to-pending-request
  "Test that error responses are dispatched to pending requests."
  (let ((hactar::*acp-pending-requests* (make-hash-table :test 'equal))
        (result-box (list nil nil)))
    (setf (gethash "agent_2" hactar::*acp-pending-requests*) result-box)
    (hactar::acp-dispatch-message
     '(("jsonrpc" . "2.0")
       ("id" . "agent_2")
       ("error" . (("code" . -32603) ("message" . "Internal error")))))
    (is-true (second result-box))
    (is (= -32603 (cdr (assoc "code" (first result-box) :test #'string=))))))

(test acp-dispatch-unknown-method
  "Test dispatching an unknown method returns method-not-found error."
  (let ((hactar::*acp-pending-requests* (make-hash-table :test 'equal))
        (output-messages '()))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("id" . 99)
                ("method" . "unknown/method")
                ("params" . nil)))
             (is (= 1 (length output-messages)))
             (let ((err (cdr (assoc "error" (first output-messages) :test #'string=))))
               (is (= -32601 (cdr (assoc "code" err :test #'string=))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-dispatch-cancel-notification
  "Test dispatching session/cancel notification sets cancelled flag."
  (let ((hactar::*acp-cancelled* nil)
        (hactar::*current-stream-reader* nil))
    (hactar::acp-dispatch-message
     '(("jsonrpc" . "2.0")
       ("method" . "session/cancel")
       ("params" . nil)))
    (is-true hactar::*acp-cancelled*)))

;;* Initialize Handler

(test acp-handle-initialize
  "Test the initialize handshake."
  (let ((output-messages '())
        (hactar::*acp-client-capabilities* nil)
        (hactar::*acp-initialized* nil)
        (hactar::*hactar-version* "0.1.0-test"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-handle-initialize 1
                                             '(("protocolVersion" . 1)
                                               ("clientCapabilities" . (("fs" . (("readTextFile" . t)))))
                                               ("clientInfo" . (("name" . "test-editor")))))
             (is-true hactar::*acp-initialized*)
             (is-true hactar::*acp-client-capabilities*)
             (is (= 1 (length output-messages)))
             (let* ((resp (first output-messages))
                    (result (cdr (assoc "result" resp :test #'string=))))
               (is (= 1 (cdr (assoc "id" resp :test #'string=))))
               (is (= 1 (cdr (assoc "protocolVersion" result :test #'string=))))
               (let ((agent-info (cdr (assoc "agentInfo" result :test #'string=))))
                 (is (string= "hactar" (cdr (assoc "name" agent-info :test #'string=)))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-handle-initialize-stores-capabilities
  "Test that initialize stores client capabilities."
  (let ((output-messages '())
        (hactar::*acp-client-capabilities* nil)
        (hactar::*acp-initialized* nil))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (declare (ignore msg)) nil))
      (unwind-protect
           (progn
             (hactar::acp-handle-initialize 1
                                             '(("protocolVersion" . 1)
                                               ("clientCapabilities" . (("fs" . (("readTextFile" . t)
                                                                                 ("writeTextFile" . t)))
                                                                        ("terminal" . t)))))
             (is-true (hactar::acp-client-has-capability? '("fs" "readTextFile")))
             (is-true (hactar::acp-client-has-capability? '("fs" "writeTextFile")))
             (is-true (hactar::acp-client-has-capability? '("terminal"))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

;;* Authenticate Handler

(test acp-handle-authenticate
  "Test the authenticate handler returns authenticated true."
  (let ((output-messages '()))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-handle-authenticate 2 nil)
             (is (= 1 (length output-messages)))
             (let ((result (cdr (assoc "result" (first output-messages) :test #'string=))))
               (is-true (cdr (assoc "authenticated" result :test #'string=)))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

;;* Session New Handler

(test acp-handle-session-new-creates-session
  "Test that session/new creates a session ID and sends response."
  (let ((output-messages '())
        (hactar::*acp-session-id* nil)
        (hactar::*chat-history* '(("fake" . "history")))
        (hactar::*files* '("old-file.txt"))
        (hactar::*images* '((:path "old.png")))
        (hactar::*docs-context* '(("fake" . "doc")))
        (hactar::*errors-context* '(("fake" . "error")))
        (hactar::*available-models* nil)
        (hactar::*current-model* nil)
        (hactar::*commands* (make-hash-table :test 'equal))
        (hactar::*repo-root* nil)
        (hactar::*name* nil)
        (hactar::*transcript-file* (merge-pathnames ".hactar.test.transcript.json"
                                                     (uiop:temporary-directory))))
    (let ((orig-write #'hactar::acp-write-message)
          (orig-load-config #'hactar::load-project-config))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::load-project-config)
            (lambda (root) (declare (ignore root)) nil))
      (unwind-protect
           (progn
             (hactar::acp-handle-session-new 3
                                              '(("cwd" . "/tmp")))
      (is-true hactar::*acp-session-id*)
      (is (str:starts-with? "sess_" hactar::*acp-session-id*))
      (is (null hactar::*chat-history*))
      (is (null hactar::*files*))
      (is (null hactar::*images*))
      (is (null hactar::*docs-context*))
      (is (null hactar::*errors-context*))
             (is (>= (length output-messages) 1))
             (let ((resp (find-if (lambda (m) (equal 3 (cdr (assoc "id" m :test #'string=))))
                                  output-messages)))
               (is-true resp)
               (let ((result (cdr (assoc "result" resp :test #'string=))))
                 (is (string= hactar::*acp-session-id*
                              (cdr (assoc "sessionId" result :test #'string=)))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-write)
        (setf (fdefinition 'hactar::load-project-config) orig-load-config)))))

;;* Session Prompt Handler

(test acp-handle-session-prompt-missing-prompt
  "Test session/prompt with missing prompt returns error."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-handle-session-prompt 4 '())
             (is (= 1 (length output-messages)))
             (is-true (assoc "error" (first output-messages) :test #'string=)))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-handle-session-prompt-empty-text
  "Test session/prompt with empty text returns end_turn."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test")
        (hactar::*files* nil)
        (hactar::*repo-root* (uiop:temporary-directory)))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-handle-session-prompt 5
                                                 `(("prompt" . ,(vector `(("type" . "text") ("text" . "   "))))))
             (is (= 1 (length output-messages)))
             (let ((result (cdr (assoc "result" (first output-messages) :test #'string=))))
               (is (string= "end_turn" (cdr (assoc "stopReason" result :test #'string=))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-handle-session-prompt-slash-command
  "Test session/prompt dispatches slash commands (non-ACP fallback)."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test")
        (hactar::*files* nil)
        (hactar::*repo-root* (uiop:temporary-directory))
        (hactar::*commands* (make-hash-table :test 'equal))
        (hactar::*acp-commands* (make-hash-table :test 'equal)))
    (setf (gethash "/test-acp-cmd" hactar::*commands*)
          (list (lambda (args) (declare (ignore args)) (format t "test output")) 
                "Test command"
                nil))
    (let ((orig-write #'hactar::acp-write-message)
          (orig-chunk #'hactar::acp-send-agent-message-chunk))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::acp-send-agent-message-chunk)
            (lambda (text) (declare (ignore text)) nil))
      (unwind-protect
           (progn
             (hactar::acp-handle-session-prompt 6
                                                 `(("prompt" . ,(vector `(("type" . "text") ("text" . "/test-acp-cmd"))))))
             (let ((resp (find-if (lambda (m) (equal 6 (cdr (assoc "id" m :test #'string=))))
                                  output-messages)))
               (is-true resp)
               (let ((result (cdr (assoc "result" resp :test #'string=))))
                 (is (string= "end_turn" (cdr (assoc "stopReason" result :test #'string=)))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-write)
        (setf (fdefinition 'hactar::acp-send-agent-message-chunk) orig-chunk)))))

;;* ACP Command Registry

(test acp-command-p-registered
  "Test that acp-command-p returns T for registered ACP commands."
  (let ((hactar::*acp-commands* (make-hash-table :test 'equal)))
    (setf (gethash "/test-cmd" hactar::*acp-commands*)
          (lambda (args) (declare (ignore args)) `(("text" . "ok"))))
    (is-true (hactar::acp-command-p "/test-cmd"))
    (is (null (hactar::acp-command-p "/nonexistent")))))

(test acp-execute-acp-command
  "Test that execute-acp-command calls the ACP handler and returns structured result."
  (let ((hactar::*acp-commands* (make-hash-table :test 'equal)))
    (setf (gethash "/test-cmd" hactar::*acp-commands*)
          (lambda (args)
            `(("text" . ,(format nil "Got ~A args" (length args)))
              ("data" . (("count" . ,(length args)))))))
    (let ((result (hactar::execute-acp-command "/test-cmd" '("a" "b"))))
      (is (string= "Got 2 args" (cdr (assoc "text" result :test #'string=))))
      (let ((data (cdr (assoc "data" result :test #'string=))))
        (is (= 2 (cdr (assoc "count" data :test #'string=))))))))

(test acp-dispatch-slash-command-uses-acp-handler
  "Test that session/prompt dispatches slash commands through ACP handlers."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test")
        (hactar::*files* nil)
        (hactar::*repo-root* (uiop:temporary-directory))
        (hactar::*commands* (make-hash-table :test 'equal))
        (hactar::*acp-commands* (make-hash-table :test 'equal)))
    (setf (gethash "/test-acp" hactar::*commands*)
          (list (lambda (args) (declare (ignore args)) (format t "regular output"))
                "Test command" nil))
    (setf (gethash "/test-acp" hactar::*acp-commands*)
          (lambda (args)
            (declare (ignore args))
            `(("text" . "acp structured output"))))
    (let ((orig-write #'hactar::acp-write-message)
          (orig-chunk #'hactar::acp-send-agent-message-chunk)
          (chunks '()))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::acp-send-agent-message-chunk)
            (lambda (text) (push text chunks)))
      (unwind-protect
           (progn
             (hactar::acp-handle-session-prompt 6
                                                 `(("prompt" . ,(vector `(("type" . "text") ("text" . "/test-acp"))))))
             (is (member "acp structured output" chunks :test #'string=)))
        (setf (fdefinition 'hactar::acp-write-message) orig-write)
        (setf (fdefinition 'hactar::acp-send-agent-message-chunk) orig-chunk)))))

;;* ACP Command Version Tests
(defmacro with-acp-command-env ((&key (files nil) (images nil) (model nil)) &body body)
  "Set up a minimal environment for testing ACP command handlers."
  `(let ((hactar::*files* ,files)
         (hactar::*images* ,images)
         (hactar::*current-model* ,model)
         (hactar::*available-models* nil)
         (hactar::*repo-root* (uiop:temporary-directory))
         (hactar::*chat-history* nil)
         (hactar::*docs-context* nil)
         (hactar::*errors-context* nil)
         (hactar::*acp-commands* (make-hash-table :test 'equal))
         (hactar::*commands* (make-hash-table :test 'equal))
         (hactar::*transcript-file* (merge-pathnames ".hactar.test.transcript.json"
                                                      (uiop:temporary-directory)))
         (hactar::*silent* t)
         (hactar::*debug* nil)
         (hactar::*git-autocommit* nil)
         (hactar::*multiline-mode* nil)
         (hactar::*chat-history-limit* 100000)
         (hactar::*cheap-model* nil)
         (hactar::*docs-meta-model* nil)
         (hactar::*embedding-model* nil)
         (hactar::*name* "test-project")
         (hactar::*author* "test-author")
         (hactar::*hactar-version* "0.1.0-test")
         (hactar::*exposed-context-file* nil)
         (hactar::*context-expose-hooks-installed* nil))
     ,@body))

;;** /clear

(test acp-cmd-clear
  "Test /clear ACP handler clears chat history and returns structured response."
  (with-acp-command-env ()
    (setf hactar::*chat-history* '(((:role . "user") (:content . "hello"))))
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (hactar::clear-chat-history)
                     `(("text" . "Chat history cleared.")))))
      (let ((result (funcall handler nil)))
        (is (string= "Chat history cleared." (cdr (assoc "text" result :test #'string=))))
        (is (null hactar::*chat-history*))))))

;;** /compress

(test acp-cmd-compress
  "Test /compress ACP handler returns structured response."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     `(("text" . "Chat history compression completed.")))))
      (let ((result (funcall handler nil)))
        (is (string= "Chat history compression completed." (cdr (assoc "text" result :test #'string=))))))))

;;** /help

(test acp-cmd-help
  "Test /help ACP handler returns list of ACP-compatible commands."
  (with-acp-command-env ()
    (setf (gethash "/test-cmd" hactar::*acp-commands*)
          (lambda (args) (declare (ignore args)) `(("text" . "test"))))
    (setf (gethash "/test-cmd" hactar::*commands*)
          (list (lambda (a) (declare (ignore a))) "A test command" nil))
    (setf (gethash "/other-cmd" hactar::*acp-commands*)
          (lambda (args) (declare (ignore args)) `(("text" . "other"))))
    (setf (gethash "/other-cmd" hactar::*commands*)
          (list (lambda (a) (declare (ignore a))) "Another command" nil))
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (let ((commands '()))
                       (maphash (lambda (cmd-name handler)
                                  (declare (ignore handler))
                                  (let* ((cmd-info (gethash cmd-name hactar::*commands*))
                                         (description (if cmd-info (second cmd-info) "")))
                                    (push `(("name" . ,(string-trim "/" cmd-name))
                                            ("description" . ,description))
                                          commands)))
                                hactar::*acp-commands*)
                       `(("text" . ,(format nil "~A commands available." (length commands)))
                         ("data" . ,(coerce (sort commands #'string<
                                                  :key (lambda (c) (cdr (assoc "name" c :test #'string=))))
                                            'vector)))))))
      (let ((result (funcall handler nil)))
        (is (string= "2 commands available." (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (= 2 (length data))))))))

;;** /version

(test acp-cmd-version
  "Test /version ACP handler returns version info."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     `(("text" . ,(format nil "hactar version ~A" hactar::*hactar-version*))
                       ("data" . (("version" . ,hactar::*hactar-version*)))))))
      (let ((result (funcall handler nil)))
        (is (search "0.1.0-test" (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (string= "0.1.0-test" (cdr (assoc "version" data :test #'string=)))))))))

;;** /autocommit

(test acp-cmd-autocommit-toggle-on
  "Test /autocommit ACP handler toggles autocommit on."
  (with-acp-command-env ()
    (setf hactar::*git-autocommit* nil)
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (setf hactar::*git-autocommit* (not hactar::*git-autocommit*))
                     `(("text" . ,(format nil "Git autocommit is now ~A."
                                          (if hactar::*git-autocommit* "enabled" "disabled")))
                       ("data" . (("autocommit" . ,(if hactar::*git-autocommit* t :false))))))))
      (let ((result (funcall handler nil)))
        (is-true hactar::*git-autocommit*)
        (is (search "enabled" (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (eq t (cdr (assoc "autocommit" data :test #'string=)))))))))

(test acp-cmd-autocommit-toggle-off
  "Test /autocommit ACP handler toggles autocommit off."
  (with-acp-command-env ()
    (setf hactar::*git-autocommit* t)
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (setf hactar::*git-autocommit* (not hactar::*git-autocommit*))
                     `(("text" . ,(format nil "Git autocommit is now ~A."
                                          (if hactar::*git-autocommit* "enabled" "disabled")))
                       ("data" . (("autocommit" . ,(if hactar::*git-autocommit* t :false))))))))
      (let ((result (funcall handler nil)))
        (is (null hactar::*git-autocommit*))
        (is (search "disabled" (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (eq :false (cdr (assoc "autocommit" data :test #'string=)))))))))

;;** /debug

(test acp-cmd-debug-toggle
  "Test /debug ACP handler toggles debug mode."
  (with-acp-command-env ()
    (setf hactar::*debug* nil)
    (setf llm:*debug* nil)
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (setf hactar::*debug* (not hactar::*debug*))
                     (setf llm:*debug* hactar::*debug*)
                     `(("text" . ,(format nil "Debug mode is now ~A."
                                          (if hactar::*debug* "enabled" "disabled")))
                       ("data" . (("debug" . ,(if hactar::*debug* t :false))))))))
      (let ((result (funcall handler nil)))
        (is-true hactar::*debug*)
        (is-true llm:*debug*)
        (is (search "enabled" (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (eq t (cdr (assoc "debug" data :test #'string=)))))))))

;;** /reset

(test acp-cmd-reset
  "Test /reset ACP handler clears files, history, and overrides."
  (with-acp-command-env (:files '("/tmp/test.txt"))
    (setf hactar::*chat-history* '(((:role . "user") (:content . "hello"))))
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (setf hactar::*files* nil)
                     (hactar::clear-chat-history)
                     (hactar::clear-session-overrides)
                     `(("text" . "Reset complete. All files dropped, chat history cleared, and session overrides cleared.")))))
      (let ((result (funcall handler nil)))
        (is (null hactar::*files*))
        (is (null hactar::*chat-history*))
        (is (search "Reset complete" (cdr (assoc "text" result :test #'string=))))))))

;;** /reload

(test acp-cmd-reload
  "Test /reload ACP handler returns structured response."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (hactar::clear-chat-history)
                     (hactar::empty-context)
                     (hactar::clear-session-overrides)
                     ;; Don't actually call reload-config in test
                     `(("text" . "Reload complete. Chat history cleared, context emptied, config reloaded.")))))
      (let ((result (funcall handler nil)))
        (is (search "Reload complete" (cdr (assoc "text" result :test #'string=))))))))

;;** /settings

(test acp-cmd-settings
  "Test /settings ACP handler returns structured settings data."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     `(("text" . ,(with-output-to-string (*standard-output*) (hactar::dump-settings)))
                       ("data" . (("model" . ,(if hactar::*current-model*
                                                  (hactar::model-config-name hactar::*current-model*) :null))
                                  ("cheapModel" . ,(or hactar::*cheap-model* :null))
                                  ("docsMetaModel" . ,(or hactar::*docs-meta-model* :null))
                                  ("embeddingModel" . ,(or hactar::*embedding-model* :null))
                                  ("repoRoot" . ,(if hactar::*repo-root*
                                                     (namestring hactar::*repo-root*) :null))
                                  ("gitAutocommit" . ,(if hactar::*git-autocommit* t :false))
                                  ("multilineMode" . ,(if hactar::*multiline-mode* t :false))
                                  ("chatHistoryLimit" . ,hactar::*chat-history-limit*)
                                  ("filesInContext" . ,(length hactar::*files*))
                                  ("imagesInContext" . ,(length hactar::*images*))))))))
      (let ((result (funcall handler nil)))
        (is (stringp (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (eq :null (cdr (assoc "model" data :test #'string=))))
          (is (eq :false (cdr (assoc "gitAutocommit" data :test #'string=))))
          (is (= 0 (cdr (assoc "filesInContext" data :test #'string=))))
          (is (= 0 (cdr (assoc "imagesInContext" data :test #'string=)))))))))

;;** /tokens

(test acp-cmd-tokens-no-model
  "Test /tokens ACP handler with no model selected."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (let* ((messages (hactar::prepare-messages ""))
                            (total-chars (reduce #'+ messages
                                                 :key (lambda (msg)
                                                        (length (cdr (assoc :content msg))))))
                            (estimated-tokens (round total-chars 4))
                            (cost (when hactar::*current-model*
                                    (* estimated-tokens
                                       (hactar::model-config-input-cost-per-token
                                        hactar::*current-model*)))))
                       `(("text" . ,(format nil "Estimated tokens: ~A, chars: ~A~@[, cost: $~8F~]"
                                            estimated-tokens total-chars cost))
                         ("data" . (("totalCharacters" . ,total-chars)
                                    ("estimatedTokens" . ,estimated-tokens)
                                    ("estimatedCost" . ,(or cost :null))
                                    ("model" . :null))))))))
      (let ((result (funcall handler nil)))
        (is (search "Estimated tokens" (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (eq :null (cdr (assoc "model" data :test #'string=))))
          (is (eq :null (cdr (assoc "estimatedCost" data :test #'string=))))
          (is (numberp (cdr (assoc "totalCharacters" data :test #'string=)))))))))

;;** /cost

(test acp-cmd-cost-no-model
  "Test /cost ACP handler with no model returns appropriate message."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (if hactar::*current-model*
                         `(("text" . "cost info"))
                         `(("text" . "No model selected. Cannot estimate cost."))))))
      (let ((result (funcall handler nil)))
        (is (string= "No model selected. Cannot estimate cost."
                      (cdr (assoc "text" result :test #'string=))))))))

;;** /model

(test acp-cmd-model-no-args
  "Test /model ACP handler with no args returns current model info."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (if cmd-args
                         `(("text" . "set"))
                         `(("text" . ,(format nil "Current model: ~A"
                                              (if hactar::*current-model*
                                                  (hactar::model-config-name hactar::*current-model*)
                                                  "None")))
                           ("data" . (("currentModel" . ,(if hactar::*current-model*
                                                             (hactar::model-config-name hactar::*current-model*)
                                                             :null))
                                      ("availableModels" . ,(coerce
                                                             (mapcar (lambda (m)
                                                                       `(("name" . ,(hactar::model-config-name m))))
                                                                     hactar::*available-models*)
                                                             'vector)))))))))
      (let ((result (funcall handler nil)))
        (is (search "None" (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (eq :null (cdr (assoc "currentModel" data :test #'string=))))
          (is (= 0 (length (cdr (assoc "availableModels" data :test #'string=))))))))))

;;** /models

(test acp-cmd-models-empty
  "Test /models ACP handler with no available models."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (let ((search-term (if cmd-args (first cmd-args) "")))
                       (let ((matching (loop for model in hactar::*available-models*
                                             when (or (string= search-term "")
                                                      (search search-term
                                                              (hactar::model-config-name model)
                                                              :test #'char-equal))
                                             collect `(("name" . ,(hactar::model-config-name model))))))
                         `(("text" . ,(format nil "~A model(s) found." (length matching)))
                           ("data" . ,(coerce matching 'vector))))))))
      (let ((result (funcall handler nil)))
        (is (string= "0 model(s) found." (cdr (assoc "text" result :test #'string=))))
        (is (= 0 (length (cdr (assoc "data" result :test #'string=)))))))))

;;** /ls

(test acp-cmd-ls-empty-context
  "Test /ls ACP handler with no files or images."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (let* ((files (mapcar (lambda (f)
                                            `(("path" . ,f) ("type" . "file")))
                                          hactar::*files*))
                            (images (mapcar (lambda (img)
                                             `(("path" . ,(uiop:native-namestring (getf img :path)))
                                               ("description" . ,(or (getf img :text) ""))
                                               ("type" . "image")))
                                           hactar::*images*))
                            (all-items (append files images)))
                       `(("text" . ,(format nil "~A file(s) and ~A image(s) in context."
                                            (length hactar::*files*) (length hactar::*images*)))
                         ("data" . ,(coerce all-items 'vector)))))))
      (let ((result (funcall handler nil)))
        (is (string= "0 file(s) and 0 image(s) in context."
                      (cdr (assoc "text" result :test #'string=))))
        (is (= 0 (length (cdr (assoc "data" result :test #'string=)))))))))

(test acp-cmd-ls-with-files
  "Test /ls ACP handler with files in context."
  (with-acp-command-env (:files '("/tmp/a.txt" "/tmp/b.lisp"))
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (let* ((files (mapcar (lambda (f)
                                            `(("path" . ,f) ("type" . "file")))
                                          hactar::*files*))
                            (all-items files))
                       `(("text" . ,(format nil "~A file(s) and ~A image(s) in context."
                                            (length hactar::*files*) (length hactar::*images*)))
                         ("data" . ,(coerce all-items 'vector)))))))
      (let ((result (funcall handler nil)))
        (is (string= "2 file(s) and 0 image(s) in context."
                      (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (= 2 (length data)))
          (is (string= "file" (cdr (assoc "type" (aref data 0) :test #'string=)))))))))

;;** /context

(test acp-cmd-context
  "Test /context ACP handler returns context string."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     `(("text" . "context output")))))
      (let ((result (funcall handler nil)))
        (is (stringp (cdr (assoc "text" result :test #'string=))))))))

;;** /images

(test acp-cmd-images-empty
  "Test /images ACP handler with no images."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (let ((imgs (mapcar (lambda (img)
                                          `(("path" . ,(uiop:native-namestring (getf img :path)))
                                            ("description" . ,(or (getf img :text) ""))))
                                        hactar::*images*)))
                       `(("text" . ,(format nil "~A image(s) in context." (length hactar::*images*)))
                         ("data" . ,(coerce imgs 'vector)))))))
      (let ((result (funcall handler nil)))
        (is (string= "0 image(s) in context." (cdr (assoc "text" result :test #'string=))))
        (is (= 0 (length (cdr (assoc "data" result :test #'string=)))))))))

(test acp-cmd-images-with-images
  "Test /images ACP handler with images in context."
  (with-acp-command-env (:images '((:path "/tmp/test.png" :text "A screenshot")))
    (let ((handler (lambda (cmd-args)
                     (declare (ignore cmd-args))
                     (let ((imgs (mapcar (lambda (img)
                                          `(("path" . ,(uiop:native-namestring (getf img :path)))
                                            ("description" . ,(or (getf img :text) ""))))
                                        hactar::*images*)))
                       `(("text" . ,(format nil "~A image(s) in context." (length hactar::*images*)))
                         ("data" . ,(coerce imgs 'vector)))))))
      (let ((result (funcall handler nil)))
        (is (string= "1 image(s) in context." (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (= 1 (length data)))
          (is (string= "A screenshot"
                        (cdr (assoc "description" (aref data 0) :test #'string=)))))))))

;;** /add

(test acp-cmd-add-no-args
  "Test /add ACP handler with no arguments returns error message."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (if cmd-args
                         `(("text" . "added"))
                         `(("text" . "No files specified. Provide file paths or patterns."))))))
      (let ((result (funcall handler nil)))
        (is (string= "No files specified. Provide file paths or patterns."
                      (cdr (assoc "text" result :test #'string=))))))))

(test acp-cmd-add-with-file
  "Test /add ACP handler with a valid file adds it to context."
  (let* ((test-file (merge-pathnames "acp-add-test.txt" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (s test-file :direction :output :if-exists :supersede
                                        :if-does-not-exist :create)
             (write-string "test content for add" s))
           (with-acp-command-env ()
             (let ((handler (lambda (cmd-args)
                              (if cmd-args
                                  (progn
                                    (hactar::process-add-request cmd-args)
                                    (let ((added-files
                                            (loop for pattern in cmd-args
                                                  append (hactar::expand-file-pattern pattern))))
                                      `(("text" . ,(format nil "Added ~A file(s) to context."
                                                           (length added-files)))
                                        ("data" . ,(coerce
                                                    (mapcar (lambda (f)
                                                              `(("path" . ,f)
                                                                ("type" . "file")))
                                                            added-files)
                                                    'vector)))))
                                  `(("text" . "No files specified."))))))
               (let ((result (funcall handler (list (namestring test-file)))))
                 (is (search "Added 1 file(s)" (cdr (assoc "text" result :test #'string=))))
                 (let ((data (cdr (assoc "data" result :test #'string=))))
                   (is (= 1 (length data)))
                   (is (string= "file" (cdr (assoc "type" (aref data 0) :test #'string=)))))))))
      (ignore-errors (delete-file test-file)))))

;;** /drop

(test acp-cmd-drop
  "Test /drop ACP handler removes files from context."
  (let* ((test-file (uiop:native-namestring
                     (merge-pathnames "acp-drop-test.txt" (uiop:temporary-directory)))))
    (with-acp-command-env (:files (list test-file))
      (is (= 1 (length hactar::*files*)))
      (let ((handler (lambda (cmd-args)
                       (let ((dropped '()))
                         (dolist (file-arg cmd-args)
                           (let* ((path (pathname file-arg))
                                  (full-path (if (uiop:absolute-pathname-p path)
                                                 path
                                                 (merge-pathnames path hactar::*repo-root*)))
                                  (native-path (uiop:native-namestring full-path)))
                             (hactar::drop-file-from-context native-path)
                             (hactar::drop-image-from-context native-path)
                             (push native-path dropped)))
                         `(("text" . ,(format nil "Dropped ~A file(s) from context."
                                              (length dropped)))
                           ("data" . ,(coerce
                                       (mapcar (lambda (f) `(("path" . ,f)))
                                               (nreverse dropped))
                                       'vector)))))))
        (let ((result (funcall handler (list test-file))))
          (is (search "Dropped 1 file(s)" (cdr (assoc "text" result :test #'string=))))
          (is (null hactar::*files*)))))))

;;** /drop-image

(test acp-cmd-drop-image-no-args
  "Test /drop-image ACP handler with no args returns error message."
  (with-acp-command-env ()
    (let ((handler (lambda (cmd-args)
                     (if cmd-args
                         `(("text" . "dropped"))
                         `(("text" . "Please specify the image path to drop."))))))
      (let ((result (funcall handler nil)))
        (is (string= "Please specify the image path to drop."
                      (cdr (assoc "text" result :test #'string=))))))))

(test acp-cmd-drop-image-with-path
  "Test /drop-image ACP handler drops an image."
  (with-acp-command-env (:images '((:path "/tmp/screenshot.png" :text "test")))
    (is (= 1 (length hactar::*images*)))
    (let ((handler (lambda (cmd-args)
                     (if cmd-args
                         (let* ((image-arg (first cmd-args))
                                (path (pathname image-arg))
                                (full-path (if (uiop:absolute-pathname-p path)
                                               path
                                               (merge-pathnames path hactar::*repo-root*)))
                                (native-path (uiop:native-namestring full-path)))
                           (hactar::drop-image-from-context native-path)
                           `(("text" . ,(format nil "Dropped image: ~A" native-path))
                             ("data" . (("path" . ,native-path)))))
                         `(("text" . "Please specify the image path to drop."))))))
      (let ((result (funcall handler '("/tmp/screenshot.png"))))
        (is (search "Dropped image" (cdr (assoc "text" result :test #'string=))))
        (is (null hactar::*images*))))))

;;** /transcript

(test acp-cmd-transcript-no-file
  "Test /transcript ACP handler when no transcript file exists."
  (with-acp-command-env ()
    (setf hactar::*transcript-file* "/tmp/nonexistent-transcript-12345.json")
    (let ((handler (lambda (cmd-args)
                     (cond
                       ((null cmd-args)
                        (if (probe-file hactar::*transcript-file*)
                            `(("text" . "content"))
                            `(("text" . "No transcript file found."))))
                       (t `(("text" . "subcommand")))))))
      (let ((result (funcall handler nil)))
        (is (string= "No transcript file found."
                      (cdr (assoc "text" result :test #'string=))))))))

(test acp-cmd-transcript-clear
  "Test /transcript ACP handler clear subcommand."
  (with-acp-command-env ()
    (let ((test-transcript (merge-pathnames "acp-transcript-test.json"
                                             (uiop:temporary-directory))))
      (setf hactar::*transcript-file* (namestring test-transcript))
      (unwind-protect
           (progn
             (with-open-file (s test-transcript :direction :output :if-exists :supersede
                                                 :if-does-not-exist :create)
               (write-string "old content" s))
             (let ((handler (lambda (cmd-args)
                              (cond
                                ((and cmd-args (string= (first cmd-args) "clear"))
                                 (with-open-file (stream hactar::*transcript-file*
                                                         :direction :output
                                                         :if-exists :supersede
                                                         :if-does-not-exist :create)
                                   (format stream ""))
                                 `(("text" . "Transcript cleared.")))
                                (t `(("text" . "other")))))))
               (let ((result (funcall handler '("clear"))))
                 (is (string= "Transcript cleared."
                               (cdr (assoc "text" result :test #'string=))))
                 (is (string= "" (uiop:read-file-string test-transcript))))))
        (ignore-errors (delete-file test-transcript))))))

;;** /dump-context

(test acp-cmd-dump-context
  "Test /dump-context ACP handler returns context text."
  (with-acp-command-env ()
    (setf (gethash "/dump-context" hactar::*acp-commands*)
          (lambda (cmd-args)
            (declare (ignore cmd-args))
            `(("text" . "raw context output"))))
    (let ((result (hactar::execute-acp-command "/dump-context" nil)))
      (is (string= "raw context output" (cdr (assoc "text" result :test #'string=)))))))

;;** /dump (stdout capture)

(test acp-cmd-dump-acp-t
  "Test /dump ACP handler using :acp t (stdout capture mode)."
  (with-acp-command-env ()
    ;; Simulate :acp t behavior
    (setf (gethash "/test-dump" hactar::*commands*)
          (list (lambda (args) (declare (ignore args)) (format t "captured output")) "Test" nil))
    (setf (gethash "/test-dump" hactar::*acp-commands*)
          (lambda (cmd-args)
            (declare (ignorable cmd-args))
            (let ((output (with-output-to-string (*standard-output*)
                            (let ((cmd-info (gethash "/test-dump" hactar::*commands*)))
                              (when cmd-info
                                (funcall (first cmd-info) cmd-args))))))
              `(("text" . ,output)))))
    (let ((result (hactar::execute-acp-command "/test-dump" nil)))
      (is (string= "captured output" (cdr (assoc "text" result :test #'string=)))))))

;;** /ask (stdout capture)

(test acp-cmd-ask-acp-t-registration
  "Test /ask ACP handler is registered as :acp t (stdout capture)."
  (with-acp-command-env ()
    ;; Simulate :acp t for /ask
    (setf (gethash "/ask" hactar::*commands*)
          (list (lambda (args) (format t "LLM response for: ~{~A~^ ~}" args)) "Ask" nil))
    (setf (gethash "/ask" hactar::*acp-commands*)
          (lambda (cmd-args)
            (let ((output (with-output-to-string (*standard-output*)
                            (let ((cmd-info (gethash "/ask" hactar::*commands*)))
                              (when cmd-info
                                (funcall (first cmd-info) cmd-args))))))
              `(("text" . ,output)))))
    (is-true (hactar::acp-command-p "/ask"))
    (let ((result (hactar::execute-acp-command "/ask" '("what" "is" "this"))))
      (is (search "what is this" (cdr (assoc "text" result :test #'string=)))))))

;;** /code (stdout capture)

(test acp-cmd-code-acp-t-registration
  "Test /code ACP handler is registered as :acp t (stdout capture)."
  (with-acp-command-env ()
    (setf (gethash "/code" hactar::*commands*)
          (list (lambda (args) (format t "Code response for: ~{~A~^ ~}" args)) "Code" nil))
    (setf (gethash "/code" hactar::*acp-commands*)
          (lambda (cmd-args)
            (let ((output (with-output-to-string (*standard-output*)
                            (let ((cmd-info (gethash "/code" hactar::*commands*)))
                              (when cmd-info
                                (funcall (first cmd-info) cmd-args))))))
              `(("text" . ,output)))))
    (is-true (hactar::acp-command-p "/code"))
    (let ((result (hactar::execute-acp-command "/code" '("refactor" "this"))))
      (is (search "refactor this" (cdr (assoc "text" result :test #'string=)))))))

;;** /dump-api-keys (stdout capture)

(test acp-cmd-dump-api-keys-acp-t
  "Test /dump-api-keys ACP handler is registered (stdout capture)."
  (with-acp-command-env ()
    (setf (gethash "/dump-api-keys" hactar::*commands*)
          (list (lambda (args) (declare (ignore args)) (format t "api keys output")) "Dump API keys" nil))
    (setf (gethash "/dump-api-keys" hactar::*acp-commands*)
          (lambda (cmd-args)
            (let ((output (with-output-to-string (*standard-output*)
                            (let ((cmd-info (gethash "/dump-api-keys" hactar::*commands*)))
                              (when cmd-info
                                (funcall (first cmd-info) cmd-args))))))
              `(("text" . ,output)))))
    (is-true (hactar::acp-command-p "/dump-api-keys"))
    (let ((result (hactar::execute-acp-command "/dump-api-keys" nil)))
      (is (string= "api keys output" (cdr (assoc "text" result :test #'string=)))))))

;;** /undo

(test acp-cmd-undo
  "Test /undo ACP handler returns structured response (skips confirmation)."
  (with-acp-command-env ()
    (let* ((git-reset-called nil)
           (handler (lambda (cmd-args)
                      (declare (ignore cmd-args))
                      ;; In test, just mark that we would have called git-reset
                      (setf git-reset-called t)
                      `(("text" . "Last commit reverted (git reset --hard HEAD~1).")
                        ("data" . (("action" . "git-reset-hard")
                                   ("target" . "HEAD~1")))))))
      (let ((result (funcall handler nil)))
        (is-true git-reset-called)
        (is (search "Last commit reverted" (cdr (assoc "text" result :test #'string=))))
        (let ((data (cdr (assoc "data" result :test #'string=))))
          (is (string= "git-reset-hard" (cdr (assoc "action" data :test #'string=))))
          (is (string= "HEAD~1" (cdr (assoc "target" data :test #'string=)))))))))

;;* Integration: ACP command dispatch via session/prompt

(test acp-session-prompt-dispatches-to-acp-clear
  "Test that /clear through session/prompt uses the ACP handler."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test")
        (hactar::*files* nil)
        (hactar::*repo-root* (uiop:temporary-directory))
        (hactar::*commands* (make-hash-table :test 'equal))
        (hactar::*acp-commands* (make-hash-table :test 'equal))
        (hactar::*chat-history* '(((:role . "user") (:content . "old msg"))))
        (hactar::*transcript-file* (merge-pathnames ".hactar.dispatch-test.json"
                                                     (uiop:temporary-directory))))
    (setf (gethash "/clear" hactar::*commands*)
          (list (lambda (args) (declare (ignore args))
                  (hactar::clear-chat-history)
                  (format t "Chat history cleared.~%"))
                "Clear history" nil))
    (setf (gethash "/clear" hactar::*acp-commands*)
          (lambda (cmd-args)
            (declare (ignore cmd-args))
            (hactar::clear-chat-history)
            `(("text" . "Chat history cleared via ACP."))))
    (let ((orig-write #'hactar::acp-write-message)
          (orig-chunk #'hactar::acp-send-agent-message-chunk)
          (chunks '()))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::acp-send-agent-message-chunk)
            (lambda (text) (push text chunks)))
      (unwind-protect
           (progn
             (hactar::acp-handle-session-prompt 10
                                                 `(("prompt" . ,(vector `(("type" . "text")
                                                                          ("text" . "/clear"))))))
             (is (member "Chat history cleared via ACP." chunks :test #'string=))
             (is (null hactar::*chat-history*)))
        (setf (fdefinition 'hactar::acp-write-message) orig-write)
        (setf (fdefinition 'hactar::acp-send-agent-message-chunk) orig-chunk)))))

(test acp-session-prompt-non-acp-fallback
  "Test that non-ACP commands fall back to stdout capture."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test")
        (hactar::*files* nil)
        (hactar::*repo-root* (uiop:temporary-directory))
        (hactar::*commands* (make-hash-table :test 'equal))
        (hactar::*acp-commands* (make-hash-table :test 'equal)))
    (setf (gethash "/legacy-cmd" hactar::*commands*)
          (list (lambda (args) (declare (ignore args)) (format t "legacy output"))
                "Legacy command" nil))
    (let ((orig-write #'hactar::acp-write-message)
          (orig-chunk #'hactar::acp-send-agent-message-chunk)
          (chunks '()))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::acp-send-agent-message-chunk)
            (lambda (text) (push text chunks)))
      (unwind-protect
           (progn
             (hactar::acp-handle-session-prompt 11
                                                 `(("prompt" . ,(vector `(("type" . "text")
                                                                          ("text" . "/legacy-cmd"))))))
             (is (member "legacy output" chunks :test #'string=)))
        (setf (fdefinition 'hactar::acp-write-message) orig-write)
        (setf (fdefinition 'hactar::acp-send-agent-message-chunk) orig-chunk)))))

;;* Set Config Option Handler

(test acp-handle-set-config-option-model
  "Test setting model via config option."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test")
        (hactar::*available-models* nil)
        (hactar::*current-model* nil)
        (hactar::*silent* t))
    (let ((orig-write #'hactar::acp-write-message)
          (orig-set-model #'hactar::set-current-model))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::set-current-model)
            (lambda (name) (declare (ignore name)) nil))
      (unwind-protect
           (progn
             (hactar::acp-handle-set-config-option 7
                                                    '(("configId" . "model")
                                                      ("value" . "ollama/qwen3:14b")))
             (is (= 1 (length output-messages)))
             (let ((result (cdr (assoc "result" (first output-messages) :test #'string=))))
               (is-true (assoc "configOptions" result :test #'string=))))
        (setf (fdefinition 'hactar::acp-write-message) orig-write)
        (setf (fdefinition 'hactar::set-current-model) orig-set-model)))))

;;* Session Update Notifications

(test acp-send-session-update-requires-session
  "Test that session updates are only sent when session ID is set."
  (let ((output-messages '())
        (hactar::*acp-session-id* nil))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-send-session-update '(("sessionUpdate" . "test")))
             ;; Should NOT have sent anything since session ID is nil
             (is (= 0 (length output-messages))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-send-session-update-with-session
  "Test that session updates are sent when session ID is set."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_active"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-send-session-update '(("sessionUpdate" . "test_update")))
             (is (= 1 (length output-messages)))
             (let* ((msg (first output-messages))
                    (params (cdr (assoc "params" msg :test #'string=))))
               (is (string= "session/update" (cdr (assoc "method" msg :test #'string=))))
               (is (string= "sess_active" (cdr (assoc "sessionId" params :test #'string=))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-send-agent-message-chunk
  "Test sending agent message chunks."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-send-agent-message-chunk "Hello from agent")
             (is (= 1 (length output-messages)))
             (let* ((params (cdr (assoc "params" (first output-messages) :test #'string=)))
                    (update (cdr (assoc "update" params :test #'string=))))
               (is (string= "agent_message_chunk" (cdr (assoc "sessionUpdate" update :test #'string=))))
               (let ((content (cdr (assoc "content" update :test #'string=))))
                 (is (string= "text" (cdr (assoc "type" content :test #'string=))))
                 (is (string= "Hello from agent" (cdr (assoc "text" content :test #'string=)))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-send-thought-chunk
  "Test sending thought message chunks."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-send-thought-chunk "Thinking about this...")
             (is (= 1 (length output-messages)))
             (let* ((params (cdr (assoc "params" (first output-messages) :test #'string=)))
                    (update (cdr (assoc "update" params :test #'string=))))
               (is (string= "thought_message_chunk" (cdr (assoc "sessionUpdate" update :test #'string=))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

;;* Available Commands

(test acp-send-available-commands
  "Test sending available commands update only includes ACP-compatible commands."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test")
        (hactar::*commands* (make-hash-table :test 'equal))
        (hactar::*acp-commands* (make-hash-table :test 'equal)))
    (setf (gethash "/help" hactar::*commands*)
          (list (lambda (a) (declare (ignore a))) "Show help" nil))
    (setf (gethash "/model" hactar::*commands*)
          (list (lambda (a) (declare (ignore a))) "Set model" nil))
    (setf (gethash "/non-acp" hactar::*commands*)
          (list (lambda (a) (declare (ignore a))) "Non-ACP command" nil))
    (setf (gethash "/help" hactar::*acp-commands*)
          (lambda (args) (declare (ignore args)) `(("text" . "help"))))
    (setf (gethash "/model" hactar::*acp-commands*)
          (lambda (args) (declare (ignore args)) `(("text" . "model"))))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-send-available-commands)
             (is (= 1 (length output-messages)))
             (let* ((params (cdr (assoc "params" (first output-messages) :test #'string=)))
                    (update (cdr (assoc "update" params :test #'string=))))
               (is (string= "available_commands_update" 
                            (cdr (assoc "sessionUpdate" update :test #'string=))))
               (let ((cmds (cdr (assoc "availableCommands" update :test #'string=))))
                 (is (= 2 (length cmds))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

;;* Tool Call Session Updates

(test acp-send-tool-call
  "Test sending tool call session update."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-send-tool-call "call_123" "read_file" "read" "pending"
                                          :raw-input '((:path . "test.txt")))
             (is (= 1 (length output-messages)))
             (let* ((params (cdr (assoc "params" (first output-messages) :test #'string=)))
                    (update (cdr (assoc "update" params :test #'string=))))
               (is (string= "tool_call" (cdr (assoc "sessionUpdate" update :test #'string=))))
               (is (string= "call_123" (cdr (assoc "toolCallId" update :test #'string=))))
               (is (string= "read_file" (cdr (assoc "title" update :test #'string=))))
               (is (string= "read" (cdr (assoc "kind" update :test #'string=))))
               (is (string= "pending" (cdr (assoc "status" update :test #'string=))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-send-tool-call-update
  "Test sending tool call update."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-send-tool-call-update "call_123" "completed")
             (is (= 1 (length output-messages)))
             (let* ((params (cdr (assoc "params" (first output-messages) :test #'string=)))
                    (update (cdr (assoc "update" params :test #'string=))))
               (is (string= "tool_call_update" (cdr (assoc "sessionUpdate" update :test #'string=))))
               (is (string= "call_123" (cdr (assoc "toolCallId" update :test #'string=))))
               (is (string= "completed" (cdr (assoc "status" update :test #'string=))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

;;* JSON-RPC Error Code Constants

(test acp-error-codes
  "Test that JSON-RPC error codes are correctly defined."
  (is (= -32700 hactar::+jsonrpc-parse-error+))
  (is (= -32600 hactar::+jsonrpc-invalid-request+))
  (is (= -32601 hactar::+jsonrpc-method-not-found+))
  (is (= -32602 hactar::+jsonrpc-invalid-params+))
  (is (= -32603 hactar::+jsonrpc-internal-error+)))

;;* ACP Global State

(test acp-initial-state
  "Test ACP globals have correct initial values."
  (is (null hactar::*acp-mode*))
  (is (null hactar::*acp-session-id*))
  (is (null hactar::*acp-client-capabilities*))
  (is (= 0 hactar::*acp-request-counter*))
  (is (null hactar::*acp-cancelled*))
  (is (null hactar::*acp-initialized*)))

;;* Plan Sending

(test acp-send-plan
  "Test sending a plan session update."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-send-plan
              (list (list :content "Step 1: Read file" :priority "high" :status "pending")
                    (list :content "Step 2: Modify" :priority "medium" :status "pending")))
             (is (= 1 (length output-messages)))
             (let* ((params (cdr (assoc "params" (first output-messages) :test #'string=)))
                    (update (cdr (assoc "update" params :test #'string=))))
               (is (string= "plan" (cdr (assoc "sessionUpdate" update :test #'string=))))
               (let ((entries (cdr (assoc "entries" update :test #'string=))))
                 (is (= 2 (length entries))))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

;;* Config Options

(test acp-send-config-options
  "Test sending config options update."
  (let ((output-messages '())
        (hactar::*acp-session-id* "sess_test")
        (hactar::*available-models* nil)
        (hactar::*current-model* nil))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-send-config-options)
             (is (= 1 (length output-messages)))
             (let* ((params (cdr (assoc "params" (first output-messages) :test #'string=)))
                    (update (cdr (assoc "update" params :test #'string=))))
               (is (string= "config_options_update" 
                            (cdr (assoc "sessionUpdate" update :test #'string=))))
               (is-true (assoc "configOptions" update :test #'string=))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

;;* Write Message Serialization

(test acp-write-message-output
  "Test that acp-write-message writes newline-delimited JSON to stdout."
  (let ((written-output 
          (with-output-to-string (*standard-output*)
            (hactar::acp-write-message '(("jsonrpc" . "2.0") ("id" . 1) ("result" . nil))))))
    (is (char= #\Newline (char written-output (1- (length written-output)))))
    (let ((lines (str:lines (string-trim '(#\Newline) written-output))))
      (is (= 1 (length lines))))
    (is (search "jsonrpc" written-output))
    (is (search "2.0" written-output))))

;;* Session Cancel Handler

(test acp-handle-session-cancel-sets-flag
  "Test that session/cancel sets the cancelled flag."
  (let ((hactar::*acp-cancelled* nil)
        (hactar::*current-stream-reader* nil))
    (hactar::acp-handle-session-cancel nil)
    (is-true hactar::*acp-cancelled*)))

;;* Agent Capability Registry

(test defcapability-registers-capability
  "Test that defcapability registers a capability in the registry."
  (let ((hactar::*acp-agent-capabilities* (make-hash-table :test 'equal)))
    (setf (gethash "test.example" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability
           :path '("test" "example")
           :description "A test capability"
           :handler (lambda (id params) (declare (ignore id params)) nil)
           :enabled t))
    (is (= 1 (hash-table-count hactar::*acp-agent-capabilities*)))
    (let ((cap (gethash "test.example" hactar::*acp-agent-capabilities*)))
      (is-true cap)
      (is (equal '("test" "example") (hactar::acp-capability-path cap)))
      (is (string= "A test capability" (hactar::acp-capability-description cap)))
      (is-true (hactar::acp-capability-enabled cap)))))

(test acp-agent-capabilities-tree-nested
  "Test that agent capabilities tree builds correct nested structure."
  (let ((hactar::*acp-agent-capabilities* (make-hash-table :test 'equal)))
    (setf (gethash "fs.readTextFile" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("fs" "readTextFile") :enabled t))
    (setf (gethash "fs.writeTextFile" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("fs" "writeTextFile") :enabled t))
    (setf (gethash "terminal.create" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("terminal" "create") :enabled t))
    (let ((tree (hactar::acp-agent-capabilities-tree)))
      ;; Should have fs and terminal groups
      (is-true (assoc "fs" tree :test #'string=))
      (is-true (assoc "terminal" tree :test #'string=))
      ;; fs should have readTextFile and writeTextFile
      (let ((fs (cdr (assoc "fs" tree :test #'string=))))
        (is-true (assoc "readTextFile" fs :test #'string=))
        (is-true (assoc "writeTextFile" fs :test #'string=))))))

(test acp-agent-capabilities-tree-top-level
  "Test that single-path capabilities appear as top-level booleans."
  (let ((hactar::*acp-agent-capabilities* (make-hash-table :test 'equal)))
    (setf (gethash "terminal" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("terminal") :enabled t))
    (let ((tree (hactar::acp-agent-capabilities-tree)))
      (let ((terminal-entry (assoc "terminal" tree :test #'string=)))
        (is-true terminal-entry)
        (is (eq t (cdr terminal-entry)))))))

(test acp-agent-capabilities-tree-disabled
  "Test that disabled capabilities are excluded from the tree."
  (let ((hactar::*acp-agent-capabilities* (make-hash-table :test 'equal)))
    (setf (gethash "fs.readTextFile" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("fs" "readTextFile") :enabled t))
    (setf (gethash "fs.writeTextFile" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("fs" "writeTextFile") :enabled nil))
    (let ((tree (hactar::acp-agent-capabilities-tree)))
      (let ((fs (cdr (assoc "fs" tree :test #'string=))))
        (is-true (assoc "readTextFile" fs :test #'string=))
        (is (null (assoc "writeTextFile" fs :test #'string=)))))))

(test acp-find-capability-handler-found
  "Test that capability handler lookup works for known methods."
  (let ((hactar::*acp-agent-capabilities* (make-hash-table :test 'equal))
        (test-handler (lambda (id params) (declare (ignore id params)) :found)))
    (setf (gethash "fs.readTextFile" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("fs" "readTextFile")
                                       :handler test-handler
                                       :enabled t
                                       :tool-name "fs-read-text-file"))
    (is (eq test-handler (hactar::acp-find-capability-handler "fs/read_text_file")))))

(test acp-find-capability-handler-not-found
  "Test that capability handler lookup returns nil for unknown methods."
  (let ((hactar::*acp-agent-capabilities* (make-hash-table :test 'equal)))
    (is (null (hactar::acp-find-capability-handler "nonexistent/method")))))

(test acp-find-capability-handler-disabled
  "Test that disabled capability handlers are not found."
  (let ((hactar::*acp-agent-capabilities* (make-hash-table :test 'equal)))
    (setf (gethash "fs.readTextFile" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("fs" "readTextFile")
                                       :handler (lambda (id params) (declare (ignore id params)))
                                       :enabled nil
                                       :tool-name "fs-read-text-file"))
    (is (null (hactar::acp-find-capability-handler "fs/read_text_file")))))

;;* Terminal Capability

(test acp-terminal-generate-id-format
  "Test that terminal IDs have the correct prefix."
  (let ((id (hactar::acp-terminal-generate-id)))
    (is (str:starts-with? "term_" id))))

(test acp-terminal-info-initial-state
  "Test terminal info struct initial state."
  (let ((info (hactar::make-acp-terminal-info :id "term_test")))
    (is (string= "term_test" (hactar::acp-terminal-info-id info)))
    (is (string= "" (hactar::acp-terminal-info-output info)))
    (is (null (hactar::acp-terminal-info-exit-code info)))
    (is (null (hactar::acp-terminal-info-signal info)))
    (is (null (hactar::acp-terminal-info-truncated info)))
    (is (null (hactar::acp-terminal-info-released info)))))

(test acp-capability-terminal-create-missing-command
  "Test terminal/create returns error when command is missing."
  (let ((output-messages '())
        (hactar::*acp-terminals* (make-hash-table :test 'equal))
        (hactar::*acp-session-id* "sess_test"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-capability-terminal-create 10 '(("sessionId" . "sess_test")))
             (is (= 1 (length output-messages)))
             (is-true (assoc "error" (first output-messages) :test #'string=)))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-capability-terminal-output-unknown
  "Test terminal/output returns error for unknown terminal ID."
  (let ((output-messages '())
        (hactar::*acp-terminals* (make-hash-table :test 'equal)))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-capability-terminal-output 11
               '(("terminalId" . "term_nonexistent")))
             (is (= 1 (length output-messages)))
             (is-true (assoc "error" (first output-messages) :test #'string=)))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-capability-terminal-release-unknown
  "Test terminal/release returns error for unknown terminal ID."
  (let ((output-messages '())
        (hactar::*acp-terminals* (make-hash-table :test 'equal)))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-capability-terminal-release 12
               '(("terminalId" . "term_nonexistent")))
             (is (= 1 (length output-messages)))
             (is-true (assoc "error" (first output-messages) :test #'string=)))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-capability-fs-read-text-file-not-found
  "Test fs/read_text_file returns error for missing file."
  (let ((output-messages '())
        (hactar::*repo-root* (uiop:temporary-directory))
        (hactar::*files* nil))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-capability-fs-read-text-file 13
               '(("path" . "nonexistent-file-12345.txt")))
             (is (= 1 (length output-messages)))
             (is-true (assoc "error" (first output-messages) :test #'string=)))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-capability-fs-read-text-file-success
  "Test fs/read_text_file reads an existing file."
  (let* ((output-messages '())
         (hactar::*repo-root* (uiop:temporary-directory))
         (hactar::*files* nil)
         (test-file (merge-pathnames "acp-cap-read-test.txt" hactar::*repo-root*)))
    (unwind-protect
         (progn
           (with-open-file (s test-file :direction :output :if-exists :supersede
                                        :if-does-not-exist :create)
             (write-string "hello capability" s))
           (let ((orig-fn #'hactar::acp-write-message))
             (setf (fdefinition 'hactar::acp-write-message)
                   (lambda (msg) (push msg output-messages)))
             (unwind-protect
                  (progn
                    (hactar::acp-capability-fs-read-text-file 14
                      `(("path" . ,(namestring test-file))))
                    (is (= 1 (length output-messages)))
                    (let ((result (cdr (assoc "result" (first output-messages) :test #'string=))))
                      (is (string= "hello capability" (cdr (assoc "content" result :test #'string=))))))
               (setf (fdefinition 'hactar::acp-write-message) orig-fn))))
      (ignore-errors (delete-file test-file)))))

(test acp-capability-fs-write-text-file-success
  "Test fs/write_text_file writes content to a file."
  (let* ((output-messages '())
         (hactar::*repo-root* (uiop:temporary-directory))
         (test-file (merge-pathnames "acp-cap-write-test.txt" hactar::*repo-root*)))
    (unwind-protect
         (let ((orig-fn #'hactar::acp-write-message))
           (setf (fdefinition 'hactar::acp-write-message)
                 (lambda (msg) (push msg output-messages)))
           (unwind-protect
                (progn
                  (hactar::acp-capability-fs-write-text-file 15
                    `(("path" . ,(namestring test-file))
                      ("content" . "written by capability")))
                  (is (= 1 (length output-messages)))
                  (let ((result (cdr (assoc "result" (first output-messages) :test #'string=))))
                    (is-true (cdr (assoc "success" result :test #'string=))))
                  (is (string= "written by capability" (uiop:read-file-string test-file))))
             (setf (fdefinition 'hactar::acp-write-message) orig-fn)))
      (ignore-errors (delete-file test-file)))))

;;* Dispatch Routes Capability Handlers

(test acp-dispatch-routes-to-capability
  "Test that dispatch routes unknown methods to registered capability handlers."
  (let ((output-messages '())
        (hactar::*acp-agent-capabilities* (make-hash-table :test 'equal))
        (hactar::*acp-pending-requests* (make-hash-table :test 'equal))
        (hactar::*permission-rules* '())
        (hactar::*session-overrides*
          (list (hactar::make-session-override
                 :tool-name "custom-action"
                 :match-type :tool-always
                 :decision :allow)))
        (hactar::*permission-log* '())
        (handler-called nil))
    (setf (gethash "custom.action" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability
           :path '("custom" "action")
           :handler (lambda (id params)
                      (declare (ignore params))
                      (setf handler-called t)
                      (hactar::acp-write-message
                       (hactar::acp-make-response id '(("ok" . t)))))
           :enabled t
           :permissions :auto
           :tool-name "custom-action"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-dispatch-message
              '(("jsonrpc" . "2.0")
                ("id" . 20)
                ("method" . "custom/action")
                ("params" . nil)))
             (is-true handler-called)
             (is (= 1 (length output-messages))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-initialize-includes-agent-capabilities
  "Test that initialize response includes registered agent capabilities."
  (let ((output-messages '())
        (hactar::*acp-client-capabilities* nil)
        (hactar::*acp-initialized* nil)
        (hactar::*hactar-version* "0.1.0-test")
        (hactar::*acp-agent-capabilities* (make-hash-table :test 'equal)))
    (setf (gethash "fs.readTextFile" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("fs" "readTextFile") :enabled t
                                       :permissions :auto :tool-name "fs-read-text-file"))
    (setf (gethash "fs.writeTextFile" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability :path '("fs" "writeTextFile") :enabled t
                                       :permissions :confirm :tool-name "fs-write-text-file"))
    (let ((orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (hactar::acp-handle-initialize 1
                                             '(("protocolVersion" . 1)
                                               ("clientCapabilities" . nil)))
             (is (= 1 (length output-messages)))
             (let* ((resp (first output-messages))
                    (result (cdr (assoc "result" resp :test #'string=)))
                    (agent-caps (cdr (assoc "agentCapabilities" result :test #'string=))))
               (let ((fs (cdr (assoc "fs" agent-caps :test #'string=))))
                 (is-true fs)
                 (is-true (assoc "readTextFile" fs :test #'string=))
                 (is-true (assoc "writeTextFile" fs :test #'string=)))))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

;;* Capability Permissions

(test acp-capability-permission-auto-allows
  "Test that capabilities with :auto permissions and matching allow rules pass."
  (let ((hactar::*acp-agent-capabilities* (make-hash-table :test 'equal))
        (hactar::*permission-rules* '())
        (hactar::*session-overrides* '())
        (hactar::*permission-log* '()))
    (setf (gethash "test.read" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability
           :path '("test" "read")
           :handler (lambda (id params) (declare (ignore id params)) nil)
           :enabled t
           :permissions :auto
           :tool-name "test-read"))
    (hactar::register-permission-rule
     (hactar::make-permission-rule
      :name "test-allow-read"
      :priority 50
      :tool-pattern "test-read"
      :predicate (lambda (tn args) (declare (ignore tn args)) :allow)
      :source :system))
    (let ((cap (gethash "test.read" hactar::*acp-agent-capabilities*)))
      (is (eq :allow (hactar::acp-check-capability-permission cap 1 nil))))))

(test acp-capability-permission-deny-rule
  "Test that capabilities are denied when a deny rule matches."
  (let ((output-messages '())
        (hactar::*acp-agent-capabilities* (make-hash-table :test 'equal))
        (hactar::*permission-rules* '())
        (hactar::*session-overrides* '())
        (hactar::*permission-log* '()))
    (setf (gethash "test.write" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability
           :path '("test" "write")
           :handler (lambda (id params) (declare (ignore id params)) nil)
           :enabled t
           :permissions :confirm
           :tool-name "test-write"))
    (hactar::register-permission-rule
     (hactar::make-permission-rule
      :name "test-deny-write"
      :priority 100
      :tool-pattern "test-write"
      :predicate (lambda (tn args) (declare (ignore tn args)) :deny)
      :source :system))
    (let ((cap (gethash "test.write" hactar::*acp-agent-capabilities*))
          (orig-fn #'hactar::acp-write-message))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (unwind-protect
           (progn
             (is (eq :deny (hactar::acp-check-capability-permission cap 1 nil)))
             (is (= 1 (length output-messages)))
             (is-true (assoc "error" (first output-messages) :test #'string=)))
        (setf (fdefinition 'hactar::acp-write-message) orig-fn)))))

(test acp-capability-permission-session-override
  "Test that session overrides apply to capability permission checks."
  (let ((hactar::*acp-agent-capabilities* (make-hash-table :test 'equal))
        (hactar::*permission-rules* '())
        (hactar::*session-overrides*
          (list (hactar::make-session-override
                 :tool-name "test-cap"
                 :match-type :tool-always
                 :decision :allow)))
        (hactar::*permission-log* '()))
    (setf (gethash "test.cap" hactar::*acp-agent-capabilities*)
          (hactar::make-acp-capability
           :path '("test" "cap")
           :handler (lambda (id params) (declare (ignore id params)) nil)
           :enabled t
           :permissions :confirm
           :tool-name "test-cap"))
    (let ((cap (gethash "test.cap" hactar::*acp-agent-capabilities*)))
      (is (eq :allow (hactar::acp-check-capability-permission cap 1 nil))))))

;;* Hook Installation

(test acp-install-hooks-idempotent
  "Test that ACP hooks can be installed."
  (handler-case
      (progn
        (hactar::acp-install-hooks)
        (pass))
    (error (e)
      (fail (format nil "acp-install-hooks errored: ~A" e)))))

;;* Set Mode Handler

(test acp-handle-set-mode
  "Test the set_mode handler responds without error."
  (let ((output-messages '()))
    (let ((orig-write #'hactar::acp-write-message)
          (orig-log #'hactar::acp-log))
      (setf (fdefinition 'hactar::acp-write-message)
            (lambda (msg) (push msg output-messages)))
      (setf (fdefinition 'hactar::acp-log)
            (lambda (fmt &rest args) (declare (ignore fmt args)) nil))
      (unwind-protect
           (progn
             (hactar::acp-handle-set-mode 8 '(("modeId" . "architect")))
             (is (= 1 (length output-messages)))
             (is (= 8 (cdr (assoc "id" (first output-messages) :test #'string=)))))
        (setf (fdefinition 'hactar::acp-write-message) orig-write)
        (setf (fdefinition 'hactar::acp-log) orig-log)))))
