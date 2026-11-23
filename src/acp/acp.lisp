(in-package #:hactar-acp)

;;; Agent Client Protocol (ACP) adapter over hactar-jsonrpc and hactar-core.

(defstruct acp-session
  id
  cwd
  (mode "default")
  model
  cancelled-p)

(defvar *acp-capabilities* (make-hash-table :test 'equal)
  "Extensible ACP capability handlers: method-name -> function of (params session server).")

(defun register-acp-capability (method handler)
  "Register a custom ACP JSON-RPC method handler."
  (setf (gethash method *acp-capabilities*) handler))

(defun %field (name alist)
  (when (listp alist)
    (cdr (assoc name alist :test #'string-equal :key #'string))))

(defun parse-content-blocks (blocks)
  "Extract concatenated text from ACP prompt content blocks."
  (with-output-to-string (out)
    (let ((seq (if (vectorp blocks) (coerce blocks 'list) blocks)))
      (dolist (block seq)
        (let ((type (%field "type" block))
              (text (%field "text" block)))
          (when (and (or (null type) (string-equal type "text"))
                     (stringp text))
            (write-string text out)))))))

(defun send-session-update (output session-id update &key (framing :newline))
  "Emit an ACP session/update notification on OUTPUT."
  (when output
    (hactar-jsonrpc:write-message
     (hactar-jsonrpc:make-notification
      "session/update"
      `(("sessionId" . ,session-id)
        ("update" . ,update)))
     output
     :framing framing)))

(defun %empty-object ()
  (make-hash-table :test 'equal))

(defun make-acp-server (&key (input *standard-input*) (output *standard-output*)
                             (framing :newline))
  "Create a configured JSON-RPC server speaking ACP against hactar-core."
  (let ((server (hactar-jsonrpc:make-server :input input :output output :framing framing))
        (sessions (make-hash-table :test 'equal))
        (counter 0)
        (active-session-id nil))
    (hactar-jsonrpc:register-method
     server "initialize"
     (lambda (params)
       (declare (ignore params))
       `(("protocolVersion" . 1)
         ("agentInfo" . (("name" . "hactar") ("version" . "0.2.0")))
         ("agentCapabilities" . (("loadSession" . nil)
                                 ("promptCapabilities" . (("embeddedContext" . t))))))))
    (hactar-jsonrpc:register-method
     server "authenticate"
     (lambda (params) (declare (ignore params)) (%empty-object)))
    (hactar-jsonrpc:register-method
     server "session/new"
     (lambda (params)
       (incf counter)
       (let* ((sid (format nil "session-~D" counter))
              (cwd (or (%field "cwd" params)
                       (namestring (or hactar-core:*repo-root* (uiop:getcwd)))))
              (session (make-acp-session :id sid :cwd cwd
                                         :model hactar-core:*current-model*)))
         (setf (gethash sid sessions) session
               active-session-id sid)
         `(("sessionId" . ,sid)
           ("modes" . (("currentModeId" . "default")
                       ("availableModes" . #((("id" . "default") ("name" . "Default"))))))
           ("models" . (("currentModelId" . ,hactar-core:*current-model*)))))))
    (hactar-jsonrpc:register-method
     server "session/prompt"
     (lambda (params)
       (let* ((sid (or (%field "sessionId" params) active-session-id))
              (session (and sid (gethash sid sessions))))
         (unless session
           (error 'hactar-jsonrpc:rpc-error
                  :code hactar-jsonrpc:+invalid-params+
                  :message (format nil "Unknown ACP session: ~A" sid)))
         (let* ((text (parse-content-blocks (%field "prompt" params)))
                (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
                (reply
                  (if (and (plusp (length trimmed)) (char= (char trimmed 0) #\/))
                      (let* ((parts (remove "" (uiop:split-string (subseq trimmed 1)
                                                                  :separator '(#\Space #\Tab))
                                            :test #'string=))
                             (res (hactar-core:dispatch parts)))
                        (hactar-core:render-result res :text))
                      (hactar-core:result-message (hactar-core:chat-turn trimmed)))))
           (send-session-update
            output sid
            `(("sessionUpdate" . "agent_message_chunk")
              ("content" . (("type" . "text") ("text" . ,reply))))
            :framing framing)
           `(("stopReason" . "end_turn"))))))
    (hactar-jsonrpc:register-method
     server "session/set_mode"
     (lambda (params)
       (let* ((sid (or (%field "sessionId" params) active-session-id))
              (session (and sid (gethash sid sessions)))
              (mode-id (%field "modeId" params)))
         (when (and session mode-id)
           (setf (acp-session-mode session) mode-id))
         (%empty-object))))
    (hactar-jsonrpc:register-method
     server "session/set_config_option"
     (lambda (params)
       (let* ((sid (or (%field "sessionId" params) active-session-id))
              (session (and sid (gethash sid sessions)))
              (config-id (%field "configId" params))
              (value (%field "value" params)))
         (when (and session (string-equal config-id "model") value)
           (setf (acp-session-model session) value
                 hactar-core:*current-model* value))
         (%empty-object))))
    (hactar-jsonrpc:register-method
     server "session/cancel"
     (lambda (params)
       (let* ((sid (or (%field "sessionId" params) active-session-id))
              (session (and sid (gethash sid sessions))))
         (when session
           (setf (acp-session-cancelled-p session) t))
         nil)))
    (maphash (lambda (method handler)
               (hactar-jsonrpc:register-method
                server method
                (lambda (params)
                  (let ((session (and active-session-id
                                      (gethash active-session-id sessions))))
                    (funcall handler params session server)))))
             *acp-capabilities*)
    server))

(defun serve-acp (&key (input *standard-input*) (output *standard-output*)
                       (framing :newline))
  "Run the ACP JSON-RPC loop until EOF."
  (hactar-jsonrpc:serve (make-acp-server :input input :output output :framing framing)))
