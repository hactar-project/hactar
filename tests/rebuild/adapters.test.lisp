(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

;;; ---- hactar-acp ----
(test acp-initialize-session-and-prompt-over-streams
  (with-temp-project (root)
    (let* ((hactar-rebuild:*auto-update* nil)
           (hactar-rebuild:*mock-llm-output* "acp reply")
           (requests
             (with-output-to-string (s)
               (hactar-jsonrpc:write-message
                (hactar-jsonrpc:make-request 1 "initialize") s)
               (hactar-jsonrpc:write-message
                (hactar-jsonrpc:make-request 2 "session/new"
                                             `(("cwd" . ,(namestring root))))
                s)
               (hactar-jsonrpc:write-message
                (hactar-jsonrpc:make-request
                 3 "session/prompt"
                 '(("sessionId" . "session-1")
                   ("prompt" . #((("type" . "text") ("text" . "hello"))))))
                s)))
           (out (make-string-output-stream)))
      (hactar-acp:serve-acp :input (make-string-input-stream requests) :output out)
      (let* ((lines (remove "" (uiop:split-string (get-output-stream-string out)
                                                  :separator '(#\Newline))
                            :test #'string=))
             (msgs (mapcar #'hactar-jsonrpc:decode-message lines)))
        (is (= 4 (length msgs)))
        (is (= 1 (cdr (assoc "id" (first msgs) :test #'string=))))
        (is (string= "session-1"
                     (cdr (assoc "sessionId"
                                 (cdr (assoc "result" (second msgs) :test #'string=))
                                 :test #'string=))))
        (is (string= "session/update"
                     (cdr (assoc "method" (third msgs) :test #'string=))))
        (is (string= "end_turn"
                     (cdr (assoc "stopReason"
                                 (cdr (assoc "result" (fourth msgs) :test #'string=))
                                 :test #'string=))))))))

;;; ---- hactar-mcp ----
(test mcp-server-exposes-tools-and-client-registers-deftools
  (with-temp-project (root)
    (hactar-rebuild:deftool multiply (a b) "Multiply two numbers" (* a b))
    (let* ((req-stream
             (with-output-to-string (s)
               (hactar-jsonrpc:write-message
                (hactar-jsonrpc:make-request 1 "initialize") s)
               (hactar-jsonrpc:write-message
                (hactar-jsonrpc:make-request 2 "tools/list") s)
               (hactar-jsonrpc:write-message
                (hactar-jsonrpc:make-request
                 3 "tools/call"
                 '(("name" . "multiply")
                   ("arguments" . (("a" . 6) ("b" . 7)))))
                s)))
           (resp-stream (make-string-output-stream)))
      (hactar-mcp:serve-mcp :input (make-string-input-stream req-stream)
                            :output resp-stream)
      (let* ((lines (remove "" (uiop:split-string (get-output-stream-string resp-stream)
                                                  :separator '(#\Newline))
                            :test #'string=))
             (msgs (mapcar #'hactar-jsonrpc:decode-message lines))
             (call-result (cdr (assoc "result" (third msgs) :test #'string=))))
        (is (= 3 (length msgs)))
        (is (search "42" (format nil "~S" call-result)))))
    ;; Client side against scripted server responses
    (let* ((server-replies
             (with-output-to-string (s)
               (hactar-jsonrpc:write-message
                (hactar-jsonrpc:make-response 1 '(("protocolVersion" . "2024-11-05"))) s)
               (hactar-jsonrpc:write-message
                (hactar-jsonrpc:make-response
                 2 '(("tools" . #((("name" . "ping") ("description" . "Ping remote"))))))
                s)
               (hactar-jsonrpc:write-message
                (hactar-jsonrpc:make-response
                 3 '(("content" . #((("type" . "text") ("text" . "pong"))))
                     ("isError" . nil)))
                s)))
           (out (make-string-output-stream))
           (client (hactar-mcp:connect-mcp :input (make-string-input-stream server-replies)
                                           :output out)))
      (let ((registered (hactar-mcp:mcp-tools->deftools client :prefix "ext")))
        (is (equal '("ext.ping") registered))
        (let ((r (hactar-rebuild:run-tool "ext.ping")))
          (is (eq :ok (hactar-rebuild:result-status r)))
          (is (string= "pong" (hactar-rebuild:result-message r))))))))

;;; ---- hactar-http ----
(test http-app-dispatches-completes-and-enforces-localhost
  (with-temp-project (root)
    (let ((app (hactar-http:make-http-app))
          (hactar-rebuild:*mock-llm-output* "http-ok"))
      (let ((health (funcall app '(:request-method :get :path-info "/health"))))
        (is (= 200 (first health))))
      (let ((remote (funcall app '(:request-method :get :path-info "/health"
                                   :remote-addr "10.0.0.5"))))
        (is (= 403 (first remote))))
      (let ((comp (funcall app '(:request-method :post :path-info "/api/complete"
                                 :body-string "{\"text\":\"hello\"}"))))
        (is (= 200 (first comp)))
        (is (search "http-ok" (first (third comp))))))))

;;; ---- hactar-proxy ----
(test proxy-app-injects-context-and-forwards
  (with-temp-project (root)
    (hactar-rebuild:clear-history)
    (setf hactar-rebuild:*stack* '("common-lisp"))
    (let* ((captured-payload nil)
           (hactar-proxy:*proxy-forward-fn*
             (lambda (url headers payload)
               (declare (ignore url headers))
               (setf captured-payload payload)
               (values "{\"id\":\"1\",\"choices\":[{\"message\":{\"content\":\"proxied\"}}]}"
                       200 nil)))
           (app (hactar-proxy:make-proxy-app))
           (resp (funcall app
                          '(:request-method :post
                            :path-info "/v1/chat/completions"
                            :body-string "{\"model\":\"gpt-4o\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}"))))
      (is (= 200 (first resp)))
      (is (search "proxied" (first (third resp))))
      (is (search "common-lisp" captured-payload)))))
