;;* OpenRouter-compatible LLM Proxy
;; Intercepts requests, injects Hactar context, and forwards to upstream.
(in-package :hactar)

;;** Proxy Hooks

;; Hook type: handler receives a request plist and returns a (possibly modified) request plist.
;; The request plist has keys: :messages (vector/list of message alists),
;;                             :model (string),
;;                             :system-prompt (string or nil),
;;                             :raw-body (the full parsed JSON hash-table),
;;                             :headers (alist of request headers)
;; Handlers should return the (possibly modified) request plist.
;; Use combine-composed-hook so handlers compose (each gets previous handler's output).
(nhooks:define-hook-type proxy-request (function (list) list)
  "Hook run on each proxy request before forwarding. Handlers receive and return a request plist.")

(defvar *proxy-request-hook* (make-instance 'hook-proxy-request
                                            :combination #'nhooks:combine-composed-hook)
  "Hook for modifying proxy requests. Handlers are composed: each receives the output of the previous.")

;; Hook for modifying the proxy response before returning to the client.
(nhooks:define-hook-type proxy-response (function (list list) list)
  "Hook run on proxy response. Handlers receive (response-plist request-plist) and return response-plist.")

(defvar *proxy-response-hook* (make-instance 'hook-proxy-response)
  "Hook for inspecting/modifying proxy responses before returning to client.")

;;** Proxy Request Processing

(defun proxy-extract-system-prompt (messages)
  "Extract system prompt from messages array/list. Returns (values system-prompt-string remaining-messages)."
  (let ((system-prompt nil)
        (remaining '()))
    (loop for msg across (if (vectorp messages) messages (coerce messages 'vector))
          do (let ((role (or (gethash "role" msg)
                             (cdr (assoc :role msg))
                             (cdr (assoc :|role| msg)))))
               (if (string-equal role "system")
                   (let ((content (or (gethash "content" msg)
                                      (cdr (assoc :content msg))
                                      (cdr (assoc :|content| msg)))))
                     (setf system-prompt
                           (if system-prompt
                               (format nil "~A~%~%~A" system-prompt content)
                               content)))
                   (push msg remaining))))
    (values system-prompt (nreverse remaining))))

(defun proxy-build-context-section ()
  "Build the Hactar context string for injection into proxy requests.
   This includes files, docs, rules, stack info, etc."
  (let ((sections '()))
    ;; Active rules
    (when (> (hash-table-count *active-rules*) 0)
      (push (with-output-to-string (s)
              (format s "## Active Rules~%~%")
              (maphash (lambda (name text)
                         (format s "### ~A~%~A~%~%" name text))
                       *active-rules*))
            sections))

    ;; Stack info
    (when *stack*
      (push (format nil "## Technology Stack~%~{- ~A~%~}" *stack*) sections))

    ;; Files in context
    (when *files*
      (push (with-output-to-string (s)
              (format s "## Files in Context~%~%")
              (dolist (file *files*)
                (let* ((content (get-file-content file))
                       (extension (pathname-type (pathname file)))
                       (lang (or (ignore-errors (get-language-hint-from-extension extension)) ""))
                       (rel (uiop:native-namestring (uiop:enough-pathname file *repo-root*))))
                  (when content
                    (format s "```~A ~A~%~A~%```~%~%" lang rel content)))))
            sections))

    ;; Docs in context
    (when *docs-context*
      (push (with-output-to-string (s)
              (format s "## Documentation~%~%")
              (dolist (doc *docs-context*)
                (format s "### ~A~%~A~%~%"
                        (cdr (assoc :title doc))
                        (or (cdr (assoc :content doc)) ""))))
            sections))

    ;; Entity rules (from features/BOT)
    (let ((entity-rules (get-entity-rules-section)))
      (when (and entity-rules (> (length entity-rules) 0))
        (push (format nil "## Entity Rules~%~%~A" entity-rules) sections)))

    ;; Feature rules
    (let ((feature-rules (get-feature-rules-section)))
      (when (and feature-rules (> (length feature-rules) 0))
        (push (format nil "## Feature Rules~%~%~A" feature-rules) sections)))

    (if sections
        (format nil "# Hactar Context~%~%~{~A~%~}" (nreverse sections))
        "")))

(defun proxy-inject-context (request-plist)
  "Default proxy hook handler: injects Hactar context into the system prompt."
  (let* ((system-prompt (getf request-plist :system-prompt))
         (context (proxy-build-context-section))
         (new-system-prompt (if (and system-prompt (> (length system-prompt) 0))
                                (if (> (length context) 0)
                                    (format nil "~A~%~%~A" system-prompt context)
                                    system-prompt)
                                (if (> (length context) 0)
                                    context
                                    system-prompt))))
    (setf (getf request-plist :system-prompt) new-system-prompt)
    request-plist))

;; Install the default context injector as the base hook handler.
(nhooks:add-hook *proxy-request-hook*
                 (make-instance 'nhooks:handler
                                :fn #'proxy-inject-context
                                :name 'proxy-inject-context))

;;** Proxy Request Forwarding

(defun proxy-reassemble-messages (system-prompt messages)
  "Reassemble messages with system prompt for forwarding to upstream API."
  (let ((result '()))
    (when (and system-prompt (> (length system-prompt) 0))
      (let ((sys-msg (make-hash-table :test 'equal)))
        (setf (gethash "role" sys-msg) "system")
        (setf (gethash "content" sys-msg) system-prompt)
        (push sys-msg result)))
    (dolist (msg messages)
      (push msg result))
    (coerce (nreverse result) 'vector)))

(defun proxy-forward-request (request-plist)
  "Forward the processed request to the upstream LLM provider.
   Returns the raw Drakma response values for streaming or full body."
  (let* ((raw-body (getf request-plist :raw-body))
         (system-prompt (getf request-plist :system-prompt))
         (messages (getf request-plist :messages))
         (upstream-url (or *proxy-upstream-url*
                           "https://openrouter.ai/api/v1/chat/completions"))
         (stream-p (gethash "stream" raw-body))
         ;; Reassemble messages with injected system prompt
         (final-messages (proxy-reassemble-messages system-prompt messages)))

    ;; Replace messages in the raw body
    (setf (gethash "messages" raw-body) final-messages)

    (let* ((shasht:*write-alist-as-object* t)
           (shasht:*write-plist-as-object* t)
           (payload (shasht:write-json raw-body nil))
           (upstream-headers '())
           (request-headers (getf request-plist :headers)))

      ;; Forward relevant headers (Authorization, etc.)
      (dolist (header-pair request-headers)
        (let ((name (car header-pair))
              (value (cdr header-pair)))
          (when (member name '("authorization" "http-referer" "x-title"
                               "openrouter-transforms")
                        :test #'string-equal)
            (push (cons name value) upstream-headers))))

      ;; If no Authorization header was forwarded, try our own API key
      (unless (assoc "authorization" upstream-headers :test #'string-equal)
        (when llm:*openrouter-api-key*
          (push (cons "Authorization"
                      (format nil "Bearer ~A" llm:*openrouter-api-key*))
                upstream-headers)))

      (debug-log "Proxy forwarding to:" upstream-url)
      (debug-log "Proxy payload length:" (length payload))

      (if stream-p
          ;; Streaming: return the raw stream
          (multiple-value-bind (stream status-code resp-headers)
              (drakma:http-request upstream-url
                                   :method :post
                                   :content-type "application/json"
                                   :content payload
                                   :additional-headers upstream-headers
                                   :connection-timeout llm:*read-timeout*
                                   :external-format-out :utf-8
                                   :external-format-in :utf-8
                                   :want-stream t)
            (values stream status-code resp-headers :stream))
          ;; Non-streaming: return body
          (multiple-value-bind (body status-code resp-headers)
              (drakma:http-request upstream-url
                                   :method :post
                                   :content-type "application/json"
                                   :content payload
                                   :additional-headers upstream-headers
                                   :connection-timeout llm:*read-timeout*
                                   :external-format-out :utf-8
                                   :external-format-in :utf-8)
            (values body status-code resp-headers :body))))))

;;** Proxy Ningle Route Handler

(defun proxy-extract-request-headers (env)
  "Extract relevant HTTP headers from the Lack environment plist."
  (let ((headers '()))
    (loop for (key val) on env by #'cddr
          when (and (keywordp key)
                    (str:starts-with-p "HTTP-" (symbol-name key)))
          do (let* ((header-name (string-downcase
                                  (substitute #\- #\_ 
                                              (subseq (symbol-name key) 5)))))
               (push (cons header-name val) headers)))
    ;; Also check for content-type
    (let ((ct (getf env :content-type)))
      (when ct (push (cons "content-type" ct) headers)))
    headers))

(defun handle-proxy-request (params env)
  "Handle an incoming proxy request: parse, run hooks, forward, return response."
  (declare (ignore params))
  (handler-case
      (let* ((request-body-bytes (lack.request:request-content ningle:*request*))
             (request-body-str (babel:octets-to-string request-body-bytes :encoding :utf-8))
             (json-body (shasht:read-json request-body-str))
             (messages-raw (gethash "messages" json-body))
             (model (gethash "model" json-body))
             (request-headers (proxy-extract-request-headers env)))

        (debug-log "Proxy received request for model:" model)

        ;; Extract system prompt from messages
        (multiple-value-bind (system-prompt non-system-messages)
            (proxy-extract-system-prompt messages-raw)

          ;; Build request plist for hooks
          (let* ((request-plist (list :messages non-system-messages
                                      :model (or model "")
                                      :system-prompt system-prompt
                                      :raw-body json-body
                                      :headers request-headers))
                 ;; Run proxy request hooks (composed)
                 (processed-plist (nhooks:run-hook *proxy-request-hook* request-plist)))

            ;; Forward to upstream
            (multiple-value-bind (response status-code resp-headers response-type)
                (proxy-forward-request processed-plist)

              (case response-type
                (:stream
                 ;; Streaming response: read from upstream and relay
                 (let* ((flexi-stream (flexi-streams:make-flexi-stream response :external-format :utf-8))
                        (collected-chunks (make-string-output-stream)))
                   ;; For streaming, we need to collect and relay.
                   ;; Ningle/Lack doesn't natively support streaming responses easily,
                   ;; so we collect the full streamed response and return it.
                   ;; A production proxy would use a streaming-capable server.
                   (handler-case
                       (loop for line = (read-line flexi-stream nil nil)
                             while line
                             do (write-string line collected-chunks)
                                (write-char #\Newline collected-chunks))
                     (end-of-file () nil)
                     (error (e) (debug-log "Proxy stream read error:" e)))
                   (ignore-errors (close response))
                   (let ((body-str (get-output-stream-string collected-chunks)))
                     (list (or status-code 200)
                           '(:content-type "text/event-stream"
                             :cache-control "no-cache"
                             :transfer-encoding "chunked")
                           (list body-str)))))

                (:body
                 ;; Non-streaming response
                 (let ((body-str (if (stringp response)
                                     response
                                     (babel:octets-to-string response :encoding :utf-8))))
                   (list (or status-code 200)
                         '(:content-type "application/json")
                         (list body-str))))

                (t
                 '(502 (:content-type "application/json")
                   ("{\"error\": \"Bad gateway: unexpected response type from upstream.\"}"))))))))
    (error (e)
      (format t "~&Proxy error: ~A~%" e)
      (debug-log "Proxy error:" e)
      `(500 (:content-type "application/json")
        (,(format nil "{\"error\": \"Proxy internal error: ~A\"}"
                  (substitute #\" #\' (format nil "~A" e))))))))

;;** Proxy Route Registration

(defun register-proxy-routes ()
  "Register the proxy routes on the Ningle app."
  ;; OpenRouter/OpenAI compatible chat completions endpoint
  (setf (ningle:route *app* "/v1/chat/completions" :method :POST)
        (lambda (params)
          (handle-proxy-request params (lack.request:request-env ningle:*request*))))

  ;; Also support /chat/completions without the /v1 prefix
  (setf (ningle:route *app* "/chat/completions" :method :POST)
        (lambda (params)
          (handle-proxy-request params (lack.request:request-env ningle:*request*))))

  ;; Models endpoint - proxy through to upstream
  (setf (ningle:route *app* "/v1/models" :method :GET)
        (lambda (params)
          (declare (ignore params))
          (handler-case
              (let* ((upstream-url (format nil "~A/../models"
                                           (or *proxy-upstream-url*
                                               "https://openrouter.ai/api/v1/chat/completions")))
                     (models-url (cl-ppcre:regex-replace "/chat/completions/../models" upstream-url "/models"))
                     (headers '()))
                ;; Try to forward auth
                (when llm:*openrouter-api-key*
                  (push (cons "Authorization"
                              (format nil "Bearer ~A" llm:*openrouter-api-key*))
                        headers))
                (multiple-value-bind (body status-code)
                    (drakma:http-request models-url
                                         :method :get
                                         :additional-headers headers
                                         :external-format-in :utf-8)
                  (let ((body-str (if (stringp body)
                                      body
                                      (babel:octets-to-string body :encoding :utf-8))))
                    (list (or status-code 200)
                          '(:content-type "application/json")
                          (list body-str)))))
            (error (e)
              `(502 (:content-type "application/json")
                (,(format nil "{\"error\": \"Failed to fetch models: ~A\"}"
                          (substitute #\" #\' (format nil "~A" e))))))))))

;;** Proxy Start/Stop Commands

(defun start-proxy (&key (port *proxy-port*))
  "Start the LLM proxy server. Registers routes and starts the HTTP server if needed."
  (register-proxy-routes)
  (unless *silent*
    (format t "~&Proxy routes registered on HTTP server.~%")
    (format t "~&Proxy endpoint: http://localhost:~A/v1/chat/completions~%" (or *http-port* port))
    (format t "~&Set your client's base URL to: http://localhost:~A/v1~%" (or *http-port* port)))
  ;; Ensure HTTP server is running
  (unless *http-server*
    (start-http-server :port (or *http-port* port)))
  t)

(define-command proxy.start (args)
  "Start the OpenRouter-compatible LLM proxy."
  (declare (ignore args))
  (start-proxy)
  :acp (lambda (cmd-args)
         (declare (ignore cmd-args))
         (start-proxy)
         `(("text" . ,(format nil "Proxy started on http://localhost:~A/v1" *http-port*))
           ("data" . (("port" . ,*http-port*)
                      ("endpoint" . ,(format nil "http://localhost:~A/v1/chat/completions" *http-port*)))))))

(define-command proxy.status (args)
  "Show proxy status and configuration."
  (declare (ignore args))
  (format t "Proxy Status:~%")
  (format t "  HTTP Server: ~A~%" (if *http-server* "Running" "Stopped"))
  (format t "  Port: ~A~%" *http-port*)
  (format t "  Upstream URL: ~A~%" (or *proxy-upstream-url* "https://openrouter.ai/api/v1/chat/completions"))
  (format t "  Request Hook Handlers: ~A~%" (length (nhooks:handlers *proxy-request-hook*)))
  (format t "  Response Hook Handlers: ~A~%" (length (nhooks:handlers *proxy-response-hook*)))
  (format t "  Files in Context: ~A~%" (length *files*))
  (format t "  Active Rules: ~A~%" (hash-table-count *active-rules*))
  (format t "  Stack: ~{~A~^, ~}~%" *stack*)
  :acp (lambda (cmd-args)
         (declare (ignore cmd-args))
         `(("text" . ,(format nil "Proxy on port ~A, ~A handler(s)" *http-port*
                              (length (nhooks:handlers *proxy-request-hook*))))
           ("data" . (("running" . ,(if *http-server* t :false))
                      ("port" . ,*http-port*)
                      ("upstreamUrl" . ,(or *proxy-upstream-url* "https://openrouter.ai/api/v1/chat/completions"))
                      ("hookHandlers" . ,(length (nhooks:handlers *proxy-request-hook*)))
                      ("filesInContext" . ,(length *files*))
                      ("activeRules" . ,(hash-table-count *active-rules*))
                      ("stack" . ,(coerce *stack* 'vector)))))))

;;** Utility: Detect content in messages

(defun proxy-messages-contain-p (messages &rest search-strings)
  "Return T if any message content in MESSAGES contains any of SEARCH-STRINGS (case-insensitive)."
  (let ((msgs (if (vectorp messages) (coerce messages 'list) messages)))
    (dolist (msg msgs)
      (let ((content (or (gethash "content" msg)
                         (cdr (assoc :content msg))
                         (cdr (assoc :|content| msg)))))
        (when (stringp content)
          (let ((lower-content (string-downcase content)))
            (dolist (s search-strings)
              (when (search (string-downcase s) lower-content)
                (return-from proxy-messages-contain-p t)))))))
    nil))

(defun proxy-model-matches-p (model &rest patterns)
  "Return T if MODEL string matches any of the PATTERNS (case-insensitive substring)."
  (when (and model (stringp model))
    (let ((lower-model (string-downcase model)))
      (dolist (p patterns)
        (when (search (string-downcase p) lower-model)
          (return-from proxy-model-matches-p t))))
    nil))
