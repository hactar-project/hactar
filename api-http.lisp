(in-package :hactar)
(defvar *app* (make-instance 'ningle:app)
              "The Ningle application instance.")

;;* middleware
(defun localhost-only-middleware (app)
  "Middleware to reject requests not originating from localhost."
  (lambda (env)
    (let ((remote-addr (getf env :remote-addr)))
      (if (or (string= remote-addr "127.0.0.1")
              (string= remote-addr "::1"))
        (funcall app env)
        (progn
          (debug-log "Rejected request from non-localhost address:" remote-addr)
          '(403 (:content-type "text/plain") ("Forbidden: Access allowed only from localhost.")))))))
;;* core
(defun write-port-file (port)
  "Writes the port number to the .hactar.port file using UTF-8."
  (let ((port-file (merge-pathnames ".hactar.port" *repo-root*)))
    (handler-case
        (with-open-file (stream port-file :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (format stream "~A~%" port))
      (error (e)
        (format t "~&Warning: Could not write port file ~A: ~A~%" port-file e)))))

(defun start-http-server (&key (port *http-port*))
  "Starts the Clack server with Woo, checking port availability."
  (if *http-server*
    (progn
      (unless *silent* (format t "HTTP server already running on port ~A.~%" *http-port*))
      nil)
    (progn
      ;; Check initial port availability
      (unless (is-port-available-p port)
        (unless *silent* (format t "Warning: Requested port ~A is already in use.~%" port))
        (let ((original-port port))
          (loop for p from (1+ port) to 65535
                do (when (is-port-available-p p)
                     (setf port p)
                     (unless *silent* (format t "Found available port: ~A. Using this instead.~%" port))
                     (return))
                finally (progn
                          (unless *silent* (format t "Error: Could not find an available port starting from ~A.~%" (1+ original-port)))
                          (return-from start-http-server nil)))))

      (setf *http-port* port)

      (unless *silent* (format t "Attempting to start HTTP server on http://localhost:~A~%" *http-port*))
      (handler-case
          (progn
            (setf *http-server*
                  (clack:clackup
                    *app*
                    ;; (funcall #'localhost-only-middleware *app*)
                    :server :woo
                    :port *http-port*
                    ;; :use-thread t
                    ))
            (write-port-file *http-port*)
            (unless *silent* (format t "HTTP server started successfully.~%"))
            *http-server*)
        (error (e)
          (unless *silent* (format t "~&Error starting HTTP server on port ~A: ~A~%" *http-port* e))
          (setf *http-server* nil))))))

(defun stop-http-server ()
  "Stops the running Clack server."
  (when *http-server*
    (format t "Stopping HTTP server...~%")
    (handler-case (clack:stop *http-server*)
      (error (e)
        (format t "~&Error stopping HTTP server: ~A~%" e)))
    (setf *http-server* nil)
    (let ((port-file (merge-pathnames ".hactar.port" *repo-root*)))
      (when (probe-file port-file)
        (ignore-errors (delete-file port-file))))
    (format t "HTTP server stopped.~%")))
;;* routes
(setf (ningle:route *app* "/api/complete" :method :POST)
      #'(lambda (params)
          (declare (ignore params))
          (let* ((request-body (babel:octets-to-string (lack.request:request-content ningle:*request*)))
                 (json-body (when request-body
                              (handler-case
                                  (shasht:read-json request-body)
                                (error (e)
                                  (format t "Error parsing JSON body: ~A~%" e)
                                  (debug-log "Error parsing JSON body:" e)
                                 nil))))
                 (input-text (gethash "text" json-body)))
            (cond
              ((not *completion-model*)
                '(500 (:content-type "application/json")
                  ("{\"error\": \"Completion model not configured. Use /set-completion-model command.\"}")))
              ((not input-text)
                '(400 (:content-type "application/json")
                  ("{\"error\": \"Missing 'text' field in JSON body or invalid JSON.\"}")))
              (t
                (handler-case
                    (let* ((prompt-template (get-prompt 'complete-text "complete-text.mustache"))
                           (prompt (mustache:render* prompt-template `((:text . ,input-text))))
                           (provider-type (intern (string-upcase (model-config-provider *completion-model*)) :keyword))
                           ;; Use the *completion-model* for this specific task
                           (completion
                             (normalize-completion
                               (llm:complete provider-type
                                             (prepare-messages prompt)
                                             :model (model-config-model-name *completion-model*)
                                             :max-tokens (model-config-max-output-tokens *completion-model*)
                                             :system-prompt (system-prompt)
                                             :max-context (model-config-max-input-tokens *completion-model*)
                                             :extra-headers (when (model-config-extra-params *completion-model*)
                                                              (gethash "extra_headers" (model-config-extra-params *completion-model*)))
                                             :stream nil)))
                           ;; remove the input-text from the beginning of the completion
                           (completion (remove-prefix input-text completion)))
                      (list 200
                            '(:content-type "application/json")
                            (list (to-json `((:completion . ,completion))))))
                  (error (e)
                    (format t "~&Error during LLM completion: ~A~%" e)
                    (debug-log "Error during LLM completion for /api/complete:" e)
                    '(500 (:content-type "application/json")
                      ("{\"error\": \"Failed to get completion from LLM.\"}"))))
                )))))

(setf (ningle:route *app* "/api/assistant-mute" :method :POST)
      #'(lambda (params)
          (declare (ignore params))
          (setf *assistant-audio-muted* t)
          (format t "~&Assistant audio muted.~%")
          '(200 (:content-type "application/json") ("{\"status\": \"assistant audio muted\"}"))))

(setf (ningle:route *app* "/api/assistant-unmute" :method :POST)
      #'(lambda (params)
          (declare (ignore params))
          (setf *assistant-audio-muted* nil)
          (format t "~&Assistant audio unmuted.~%")
          '(200 (:content-type "application/json") ("{\"status\": \"assistant audio unmuted\"}"))))

(setf (ningle:route *app* "/api/assistant-text" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          (if *assistant-extraction*
              (list 200
                    '(:content-type "application/json")
                    (list (to-json `((:extraction . ,*assistant-extraction*)))))
              '(404 (:content-type "application/json") ("{\"error\": \"No assistant extraction available.\"}")))))

(setf (ningle:route *app* "/api/assistant-update" :method :POST)
      #'(lambda (params)
          (declare (ignore params))
          (if *assistant-mode-active*
              (let* ((request-body-string (babel:octets-to-string (lack.request:request-content ningle:*request*)))
                     (json-body (when request-body-string
                                  (handler-case
                                      (shasht:read-json request-body-string)
                                    (error (e)
                                      (debug-log "Error parsing JSON body for /api/assistant-update:" e)
                                      nil))))
                     (user-prompt (if json-body
                                      (gethash "prompt" json-body "Describe what you see.")
                                      "Describe what you see."))) ; Default prompt
                (let ((response-text (assistant-update-query user-prompt)))
                  (if response-text
                      (list 200 '(:content-type "application/json")
                            (list (to-json `((:status . "assistant update processed")
                                             (:extraction . ,response-text)))))
                      '(500 (:content-type "application/json") ("{\"error\": \"Failed to process assistant update.\"}")))))
              '(400 (:content-type "application/json") ("{\"error\": \"Assistant mode not active.\"}")))))

(setf (ningle:route *app* "/api/assistant-read" :method :POST)
      #'(lambda (params)
          (declare (ignore params))
          (cond
            ((not *assistant-last-audio-file*)
              '(404 (:content-type "application/json") ("{\"error\": \"No audio file to read.\"}")))
            (*assistant-audio-muted*
              '(200 (:content-type "application/json") ("{\"status\": \"Audio is muted.\"}")))
            ((play-audio-file *assistant-last-audio-file*)
              '(200 (:content-type "application/json") ("{\"status\": \"Audio playback started.\"}")))
            (t
              '(500 (:content-type "application/json") ("{\"error\": \"Failed to play audio file.\"}"))))))

(setf (ningle:route *app* "/api/assistant-search" :method :POST)
      #'(lambda (params)
          (declare (ignore params))
          (let* ((request-body-string (handler-case (babel:octets-to-string (lack.request:request-content ningle:*request*))
                                        (error () ""))) ; Handle empty body
                 (json-body (when (string/= request-body-string "")
                              (handler-case (shasht:read-json request-body-string)
                                (error (e)
                                       (debug-log "Error parsing JSON body for /api/assistant-search:" e)
                                  nil))))
                 (user-query (if json-body (gethash "query" json-body) nil)))
            (let ((search-result-string (perform-github-code-search user-query :stream-output-p nil)))
              (if (and search-result-string (not (str:starts-with-p "Error:" search-result-string)))
                  (list 200 '(:content-type "application/json")
                        (list (to-json `((:result . ,search-result-string)))))
                (list 500 '(:content-type "application/json")
                      (list (to-json `((:error . ,(or search-result-string "Failed to perform search.")))))))))))

(setf (ningle:route *app* "/api/docs/*")
      #'(lambda (params)
          (declare (ignore params))
          (let* ((req ningle:*request*)
                 (path-info (lack.request:request-path-info req)))
            (if (and (>= (length path-info) 10)
                     (string= (subseq path-info 0 10) "/api/docs/"))
                (let* ((subpath (subseq path-info 10))
                       (uri (format nil "docs:~A" subpath)))
                  (multiple-value-bind (content mime-type metadata)
                      (resolve-hypertext-uri uri)
                    (declare (ignore metadata))
                    (if content
                        (list 200
                              (list :content-type mime-type)
                              (list content))
                        '(404 (:content-type "application/json")
                          ("{\"error\": \"Documentation URI not found.\"}")))))
                '(400 (:content-type "application/json")
                  ("{\"error\": \"Invalid request path.\"}"))))))

(defun start-http ()
  "Start the HTTP server if not already running."
  (if *http-server*
      (progn
        (unless *silent*
          (if *lisp-rpc-mode*
              (rpc-log :info "HTTP server already running" :port *http-port*)
              (format t "HTTP server already running on port ~A.~%" *http-port*)))
        *http-port*)
      (handler-case
          (progn
            (with-suppressed-output-if-rpc
              (start-http-server :port *http-port*))
            (when *lisp-rpc-mode*
              (rpc-log :info "HTTP server started" :port *http-port*))
            (unless *silent*
              (format t "HTTP server running on port ~A.~%" *http-port*))
            *http-port*)
        (error (e)
          (if *lisp-rpc-mode*
              (rpc-error (format nil "Failed to start HTTP server: ~A" e))
              (format *error-output* "~&Error starting HTTP server: ~A~%" e))
          nil))))

(define-command http (args)
  "Start the HTTP API server manually (if not already running)."
  (declare (ignore args))
  (let ((port (start-http)))
    (when port
      (format t "HTTP server running on port ~A.~%" port)))
  :acp (lambda (cmd-args)
         (declare (ignore cmd-args))
         (let ((port (start-http)))
           (if port
               `(("text" . ,(format nil "HTTP server running on port ~A." port))
                 ("data" . (("port" . ,port))))
               `(("text" . "Failed to start HTTP server."))))))

(define-command http.stop (args)
  "Stop the HTTP API server if it is running."
  (declare (ignore args))
  (if *http-server*
      (stop-http-server)
      (format t "HTTP server is not running.~%"))
  :acp (lambda (cmd-args)
         (declare (ignore cmd-args))
         (if *http-server*
             (progn (stop-http-server)
                    `(("text" . "HTTP server stopped.")))
             `(("text" . "HTTP server is not running.")))))
