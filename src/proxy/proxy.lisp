(in-package #:hactar-proxy)

;;; OpenRouter/OpenAI-compatible LLM proxy injecting Hactar context.

(defvar *proxy-server* nil)
(defvar *proxy-port* 7891)
(defvar *proxy-upstream-url* "https://openrouter.ai/api/v1/chat/completions")
(defvar *proxy-forward-fn* nil
  "Optional transport fn: (url headers body-string) -> (values body status headers).")
(defvar *proxy-request-hooks* nil)
(defvar *proxy-response-hooks* nil)

(defun add-proxy-request-hook (fn)
  (push fn *proxy-request-hooks*)
  fn)

(defun add-proxy-response-hook (fn)
  (push fn *proxy-response-hooks*)
  fn)

(defun %json-encode (value)
  (let ((shasht:*write-alist-as-object* t)
        (shasht:*write-plist-as-object* nil))
    (shasht:write-json value nil)))

(defun %json-decode (text)
  (let ((shasht:*read-default-object-format* :alist)
        (shasht:*read-default-array-format* :list))
    (shasht:read-json text)))

(defun %field (name alist)
  (when (listp alist)
    (cdr (assoc name alist :test #'string-equal :key #'string))))

(defun extract-system-prompt (messages)
  "Split system messages from MESSAGES, returning (values system-prompt remaining)."
  (let ((sys-parts nil)
        (rest nil))
    (dolist (msg (if (vectorp messages) (coerce messages 'list) messages))
      (if (string-equal (or (%field "role" msg) "") "system")
          (push (or (%field "content" msg) "") sys-parts)
          (push msg rest)))
    (values (when sys-parts (format nil "~{~A~^~%~%~}" (nreverse sys-parts)))
            (nreverse rest))))

(defun inject-context (request-plist)
  "Compose HACTAR-CORE context into REQUEST-PLIST's :SYSTEM-PROMPT."
  (let* ((existing (getf request-plist :system-prompt))
         (ctx (hactar-core:render-system-prompt))
         (combined (cond
                     ((and existing (plusp (length existing)) (plusp (length ctx)))
                      (format nil "~A~%~%~A" existing ctx))
                     ((plusp (length ctx)) ctx)
                     (t existing))))
    (setf (getf request-plist :system-prompt) combined)
    request-plist))

(defun reassemble-messages (system-prompt messages)
  "Prepend SYSTEM-PROMPT (if non-empty) to MESSAGES as a vector."
  (coerce
   (if (and system-prompt (plusp (length system-prompt)))
       (cons `(("role" . "system") ("content" . ,system-prompt)) messages)
       messages)
   'vector))

(defun %default-forward (url headers payload)
  (handler-case
      (multiple-value-bind (body status resp-headers)
          (dex:post url :headers headers :content payload)
        (values (if (stringp body)
                    body
                    (babel:octets-to-string body :encoding :utf-8))
                status
                resp-headers))
    (dex:http-request-failed (e)
      (values (princ-to-string (dex:response-body e))
              (dex:response-status e)
              nil))))

(defun forward-proxy-request (request-plist &key (upstream-url *proxy-upstream-url*))
  "Run request hooks, inject context, forward upstream, and run response hooks."
  (let ((req (inject-context (copy-list request-plist))))
    (dolist (hook *proxy-request-hooks*)
      (setf req (funcall hook req)))
    (let* ((raw (copy-alist (or (getf req :raw-body) '())))
           (msgs (reassemble-messages (getf req :system-prompt)
                                      (getf req :messages)))
           (updated (cons (cons "messages" msgs)
                          (remove "messages" raw :key #'car :test #'string-equal)))
           (payload (%json-encode updated))
           (headers (cons '("Content-Type" . "application/json")
                          (getf req :headers)))
           (fn (or *proxy-forward-fn* #'%default-forward)))
      (multiple-value-bind (resp-body status resp-headers)
          (funcall fn upstream-url headers payload)
        (declare (ignore resp-headers))
        (let ((result (list :status (or status 200) :body resp-body)))
          (dolist (hook *proxy-response-hooks*)
            (setf result (funcall hook result req)))
          result)))))

(defun %read-body (env)
  (or (getf env :body-string)
      (let ((raw (getf env :raw-body)))
        (cond
          ((null raw) "")
          ((stringp raw) raw)
          ((typep raw '(vector (unsigned-byte 8)))
           (babel:octets-to-string raw :encoding :utf-8))
          ((streamp raw)
           (with-output-to-string (out)
             (loop for line = (read-line raw nil nil)
                   while line do (write-line line out))))
          (t "")))))

(defun make-proxy-app (&key (upstream-url *proxy-upstream-url*))
  "Build a pure Clack handler for OpenAI/OpenRouter chat completion proxying."
  (lambda (env)
    (let ((method (getf env :request-method))
          (path (or (getf env :path-info) "/")))
      (cond
        ((and (eq method :get) (string= path "/health"))
         (list 200 '(:content-type "application/json")
               (list (%json-encode '(("status" . "ok"))))))
        ((and (eq method :post)
              (member path '("/v1/chat/completions" "/chat/completions") :test #'string=))
         (let ((json (handler-case (%json-decode (%read-body env))
                       (error () nil))))
           (if (null json)
               (list 400 '(:content-type "application/json")
                     (list (%json-encode '(("error" . "Invalid JSON")))))
               (multiple-value-bind (sys msgs)
                   (extract-system-prompt (%field "messages" json))
                 (let* ((req (list :model (%field "model" json)
                                   :system-prompt sys
                                   :messages msgs
                                   :raw-body json
                                   :headers (getf env :headers)))
                        (resp (forward-proxy-request req :upstream-url upstream-url)))
                   (list (getf resp :status)
                         '(:content-type "application/json")
                         (list (getf resp :body))))))))
        (t
         (list 404 '(:content-type "application/json")
               (list (%json-encode '(("error" . "Not found"))))))))))

(defun start-proxy (&key (port *proxy-port*) (address "127.0.0.1")
                         (upstream-url *proxy-upstream-url*))
  "Start the proxy HTTP server."
  (when *proxy-server*
    (stop-proxy))
  (setf *proxy-port* port
        *proxy-upstream-url* upstream-url
        *proxy-server* (clack:clackup (make-proxy-app :upstream-url upstream-url)
                                      :server :hunchentoot
                                      :address address
                                      :port port
                                      :use-thread t)))

(defun stop-proxy ()
  "Stop the running proxy HTTP server, if any."
  (when *proxy-server*
    (ignore-errors (clack:stop *proxy-server*))
    (setf *proxy-server* nil)
    t))
