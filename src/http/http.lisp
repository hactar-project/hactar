(in-package #:hactar-http)

;;; HTTP API adapter for Hactar (pure Clack handler + server lifecycle).

(defvar *http-server* nil)
(defvar *http-port* 7890)

(defun %json-encode (value)
  (let ((shasht:*write-alist-as-object* t)
        (shasht:*write-plist-as-object* nil))
    (shasht:write-json value nil)))

(defun %json-decode (text)
  (let ((shasht:*read-default-object-format* :alist)
        (shasht:*read-default-array-format* :list))
    (shasht:read-json text)))

(defun %json-response (status payload)
  (list status '(:content-type "application/json") (list (%json-encode payload))))

(defun %field (name alist)
  (when (listp alist)
    (cdr (assoc name alist :test #'string-equal :key #'string))))

(defun %read-body-string (env)
  (or (getf env :body-string)
      (let ((raw (getf env :raw-body)))
        (cond
          ((null raw) "")
          ((stringp raw) raw)
          ((typep raw '(vector (unsigned-byte 8)))
           (babel:octets-to-string raw :encoding :utf-8))
          ((streamp raw)
           (let ((el (stream-element-type raw)))
             (if (subtypep el '(unsigned-byte 8))
                 (let ((buf (make-array 0 :element-type '(unsigned-byte 8)
                                          :fill-pointer 0 :adjustable t)))
                   (loop for b = (read-byte raw nil nil)
                         while b do (vector-push-extend b buf))
                   (babel:octets-to-string buf :encoding :utf-8))
                 (with-output-to-string (out)
                   (loop for line = (read-line raw nil nil)
                         while line do (write-line line out))))))
          (t "")))))

(defun localhost-only-middleware (app)
  "Wrap Clack APP so non-localhost :REMOTE-ADDR requests receive 403."
  (lambda (env)
    (let ((addr (getf env :remote-addr)))
      (if (or (null addr)
              (member addr '("127.0.0.1" "::1" "localhost" "::ffff:127.0.0.1")
                      :test #'string=))
          (funcall app env)
          (%json-response 403 '(("error" . "Forbidden: localhost only")))))))

(defun %parse-json-body (env)
  (let ((body (%read-body-string env)))
    (when (plusp (length (string-trim '(#\Space #\Tab #\Return #\Newline) body)))
      (handler-case (%json-decode body)
        (error () :invalid)))))

(defun make-http-app (&key (localhost-only t))
  "Build a pure Clack handler exposing HACTAR-CORE over HTTP."
  (let ((handler
          (lambda (env)
            (let ((method (getf env :request-method))
                  (path (or (getf env :path-info) "/")))
              (cond
                ((and (eq method :get) (string= path "/health"))
                 (%json-response 200 '(("status" . "ok"))))
                ((and (eq method :get) (string= path "/api/tools"))
                 (%json-response
                  200
                  `(("tools" . ,(coerce
                                 (mapcar (lambda (tl)
                                           `(("name" . ,(getf tl :name))
                                             ("doc" . ,(getf tl :doc))))
                                         (hactar-core:list-tools))
                                 'vector)))))
                ((and (eq method :get) (uiop:string-prefix-p "/api/docs/" path))
                 (let* ((name (subseq path (length "/api/docs/")))
                        (doc (hactar-core:lookup-doc name)))
                   (if (plusp (length doc))
                       (list 200 '(:content-type "text/plain; charset=utf-8") (list doc))
                       (%json-response 404 '(("error" . "Document not found"))))))
                ((and (eq method :post) (string= path "/api/dispatch"))
                 (let ((json (%parse-json-body env)))
                   (if (eq json :invalid)
                       (%json-response 400 '(("error" . "Invalid JSON")))
                       (let* ((raw-args (%field "args" json))
                              (cmd (%field "command" json))
                              (argv (cond
                                      ((listp raw-args) raw-args)
                                      ((stringp cmd)
                                       (remove "" (uiop:split-string cmd :separator '(#\Space #\Tab))
                                               :test #'string=))
                                      (t nil)))
                              (res (hactar-core:dispatch argv)))
                         (%json-response
                          (if (hactar-core:result-ok-p res) 200 400)
                          `(("status" . ,(string-downcase (symbol-name (hactar-core:result-status res))))
                            ("message" . ,(or (hactar-core:result-message res) ""))))))))
                ((and (eq method :post) (string= path "/api/complete"))
                 (let ((json (%parse-json-body env)))
                   (if (or (null json) (eq json :invalid) (null (%field "text" json)))
                       (%json-response 400 '(("error" . "Missing 'text' field")))
                       (let ((answer (hactar-core:ask (%field "text" json)
                                                      :model (%field "model" json))))
                         (%json-response 200 `(("completion" . ,answer)))))))
                ((and (eq method :post) (uiop:string-prefix-p "/api/tools/" path))
                 (let* ((tool-name (subseq path (length "/api/tools/")))
                        (json (%parse-json-body env))
                        (args (and (listp json) (%field "args" json)))
                        (res (apply #'hactar-core:run-tool tool-name args)))
                   (%json-response
                    (if (hactar-core:result-ok-p res) 200 400)
                    `(("status" . ,(string-downcase (symbol-name (hactar-core:result-status res))))
                      ("message" . ,(or (hactar-core:result-message res) ""))))))
                (t
                 (%json-response 404 '(("error" . "Not found")))))))))
    (if localhost-only
        (localhost-only-middleware handler)
        handler)))

(defun start-server (&key (port *http-port*) (address "127.0.0.1") (localhost-only t))
  "Start the Clack/Hunchentoot HTTP server."
  (when *http-server*
    (stop-server))
  (setf *http-port* port
        *http-server* (clack:clackup (make-http-app :localhost-only localhost-only)
                                     :server :hunchentoot
                                     :address address
                                     :port port
                                     :use-thread t)))

(defun stop-server ()
  "Stop the running HTTP server, if any."
  (when *http-server*
    (ignore-errors (clack:stop *http-server*))
    (setf *http-server* nil)
    t))
