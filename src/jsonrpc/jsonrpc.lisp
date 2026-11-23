(in-package #:hactar-jsonrpc)

(defconstant +parse-error+ -32700)
(defconstant +invalid-request+ -32600)
(defconstant +method-not-found+ -32601)
(defconstant +invalid-params+ -32602)
(defconstant +internal-error+ -32603)

(define-condition rpc-error (error)
  ((code :initarg :code :reader rpc-error-code)
   (message :initarg :message :reader rpc-error-message)
   (data :initarg :data :initform nil :reader rpc-error-data))
  (:report (lambda (condition stream)
             (format stream "JSON-RPC error ~A: ~A"
                     (rpc-error-code condition)
                     (rpc-error-message condition)))))

(defun field (name object)
  (when (listp object)
    (cdr (assoc name object :test #'string-equal
                           :key (lambda (key) (string key))))))

(defun field-entry (name object)
  (when (listp object)
    (assoc name object :test #'string-equal
                       :key (lambda (key) (string key)))))

(defun make-request (id method &optional params)
  `(("jsonrpc" . "2.0") ("id" . ,id) ("method" . ,method)
    ,@(when params `(("params" . ,params)))))

(defun make-response (id result)
  `(("jsonrpc" . "2.0") ("id" . ,id) ("result" . ,result)))

(defun make-error-response (id code message &optional data)
  `(("jsonrpc" . "2.0") ("id" . ,id)
    ("error" . (("code" . ,code) ("message" . ,message)
                ,@(when data `(("data" . ,data)))))))

(defun make-notification (method &optional params)
  `(("jsonrpc" . "2.0") ("method" . ,method)
    ,@(when params `(("params" . ,params)))))

(defun encode-message (message)
  (let ((shasht:*write-alist-as-object* t)
        (shasht:*write-plist-as-object* nil))
    (remove-if (lambda (char) (member char '(#\Newline #\Return)))
               (shasht:write-json message nil))))

(defun decode-message (source)
  (let ((shasht:*read-default-object-format* :alist))
    (handler-case (shasht:read-json source)
      (error (condition)
        (error 'rpc-error :code +parse-error+
                          :message "Invalid JSON"
                          :data (princ-to-string condition))))))

(defun write-message (message stream &key (framing :newline))
  "Write MESSAGE to STREAM using :NEWLINE or :CONTENT-LENGTH framing."
  (let ((payload (encode-message message)))
    (ecase framing
      (:newline (write-line payload stream))
      (:content-length
       (format stream "Content-Length: ~D~C~C~C~C~A"
               (length (babel:string-to-octets payload :encoding :utf-8))
               #\Return #\Newline #\Return #\Newline payload)))
    (force-output stream)
    message))

(defun read-content-length (stream)
  (let ((first-line (read-line stream nil nil))
        (length nil))
    (unless first-line (return-from read-content-length nil))
    (handler-case
        (loop for line = first-line then (read-line stream nil nil)
              while line
              for clean = (string-right-trim '(#\Return) line)
              until (string= clean "")
              do (when (and (>= (length clean) 15)
                            (string-equal "Content-Length:" clean :end2 15))
                   (setf length
                         (parse-integer
                          (string-trim '(#\Space #\Tab)
                                       (subseq clean 15))))))
      (error (condition)
        (error 'rpc-error :code +parse-error+
                          :message "Invalid Content-Length header"
                          :data (princ-to-string condition))))
    (unless (and length (<= 0 length (* 16 1024 1024)))
      (error 'rpc-error :code +parse-error+
                        :message "Missing or invalid Content-Length header"))
    length))

(defun read-message (stream &key (framing :newline))
  "Read one framed message.
The second value is true when a JSON value was read, distinguishing JSON null
from clean EOF."
  (ecase framing
    (:newline
     (let ((line (read-line stream nil nil)))
       (if line
           (values (decode-message line) t)
           (values nil nil))))
    (:content-length
     (let ((length (read-content-length stream)))
       (if length
         (let ((characters nil)
               (octets 0))
           (loop while (< octets length)
                 for char = (read-char stream nil nil)
                 do (unless char
                      (error 'rpc-error :code +parse-error+
                                        :message "Truncated JSON-RPC payload"))
                    (push char characters)
                    (incf octets
                          (length (babel:string-to-octets
                                   (string char) :encoding :utf-8))))
           (unless (= octets length)
             (error 'rpc-error :code +parse-error+
                               :message "Content-Length splits a UTF-8 character"))
           (values (decode-message (coerce (nreverse characters) 'string)) t))
         (values nil nil))))))

(defstruct (server (:constructor %make-server))
  (methods (make-hash-table :test 'equal))
  (input *standard-input*)
  (output *standard-output*)
  (framing :newline))

(defun make-server (&key (input *standard-input*) (output *standard-output*)
                         (framing :newline))
  (%make-server :input input :output output :framing framing))

(defun register-method (server name function)
  (setf (gethash name (server-methods server)) function)
  server)

(defun dispatch-message (server message)
  "Dispatch MESSAGE and return a response, or NIL for notifications."
  (unless (and (listp message) (every #'consp message))
    (return-from dispatch-message
      (make-error-response nil +invalid-request+ "Invalid JSON-RPC request")))
  (let ((version (field "jsonrpc" message))
        (id-entry (field-entry "id" message))
        (method (field "method" message))
        (params (field "params" message)))
    (let ((id (cdr id-entry)))
    (unless (and (stringp version) (string= version "2.0")
                 (stringp method))
      (return-from dispatch-message
        (make-error-response id +invalid-request+ "Invalid JSON-RPC request")))
    (let ((handler (gethash method (server-methods server))))
      (unless handler
        (return-from dispatch-message
          (when id-entry
            (make-error-response id +method-not-found+
                                 (format nil "Method not found: ~A" method)))))
      (handler-case
          (let ((result (funcall handler params)))
            (when id-entry (make-response id result)))
        (rpc-error (condition)
          (when id-entry
            (make-error-response id (rpc-error-code condition)
                                 (rpc-error-message condition)
                                 (rpc-error-data condition))))
        (error (condition)
          (when id-entry
            (make-error-response id +internal-error+
                                 (princ-to-string condition)))))))))

(defun serve (server)
  "Serve messages until EOF. Returns SERVER."
  (loop
    (handler-case
        (multiple-value-bind (message present-p)
            (read-message (server-input server)
                          :framing (server-framing server))
          (unless present-p (return))
          (let ((response (dispatch-message server message)))
            (when response
              (write-message response (server-output server)
                             :framing (server-framing server)))))
      (rpc-error (condition)
        (write-message
         (make-error-response nil (rpc-error-code condition)
                              (rpc-error-message condition)
                              (rpc-error-data condition))
         (server-output server) :framing (server-framing server)))))
  server)

(defstruct (client (:constructor %make-client))
  input output (framing :newline) (next-id 0))

(defun make-client (&key input output (framing :newline))
  (%make-client :input input :output output :framing framing))

(defun call (client method &optional params)
  "Send a request and synchronously return its result."
  (let ((id (incf (client-next-id client))))
    (write-message (make-request id method params) (client-output client)
                   :framing (client-framing client))
    (loop
      (multiple-value-bind (response present-p)
          (read-message (client-input client)
                        :framing (client-framing client))
        (unless present-p
          (error 'rpc-error :code +internal-error+ :message "Unexpected EOF"))
        (when (equal id (field "id" response))
          (let ((error-object (field "error" response)))
            (when error-object
              (error 'rpc-error :code (field "code" error-object)
                                :message (field "message" error-object)
                                :data (field "data" error-object)))
            (return (field "result" response))))))))

(defun notify (client method &optional params)
  (write-message (make-notification method params) (client-output client)
                 :framing (client-framing client)))
