;;* Lisp-RPC Protocol — Standard output forms for Lisp-RPC mode
;;
;; Each line of stdout is a single s-expression. The client defines
;; handlers for each function name (hactar-hello, hactar-response, etc).
(in-package :hactar)

(defvar *rpc-output-stream* nil
  "When non-nil, rpc-emit writes to this stream instead of computing the
   output stream.  Bind this in tests to capture RPC output.  In production
   leave it NIL so rpc-emit uses sb-sys:*stdout* (bypassing Slynk redirection).")

(defun rpc-emit (form)
  "Write a single s-expression to stdout and flush. Only active in lisp-rpc-mode.
   If *rpc-output-stream* is bound to a stream, uses that (for tests).
   Otherwise uses sb-sys:*stdout* when Slynk is connected (to bypass Slynk's
   *standard-output* redirection), falling back to *standard-output*."
  (when *lisp-rpc-mode*
    (let ((stream (or *rpc-output-stream*
                      #+sbcl sb-sys:*stdout*
                      #-sbcl *standard-output*))
          (*package* (find-package :hactar))
          (*print-case* :downcase))
      (format stream "~&~S~%" form)
      (force-output stream))))

;;** Standard emitters

(defun rpc-hello (version model)
  (rpc-emit `(hactar-hello ,version ,model)))

(defun rpc-ready ()
  (rpc-emit '(hactar-ready)))

(defun rpc-response (text)
  (rpc-emit `(hactar-response ,text)))

(defun rpc-status (kind &rest args)
  (rpc-emit `(hactar-status ,kind ,@args)))

(defun rpc-eval-result (result form)
  (rpc-emit `(hactar-eval-result ,result ,form)))

(defun rpc-eval-output (output form)
  (rpc-emit `(hactar-eval-output ,output ,form)))

(defun rpc-eval-error (message form)
  (rpc-emit `(hactar-eval-error ,message ,form)))

(defun rpc-error (message)
  (rpc-emit `(hactar-error ,message)))

(defun rpc-warning (message &rest details)
  (rpc-emit `(hactar-warning ,message ,@details)))

(defun rpc-cancelled ()
  (rpc-emit '(hactar-cancelled)))

(defun rpc-interrupted ()
  (rpc-emit '(hactar-interrupted)))

(defun rpc-exit (&optional reason)
  (rpc-emit `(hactar-exit ,reason)))

(defun rpc-model-changed (name)
  (rpc-emit `(hactar-model-changed ,name)))

(defun rpc-model-not-found (name)
  (rpc-emit `(hactar-model-not-found ,name)))

(defun rpc-history-compressed ()
  (rpc-emit '(hactar-history-compressed)))

(defun rpc-command-output (text command)
  (rpc-emit `(hactar-command-output ,text ,command)))

(defun rpc-permission-request (tool args description options)
  (rpc-emit `(hactar-permission-request ,tool ,args ,description ,options)))

(defun rpc-stream-chunk (chunk)
  "Emit a streaming chunk of LLM response text."
  (rpc-emit `(hactar-stream-chunk ,chunk)))

(defun rpc-stream-end ()
  "Emit end-of-stream marker."
  (rpc-emit '(hactar-stream-end)))

(defun rpc-event (kind &rest data)
  "Emit an extensible event form."
  (rpc-emit `(hactar-event ,kind ,@data)))

(defun rpc-log (level message &rest data)
  "Emit a log form. LEVEL is :debug, :info, :warn, or :error."
  (rpc-emit `(hactar-log ,level ,message ,@data)))
