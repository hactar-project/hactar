(in-package #:hactar-sandbox)

(defstruct policy
  (name "sandbox")
  allow
  deny
  directory
  environment)

(define-condition sandbox-violation (error)
  ((policy :initarg :policy :reader sandbox-violation-policy)
   (command :initarg :command :reader sandbox-violation-command)
   (reason :initarg :reason :reader sandbox-violation-reason))
  (:report (lambda (condition stream)
             (format stream "~A"
                     (sandbox-violation-reason condition)))))

(defvar *sandbox-policy* nil)

(defun command-string (command)
  (if (stringp command)
      command
      (format nil "~{~A~^ ~}" command)))

(defun command-binary (command)
  (if (consp command)
      (princ-to-string (first command))
      (let* ((trimmed (string-left-trim '(#\Space #\Tab) command))
             (end (position-if (lambda (char)
                                 (member char '(#\Space #\Tab)))
                               trimmed)))
        (subseq trimmed 0 end))))

(defun unsafe-shell-syntax-p (command)
  (and (stringp command)
       (or (find-if (lambda (char)
                      (member char '(#\; #\| #\& #\< #\> #\` #\Newline
                                     #\Return)))
                    command)
           (search "$(" command)
           (search "${" command))))

(defun denial-reason (policy command)
  (let ((text (command-string command))
        (binary (command-binary command)))
    (cond
      ((unsafe-shell-syntax-p command)
       (format nil "Sandbox ~A rejects shell control syntax: ~A"
               (policy-name policy) text))
      ((some (lambda (denied) (search denied text :test #'char-equal))
             (policy-deny policy))
       (format nil "Sandbox ~A denies command: ~A"
               (policy-name policy) text))
      ((and (policy-allow policy)
            (not (member binary (policy-allow policy) :test #'string=)))
       (format nil "Sandbox ~A does not allow binary: ~A"
               (policy-name policy) binary)))))

(defun policy-allows-p (policy command)
  "Return true when COMMAND satisfies POLICY."
  (null (denial-reason policy command)))

(defun explain-denial (policy command)
  "Return a denial explanation, or NIL when COMMAND is allowed."
  (denial-reason policy command))

(defun enforce-policy (policy command)
  (let ((reason (and policy (denial-reason policy command))))
    (when reason
      (error 'sandbox-violation :policy policy :command command
                                :reason reason))))

(defun sandbox-exec (command &key (policy *sandbox-policy*) output error-output
                                  input ignore-error-status)
  "Run COMMAND after policy enforcement, returning UIOP's values."
  (enforce-policy policy command)
  (uiop:run-program command
                    :directory (and policy (policy-directory policy))
                    :env (and policy (policy-environment policy))
                    :output output :error-output error-output :input input
                    :ignore-error-status ignore-error-status))

(defmacro with-sandbox ((policy) &body body)
  `(let ((*sandbox-policy* ,policy))
     ,@body))
