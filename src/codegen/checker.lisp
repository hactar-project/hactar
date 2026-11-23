;; linter for the hactar transipler
(in-package #:hactar-codegen)

(defstruct check-result
  "Result of a single check."
  (level :warning :type (member :error :warning :info))
  (message "" :type string)
  (form nil)
  (source-hint nil))

;;* State
(defvar *check-rules* (make-hash-table :test 'eq)
  "Per-target list of check functions. keyword -> list of (name . lambda)")

(defvar *disabled-checks* (make-hash-table :test 'eq)
  "Set of check rule names (symbols) that are disabled.")

;;* Helpers
(defun disable-check (name)
  "Disable a check rule by NAME (symbol). Can be called from user.lisp."
  (setf (gethash name *disabled-checks*) t))

(defun enable-check (name)
  "Re-enable a previously disabled check rule by NAME (symbol)."
  (remhash name *disabled-checks*))

(defun check-enabled-p (name)
  "Return T if the check rule NAME is enabled."
  (not (gethash name *disabled-checks*)))
;;* Core
(defmacro defcheck (target name &body body)
  "Define a check rule for TARGET. Replaces any existing rule with the same NAME."
  `(let ((existing (gethash ,target *check-rules* nil)))
     (setf (gethash ,target *check-rules*)
           (cons (cons ',name (lambda (form) ,@body))
                 (remove ',name existing :key #'car)))))

(defun walk-forms (fn forms)
  "Walk every form and sub-form in FORMS, calling FN on each."
  (dolist (form forms)
    (funcall fn form)
    (when (listp form)
      (walk-forms fn (cdr form)))))

(defun collect-target-rules (target)
  "Collect all check rules for TARGET and its parents, filtering out disabled rules."
  (let ((target-def (gethash target *compiler-targets*))
        (rules (copy-list (gethash target *check-rules*))))
    (when (and target-def (target-definition-parent target-def))
      (setf rules (append rules (collect-target-rules (target-definition-parent target-def)))))
    (remove-if-not (lambda (rule) (check-enabled-p (car rule))) rules)))

(defun run-checks (target forms)
  "Run all check rules for TARGET against FORMS. Returns list of check-results."
  (let ((rules (collect-target-rules target))
        (results nil))
    (when rules
      (walk-forms
       (lambda (form)
         (dolist (rule rules)
           (let ((result (funcall (cdr rule) form)))
             (when result
               (push result results)))))
       forms))
    (nreverse results)))

(defstruct source-hint
  "Source location hint for a check result."
  (file "" :type string)
  (start-line 0 :type fixnum)
  (start-char 0 :type fixnum)
  (end-line 0 :type fixnum)
  (end-char 0 :type fixnum))

(defun detect-target-from-extension (path)
  "Detect the compilation target keyword from a file's extension."
  (let ((ext (pathname-type (pathname path))))
    (cond
      ((string-equal ext "tsx") :redwood)
      ((string-equal ext "ts")  :typescript)
      ((string-equal ext "jsx") :redwood)
      ((string-equal ext "js")  :javascript)
      ((string-equal ext "css") :css)
      (t :javascript))))

(defun format-check-results-xml (results &optional (stream *standard-output*))
  "Format check results as XML diagnostics."
  (format stream "<diagnostics>~%")
  (dolist (r results)
    (let ((hint (check-result-source-hint r)))
      (format stream "  <diagnostic>~%")
      (format stream "    <range>~%")
      (format stream "      <start>~%")
      (format stream "        <line>~A</line>~%" (if hint (source-hint-start-line hint) 0))
      (format stream "        <character>~A</character>~%" (if hint (source-hint-start-char hint) 0))
      (format stream "      </start>~%")
      (format stream "      <end>~%")
      (format stream "        <line>~A</line>~%" (if hint (source-hint-end-line hint) 0))
      (format stream "        <character>~A</character>~%" (if hint (source-hint-end-char hint) 0))
      (format stream "      </end>~%")
      (format stream "    </range>~%")
      (format stream "    <severity>~A</severity>~%"
              (ecase (check-result-level r) (:error 1) (:warning 2) (:info 3)))
      (format stream "    <code>~A</code>~%"
              (string-downcase (princ-to-string (or (check-result-source-hint r) "check"))))
      (format stream "    <source>hactar</source>~%")
      (format stream "    <message>~A</message>~%"
              (check-result-message r))
      (format stream "  </diagnostic>~%")))
  (format stream "</diagnostics>~%"))

(defun format-check-results-json (results &optional (stream *standard-output*))
  "Format check results as JSON diagnostics."
  (let ((diagnostics
          (mapcar (lambda (r)
                    (let ((hint (check-result-source-hint r)))
                      `(("range" . (("start" . (("line" . ,(if hint (source-hint-start-line hint) 0))
                                                 ("character" . ,(if hint (source-hint-start-char hint) 0))))
                                    ("end" . (("line" . ,(if hint (source-hint-end-line hint) 0))
                                              ("character" . ,(if hint (source-hint-end-char hint) 0))))))
                        ("severity" . ,(ecase (check-result-level r) (:error 1) (:warning 2) (:info 3)))
                        ("source" . "hactar")
                        ("message" . ,(check-result-message r)))))
                  results)))
    (let ((shasht:*write-alist-as-object* t)
          (shasht:*write-plist-as-object* nil))
      (shasht:write-json `(("diagnostics" . ,(coerce diagnostics 'vector))) stream))
    (terpri stream)))

;;* Core Checking functions
(defun offset-to-line-col (source offset)
  "Convert a character OFFSET in SOURCE to (line . col), both 0-based."
  (let ((line 0) (col 0))
    (loop for i from 0 below (min offset (length source))
          do (if (char= (char source i) #\Newline)
                 (progn (incf line) (setf col 0))
                 (incf col)))
    (cons line col)))

(defun read-all-forms-with-positions (source)
  "Read all forms from SOURCE, returning list of (form start-offset end-offset)."
  (let ((*package* (find-package :hactar-codegen))
        (*read-eval* nil)
        (results nil))
    (with-input-from-string (stream source)
      (loop
        (let ((start (file-position stream))
              (form (read stream nil :eof)))
          (when (eq form :eof) (return))
          (let ((end (file-position stream)))
            (push (list form start end) results)))))
    (nreverse results)))

(defun check-file-source (source &key target (file ""))
  "Check SOURCE string, returning check-results with source-hints populated.
   Target is auto-detected from (target :xxx) forms if not specified."
  (let* ((form-positions (read-all-forms-with-positions source))
         (forms (mapcar #'first form-positions))
         (target (or target (detect-target-from-forms forms)))
         (rules (collect-target-rules target))
         (results nil))
    (when rules
      (dolist (fp form-positions)
        (let ((top-form (first fp))
              (start-off (second fp))
              (end-off (third fp)))
          (walk-forms
           (lambda (form)
             (dolist (rule rules)
               (let ((result (funcall (cdr rule) form)))
                 (when result
                   (let* ((sl (offset-to-line-col source start-off))
                          (el (offset-to-line-col source end-off)))
                     (setf (check-result-source-hint result)
                           (make-source-hint :file file
                                             :start-line (car sl) :start-char (cdr sl)
                                             :end-line (car el) :end-char (cdr el))))
                   (push result results)))))
           (list top-form)))))
    (nreverse results)))

(defun check-unsupported-form (target form supported-ops)
  "Generic check: if FORM's operator is not in SUPPORTED-OPS, return an error."
  (when (and (listp form) (symbolp (car form))
             (not (member (car form) supported-ops)))
    (make-check-result :level :error
                       :message (format nil "Unsupported form ~A for target ~A"
                                        (string-downcase (symbol-name (car form))) target)
                       :form form)))
