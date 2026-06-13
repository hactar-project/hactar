;; Format handling for commands
(in-package :hactar)

(defvar *format-wrapper-style* :xml-tags
  "How to wrap formatted output. One of :xml-tags, :org-mode, :markdown, :none.")

(defparameter *supported-formats*
  '(:json :xml :yaml :markdown :org-mode :toml)
  "Format keywords recognised by the universal --format flag and
   by define-command's format-handler registration.")

(defvar *format-handlers* (make-hash-table :test 'equal)
  "Maps (cons command-name format-keyword) -> handler function (lambda (args)).")

(defun format-tag-name (format)
  "Return the lowercase tag name for the FORMAT keyword."
  (string-downcase (symbol-name format)))

(defun wrap-formatted-output (content format &optional (style *format-wrapper-style*))
  "Wrap CONTENT (a string) for output in FORMAT, using STYLE.
   STYLE: :xml-tags (default), :org-mode, :markdown, or :none."
  (let ((tag (format-tag-name format)))
    (ecase style
      (:none content)
      (:xml-tags
       (format nil "<~A>~%~A~%</~A>" tag content tag))
      (:org-mode
       (format nil "#+begin_src ~A~%~A~%#+end_src" tag content))
      (:markdown
       (format nil "```~A~%~A~%```" tag content)))))

(defun register-format-handler (cmd-name format handler)
  "Register HANDLER for CMD-NAME (with leading slash) and FORMAT keyword."
  (setf (gethash (cons cmd-name format) *format-handlers*) handler))

(defun get-format-handler (cmd-name format)
  "Look up the handler registered for CMD-NAME and FORMAT keyword."
  (gethash (cons cmd-name format) *format-handlers*))

(defun unregister-format-handler (cmd-name format)
  "Remove the format handler registration for CMD-NAME and FORMAT."
  (remhash (cons cmd-name format) *format-handlers*))

(defun parse-format-keyword (format-str)
  "Parse a string like \"json\" into the keyword :json if supported, else NIL."
  (when (and format-str (stringp format-str) (> (length format-str) 0))
    (let ((kw (intern (string-upcase format-str) :keyword)))
      (when (member kw *supported-formats*)
        kw))))

(defun supported-format-names ()
  "Return the supported format keywords as lowercase strings."
  (mapcar (lambda (k) (string-downcase (symbol-name k))) *supported-formats*))

(defparameter *shorthand-format-flags*
  '(("--json" . "json") ("-json" . "json")
    ("--markdown" . "markdown") ("-md" . "markdown")
    ("--xml" . "xml") ("-xml" . "xml")
    ("--yaml" . "yaml") ("-yml" . "yaml")
    ("--org" . "org-mode") ("-org" . "org-mode")
    ("--org-mode" . "org-mode")
    ("--toml" . "toml") ("-toml" . "toml"))
  "Mapping of shorthand CLI flags to format keywords.")

(defun extract-format-string (args)
  "Scan ARGS (a list of strings) for --format=X, --format X, or shorthand format flags.
   Return the X portion as a string, or NIL if not present.
   Does not mutate or consume ARGS."
  (loop with i = 0
        while (< i (length args))
        for arg = (nth i args)
        do (cond
             ((not (stringp arg)) (incf i))
             ((and (>= (length arg) (length "--format="))
                   (string= arg "--format=" :end1 (length "--format=")))
              (return (subseq arg (length "--format="))))
             ((string= arg "--format")
              (return (nth (1+ i) args)))
             ((assoc arg *shorthand-format-flags* :test #'string=)
              (return (cdr (assoc arg *shorthand-format-flags* :test #'string=))))
             (t (incf i)))
        finally (return nil)))

(defun write-toml-value (val stream)
  (cond
    ((eq val t) (format stream "true"))
    ((eq val nil) (format stream "false"))
    ((eq val :true) (format stream "true"))
    ((eq val :false) (format stream "false"))
    ((stringp val) (format stream "~S" val))
    ((numberp val) (format stream "~A" val))
    ((symbolp val) (format stream "~S" (string-downcase (symbol-name val))))
    ((and (vectorp val) (every (lambda (x) (or (stringp x) (numberp x) (typep x 'boolean) (eq x :true) (eq x :false))) val))
     (format stream "[~{~A~^, ~}]"
             (mapcar (lambda (x) (cond ((stringp x) (format nil "~S" x))
                                       ((or (eq x t) (eq x :true)) "true")
                                       ((or (eq x nil) (eq x :false)) "false")
                                       (t x)))
                     (coerce val 'list))))
    ((and (listp val) (every (lambda (x) (or (stringp x) (numberp x) (typep x 'boolean) (eq x :true) (eq x :false))) val))
     (format stream "[~{~A~^, ~}]"
             (mapcar (lambda (x) (cond ((stringp x) (format nil "~S" x))
                                       ((or (eq x t) (eq x :true)) "true")
                                       ((or (eq x nil) (eq x :false)) "false")
                                       (t x)))
                     val)))
    (t (format stream "~S" (princ-to-string val)))))

(defun serialize-to-toml (val &optional (stream nil))
  (with-output-to-string (s)
    (let ((out (or stream s)))
      (cond
        ((and (or (vectorp val) (listp val))
              (every (lambda (x) (and (listp x) (consp (car x))))
                     (if (vectorp val) (coerce val 'list) val)))
         (let ((items (if (vectorp val) (coerce val 'list) val)))
           (dolist (item items)
             (format out "[[items]]~%")
             (loop for (k . v) in item
                   for key-str = (if (symbolp k) (string-downcase (symbol-name k)) (princ-to-string k))
                   do (format out "~A = " key-str)
                      (write-toml-value v out)
                      (terpri out))
             (terpri out))))
        ((and (listp val) (consp (car val)))
         (loop for (k . v) in val
               for key-str = (if (symbolp k) (string-downcase (symbol-name k)) (princ-to-string k))
               do (cond
                    ((and (or (vectorp v) (listp v))
                          (every (lambda (x) (and (listp x) (consp (car x))))
                                 (if (vectorp v) (coerce v 'list) v)))
                     (let ((items (if (vectorp v) (coerce v 'list) v)))
                       (dolist (item items)
                         (format out "[[~A]]~%" key-str)
                         (loop for (sub-k . sub-v) in item
                               for sub-key-str = (if (symbolp sub-k) (string-downcase (symbol-name sub-k)) (princ-to-string sub-k))
                               do (format out "  ~A = " sub-key-str)
                                  (write-toml-value sub-v out)
                                  (terpri out)))))
                    (t
                     (format out "~A = " key-str)
                     (write-toml-value v out)
                     (terpri out)))))))))

(defun execute-format-command (cmd-name format args)
  "Invoke the registered FORMAT handler for CMD-NAME with ARGS, then
   wrap and print the result using *format-wrapper-style*."
  (let* ((handler (get-format-handler cmd-name format))
         (result (if handler
                     (funcall handler args)
                     (when (eq format :toml)
                       (let ((json-handler (get-format-handler cmd-name :json)))
                         (when json-handler
                           (let* ((json-str (funcall json-handler args))
                                  (parsed (cl-json:decode-json-from-string json-str)))
                             (serialize-to-toml parsed))))))))
    (when result
      (let ((content (if (stringp result) result (princ-to-string result))))
        (write-string (wrap-formatted-output content format))
        (terpri)
        (force-output)))
    result))
