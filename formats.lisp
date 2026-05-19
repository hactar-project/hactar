;; Format handling for define-command.
(in-package :hactar)

(defvar *format-wrapper-style* :xml-tags
  "How to wrap formatted output. One of :xml-tags, :org-mode, :markdown, :none.")

(defparameter *supported-formats*
  '(:json :xml :yaml :markdown :org-mode :repl :tui)
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
    ("--org-mode" . "org-mode"))
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

(defun execute-format-command (cmd-name format args)
  "Invoke the registered FORMAT handler for CMD-NAME with ARGS, then
   wrap and print the result using *format-wrapper-style*."
  (let* ((handler (get-format-handler cmd-name format))
         (result (funcall handler args)))
    (when result
      (let ((content (if (stringp result) result (princ-to-string result))))
        (write-string (wrap-formatted-output content format))
        (terpri)
        (force-output)))
    result))
