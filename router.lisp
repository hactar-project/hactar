;; Generic router using regex pattern matching.
;; This is the central dispatch slash commands, subcommands, docs searching, wiki queriesetc. All are "routes".
(in-package :hactar)

(defvar *routes* (make-hash-table :test 'equal)
  "Hash table mapping route names to route definitions.")

(defvar *route-reinit-hooks* '()
  "List of functions to re-register default/built-in routes when *routes* is empty.")

(defvar *flags* (make-hash-table :test 'equal)
  "Hash table mapping flag names (strings, both long and short) to flag structs.")

(defvar *route-fallback-fn* nil
  "Function (string -> any) invoked by EXECUTE-ROUTE when no route matches.
Used by the wiki module to synthesize answers with an LLM. If NIL, prints a
'no matching route' message.")

(defstruct route
  "Represents a generic route with regex pattern matching."
  name           ; Route name (symbol)
  pattern        ; Regex pattern string
  param-names    ; List of parameter names to extract from regex groups
  priority       ; Integer priority (higher = checked first)
  handler)       ; Function that handles the route

(defstruct flag
  "Represents a CLI flag registered via DEFFLAG."
  name             ; Symbol identifier
  long-names       ; List of strings, e.g. ("--model")
  short-names      ; List of strings, e.g. ("-m")
  takes-value      ; T if flag consumes the next arg
  description      ; Description for help output
  handler)         ; Function: (lambda (value)) — value is T for boolean flags

(defun register-route (name pattern param-names priority handler)
  "Register a route in the global routes table."
  (setf (gethash name *routes*)
        (make-route :name name
                    :pattern pattern
                    :param-names param-names
                    :priority priority
                    :handler handler)))

(defun match-route (input)
  "Try to match input against all registered routes, returning (values route params) or (values nil nil).
Always runs reinit hooks (idempotent) so routes cleared in other tests are restored."
  ;; Re-register any missing default/custom routes (hooks are written to be idempotent).
  (dolist (hook *route-reinit-hooks*)
    (ignore-errors (funcall hook)))
  (let ((sorted-routes (sort (loop for route being the hash-values of *routes*
                                  collect route)
                            #'> :key #'route-priority)))
    (loop for route in sorted-routes
          do (multiple-value-bind (match groups)
                 (cl-ppcre:scan-to-strings (route-pattern route) input)
               (when match
                 (let ((params (loop for param-name in (route-param-names route)
                                    for group across groups
                                    collect (cons param-name group))))
                   (return-from match-route (values route params)))))
          finally (return (values nil nil)))))

(defun execute-route (input)
  "Match and execute a route for the given input string.
If no route matches and *ROUTE-FALLBACK-FN* is set, the fallback is invoked
(used by the wiki module to synthesize answers with an LLM)."
  (multiple-value-bind (route params)
      (match-route input)
    (cond
      (route
       (apply (route-handler route) (mapcar #'cdr params)))
      (*route-fallback-fn*
       (funcall *route-fallback-fn* input))
      (t
       (format t "No matching route for: ~A~%" input)
       nil))))

(defmacro defroute (pattern args &rest rest-args)
  "Define a route with regex pattern matching.

   PATTERN: A regex pattern string with capture groups
   ARGS: Lambda list for the handler function (receives extracted params)
   :PARAMS: List of parameter names corresponding to regex capture groups
   :PRIORITY: Integer priority for route matching (default 10, higher = checked first)
   BODY: Code to execute when route matches

   Example:
   (defroute \"npm:([^@]+)@(.+)\\.(.+)$\" (package version extension)
     :params (:package :version :extension)
     :priority 10
     (format t \"Package: ~A, Version: ~A, Extension: ~A~%\"
             package version extension))"
  (let ((params nil)
        (priority 10)
        (body nil)
        (state nil)) ; Track what we're expecting: nil, :params, or :priority
    ;; Parse rest-args to extract :params, :priority, and body
    (loop for arg in rest-args
          do (cond
	       ((eq state :params)
                (setf params arg)
                (setf state nil))
	       ((eq state :priority)
                (setf priority arg)
                (setf state nil))
	       ((eq arg :params)
                (setf state :params))
	       ((eq arg :priority)
                (setf state :priority))
	       (t (push arg body))))
    (setf body (nreverse body))
    (let ((route-name (gensym "ROUTE-")))
      `(progn
         (register-route ',route-name
                        ,pattern
                        ',params
                        ,priority
                        (lambda ,args
                          ,@body))
         ',route-name))))

(defun unregister-route (name)
  "Remove a route from the global routes table."
  (remhash name *routes*))

;;* flags 
(defun register-flag (name long-names short-names takes-value description handler)
  "Register a flag. Both LONG-NAMES and SHORT-NAMES are lists of strings."
  (let ((flag (make-flag :name name
                         :long-names long-names
                         :short-names short-names
                         :takes-value takes-value
                         :description description
                         :handler handler)))
    (dolist (n (append long-names short-names))
      (setf (gethash n *flags*) flag))
    flag))

(defun unregister-flag (name)
  "Remove a flag by its keyword/symbol NAME."
  (maphash (lambda (k v)
             (when (eq (flag-name v) name)
               (remhash k *flags*)))
           *flags*))

(defmacro defflag (name aliases lambda-list &body body)
  "Define a CLI flag.

  NAME       -- a keyword or symbol identifier.
  ALIASES    -- a list of strings, e.g. (\"--model\" \"-m\").
  LAMBDA-LIST -- () for boolean flags, or (value) for value flags.

  Recognized keyword options in BODY:
    :description STRING -- help text
    :takes-value BOOL   -- override auto-detection (default: t if lambda-list non-empty)

  Example:
    (defflag :model (\"--model\" \"-m\") (val)
      :description \"LLM model to use\"
      (setf hactar::*pending-model* val))

    (defflag :debug (\"--debug\") ()
      :description \"Enable debug output\"
      (setf hactar::*debug* t))"
  (let ((description "")
        (takes-value-supplied nil)
        (takes-value (not (null lambda-list)))
        (real-body body))
    (declare (ignorable takes-value-supplied))
    (loop while (and real-body (keywordp (car real-body)))
          do (case (car real-body)
               (:description (setf description (cadr real-body)))
               (:takes-value (setf takes-value (cadr real-body)
                                   takes-value-supplied t)))
             (setf real-body (cddr real-body)))
    (let ((long-names (remove-if-not (lambda (s) (and (stringp s) (>= (length s) 2)
                                                       (string= s "--" :end1 2)))
                                     aliases))
          (short-names (remove-if-not (lambda (s) (and (stringp s) (>= (length s) 1)
                                                        (char= (char s 0) #\-)
                                                        (or (= (length s) 2)
                                                            (and (> (length s) 1)
                                                                 (char/= (char s 1) #\-)))))
                                      aliases)))
      `(register-flag ',name
                      ',long-names
                      ',short-names
                      ,takes-value
                      ,description
                      (lambda ,lambda-list
                        (declare (ignorable ,@lambda-list))
                        ,@real-body)))))

(defun parse-cli-input (args)
  "Parse ARGS (list of strings), apply flag handlers, return the remaining
positional arguments. Flags consume their value argument when :takes-value is T.
Unknown flags are passed through unchanged."
  (let ((positional '())
        (remaining args))
    (loop while remaining
          do (let* ((arg (first remaining))
                    (eq-pos (and (> (length arg) 0)
                                 (char= (char arg 0) #\-)
                                 (position #\= arg)))
                    (key (if eq-pos (subseq arg 0 eq-pos) arg))
                    (inline-val (when eq-pos (subseq arg (1+ eq-pos))))
                    (flag (gethash key *flags*)))
               (cond
                 (flag
                  (cond
                    ((flag-takes-value flag)
                     (let ((val (or inline-val (second remaining))))
                       (when val
                         (handler-case (funcall (flag-handler flag) val)
                           (error (e) (format *error-output*
                                              "~&Error applying flag ~A: ~A~%" key e))))
                       (setf remaining (if inline-val (cdr remaining)
                                           (cddr remaining)))))
                    (t
                     (handler-case (funcall (flag-handler flag))
                       (error (e) (format *error-output*
                                          "~&Error applying flag ~A: ~A~%" key e)))
                     (setf remaining (cdr remaining)))))
                 (t
                  (push arg positional)
                  (setf remaining (cdr remaining))))))
    (nreverse positional)))

(defun list-registered-flags ()
  "Return a deduplicated list of all registered flag structs."
  (let ((seen '())
        (result '()))
    (maphash (lambda (k v)
               (declare (ignore k))
               (unless (member v seen :test #'eq)
                 (push v seen)
                 (push v result)))
             *flags*)
    (nreverse result)))
