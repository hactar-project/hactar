;; Generic router using regex pattern matching
(in-package :hactar)

(defvar *routes* (make-hash-table :test 'equal)
  "Hash table mapping route names to route definitions.")

(defvar *route-reinit-hooks* '()
  "List of functions to re-register default/built-in routes when *routes* is empty.")

(defstruct route
  "Represents a generic route with regex pattern matching."
  name           ; Route name (symbol)
  pattern        ; Regex pattern string
  param-names    ; List of parameter names to extract from regex groups
  priority       ; Integer priority (higher = checked first)
  handler)       ; Function that handles the route

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
  "Match and execute a route for the given input string."
  (multiple-value-bind (route params)
      (match-route input)
    (if route
        (apply (route-handler route)
               (mapcar #'cdr params))
        (progn
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
