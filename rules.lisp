;; rules handling. yes it is that short!
(in-package :hactar)
(defmacro defrule (name hook-spec (&rest args) &body body)
  "Define a rule that can modify the system prompt based on hook events.
   The body should return a string (the rule text to add/update) or nil (to remove the rule)."
  (let ((rule-fn-name (intern (format nil "RULE-~A" (symbol-name name))))
        (hook-var (if (listp hook-spec) (first hook-spec) hook-spec))
        (filter (when (listp hook-spec) (second hook-spec))))
    `(progn
       (defun ,rule-fn-name ,args
         (let ((rule-text (locally ,@body)))
           (if rule-text
               (setf (gethash ',name *active-rules*) rule-text)
               (remhash ',name *active-rules*))))
       (nhooks:add-hook ,hook-var
                        (make-instance 'nhooks:handler
                                       :fn (lambda (&rest hook-args)
                                             (if (or (not ,filter)
                                                     (apply ,filter hook-args))
						 (apply #',rule-fn-name hook-args)
						 nil))
                                       :name ',name)))))
