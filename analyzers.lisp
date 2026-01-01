;; Analyzers
(in-package :hactar)
(defvar *analyzer-registry* (make-hash-table :test 'equal)
                            "Registry storing analyzer definitions and their enabled status. Maps symbol name to plist (:fn function :hooks list :enabled boolean).")

(defun is-analyzer-enabled? (analyzer-name)
  "Check if an analyzer is currently enabled."
  (getf (gethash analyzer-name *analyzer-registry*) :enabled))

(defun set-analyzer-enabled (analyzer-name enabled-p)
  "Set the enabled status of an analyzer."
  (let ((entry (gethash analyzer-name *analyzer-registry*)))
    (if entry
      (setf (getf entry :enabled) enabled-p)
      (warn "Attempted to set status for unknown analyzer: ~A" analyzer-name))))

(defmacro def-analyzer (name hooks enabled (&rest args) &body body)
  "Define an analyzer function, register it, and attach it to specified hooks.
   ENABLED is a boolean indicating if the analyzer should be enabled by default."
  (let ((analyzer-fn-name (intern (format nil "ANALYZER-~A" (symbol-name name)))))
    `(progn
       (defun ,analyzer-fn-name ,args
         ,@body)
       (setf (gethash ',name *analyzer-registry*)
             (list :fn #',analyzer-fn-name
                   :hooks ',hooks
                   :enabled ,enabled))
       ,@(loop for hook-spec in hooks
               collect (let* ((hook-var (if (listp hook-spec) (first hook-spec) hook-spec))
                              (filter (when (listp hook-spec) (second hook-spec))))
                         `(nhooks:add-hook ,hook-var
                                           (make-instance 'nhooks:handler
                                                          :fn (lambda (&rest hook-args)
                                                                (when (is-analyzer-enabled? ',name)
                                                                  (if (or (not ,filter) (apply ,filter hook-args))
                                                                      (apply #',analyzer-fn-name hook-args)
                                                                      nil)))
                                                          :name ',name)))))))
