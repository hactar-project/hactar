(in-package #:hactar-rebuild)

;;; Emacs/nhooks-inspired hook runtime. `defhook` (in surface.lisp) declares a
;;; named hook; handlers are attached here and run newest-first with per-handler
;;; error isolation.

(defun add-handler (hook-name fn)
  "Attach FN to the named hook. Newest handlers run first."
  (let* ((key (string-downcase (string hook-name)))
         (hook (gethash key *hooks*)))
    (unless hook
      (error "Unknown hook ~A" hook-name))
    (push fn (hook-def-handlers hook))
    fn))

(defun clear-handlers (hook-name)
  "Remove all handlers from the named hook (keeps the hook declared)."
  (let ((hook (gethash (string-downcase (string hook-name)) *hooks*)))
    (when hook
      (setf (hook-def-handlers hook) '()))))

(defun run-hook (hook-name &rest args)
  "Run every handler of HOOK-NAME with ARGS, newest first.
Collects results; each handler's error is isolated and logged."
  (let ((hook (gethash (string-downcase (string hook-name)) *hooks*)))
    (when hook
      (loop for fn in (hook-def-handlers hook)
            collect (handler-case (apply fn args)
                      (error (e)
                        (warn "hook ~A handler failed: ~A" hook-name e)
                        nil))))))
