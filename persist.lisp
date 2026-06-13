;;* state persistence
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(capture-state apply-state defsession)))

(defun capture-state ()
  "Capture the current hactar context state as a canonical plist."
  (let ((rules '()))
    (maphash (lambda (k v) (push (cons k v) rules)) *active-rules*)
    (list :name *name*
          :author *author*
          :language *language*
          :stack (copy-list *stack*)
          :model (when *current-model* (model-config-name *current-model*))
          :files (copy-list *files*)
          :images (copy-list *images*)
          :docs-context (copy-list *docs-context*)
          :errors-context (copy-list *errors-context*)
          :chat-history (copy-list *chat-history*)
          :active-presets (copy-list *active-presets*)
          :active-rules rules
          :git-autocommit *git-autocommit*
          :tool-use-enabled *tool-use-enabled*)))

(defun apply-state (plist &key merge)
  "Apply a captured state PLIST. When MERGE is NIL, replace context first."
  (unless merge
    (setf *files* nil *images* nil *docs-context* nil *errors-context* nil)
    (clrhash *active-rules*))
  (when (getf plist :name) (setf *name* (getf plist :name)))
  (when (getf plist :author) (setf *author* (getf plist :author)))
  (when (getf plist :language) (setf *language* (getf plist :language)))
  (when (getf plist :stack) (setf *stack* (copy-list (getf plist :stack))))
  (when (getf plist :model)
    (ignore-errors (set-current-model (getf plist :model))))
  (dolist (f (getf plist :files))
    (when (probe-file f) (pushnew f *files* :test #'string=)))
  (when (getf plist :images)
    (setf *images* (copy-list (getf plist :images))))
  (when (getf plist :docs-context)
    (setf *docs-context* (copy-list (getf plist :docs-context))))
  (when (getf plist :errors-context)
    (setf *errors-context* (copy-list (getf plist :errors-context))))
  (when (getf plist :chat-history)
    (setf *chat-history* (copy-list (getf plist :chat-history))))
  (dolist (rule (getf plist :active-rules))
    (setf (gethash (car rule) *active-rules*) (cdr rule)))
  (when (member :git-autocommit plist)
    (setf *git-autocommit* (getf plist :git-autocommit)))
  (when (member :tool-use-enabled plist)
    (setf *tool-use-enabled* (getf plist :tool-use-enabled)))
  plist)

(defmacro defsession (name &body plist)
  "A session file is a (defsession <name> ...plist...) form. Loading it records
   the captured-state plist into *loaded-state* so apply-state can restore it."
  (declare (ignore name))
  `(setf *loaded-state* (list ,@plist)))

