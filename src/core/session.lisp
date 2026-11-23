(in-package #:hactar-rebuild)

;;; Engine-owned session, history, context, and preset state.

;;; ---------------------------------------------------------------------------
;;; Chat history
;;; ---------------------------------------------------------------------------

(defun append-history (role content &key tool-calls)
  "Append a message alist to *CHAT-HISTORY* and return it."
  (let ((entry `((:role . ,(string-downcase (string role)))
                 (:content . ,(or content ""))
                 ,@(when tool-calls `((:tool_calls . ,tool-calls))))))
    (setf *chat-history* (append *chat-history* (list entry)))
    entry))

(defun clear-history ()
  (setf *chat-history* '())
  t)

(defun serialize-history-yaml (history)
  "Serialize chat HISTORY to a readable YAML string."
  (with-output-to-string (s)
    (dolist (message history)
      (format s "- role: ~A~%" (or (cdr (assoc :role message)) ""))
      (let ((content (cdr (assoc :content message))))
        (if (and content (find #\Newline content))
            (progn
              (format s "  content: |~%")
              (dolist (line (str:lines content))
                (format s "    ~A~%" line)))
            (format s "  content: ~S~%" (or content "")))))))

;;; ---------------------------------------------------------------------------
;;; Presets
;;; ---------------------------------------------------------------------------

(defstruct preset-def name description model rules files instructions)

(defun register-preset (name &key description model rules files instructions)
  (let ((key (key-string name)))
    (setf (gethash key *presets*)
          (make-preset-def :name key :description description :model model
                           :rules rules :files files :instructions instructions))))

(defmacro defpreset (name &key description model rules files instructions)
  `(register-preset ',name :description ,description :model ,model
                           :rules ',rules :files ',files
                           :instructions ,instructions))

(defun find-preset (name)
  (gethash (key-string name) *presets*))

(defun apply-preset (name)
  "Apply preset NAME to current session state (model, rules, files)."
  (let ((p (find-preset name)))
    (unless p (error "Unknown preset: ~A" name))
    (when (preset-def-model p)
      (setf *current-model* (preset-def-model p)))
    (dolist (r (preset-def-rules p))
      (setf (gethash (key-string (car r)) *active-rules*) (cdr r)))
    (dolist (f (preset-def-files p))
      (context-add-file f))
    (pushnew (preset-def-name p) *active-presets* :test #'string=)
    p))

;;; ---------------------------------------------------------------------------
;;; Context prompt composition
;;; ---------------------------------------------------------------------------

(defun render-system-prompt (&key base-instructions (root (or *repo-root* (uiop:getcwd))))
  "Compose the engine system prompt from instructions, stack, active rules, and context."
  (with-output-to-string (s)
    (format s "~A~%" (or base-instructions "You are Hactar, a deterministic pair programmer."))
    (when *stack*
      (format s "~%Stack: ~{~A~^, ~}~%" *stack*))
    (when (plusp (hash-table-count *active-rules*))
      (format s "~%Active Rules:~%")
      (maphash (lambda (k v) (format s "- ~A: ~A~%" k v)) *active-rules*))
    (when *context-files*
      (format s "~%Context Files:~%~A" (render-context-payload root)))))

;;; ---------------------------------------------------------------------------
;;; State capture / restore & resumable session files
;;; ---------------------------------------------------------------------------

(defun capture-state ()
  "Capture current engine session state as a plain plist."
  (let ((rules '()))
    (maphash (lambda (k v) (push (cons k v) rules)) *active-rules*)
    (list :repo-root (when *repo-root* (namestring *repo-root*))
          :stack (copy-list *stack*)
          :model *current-model*
          :context-files (copy-tree *context-files*)
          :chat-history (copy-tree *chat-history*)
          :active-presets (copy-list *active-presets*)
          :active-rules (sort rules #'string< :key #'car)
          :auto-update *auto-update*)))

(defun apply-state (plist &key merge)
  "Restore engine session state from PLIST. Replaces unless MERGE is true."
  (unless merge
    (setf *context-files* '()
          *chat-history* '()
          *active-presets* '())
    (clrhash *active-rules*))
  (when (getf plist :stack)
    (setf *stack* (copy-list (getf plist :stack))))
  (when (getf plist :model)
    (setf *current-model* (getf plist :model)))
  (dolist (entry (getf plist :context-files))
    (context-add-file (car entry) (cdr entry)))
  (when (getf plist :chat-history)
    (setf *chat-history* (if merge
                             (append *chat-history* (copy-tree (getf plist :chat-history)))
                             (copy-tree (getf plist :chat-history)))))
  (dolist (preset-name (getf plist :active-presets))
    (pushnew preset-name *active-presets* :test #'string=))
  (dolist (rule (getf plist :active-rules))
    (setf (gethash (car rule) *active-rules*) (cdr rule)))
  (when (member :auto-update plist)
    (setf *auto-update* (getf plist :auto-update)))
  plist)

(defun sessions-dir (&optional (root (or *repo-root* (uiop:getcwd))))
  (uiop:ensure-directory-pathname
   (merge-pathnames "sessions/" (context-dir root))))

(defun session-file-path (name &optional (root (or *repo-root* (uiop:getcwd))))
  (merge-pathnames (format nil "~A.lisp" (key-string name))
                   (sessions-dir root)))

(defun save-session (name &optional (root (or *repo-root* (uiop:getcwd))))
  "Persist captured session state as a readable (defsession ...) Lisp file via HACTAR-VFS."
  (let* ((rel (format nil ".hactar/sessions/~A.lisp" (key-string name)))
         (state (capture-state))
         (vfs (hactar-vfs:make-vfs :root root))
         (content (with-output-to-string (s)
                    (let ((*print-case* :downcase))
                      (format s ";;; Hactar session: ~A~%(defsession ~S~%"
                              (key-string name) (key-string name))
                      (loop for (k v) on state by #'cddr
                            do (format s "  ~S ~S~%" k v))
                      (format s ")~%")))))
    (hactar-vfs:vfs-write vfs rel content)
    (first (hactar-vfs:vfs-commit! vfs))))

(defun load-session (name &optional (root (or *repo-root* (uiop:getcwd))) &key merge)
  "Read a saved session file safely (*READ-EVAL* NIL) and apply its state."
  (let ((path (session-file-path name root)))
    (unless (probe-file path)
      (error "Session not found: ~A" (namestring path)))
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (let ((*read-eval* nil)
            (*package* (find-package :keyword)))
        (let ((form (read stream nil nil)))
          (unless (and (consp form)
                       (symbolp (first form))
                       (string-equal (symbol-name (first form)) "DEFSESSION"))
            (error "Invalid session file: ~A" (namestring path)))
          (apply-state (cddr form) :merge merge))))))

(defun list-sessions (&optional (root (or *repo-root* (uiop:getcwd))))
  "Return sorted names of saved sessions under ROOT."
  (let ((dir (sessions-dir root)))
    (when (uiop:directory-exists-p dir)
      (sort (mapcar #'pathname-name
                    (directory (merge-pathnames "*.lisp" dir)))
            #'string<))))
