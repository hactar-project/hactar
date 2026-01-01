;;; session.lisp - Hactar Session Management
;;; Sessions capture the full state of a hactar instance and persist
;;; them as Lisp files that can be loaded to restore state.
;;; Session files are stored as human-readable/editable Lisp forms.

(in-package :hactar)

;;* Data Structures

(defstruct hactar-session
  "A saved hactar session state."
  (name nil :type (or null string))
  (description nil :type (or null string))
  (timestamp nil :type (or null integer))
  ;; Project info
  (repo-root nil :type (or null string))
  (project-name nil :type (or null string))
  ;; Model state
  (model-name nil :type (or null string))
  (cheap-model nil :type (or null string))
  (docs-meta-model nil :type (or null string))
  ;; Context state
  (files '() :type list)
  (images '() :type list)         ; stored as plists without binary data
  (docs-context '() :type list)
  (errors-context '() :type list)
  ;; Chat state
  (chat-history '() :type list)
  ;; Stack & presets
  (stack '() :type list)
  (active-presets '() :type list)
  ;; Rules (name -> text pairs)
  (active-rules '() :type list)
  ;; Flags
  (git-autocommit nil :type boolean)
  (tool-use-enabled nil :type boolean)
  ;; Arbitrary metadata for extensibility
  (metadata nil :type list))

;;* Session Directory Management

(defun session-project-dir ()
  "Return the project-local sessions directory, ensuring it exists."
  (when *repo-root*
    (let ((dir (or *sessions-dir*
                   (merge-pathnames ".hactar/sessions/" *repo-root*))))
      (ensure-directories-exist dir)
      dir)))

(defun session-global-dir ()
  "Return the global sessions directory, ensuring it exists."
  (ensure-directories-exist *global-sessions-dir*)
  *global-sessions-dir*)

(defun session-file-path (name &key global)
  "Return the file path for a session by name.
   If GLOBAL is T, use the global sessions directory."
  (let ((dir (if global (session-global-dir) (session-project-dir))))
    (when dir
      (merge-pathnames (format nil "~A.session.lisp" name) dir))))

(defun session-search-directories ()
  "Return list of directories to search for session files (project-local first)."
  (remove nil
          (list (session-project-dir)
                (session-global-dir))))

;;* Capture Current State

(defun capture-current-session (name &key description)
  "Capture the current hactar state into a session struct."
  (let ((rules-alist '()))
    (maphash (lambda (k v) (push (cons k v) rules-alist)) *active-rules*)
    (make-hactar-session
     :name name
     :description description
     :timestamp (get-universal-time)
     :repo-root (when *repo-root* (namestring *repo-root*))
     :project-name *name*
     :model-name (when *current-model* (model-config-name *current-model*))
     :cheap-model *cheap-model*
     :docs-meta-model *docs-meta-model*
     :files (copy-list *files*)
     :images (mapcar (lambda (img)
                       ;; Store image metadata without binary data
                       (list :path (uiop:native-namestring (getf img :path))
                             :text (getf img :text)
                             :mime-type (getf img :mime-type)))
                     *images*)
     :docs-context (copy-list *docs-context*)
     :errors-context (copy-list *errors-context*)
     :chat-history (copy-list *chat-history*)
     :stack (copy-list *stack*)
     :active-presets (copy-list *active-presets*)
     :active-rules rules-alist
     :git-autocommit *git-autocommit*
     :tool-use-enabled *tool-use-enabled*)))

;;* Serialize / Deserialize

(defun serialize-session-to-stream (session stream)
  "Write a session to STREAM as readable Lisp forms."
  (let ((*print-readably* nil)
        (*print-pretty* t)
        (*print-right-margin* 100)
        (*print-case* :downcase))
    (format stream ";;; Hactar Session: ~A~%" (hactar-session-name session))
    (format stream ";;; Saved: ~A~%" (session-format-timestamp (hactar-session-timestamp session)))
    (when (hactar-session-description session)
      (format stream ";;; Description: ~A~%" (hactar-session-description session)))
    (format stream ";;; Project: ~A~%" (or (hactar-session-project-name session) "(unknown)"))
    (format stream ";;;~%~%")

    ;; Write each field as a self-contained form
    (format stream "(in-package :hactar)~%~%")

    ;; Use a single plist form for the whole session
    (format stream "(setf *loaded-session-data*~%")
    (format stream "  '(")

    ;; Name
    (format stream ":name ~S~%" (hactar-session-name session))

    ;; Description
    (format stream "    :description ~S~%" (hactar-session-description session))

    ;; Timestamp
    (format stream "    :timestamp ~A~%" (hactar-session-timestamp session))

    ;; Project info
    (format stream "    :repo-root ~S~%" (hactar-session-repo-root session))
    (format stream "    :project-name ~S~%" (hactar-session-project-name session))

    ;; Model
    (format stream "    :model-name ~S~%" (hactar-session-model-name session))
    (format stream "    :cheap-model ~S~%" (hactar-session-cheap-model session))
    (format stream "    :docs-meta-model ~S~%" (hactar-session-docs-meta-model session))

    ;; Files
    (format stream "    :files (~%")
    (dolist (f (hactar-session-files session))
      (format stream "      ~S~%" f))
    (format stream "    )~%")

    ;; Images (metadata only)
    (format stream "    :images (~%")
    (dolist (img (hactar-session-images session))
      (format stream "      ~S~%" img))
    (format stream "    )~%")

    ;; Stack
    (format stream "    :stack ~S~%" (hactar-session-stack session))

    ;; Active presets
    (format stream "    :active-presets ~S~%" (hactar-session-active-presets session))

    ;; Active rules
    (format stream "    :active-rules (~%")
    (dolist (rule (hactar-session-active-rules session))
      (format stream "      (~S . ~S)~%" (car rule) (cdr rule)))
    (format stream "    )~%")

    ;; Flags
    (format stream "    :git-autocommit ~A~%" (if (hactar-session-git-autocommit session) 't 'nil))
    (format stream "    :tool-use-enabled ~A~%" (if (hactar-session-tool-use-enabled session) 't 'nil))

    ;; Chat history
    (format stream "    :chat-history (~%")
    (dolist (msg (hactar-session-chat-history session))
      (format stream "      ~S~%" msg))
    (format stream "    )~%")

    ;; Docs context
    (format stream "    :docs-context (~%")
    (dolist (doc (hactar-session-docs-context session))
      (format stream "      ~S~%" doc))
    (format stream "    )~%")

    ;; Errors context
    (format stream "    :errors-context (~%")
    (dolist (err (hactar-session-errors-context session))
      (format stream "      ~S~%" err))
    (format stream "    )~%")

    ;; Metadata
    (format stream "    :metadata ~S~%" (hactar-session-metadata session))

    (format stream "  ))~%")))

(defvar *loaded-session-data* nil
  "Temporary variable used during session file loading.")

(defun deserialize-session-from-file (path)
  "Load a session from a Lisp file. Returns a hactar-session struct or NIL."
  (handler-case
      (progn
        (setf *loaded-session-data* nil)
        (load path :verbose nil :print nil)
        (when *loaded-session-data*
          (let ((data *loaded-session-data*))
            (setf *loaded-session-data* nil)
            (plist-to-session data))))
    (error (e)
      (format t "~&Error loading session file ~A: ~A~%" (uiop:native-namestring path) e)
      (setf *loaded-session-data* nil)
      nil)))

(defun plist-to-session (plist)
  "Convert a plist to a hactar-session struct."
  (make-hactar-session
   :name (getf plist :name)
   :description (getf plist :description)
   :timestamp (getf plist :timestamp)
   :repo-root (getf plist :repo-root)
   :project-name (getf plist :project-name)
   :model-name (getf plist :model-name)
   :cheap-model (getf plist :cheap-model)
   :docs-meta-model (getf plist :docs-meta-model)
   :files (getf plist :files)
   :images (getf plist :images)
   :docs-context (getf plist :docs-context)
   :errors-context (getf plist :errors-context)
   :chat-history (getf plist :chat-history)
   :stack (getf plist :stack)
   :active-presets (getf plist :active-presets)
   :active-rules (getf plist :active-rules)
   :git-autocommit (getf plist :git-autocommit)
   :tool-use-enabled (getf plist :tool-use-enabled)
   :metadata (getf plist :metadata)))

;;* Save / Load / Delete

(defun session/save (name &key description global)
  "Save the current state as a named session.
   GLOBAL: if T, save to the global sessions directory."
  (let* ((session (capture-current-session name :description description))
         (path (session-file-path name :global global)))
    (unless path
      (format t "~&Error: No sessions directory available.~%")
      (return-from session/save nil))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (serialize-session-to-stream session stream))
    (setf *current-session-name* name)
    (unless *silent*
      (format t "~&Session saved: ~A~%  → ~A~%" name (uiop:native-namestring path)))
    session))

(defun session/load (name &key global quiet)
  "Load and restore a session by name.
   Searches project-local first, then global."
  (let ((session (session/find-and-load name :global global)))
    (unless session
      (unless quiet
        (format t "~&Session not found: ~A~%" name))
      (return-from session/load nil))

    (clear-chat-history)
    (setf *files* nil)
    (setf *images* nil)
    (setf *docs-context* nil)
    (setf *errors-context* nil)
    (clrhash *active-rules*)
    (clear-session-overrides)

    (when (hactar-session-model-name session)
      (set-current-model (hactar-session-model-name session)))
    (when (hactar-session-cheap-model session)
      (setf *cheap-model* (hactar-session-cheap-model session)))
    (when (hactar-session-docs-meta-model session)
      (setf *docs-meta-model* (hactar-session-docs-meta-model session)))

    (dolist (f (hactar-session-files session))
      (if (probe-file f)
          (add-file-to-context f)
          (unless quiet
            (format t "~&  Warning: File no longer exists: ~A~%" f))))

    (setf *images* (hactar-session-images session))
    (setf *chat-history* (copy-list (hactar-session-chat-history session)))
    (save-transcript)

    (dolist (tech (hactar-session-stack session))
      (add-to-stack tech))

    (dolist (rule-pair (hactar-session-active-rules session))
      (setf (gethash (car rule-pair) *active-rules*) (cdr rule-pair)))

    (setf *git-autocommit* (hactar-session-git-autocommit session))
    (setf *tool-use-enabled* (hactar-session-tool-use-enabled session))

    (setf *docs-context* (copy-list (hactar-session-docs-context session)))
    (setf *errors-context* (copy-list (hactar-session-errors-context session)))

    (dolist (preset-name (hactar-session-active-presets session))
      (when (get-preset preset-name)
        (preset/load preset-name :quiet t)))

    (when *exposed-context-file*
      (ignore-errors (context-expose-upsert-files-section))
      (ignore-errors (context-expose-upsert-docs-section))
      (ignore-errors (context-expose-upsert-errors-section)))

    (setf *current-session-name* name)
    (unless quiet
      (format t "~&Session restored: ~A~%" name)
      (format t "  Model: ~A~%" (or (hactar-session-model-name session) "(none)"))
      (format t "  Files: ~A~%" (length (hactar-session-files session)))
      (format t "  Chat messages: ~A~%" (length (hactar-session-chat-history session)))
      (when (hactar-session-active-presets session)
        (format t "  Presets: ~{~A~^, ~}~%" (hactar-session-active-presets session)))
      (format t "  Saved: ~A~%" (session-format-timestamp (hactar-session-timestamp session))))
    session))

(defun session/find-and-load (name &key global)
  "Find a session file and deserialize it."
  (if global
      (let ((path (session-file-path name :global t)))
        (when (and path (probe-file path))
          (deserialize-session-from-file path)))
      ;; Search project-local first, then global
      (let ((project-path (session-file-path name :global nil))
            (global-path (session-file-path name :global t)))
        (cond
          ((and project-path (probe-file project-path))
           (deserialize-session-from-file project-path))
          ((and global-path (probe-file global-path))
           (deserialize-session-from-file global-path))
          (t nil)))))

(defun session/delete (name &key global)
  "Delete a session file."
  (let ((path (if global
                  (session-file-path name :global t)
                  ;; Try project-local first
                  (let ((p (session-file-path name :global nil)))
                    (if (and p (probe-file p)) p
                        (session-file-path name :global t))))))
    (when (and path (probe-file path))
      (delete-file path)
      (when (string= name *current-session-name*)
        (setf *current-session-name* nil))
      (format t "~&Deleted session: ~A~%" name)
      t)))

;;* Discovery & Listing

(defun discover-session-files ()
  "Find all session files across search directories.
   Returns list of (name path source) where source is :project or :global."
  (let ((sessions '()))
    (let ((dir (session-project-dir)))
      (when (and dir (uiop:directory-exists-p dir))
        (dolist (file (uiop:directory-files dir))
          (when (session-file-p file)
            (push (list (session-name-from-file file) file :project) sessions)))))
    (let ((dir (session-global-dir)))
      (when (uiop:directory-exists-p dir)
        (dolist (file (uiop:directory-files dir))
          (when (session-file-p file)
            (let ((name (session-name-from-file file)))
              (unless (find name sessions :key #'first :test #'string=)
                (push (list name file :global) sessions)))))))
    (sort sessions #'string< :key #'first)))

(defun session-file-p (path)
  "Check if a path looks like a session file."
  (let ((name (pathname-name path))
        (type (pathname-type path)))
    (and (stringp type)
         (string= type "lisp")
         (stringp name)
         (str:ends-with? ".session" name))))

(defun session-name-from-file (path)
  "Extract the session name from a session file path."
  (let ((name (pathname-name path)))
    (if (str:ends-with? ".session" name)
        (subseq name 0 (- (length name) (length ".session")))
        name)))

(defun session/list ()
  "List all available sessions with summary info."
  (let ((entries (discover-session-files)))
    (if entries
        (progn
          (format t "~&Sessions:~%")
          (dolist (entry entries)
            (destructuring-bind (name path source) entry
              (let ((session (deserialize-session-from-file path)))
                (format t "  ~A~A~A~%"
                        (if (string= name *current-session-name*) "* " "  ")
                        name
                        (if (eq source :global) " [global]" ""))
                (when session
                  (when (hactar-session-description session)
                    (format t "      ~A~%" (hactar-session-description session)))
                  (format t "      Model: ~A | Files: ~A | Messages: ~A | ~A~%"
                          (or (hactar-session-model-name session) "-")
                          (length (hactar-session-files session))
                          (length (hactar-session-chat-history session))
                          (session-format-timestamp (hactar-session-timestamp session))))))))
        (format t "~&No sessions found.~%"))))

(defun session/show (name)
  "Show detailed info about a session without loading it."
  (let ((session (session/find-and-load name)))
    (unless session
      (format t "~&Session not found: ~A~%" name)
      (return-from session/show nil))
    (format t "~&Session: ~A~%" (hactar-session-name session))
    (when (hactar-session-description session)
      (format t "Description: ~A~%" (hactar-session-description session)))
    (format t "Saved: ~A~%" (session-format-timestamp (hactar-session-timestamp session)))
    (format t "Project: ~A~%" (or (hactar-session-project-name session) "(unknown)"))
    (format t "Repo: ~A~%" (or (hactar-session-repo-root session) "(unknown)"))
    (format t "~%Model: ~A~%" (or (hactar-session-model-name session) "(none)"))
    (when (hactar-session-cheap-model session)
      (format t "Cheap model: ~A~%" (hactar-session-cheap-model session)))
    (format t "~%Files (~A):~%" (length (hactar-session-files session)))
    (dolist (f (hactar-session-files session))
      (let ((exists (probe-file f)))
        (format t "  ~A~A~%" f (if exists "" " [missing]"))))
    (when (hactar-session-images session)
      (format t "~%Images (~A):~%" (length (hactar-session-images session)))
      (dolist (img (hactar-session-images session))
        (format t "  ~A~%" (getf img :path))))
    (format t "~%Chat history: ~A messages~%" (length (hactar-session-chat-history session)))
    (when (hactar-session-stack session)
      (format t "Stack: ~{~A~^, ~}~%" (hactar-session-stack session)))
    (when (hactar-session-active-presets session)
      (format t "Active presets: ~{~A~^, ~}~%" (hactar-session-active-presets session)))
    (when (hactar-session-active-rules session)
      (format t "Active rules: ~{~A~^, ~}~%"
              (mapcar #'car (hactar-session-active-rules session))))
    (format t "~%Flags:~%")
    (format t "  Git autocommit: ~A~%" (if (hactar-session-git-autocommit session) "yes" "no"))
    (format t "  Tool use: ~A~%" (if (hactar-session-tool-use-enabled session) "yes" "no"))
    session))

;;* Auto-save / Auto-restore

(defun session/auto-save ()
  "Auto-save current session if enabled."
  (when (and *auto-save-session* *repo-root*)
    (handler-case
        (session/save *session-auto-save-name*
                      :description "Auto-saved on exit")
      (error (e)
        (format *error-output* "~&Warning: Failed to auto-save session: ~A~%" e)))))

(defun session/auto-restore ()
  "Auto-restore session if auto-save is enabled and an autosave exists."
  (when (and *auto-save-session* *repo-root*)
    (let ((path (session-file-path *session-auto-save-name*)))
      (when (and path (probe-file path))
        (unless *silent*
          (format t "~&Restoring auto-saved session...~%"))
        (session/load *session-auto-save-name* :quiet *silent*)))))

;;* Utility

(defun session-format-timestamp (universal-time)
  "Format a universal time for session display."
  (if universal-time
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time universal-time)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour min sec))
      "unknown"))

;;* REPL Commands

(define-command session (args)
  "Manage sessions. Usage: /session [name] - list or load a session."
  (if args
      (session/load (first args))
      (session/list))
  :acp (lambda (cmd-args)
         (if cmd-args
             (let ((session (session/load (first cmd-args))))
               (if session
                   `(("text" . ,(format nil "Session restored: ~A" (first cmd-args)))
                     ("data" . (("session" . ,(first cmd-args))
                                ("files" . ,(length (hactar-session-files session)))
                                ("messages" . ,(length (hactar-session-chat-history session))))))
                   `(("text" . ,(format nil "Session not found: ~A" (first cmd-args))))))
             (let ((entries (discover-session-files)))
               `(("text" . ,(format nil "~A session(s) available." (length entries)))
                 ("data" . ,(coerce
                             (mapcar (lambda (e)
                                       (destructuring-bind (name path source) e
                                         (declare (ignore path))
                                         `(("name" . ,name)
                                           ("source" . ,(string-downcase (symbol-name source)))
                                           ("current" . ,(if (string= name *current-session-name*) t :false)))))
                                     entries)
                             'vector)))))))

(define-command session.save (args)
  "Save current state as a session.
Usage: /session.save <name> [--global] [--description \"desc\"]"
  (if (null args)
      (format t "~&Usage: /session.save <name> [--global] [--description \"desc\"]~%")
      (let* ((name (first args))
             (rest-args (rest args))
             (global-p (member "--global" rest-args :test #'string=))
             (desc-pos (position "--description" rest-args :test #'string=))
             (description (when desc-pos (nth (1+ desc-pos) rest-args))))
        (session/save name :description description :global global-p)))
  :acp (lambda (cmd-args)
         (if cmd-args
             (let* ((name (first cmd-args))
                    (session (session/save name)))
               (if session
                   `(("text" . ,(format nil "Session saved: ~A" name))
                     ("data" . (("session" . ,name))))
                   `(("text" . "Failed to save session."))))
             `(("text" . "Usage: /session.save <name>")))))

(define-command session.load (args)
  "Load and restore a session.
Usage: /session.load <name> [--global]"
  (if (null args)
      (format t "~&Usage: /session.load <name> [--global]~%")
      (let* ((name (first args))
             (global-p (member "--global" (rest args) :test #'string=)))
        (session/load name :global global-p)))
  :acp (lambda (cmd-args)
         (if cmd-args
             (let ((session (session/load (first cmd-args))))
               (if session
                   `(("text" . ,(format nil "Session restored: ~A" (first cmd-args)))
                     ("data" . (("session" . ,(first cmd-args))
                                ("model" . ,(or (hactar-session-model-name session) :null))
                                ("files" . ,(length (hactar-session-files session)))
                                ("messages" . ,(length (hactar-session-chat-history session))))))
                   `(("text" . ,(format nil "Session not found: ~A" (first cmd-args))))))
             `(("text" . "Usage: /session.load <name>")))))

(define-command session.list (args)
  "List all available sessions."
  (declare (ignore args))
  (session/list)
  :acp (lambda (cmd-args)
         (declare (ignore cmd-args))
         (let ((entries (discover-session-files)))
           `(("text" . ,(format nil "~A session(s) available." (length entries)))
             ("data" . ,(coerce
                         (mapcar (lambda (e)
                                   (destructuring-bind (name path source) e
                                     (let ((session (deserialize-session-from-file path)))
                                       `(("name" . ,name)
                                         ("source" . ,(string-downcase (symbol-name source)))
                                         ("current" . ,(if (string= name *current-session-name*) t :false))
                                         ("model" . ,(if session (or (hactar-session-model-name session) :null) :null))
                                         ("files" . ,(if session (length (hactar-session-files session)) 0))
                                         ("messages" . ,(if session (length (hactar-session-chat-history session)) 0))
                                         ("timestamp" . ,(if (and session (hactar-session-timestamp session))
                                                             (session-format-timestamp (hactar-session-timestamp session))
                                                             :null))))))
                                 entries)
                         'vector))))))

(define-command session.show (args)
  "Show details of a session without loading it.
Usage: /session.show <name>"
  (if args
      (session/show (first args))
      (format t "~&Usage: /session.show <name>~%"))
  :acp (lambda (cmd-args)
         (if cmd-args
             (let ((session (session/find-and-load (first cmd-args))))
               (if session
                   `(("text" . ,(with-output-to-string (*standard-output*)
                                  (session/show (first cmd-args))))
                     ("data" . (("name" . ,(hactar-session-name session))
                                ("description" . ,(or (hactar-session-description session) :null))
                                ("model" . ,(or (hactar-session-model-name session) :null))
                                ("files" . ,(length (hactar-session-files session)))
                                ("messages" . ,(length (hactar-session-chat-history session)))
                                ("timestamp" . ,(if (hactar-session-timestamp session)
                                                    (session-format-timestamp (hactar-session-timestamp session))
                                                    :null))
                                ("stack" . ,(coerce (hactar-session-stack session) 'vector))
                                ("presets" . ,(coerce (hactar-session-active-presets session) 'vector)))))
                   `(("text" . ,(format nil "Session not found: ~A" (first cmd-args))))))
             `(("text" . "Usage: /session.show <name>")))))

(define-command session.delete (args)
  "Delete a session.
Usage: /session.delete <name> [--global]"
  (if args
      (let* ((name (first args))
             (global-p (member "--global" (rest args) :test #'string=)))
        (if (confirm-action (format nil "Delete session '~A'?" name))
            (session/delete name :global global-p)
            (format t "~&Cancelled.~%")))
      (format t "~&Usage: /session.delete <name>~%"))
  :acp (lambda (cmd-args)
         (if cmd-args
             (if (session/delete (first cmd-args))
                 `(("text" . ,(format nil "Deleted session: ~A" (first cmd-args))))
                 `(("text" . ,(format nil "Session not found: ~A" (first cmd-args)))))
             `(("text" . "Usage: /session.delete <name>")))))

(define-command session.auto (args)
  "Toggle auto-save session on exit.
Usage: /session.auto [on|off]"
  (cond
    ((null args)
     (setf *auto-save-session* (not *auto-save-session*)))
    ((string-equal (first args) "on")
     (setf *auto-save-session* t))
    ((string-equal (first args) "off")
     (setf *auto-save-session* nil))
    (t (format t "~&Usage: /session.auto [on|off]~%")))
  :acp (lambda (cmd-args)
         (cond
           ((null cmd-args)
            (setf *auto-save-session* (not *auto-save-session*)))
           ((string-equal (first cmd-args) "on")
            (setf *auto-save-session* t))
           ((string-equal (first cmd-args) "off")
            (setf *auto-save-session* nil)))
         `(("text" . ,(format nil "Session auto-save is now ~A."
                              (if *auto-save-session* "enabled" "disabled")))
           ("data" . (("autoSave" . ,(if *auto-save-session* t :false)))))))

;;* CLI Sub-command

(define-sub-command session (args)
  "Manage hactar sessions.
Usage: hactar session list
       hactar session save <name> [--description \"...\"]
       hactar session load <name>
       hactar session show <name>
       hactar session delete <name>"
  (let ((subcommand (first args))
        (rest-args (rest args)))
    (cond
      ((or (null subcommand) (string= subcommand "list"))
       (session/list))
      ((string= subcommand "save")
       (when rest-args
         (session/save (first rest-args)
                       :description (when (and (rest rest-args)
                                               (string= (second rest-args) "--description"))
                                     (third rest-args)))))
      ((string= subcommand "load")
       (when rest-args
         (session/load (first rest-args))))
      ((string= subcommand "show")
       (when rest-args
         (session/show (first rest-args))))
      ((string= subcommand "delete")
       (when rest-args
         (session/delete (first rest-args))))
      (t
       (format t "~&Unknown session command: ~A~%" subcommand)))))
