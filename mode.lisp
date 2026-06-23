;;* Modes
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defmode defmode-command
            activate-mode deactivate-mode active-modes
            get-mode get-active-mode-rules refresh-modes)))

(defstruct mode
  (name nil :type (or null symbol string))
  (description nil :type (or null string))
  (when-fn nil :type (or null function))
  (rules nil :type (or null string))
  (skills '() :type list)
  (on-activate nil :type (or null function))
  (on-deactivate nil :type (or null function))
  (command-definers '() :type list)
  (active-command-names '() :type list)
  (active-p nil :type boolean))

(defun mode-name-string (m)
  (let ((n (mode-name m)))
    (if (stringp n) n (string-downcase (symbol-name n)))))

(defmacro defmode (name description &body body)
  "Define a mode: a concrete command bundle activated by :when or manually.
   Keywords: :when :rules :skills :commands :on-activate :on-deactivate.
   :commands is a list of define-command forms owned by this mode."
  (let ((when-clause (getf body :when))
        (rules (getf body :rules))
        (skills (getf body :skills))
        (on-activate (getf body :on-activate))
        (on-deactivate (getf body :on-deactivate))
        (commands (getf body :commands))
        (name-str (string-downcase (string name))))
    `(progn
       (let ((existing (gethash ,name-str *modes*)))
         (when (and existing (mode-active-p existing))
           (deactivate-mode ,name-str)))
       (setf (gethash ,name-str *modes*)
             (make-mode :name ',name
                        :description ,description
                        :when-fn ,(when when-clause `(lambda () ,when-clause))
                        :rules ,rules
                        :skills ',skills
                        :on-activate ,on-activate
                        :on-deactivate ,on-deactivate))
       ,@commands
       ,(when when-clause
          `(when (funcall (mode-when-fn (gethash ,name-str *modes*)))
             (activate-mode ,name-str)))
       ',name)))

(defmacro defmode-command (mode name args &body body)
  "Register a command owned by MODE. The command is defined (registered in the
   command tables) under its bare NAME only while MODE is active, and removed
   when MODE is deactivated. This lets different modes provide commands with the
   same name (e.g. route.create for both a Rails and a React-Router mode)."
  (let ((cmd-name (string-downcase (string name)))
        (mode-str (string-downcase (string mode))))
    `(register-mode-command ,mode-str ,cmd-name
                            (lambda ()
                              (define-command ,name ,args ,@body)))))

(defun get-mode (name)
  (gethash (string-downcase (string name)) *modes*))

(defun register-mode-command (mode-name cmd-name definer)
  "Attach a command DEFINER thunk to the mode named MODE-NAME. DEFINER, when
   called, defines the command via DEFINE-COMMAND and registers it in the command
   tables. The command is only defined while the mode is active; if the mode is
   already active it is defined immediately."
  (let* ((key (string-downcase (string mode-name)))
         (m (or (gethash key *modes*)
                (setf (gethash key *modes*) (make-mode :name mode-name)))))
    (setf (mode-command-definers m)
          (cons (cons cmd-name definer)
                (remove cmd-name (mode-command-definers m)
                        :key #'car :test #'string=)))
    (when (mode-active-p m)
      (funcall definer)
      (pushnew cmd-name (mode-active-command-names m) :test #'string=))
    cmd-name))

(defun other-active-mode-definer (cmd-name except-mode)
  "Return a command DEFINER thunk for CMD-NAME from an active mode other than
   EXCEPT-MODE, or NIL if no other active mode provides it."
  (loop for name in *active-modes*
        for m = (get-mode name)
        when (and m (not (eq m except-mode)))
          do (let ((entry (assoc cmd-name (mode-command-definers m) :test #'string=)))
               (when entry (return (cdr entry))))))

(defun load-docs-for-mode (name-str)
  "Locate and load stack-specific documentation files for a mode."
  (let* ((search-dirs (list (when (and (boundp '*repo-root*) *repo-root*) (merge-pathnames "hactar/docs/" *repo-root*))
                            (when (and (boundp '*repo-root*) *repo-root*) (merge-pathnames "docs/" *repo-root*))
                            (when (and (boundp '*repo-root*) *repo-root*) (merge-pathnames "wiki/" *repo-root*))
                            (when (and (boundp '*hactar-config-path*) *hactar-config-path*)
                              (uiop:subpathname *hactar-config-path* "docs/"))
                            (when (and (boundp '*hactar-config-path*) *hactar-config-path*)
                              (uiop:subpathname *hactar-config-path* "wiki/"))))
         (prefix-patterns (list name-str (format nil "npm_~A" name-str))))
    (dolist (dir (remove nil search-dirs))
      (when (uiop:directory-exists-p dir)
        (dolist (file (uiop:directory-files dir))
          (let ((filename (pathname-name file))
                (type (pathname-type file)))
            (when (and (string-equal type "lisp")
                       (some (lambda (pref)
                               (or (string-equal filename pref)
                                   (str:starts-with? (format nil "~A_" pref) filename)
                                   (str:starts-with? (format nil "~A@" pref) filename)))
                             prefix-patterns))
              (unless (and (boundp '*silent*) *silent*)
                (format t "Auto-loading stack documentation: ~A~%" file))
              (load file :if-does-not-exist nil))))))))

(defun activate-mode (name)
  "Activate a mode: define its commands, inject its rules and load its skills."
  (let ((m (get-mode name)))
    (when (and m (not (mode-active-p m)))
      (setf (mode-active-p m) t)
      (pushnew (mode-name-string m) *active-modes* :test #'string=)
      (dolist (entry (reverse (mode-command-definers m)))
        (let ((cmd-name (car entry))
              (definer (cdr entry)))
          (funcall definer)
          (pushnew cmd-name (mode-active-command-names m) :test #'string=)))
      (when (mode-rules m)
        (setf (gethash (format nil "mode/~A" (mode-name-string m)) *active-rules*)
              (mode-rules m)))
      (dolist (skill (mode-skills m))
        (when (fboundp 'run-skills-load)
          (ignore-errors
            (run-skills-load (list :subcommand (list (string-downcase (string skill))))))))
      (load-docs-for-mode (mode-name-string m))
      (when (mode-on-activate m) (funcall (mode-on-activate m)))
      m)))

(defun deactivate-mode (name)
  "Deactivate a mode: remove its commands and rules."
  (let ((m (get-mode name)))
    (when (and m (mode-active-p m))
      (setf (mode-active-p m) nil)
      (setf *active-modes* (remove (mode-name-string m) *active-modes* :test #'string=))
      (dolist (cmd-name (mode-active-command-names m))
        (let ((other (other-active-mode-definer cmd-name m)))
          (if other
              (funcall other)
              (undefine-command cmd-name))))
      (setf (mode-active-command-names m) '())
      (remhash (format nil "mode/~A" (mode-name-string m)) *active-rules*)
      (when (mode-on-deactivate m) (funcall (mode-on-deactivate m)))
      m)))

(defun active-modes ()
  (copy-list *active-modes*))

(defun refresh-modes ()
  "Re-evaluate :when predicates for all modes and update activation."
  (maphash (lambda (name m)
             (when (mode-when-fn m)
               (if (funcall (mode-when-fn m))
                   (activate-mode name)
                   (deactivate-mode name))))
           *modes*))

(defun get-active-mode-rules ()
  "Collect active mode rules into a string for the system prompt."
  (let ((rules '()))
    (dolist (name *active-modes*)
      (let ((m (get-mode name)))
        (when (and m (mode-rules m))
          (push (mode-rules m) rules))))
    (if rules
        (format nil "~%## Active Mode Rules~%~{~A~%~}" (nreverse rules))
        "")))

;;* Commands
(defun mode (args)
  "Manage modes. Usage: /mode [name] | /mode off <name> | /mode list"
  (cond
    ((null args)
     (if (zerop (hash-table-count *modes*))
         (format t "No modes defined.~%")
         (progn
           (format t "Modes:~%")
           (maphash (lambda (name m)
                      (format t "  ~A~A — ~A~%"
                              name
                              (if (mode-active-p m) " [ACTIVE]" "")
                              (or (mode-description m) "")))
                    *modes*))))
    ((string-equal (first args) "list")
     (mode '()))
    ((string-equal (first args) "off")
     (if (second args)
         (deactivate-mode (second args))
         (format t "Usage: /mode off <name>~%")))
    (t
     (if (get-mode (first args))
         (progn
           (activate-mode (first args))
           (format t "Activated mode: ~A~%" (first args)))
         (format t "Unknown mode: ~A~%" (first args))))))

(define-command mode (args)
  "Manage modes. Usage: /mode [name] | /mode off <name> | /mode list"
  (mode args))

(define-command feature (args)
  "Manage features. Alias for /mode. Usage: /feature [name] | /feature off <name> | /feature list"
  (mode args))

(define-command feature.list (args)
  "List all available and active features (modes)."
  (mode '("list")))

(define-sub-command feature.list (args)
  "List all available and active features."
  (declare (ignore args))
  (mode '("list")))
