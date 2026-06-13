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
  "Define a command namespaced under MODE that only runs when the mode is active."
  (let ((full-name (format nil "~A.~A"
                           (string-downcase (string mode))
                           (string-downcase (string name))))
        (mode-str (string-downcase (string mode))))
    `(define-command ,full-name ,args
       (if (member ,mode-str *active-modes* :test #'string=)
           (locally ,@body)
           (format t "Mode '~A' is not active. Activate it first.~%" ,mode-str)))))

(defun get-mode (name)
  (gethash (string-downcase (string name)) *modes*))

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
  "Activate a mode: inject its rules and load its skills."
  (let ((m (get-mode name)))
    (when (and m (not (mode-active-p m)))
      (setf (mode-active-p m) t)
      (pushnew (mode-name-string m) *active-modes* :test #'string=)
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
  "Deactivate a mode: remove its rules."
  (let ((m (get-mode name)))
    (when (and m (mode-active-p m))
      (setf (mode-active-p m) nil)
      (setf *active-modes* (remove (mode-name-string m) *active-modes* :test #'string=))
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
