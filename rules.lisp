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

(defun run-rules-show ()
  "Implementation of rules.show command."
  (if (zerop (hash-table-count *active-rules*))
      (format t "No active rules.~%")
      (progn
        (format t "Active Rules:~%")
        (maphash (lambda (name text)
                   (format t "  - ~A:~%~A~%~%" name text))
                 *active-rules*))))

(define-command rules.show (args)
  "Show currently active rules."
  (declare (ignore args))
  (run-rules-show)
  :slash t :sub t)

(defun list-available-rules ()
  "Returns a list of available rule names."
  (ensure-directories-exist *hactar-rules-path*)
  (let ((rule-files (uiop:directory-files *hactar-rules-path* "*.lisp")))
    (mapcar #'pathname-name rule-files)))

(defun run-rules-list ()
  "Implementation of rules.list command."
  (let ((rules (list-available-rules)))
    (if rules
        (progn
          (format t "~&Available Rules:~%")
          (dolist (r rules)
            (format t "  ~A~%" r)))
        (format t "No rules found in ~A~%" *hactar-rules-path*))))

(define-command rules.list (args)
  "List available rules."
  (declare (ignore args))
  (run-rules-list)
  :slash t :sub t)

(defun run-rules-add (args)
  "Implementation of rules.add command."
  (let ((name (first args)))
    (unless name
      (format t "Usage: /rules.add <name>~%")
      (return-from run-rules-add))
    (let ((rule-file (merge-pathnames (format nil "~A.lisp" name) *hactar-rules-path*)))
      (if (probe-file rule-file)
          (progn
            (load rule-file)
            (format t "Loaded rule: ~A~%" name))
          (format t "Rule '~A' not found in ~A.~%" name *hactar-rules-path*)))))

(define-command rules.add (args)
  "Attach a rule to the current context by loading its lisp file."
  (run-rules-add args)
  :slash t :sub t)

(defun run-rules-install (args)
  "Implementation of rules.install command."
  (let ((source (first args)))
    (unless source
      (format t "Usage: /rules.install <source>~%")
      (return-from run-rules-install))

    (ensure-directories-exist *hactar-rules-path*)

    ;; Expand shorthand
    (when (and (not (str:starts-with? "http" source))
               (not (str:starts-with? "." source))
               (not (str:starts-with? "/" source))
               (not (str:ends-with? ".zip" source))
               (cl-ppcre:scan "^[^/]+/[^/]+$" source))
      (setf source (format nil "https://github.com/~A.git" source)))

    (cond
      ((or (str:starts-with? "http" source) (str:starts-with? "git@" source))
       (let ((temp-dir (uiop:ensure-directory-pathname (format nil "/tmp/hactar-rule-clone-~A/" (uuid:make-v4-uuid)))))
         (ensure-directories-exist temp-dir)
         (format t "Cloning ~A...~%" source)
         (multiple-value-bind (out err code)
             (uiop:run-program (list "git" "clone" "--depth" "1" source (uiop:native-namestring temp-dir))
                               :ignore-error-status t :output :string :error-output :string)
           (if (zerop code)
               (let ((rules-dir (merge-pathnames "rules/" temp-dir)))
                 (if (uiop:directory-exists-p rules-dir)
                     (let ((lisp-files (uiop:directory-files rules-dir "*.lisp")))
                       (if lisp-files
                           (dolist (f lisp-files)
                             (let ((dest (merge-pathnames (file-namestring f) *hactar-rules-path*)))
                               (when (probe-file dest) (delete-file dest))
                               (uiop:copy-file f dest)
                               (format t "Installed rule: ~A~%" (pathname-name f))))
                           (format t "No .lisp files found in rules/ subdirectory.~%")))
                     (format t "No rules/ subdirectory found in repository.~%")))
               (format t "Git clone failed: ~A~%" err))
           (uiop:delete-directory-tree temp-dir :validate t))))
      (t (format t "Unsupported source type for rules.install: ~A~%" source)))))

(define-command rules.install (args)
  "Install rules from a GitHub repository.
   Usage: /rules.install <source>
   Source can be:
     - user/repo (shorthand for GitHub)
     - https://github.com/..."
  (run-rules-install args)
  :slash t :sub t)
