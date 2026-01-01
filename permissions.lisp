;; Tool Permissions System
;; Flexible, extensible permissions for Hactar's tool calling
(in-package :hactar)

;;* Data Structures

(defstruct permission-rule
  "A rule that determines whether a tool call is allowed."
  (name "" :type string)
  (priority 0 :type integer)
  (tool-pattern nil)
  (predicate nil :type (or null function))
  (description "" :type string)
  (source :system :type keyword))

(defstruct session-override
  "A runtime override created from interactive confirmation."
  (tool-name "" :type string)
  (match-type :exact-args :type keyword)
  (args-fingerprint nil)
  (decision :allow :type keyword))

;;* Core API — Rule Registration

(defun register-permission-rule (rule)
  "Register a new permission rule, maintaining priority order (highest first).
   If a rule with the same name exists, it is replaced."
  (unregister-permission-rule (permission-rule-name rule))
  (setf *permission-rules*
        (sort (cons rule (copy-list *permission-rules*))
              #'> :key #'permission-rule-priority))
  rule)

(defun unregister-permission-rule (name)
  "Remove a rule by name."
  (setf *permission-rules*
        (remove name *permission-rules*
                :key #'permission-rule-name :test #'string=)))

(defun list-permission-rules ()
  "List all active rules."
  *permission-rules*)

(defun clear-session-overrides ()
  "Clear all session overrides."
  (setf *session-overrides* '()))

;;* Args Fingerprinting

(defun fingerprint-args (args)
  "Create a fingerprint (hash) of tool arguments for exact-match overrides."
  (sxhash (prin1-to-string args)))

;;* Permission Resolution

(defun check-session-overrides (tool-name args)
  "Check session overrides for a matching decision. Returns :allow, :deny, or NIL."
  (dolist (override *session-overrides*)
    (when (string= tool-name (session-override-tool-name override))
      (case (session-override-match-type override)
        (:tool-always
         (return-from check-session-overrides (session-override-decision override)))
        (:tool-deny
         (return-from check-session-overrides :deny))
        (:exact-args
         (when (eql (fingerprint-args args) (session-override-args-fingerprint override))
           (return-from check-session-overrides (session-override-decision override)))))))
  nil)

(defun rule-matches-tool-p (rule tool-name)
  "Check if a permission rule's tool-pattern matches the given tool-name."
  (let ((pattern (permission-rule-tool-pattern rule)))
    (cond
      ((eq pattern :any) t)
      ((stringp pattern) (string= pattern tool-name))
      (t nil))))

(defun resolve-permission (tool-name args)
  "Check all rules in priority order. Returns :allow, :deny, or :confirm.
   First checks session overrides, then walks rules."
  (let ((override-decision (check-session-overrides tool-name args)))
    (when override-decision
      (log-permission-decision tool-name args override-decision "session-override")
      (return-from resolve-permission override-decision)))

  (dolist (rule *permission-rules*)
    (when (rule-matches-tool-p rule tool-name)
      (let ((decision (funcall (permission-rule-predicate rule) tool-name args)))
        (unless (eq decision :abstain)
          (log-permission-decision tool-name args decision (permission-rule-name rule))
          (return-from resolve-permission decision)))))

  (log-permission-decision tool-name args :confirm "default-fallback")
  :confirm)

(defun log-permission-decision (tool-name args decision source)
  "Log a permission decision for debugging."
  (push (list :tool-name tool-name
              :args args
              :decision decision
              :source source
              :timestamp (get-universal-time))
        *permission-log*)
  (when (> (length *permission-log*) *permission-log-max*)
    (setf *permission-log* (subseq *permission-log* 0 *permission-log-max*))))

;;* Interactive Prompt

(defun prompt-tool-permission (tool-name args)
  "Present multi-option confirmation for a tool call.
   In ACP mode, delegates to the Client via session/request_permission.
   Returns the decision (:allow or :deny)."
  (when *acp-mode*
    (let ((tool-call-id (format nil "call_perm_~A" (uuid:make-v4-uuid))))
      (return-from prompt-tool-permission
        (acp-request-permission tool-name args tool-call-id))))
  (when *lisp-rpc-mode*
    (let ((tool-info (gethash tool-name *defined-tools*)))
      (rpc-permission-request tool-name args
                              (if tool-info (tool-definition-description tool-info) "")
                              '(:allow-once :allow-always :allow-exact :deny-once :deny-always)))
    (setf *lisp-rpc-pending-permission*
          (list :tool-name tool-name :args args :response nil))
    (setf *lisp-rpc-permission-lock* (bt:make-lock "perm-lock"))
    (setf *lisp-rpc-permission-cv* (bt:make-condition-variable :name "perm-cv"))
    (bt:with-lock-held (*lisp-rpc-permission-lock*)
      (loop until (getf *lisp-rpc-pending-permission* :response)
            do (bt:condition-wait *lisp-rpc-permission-cv* *lisp-rpc-permission-lock*
                                  :timeout *lisp-rpc-permission-timeout*))
      (unless (getf *lisp-rpc-pending-permission* :response)
        (setf (getf *lisp-rpc-pending-permission* :response) "deny-once")))
    (let ((input (getf *lisp-rpc-pending-permission* :response)))
      (setf *lisp-rpc-pending-permission* nil)
      (cond
        ((or (string-equal input "y") (string-equal input "allow-once"))
         (return-from prompt-tool-permission :allow))
        ((or (string-equal input "a") (string-equal input "allow-always"))
         (push (make-session-override
                :tool-name tool-name
                :match-type :tool-always
                :decision :allow)
               *session-overrides*)
         (return-from prompt-tool-permission :allow))
        ((or (string-equal input "e") (string-equal input "allow-exact"))
         (push (make-session-override
                :tool-name tool-name
                :match-type :exact-args
                :args-fingerprint (fingerprint-args args)
                :decision :allow)
               *session-overrides*)
         (return-from prompt-tool-permission :allow))
        ((or (string-equal input "d") (string-equal input "deny-always"))
         (push (make-session-override
                :tool-name tool-name
                :match-type :tool-deny
                :decision :deny)
               *session-overrides*)
         (return-from prompt-tool-permission :deny))
        (t (return-from prompt-tool-permission :deny)))))
  (format t "~&Tool '~A' wants to execute with args:~%  ~S~%" tool-name args)
  (format t "  [y] Allow this once~%")
  (format t "  [n] Deny this once~%")
  (format t "  [a] Always allow this tool~%")
  (format t "  [e] Always allow this exact call (tool + these args)~%")
  (format t "  [d] Always deny this tool~%")
  (format t "  [?] Show tool details~%")
  (loop
    (format t "Choice [y/n/a/e/d/?]: ")
    (force-output)
    (let ((input (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (read-line *standard-input* nil ""))))
      (cond
        ((string-equal input "y")
         (return :allow))
        ((string-equal input "n")
         (return :deny))
        ((string-equal input "a")
         (push (make-session-override
                :tool-name tool-name
                :match-type :tool-always
                :decision :allow)
               *session-overrides*)
         (unless *lisp-rpc-mode*
           (format t "~&Session override created: always allow '~A'~%" tool-name))
         (return :allow))
        ((string-equal input "e")
         (push (make-session-override
                :tool-name tool-name
                :match-type :exact-args
                :args-fingerprint (fingerprint-args args)
                :decision :allow)
               *session-overrides*)
         (unless *lisp-rpc-mode*
           (format t "~&Session override created: always allow '~A' with these exact args~%" tool-name))
         (return :allow))
        ((string-equal input "d")
         (push (make-session-override
                :tool-name tool-name
                :match-type :tool-deny
                :decision :deny)
               *session-overrides*)
         (unless *lisp-rpc-mode*
           (format t "~&Session override created: always deny '~A'~%" tool-name))
         (return :deny))
        ((string-equal input "?")
         (let ((tool (gethash tool-name *defined-tools*)))
           (if tool
               (progn
                 (format t "~&Tool: ~A~%" (tool-definition-name tool))
                 (format t "Description: ~A~%" (tool-definition-description tool))
                 (format t "Default permissions: ~A~%" (tool-definition-permissions tool))
                 (when (tool-definition-rules tool)
                   (format t "Rules: ~A~%" (tool-definition-rules tool)))
                 (format t "Parameters:~%")
                 (dolist (param (tool-definition-parameters tool))
                   (format t "  - ~A (~A~A): ~A~%"
                           (tool-parameter-name param)
                           (tool-parameter-type param)
                           (if (tool-parameter-required param) ", required" "")
                           (tool-parameter-description param))))
               (format t "~&Tool '~A' not found in registry.~%" tool-name))))
        (t
         (format t "~&Invalid choice. Please enter y, n, a, e, d, or ?.~%"))))))

;;* defperm Macro

(defmacro defperm (name (&key tool (priority 0) description (source :user)) &body body)
  "Define and register a named permission rule.

NAME: Symbol naming the rule.
TOOL: String tool name to match, or :any for all tools.
PRIORITY: Integer, higher = checked first (default 0).
DESCRIPTION: Human-readable description of the rule.
SOURCE: Origin keyword — :system, :user, :mode, :session (default :user).
BODY: Code evaluated with TOOL-NAME (string) and ARGS (plist) bound.
      Must return :allow, :deny, :confirm, or :abstain."
  (let ((fn-name (intern (format nil "PERM-~A" (symbol-name name)) :hactar))
        (rule-name-str (string-downcase (substitute #\- #\_ (symbol-name name)))))
    (let ((tn-var (intern "TOOL-NAME" :hactar))
          (args-var (intern "ARGS" :hactar)))
    `(progn
       (defun ,fn-name (,tn-var ,args-var)
         "Permission rule predicate function."
         (declare (ignorable ,tn-var ,args-var))
         ,@body)
       (register-permission-rule
        (make-permission-rule
         :name ,rule-name-str
         :priority ,priority
         :tool-pattern ,tool
         :predicate #',fn-name
         :description ,(or description "")
         :source ,source))
       ',name))))

;;* Built-in Predicates

(defun path-in-project? (path)
  "Returns T if PATH resolves to within *repo-root*."
  (when (and path *repo-root*)
    (handler-case
        (let* ((full-path (namestring (truename (merge-pathnames path *repo-root*))))
               (root (namestring (truename *repo-root*))))
          (str:starts-with-p root full-path))
      (error () nil))))

(defun path-matches? (path pattern)
  "Returns T if path matches the regex pattern."
  (when (and path pattern)
    (not (null (cl-ppcre:scan pattern path)))))

(defun path-is-readonly? (path)
  "Returns T if path is in a known read-only location (node_modules, .git, etc.)."
  (when path
    (let ((readonly-patterns '("node_modules/" ".git/" "__pycache__/"
                               ".bundle/" "vendor/bundle/" ".tox/")))
      (some (lambda (pat) (search pat path)) readonly-patterns))))

(defun command-safe? (command)
  "Returns T if command matches a safe-commands pattern."
  (when command
    (some (lambda (pattern)
            (cl-ppcre:scan pattern command))
          *safe-command-patterns*)))

(defun command-destructive? (command)
  "Returns T if command matches known destructive patterns."
  (when command
    (not (null (cl-ppcre:scan
                "(?i)^\\s*(sudo|rm\\s+-rf\\s+/|mkfs|dd\\s+|chmod\\s+-R|chown\\s+-R|:\\(\\)\\{)"
                command)))))

(defun command-in-project? (command)
  "Returns T if command operates within *repo-root*.
   Assumes commands without explicit paths run in *repo-root*."
  (declare (ignore command))
  t)

(defun args-match-pattern? (args pattern-alist)
  "Returns T if tool args match the given pattern alist (supports regex values)."
  (every (lambda (pattern-pair)
           (let* ((key (car pattern-pair))
                  (pattern (cdr pattern-pair))
                  (value (getf args key)))
             (when value
               (if (stringp pattern)
                   (not (null (cl-ppcre:scan pattern (princ-to-string value))))
                   (equal pattern value)))))
         pattern-alist))

;;* Safe Commands Registry

(defun register-safe-command (pattern)
  "Add a regex pattern to the safe commands list."
  (pushnew pattern *safe-command-patterns* :test #'string=))

(register-safe-command "^(ls|dir|cat|head|tail|wc|find|grep|rg|ag|fd|tree|file|stat|du|df)\\b")
(register-safe-command "^git\\s+(status|log|diff|show|branch|tag|remote)\\b")
(register-safe-command "^(node|python|ruby|sbcl)\\s+--version$")
(register-safe-command "^(npm|yarn|cargo|mix|pip)\\s+(list|info|show|search)\\b")

;;* Default Permission Rules

(defperm deny-dangerous-commands
  (:tool "execute_command" :priority 100 :source :system
   :description "Deny commands that could be destructive outside project scope")
  (let ((cmd (getf args :command)))
    (if (and cmd (command-destructive? cmd))
        :deny
        :abstain)))

(defperm auto-allow-project-reads
  (:tool "read_file" :priority 50 :source :system
   :description "Auto-allow reading files within the project directory")
  (if (path-in-project? (getf args :path))
      :allow
      :abstain))

(defperm auto-allow-acp-fs-reads
  (:tool "fs-read-text-file" :priority 50 :source :system
   :description "Auto-allow ACP filesystem reads within the project directory")
  (if (path-in-project? (getf args :path))
      :allow
      :abstain))

(defperm acp-fs-writes-in-project
  (:tool "fs-write-text-file" :priority 45 :source :system
   :description "Confirm ACP filesystem writes within project, deny outside")
  (let ((path (getf args :path)))
    (cond
      ((null path) :abstain)
      ((path-in-project? path) :abstain)  ; Fall through to :confirm default
      (t :deny))))

(defperm auto-allow-acp-terminal-output
  (:tool "terminal-output" :priority 50 :source :system
   :description "Auto-allow reading terminal output")
  :allow)

(defperm auto-allow-acp-terminal-wait
  (:tool "terminal-wait-for-exit" :priority 50 :source :system
   :description "Auto-allow waiting for terminal exit")
  :allow)

(defperm auto-allow-acp-terminal-release
  (:tool "terminal-release" :priority 50 :source :system
   :description "Auto-allow releasing terminals")
  :allow)

(defperm auto-allow-safe-project-commands
  (:tool "execute_command" :priority 40 :source :system
   :description "Auto-allow safe read-only commands within the project")
  (let ((cmd (getf args :command)))
    (if (and (command-safe? cmd) (command-in-project? cmd))
        :allow
        :abstain)))

(defperm tool-default-permissions
  (:tool :any :priority 10 :source :system
   :description "Fall through to the tool's own :permissions setting")
  (let ((tool (gethash tool-name *defined-tools*)))
    (if (and tool (eq (tool-definition-permissions tool) :auto))
        :allow
        :confirm)))

;;* Commands

(define-command permissions (args)
  "View and manage tool permission rules. Usage: /permissions [clear|log|reset]"
  (cond
    ((null args)
     (format t "~&Active Permission Rules:~%")
     (format t "~&~3A ~8A ~8A ~30A ~A~%" "Pri" "Source" "Tool" "Name" "Description")
     (format t "~&~A~%" (make-string 90 :initial-element #\-))
     (dolist (rule *permission-rules*)
       (format t "~3D ~8A ~8A ~30A ~A~%"
               (permission-rule-priority rule)
               (permission-rule-source rule)
               (let ((tp (permission-rule-tool-pattern rule)))
                 (if (eq tp :any) ":any" tp))
               (permission-rule-name rule)
               (permission-rule-description rule)))
     (when *session-overrides*
       (format t "~&~%Session Overrides:~%")
       (dolist (ov *session-overrides*)
         (format t "  ~A: ~A (~A)~%"
                 (session-override-tool-name ov)
                 (session-override-decision ov)
                 (session-override-match-type ov)))))
    ((string-equal (first args) "clear")
     (clear-session-overrides)
     (format t "~&Session overrides cleared.~%"))
    ((string-equal (first args) "log")
     (if *permission-log*
         (progn
           (format t "~&Recent Permission Decisions (newest first):~%")
           (dolist (entry (subseq *permission-log* 0 (min 20 (length *permission-log*))))
             (format t "  ~A → ~A (by ~A)~%"
                     (getf entry :tool-name)
                     (getf entry :decision)
                     (getf entry :source))))
         (format t "~&No permission decisions logged yet.~%")))
    ((string-equal (first args) "reset")
     (clear-session-overrides)
     (setf *permission-rules*
           (remove-if (lambda (r) (not (eq (permission-rule-source r) :system)))
                      *permission-rules*))
     (format t "~&Permission rules reset to system defaults. Session overrides cleared.~%"))
    (t
     (format t "~&Usage: /permissions [clear|log|reset]~%"))))

(define-command allow (args)
  "Add a session override to always allow a tool. Usage: /allow <tool_name>"
  (if (null args)
      (format t "~&Usage: /allow <tool_name>~%")
      (let ((tool-name (first args)))
        (push (make-session-override
               :tool-name tool-name
               :match-type :tool-always
               :decision :allow)
              *session-overrides*)
        (format t "~&Session override: always allow '~A'~%" tool-name))))

(define-command deny (args)
  "Add a session override to always deny a tool. Usage: /deny <tool_name>"
  (if (null args)
      (format t "~&Usage: /deny <tool_name>~%")
      (let ((tool-name (first args)))
        (push (make-session-override
               :tool-name tool-name
               :match-type :tool-deny
               :decision :deny)
              *session-overrides*)
        (format t "~&Session override: always deny '~A'~%" tool-name))))
