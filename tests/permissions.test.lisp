(in-package :hactar-tests)

(def-suite permissions-tests
  :description "Tests for Hactar tool permissions system")

(in-suite permissions-tests)

;;* Helpers

(defun reset-permissions-state ()
  "Reset all permissions state for testing."
  (setf hactar::*permission-rules* '())
  (setf hactar::*session-overrides* '())
  (setf hactar::*permission-log* '()))

;;* Data Structures

(test permission-rule-creation
  "Test creating a permission rule struct."
  (let ((rule (hactar::make-permission-rule
               :name "test-rule"
               :priority 50
               :tool-pattern "read_file"
               :predicate (lambda (tool-name args)
                            (declare (ignore tool-name args))
                            :allow)
               :description "Test rule"
               :source :user)))
    (is (string= "test-rule" (hactar::permission-rule-name rule)))
    (is (= 50 (hactar::permission-rule-priority rule)))
    (is (string= "read_file" (hactar::permission-rule-tool-pattern rule)))
    (is (string= "Test rule" (hactar::permission-rule-description rule)))
    (is (eq :user (hactar::permission-rule-source rule)))))

(test session-override-creation
  "Test creating a session override struct."
  (let ((override (hactar::make-session-override
                   :tool-name "read_file"
                   :match-type :tool-always
                   :decision :allow)))
    (is (string= "read_file" (hactar::session-override-tool-name override)))
    (is (eq :tool-always (hactar::session-override-match-type override)))
    (is (eq :allow (hactar::session-override-decision override)))))

;;* Rule Registration

(test register-permission-rule-basic
  "Test registering a single permission rule."
  (reset-permissions-state)
  (let ((rule (hactar::make-permission-rule
               :name "test-rule"
               :priority 50
               :tool-pattern "read_file"
               :predicate (lambda (tool-name args)
                            (declare (ignore tool-name args))
                            :allow)
               :description "Test rule")))
    (hactar::register-permission-rule rule)
    (is (= 1 (length hactar::*permission-rules*)))
    (is (string= "test-rule" (hactar::permission-rule-name (first hactar::*permission-rules*))))))

(test register-permission-rule-priority-ordering
  "Test that rules are maintained in priority order (highest first)."
  (reset-permissions-state)
  (let ((low (hactar::make-permission-rule
              :name "low" :priority 10
              :predicate (lambda (tn a) (declare (ignore tn a)) :abstain)))
        (high (hactar::make-permission-rule
               :name "high" :priority 100
               :predicate (lambda (tn a) (declare (ignore tn a)) :abstain)))
        (mid (hactar::make-permission-rule
              :name "mid" :priority 50
              :predicate (lambda (tn a) (declare (ignore tn a)) :abstain))))
    (hactar::register-permission-rule low)
    (hactar::register-permission-rule high)
    (hactar::register-permission-rule mid)
    (is (= 3 (length hactar::*permission-rules*)))
    (is (string= "high" (hactar::permission-rule-name (first hactar::*permission-rules*))))
    (is (string= "mid" (hactar::permission-rule-name (second hactar::*permission-rules*))))
    (is (string= "low" (hactar::permission-rule-name (third hactar::*permission-rules*))))))

(test register-permission-rule-replace-existing
  "Test that re-registering a rule with the same name replaces it."
  (reset-permissions-state)
  (let ((rule-v1 (hactar::make-permission-rule
                  :name "my-rule" :priority 10
                  :description "version 1"
                  :predicate (lambda (tn a) (declare (ignore tn a)) :abstain)))
        (rule-v2 (hactar::make-permission-rule
                  :name "my-rule" :priority 20
                  :description "version 2"
                  :predicate (lambda (tn a) (declare (ignore tn a)) :abstain))))
    (hactar::register-permission-rule rule-v1)
    (is (= 1 (length hactar::*permission-rules*)))
    (hactar::register-permission-rule rule-v2)
    (is (= 1 (length hactar::*permission-rules*)))
    (is (string= "version 2"
                 (hactar::permission-rule-description (first hactar::*permission-rules*))))))

(test unregister-permission-rule
  "Test removing a rule by name."
  (reset-permissions-state)
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "to-remove" :priority 50
    :predicate (lambda (tn a) (declare (ignore tn a)) :abstain)))
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "to-keep" :priority 40
    :predicate (lambda (tn a) (declare (ignore tn a)) :abstain)))
  (is (= 2 (length hactar::*permission-rules*)))
  (hactar::unregister-permission-rule "to-remove")
  (is (= 1 (length hactar::*permission-rules*)))
  (is (string= "to-keep" (hactar::permission-rule-name (first hactar::*permission-rules*)))))

;;* resolve-permission

(test resolve-permission-single-allow-rule
  "Test resolve-permission with a single allow rule."
  (reset-permissions-state)
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "allow-all" :priority 50
    :tool-pattern :any
    :predicate (lambda (tn a) (declare (ignore tn a)) :allow)))
  (is (eq :allow (hactar::resolve-permission "read_file" '(:path "test.txt")))))

(test resolve-permission-single-deny-rule
  "Test resolve-permission with a single deny rule."
  (reset-permissions-state)
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "deny-all" :priority 50
    :tool-pattern :any
    :predicate (lambda (tn a) (declare (ignore tn a)) :deny)))
  (is (eq :deny (hactar::resolve-permission "execute_command" '(:command "rm -rf /")))))

(test resolve-permission-priority-order
  "Test that higher priority rules take precedence."
  (reset-permissions-state)
  ;; Low priority: allow
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "low-allow" :priority 10
    :tool-pattern :any
    :predicate (lambda (tn a) (declare (ignore tn a)) :allow)))
  ;; High priority: deny
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "high-deny" :priority 100
    :tool-pattern :any
    :predicate (lambda (tn a) (declare (ignore tn a)) :deny)))
  (is (eq :deny (hactar::resolve-permission "anything" nil))))

(test resolve-permission-abstain-falls-through
  "Test that :abstain causes the next rule to be checked."
  (reset-permissions-state)
  ;; High priority: abstain
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "abstainer" :priority 100
    :tool-pattern :any
    :predicate (lambda (tn a) (declare (ignore tn a)) :abstain)))
  ;; Low priority: allow
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "allower" :priority 10
    :tool-pattern :any
    :predicate (lambda (tn a) (declare (ignore tn a)) :allow)))
  (is (eq :allow (hactar::resolve-permission "test_tool" nil))))

(test resolve-permission-tool-pattern-matching
  "Test that rules only match their specified tool patterns."
  (reset-permissions-state)
  ;; Only matches read_file
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "read-only" :priority 50
    :tool-pattern "read_file"
    :predicate (lambda (tn a) (declare (ignore tn a)) :allow)))
  ;; read_file should be allowed
  (is (eq :allow (hactar::resolve-permission "read_file" nil)))
  ;; Other tools should fall through to default (:confirm)
  (is (eq :confirm (hactar::resolve-permission "write_to_file" nil))))

(test resolve-permission-default-confirm
  "Test that default decision is :confirm when no rules match."
  (reset-permissions-state)
  (is (eq :confirm (hactar::resolve-permission "unknown_tool" nil))))

;;* Session Overrides

(test session-override-tool-always
  "Test session override with :tool-always match type."
  (reset-permissions-state)
  (push (hactar::make-session-override
         :tool-name "read_file"
         :match-type :tool-always
         :decision :allow)
        hactar::*session-overrides*)
  (is (eq :allow (hactar::resolve-permission "read_file" '(:path "anything.txt"))))
  ;; Other tools should not be affected
  (is (eq :confirm (hactar::resolve-permission "write_to_file" '(:path "test.txt")))))

(test session-override-tool-deny
  "Test session override with :tool-deny match type."
  (reset-permissions-state)
  (push (hactar::make-session-override
         :tool-name "execute_command"
         :match-type :tool-deny
         :decision :deny)
        hactar::*session-overrides*)
  (is (eq :deny (hactar::resolve-permission "execute_command" '(:command "ls")))))

(test session-override-exact-args
  "Test session override with :exact-args match type."
  (reset-permissions-state)
  (let ((specific-args '(:path "src/main.lisp")))
    (push (hactar::make-session-override
           :tool-name "read_file"
           :match-type :exact-args
           :args-fingerprint (hactar::fingerprint-args specific-args)
           :decision :allow)
          hactar::*session-overrides*)
    ;; Exact same args should match
    (is (eq :allow (hactar::resolve-permission "read_file" '(:path "src/main.lisp"))))
    ;; Different args should not match (falls to default)
    (is (eq :confirm (hactar::resolve-permission "read_file" '(:path "other.lisp"))))))

(test session-override-trumps-rules
  "Test that session overrides take precedence over rules."
  (reset-permissions-state)
  ;; Rule says deny
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "deny-rule" :priority 100
    :tool-pattern :any
    :predicate (lambda (tn a) (declare (ignore tn a)) :deny)))
  ;; Session override says allow
  (push (hactar::make-session-override
         :tool-name "read_file"
         :match-type :tool-always
         :decision :allow)
        hactar::*session-overrides*)
  (is (eq :allow (hactar::resolve-permission "read_file" nil))))

(test clear-session-overrides
  "Test clearing session overrides."
  (reset-permissions-state)
  (push (hactar::make-session-override
         :tool-name "test" :match-type :tool-always :decision :allow)
        hactar::*session-overrides*)
  (is (= 1 (length hactar::*session-overrides*)))
  (hactar::clear-session-overrides)
  (is (= 0 (length hactar::*session-overrides*))))

;;* Built-in Predicates

(test command-safe-predicate
  "Test the command-safe? predicate."
  (is-true (hactar::command-safe? "ls -la"))
  (is-true (hactar::command-safe? "git status"))
  (is-true (hactar::command-safe? "grep foo bar.txt"))
  (is-true (hactar::command-safe? "cat README.md"))
  (is (null (hactar::command-safe? "rm -rf /")))
  (is (null (hactar::command-safe? "npm install express"))))

(test command-destructive-predicate
  "Test the command-destructive? predicate."
  (is-true (hactar::command-destructive? "sudo apt install foo"))
  (is-true (hactar::command-destructive? "rm -rf /"))
  (is-true (hactar::command-destructive? "dd if=/dev/zero of=/dev/sda"))
  (is (null (hactar::command-destructive? "ls -la")))
  (is (null (hactar::command-destructive? "git status"))))

(test path-is-readonly-predicate
  "Test the path-is-readonly? predicate."
  (is-true (hactar::path-is-readonly? "node_modules/express/index.js"))
  (is-true (hactar::path-is-readonly? ".git/config"))
  (is-true (hactar::path-is-readonly? "__pycache__/mod.pyc"))
  (is (null (hactar::path-is-readonly? "src/main.lisp")))
  (is (null (hactar::path-is-readonly? "README.md"))))

(test args-match-pattern-predicate
  "Test the args-match-pattern? predicate."
  (is-true (hactar::args-match-pattern?
            '(:command "git status" :path "/tmp")
            '((:command . "^git"))))
  (is (null (hactar::args-match-pattern?
             '(:command "npm install" :path "/tmp")
             '((:command . "^git"))))))

;;* defperm Macro

(test defperm-defines-function-and-registers-rule
  "Test that defperm defines a function and registers the rule."
  (reset-permissions-state)
  (eval '(hactar::defperm test-perm-rule
           (:tool "read_file" :priority 42 :description "Test perm rule" :source :user)
           (declare (ignore tool-name args))
           :allow))
  ;; Rule should be registered
  (is (= 1 (length hactar::*permission-rules*)))
  (let ((rule (first hactar::*permission-rules*)))
    (is (= 42 (hactar::permission-rule-priority rule)))
    (is (string= "Test perm rule" (hactar::permission-rule-description rule)))
    (is (eq :user (hactar::permission-rule-source rule))))
  ;; Function should be callable
  (is (eq :allow (funcall (hactar::permission-rule-predicate (first hactar::*permission-rules*))
                          "read_file" nil))))

(test defperm-body-has-access-to-bindings
  "Test that defperm body can access tool-name and args."
  (reset-permissions-state)
  (eval '(hactar::defperm test-binding-access
           (:tool :any :priority 50 :description "Binding test")
           (if (string= hactar::tool-name "special_tool")
               :allow
               (if (getf hactar::args :secret)
                   :deny
                   :abstain))))
  (let ((pred (hactar::permission-rule-predicate (first hactar::*permission-rules*))))
    (is (eq :allow (funcall pred "special_tool" nil)))
    (is (eq :deny (funcall pred "other" '(:secret t))))
    (is (eq :abstain (funcall pred "other" nil)))))

;;* Integration

(test integration-execute-tool-with-permissions
  "Test that execute-tool respects permission rules."
  (clrhash hactar::*defined-tools*)
  (reset-permissions-state)

  (eval '(hactar::deftool perm-test-tool ((msg :string :required t))
           :description "Permission test tool"
           :permissions :confirm
           (getf hactar::args :msg)))

  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "allow-perm-test" :priority 50
    :tool-pattern "perm_test_tool"
    :predicate (lambda (tn a) (declare (ignore tn a)) :allow)))

  (multiple-value-bind (result success-p error-msg)
      (hactar::execute-tool "perm_test_tool" '(:msg "hello"))
    (declare (ignore error-msg))
    (is-true success-p)
    (is (string= "hello" result))))

(test integration-execute-tool-denied-by-rule
  "Test that execute-tool is denied by a permission rule."
  (clrhash hactar::*defined-tools*)
  (reset-permissions-state)

  (eval '(hactar::deftool denied-tool ((msg :string :required t))
           :description "Tool that will be denied"
           :permissions :auto
           (getf hactar::args :msg)))

  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "deny-this" :priority 100
    :tool-pattern "denied_tool"
    :predicate (lambda (tn a) (declare (ignore tn a)) :deny)))

  (multiple-value-bind (result success-p error-msg)
      (hactar::execute-tool "denied_tool" '(:msg "nope"))
    (declare (ignore error-msg))
    (is-true success-p)  ; success-p is T because it didn't error, just denied
    (is (search "denied" result))))

(test integration-session-override-allows-tool
  "Test that a session override can allow a tool that would otherwise need confirmation."
  (clrhash hactar::*defined-tools*)
  (reset-permissions-state)

  (eval '(hactar::deftool override-test ((val :string :required t))
           :description "Override test tool"
           :permissions :confirm
           (getf hactar::args :val)))

  (push (hactar::make-session-override
         :tool-name "override_test"
         :match-type :tool-always
         :decision :allow)
        hactar::*session-overrides*)

  (multiple-value-bind (result success-p error-msg)
      (hactar::execute-tool "override_test" '(:val "works"))
    (declare (ignore error-msg))
    (is-true success-p)
    (is (string= "works" result))))

(test integration-mode-extension-adds-rule
  "Test that modes can dynamically add permission rules via defperm."
  (reset-permissions-state)

  (eval '(hactar::defperm mode-specific-rule
           (:tool "execute_command" :priority 45 :source :mode
            :description "Mode-specific auto-allow")
           (let ((cmd (getf hactar::args :command)))
             (if (and cmd (cl-ppcre:scan "^rails routes" cmd))
                 :allow
                 :abstain))))

  (let ((rules (hactar::list-permission-rules)))
    (is (= 1 (length rules)))
    (is (eq :mode (hactar::permission-rule-source (first rules)))))

  (is (eq :allow (hactar::resolve-permission "execute_command" '(:command "rails routes"))))
  (is (eq :confirm (hactar::resolve-permission "execute_command" '(:command "rails console")))))

;;* Permission Log

(test permission-log-records-decisions
  "Test that permission decisions are logged."
  (reset-permissions-state)
  (hactar::register-permission-rule
   (hactar::make-permission-rule
    :name "log-test" :priority 50
    :tool-pattern :any
    :predicate (lambda (tn a) (declare (ignore tn a)) :allow)))
  (hactar::resolve-permission "test_tool" '(:key "val"))
  (is (= 1 (length hactar::*permission-log*)))
  (let ((entry (first hactar::*permission-log*)))
    (is (string= "test_tool" (getf entry :tool-name)))
    (is (eq :allow (getf entry :decision)))
    (is (string= "log-test" (getf entry :source)))))

;;* Safe Command Registry

(test register-safe-command-adds-pattern
  "Test registering a new safe command pattern."
  (let ((original-patterns (copy-list hactar::*safe-command-patterns*)))
    (unwind-protect
         (progn
           (hactar::register-safe-command "^my-safe-cmd\\b")
           (is-true (hactar::command-safe? "my-safe-cmd --verbose"))
           (is (null (hactar::command-safe? "my-unsafe-cmd"))))
      (setf hactar::*safe-command-patterns* original-patterns))))
