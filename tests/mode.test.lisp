(in-package :hactar-tests)

(def-suite mode-tests
  :description "Tests for the mode registry")

(in-suite mode-tests)

(defparameter *activate-called* nil)
(defparameter *deactivate-called* nil)

(test refactor-mode-system
  "Test mode definition, activation, deactivation, and mode commands."
  (let ((hactar::*modes* (make-hash-table :test 'equal))
        (hactar::*active-modes* '())
        (hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*stack* '()))
    (setf *activate-called* nil
          *deactivate-called* nil)
    (eval `(hactar::defmode testmode
             "A test mode"
             :when (member "teststack" hactar::*stack* :test #'string=)
             :rules "TestMode Rules"
             :on-activate (lambda () (setf *activate-called* t))
             :on-deactivate (lambda () (setf *deactivate-called* t))))

    (let ((m (hactar::get-mode "testmode")))
      (is-true m)
      (is (string= "testmode" (hactar::mode-name-string m)))
      (is-false (hactar::mode-active-p m))

      ;; refresh-modes shouldn't activate yet
      (hactar::refresh-modes)
      (is-false (hactar::mode-active-p m))

      ;; Add teststack to stack, then refresh
      (push "teststack" hactar::*stack*)
      (hactar::refresh-modes)

      (is-true (hactar::mode-active-p m))
      (is-true *activate-called*)
      (is (equal '("testmode") (hactar::active-modes)))
      (is (string= "TestMode Rules" (gethash "mode/testmode" hactar::*active-rules*)))

      ;; Deactivate mode
      (hactar::deactivate-mode "testmode")
      (is-false (hactar::mode-active-p m))
      (is-true *deactivate-called*)
      (is-false (gethash "mode/testmode" hactar::*active-rules*))
      (is-false (hactar::active-modes)))))

(test refactor-mode-active-rules
  "get-active-mode-rules collects rules from all active modes; activation is idempotent."
  (let ((hactar::*modes* (make-hash-table :test 'equal))
        (hactar::*active-modes* '())
        (hactar::*active-rules* (make-hash-table :test 'equal)))
    (eval '(hactar::defmode m1 "M1" :rules "R1"))
    (eval '(hactar::defmode m2 "M2" :rules "R2"))
    (hactar::activate-mode "m1")
    (hactar::activate-mode "m2")
    (let ((rules (hactar::get-active-mode-rules)))
      (is (search "R1" rules))
      (is (search "R2" rules))
      (is (search "Active Mode Rules" rules)))
    ;; Idempotent: re-activating doesn't duplicate.
    (hactar::activate-mode "m1")
    (is (= 2 (length (hactar::active-modes))))
    ;; Deactivating removes the rule.
    (hactar::deactivate-mode "m1")
    (is (= 1 (length (hactar::active-modes))))
    (is (not (search "R1" (hactar::get-active-mode-rules))))))

(test refactor-mode-get-mode-and-name
  "get-mode resolves modes case-insensitively and mode-name-string normalizes."
  (let ((hactar::*modes* (make-hash-table :test 'equal))
        (hactar::*active-modes* '())
        (hactar::*active-rules* (make-hash-table :test 'equal)))
    (eval '(hactar::defmode MixedMode "A mode"))
    (is-true (hactar::get-mode "mixedmode"))
    (is-true (hactar::get-mode "MIXEDMODE"))
    (is (string= "mixedmode" (hactar::mode-name-string (hactar::get-mode "mixedmode"))))))

(defun run-registered-command (cmd-name)
  "Invoke the registered slash command CMD-NAME (e.g. \"/greet\") and capture its output."
  (let ((info (gethash cmd-name hactar::*commands*)))
    (when info
      (with-output-to-string (*standard-output*)
        (funcall (first info) '())))))

(test refactor-defmode-command
  "A mode command is registered only while its owning mode is active."
  (let ((hactar::*modes* (make-hash-table :test 'equal))
        (hactar::*active-modes* '())
        (hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*commands* (make-hash-table :test 'equal))
        (hactar::*sub-commands* (make-hash-table :test 'equal))
        (hactar::*command-completions* (make-hash-table :test 'equal))
        (hactar::*acp-commands* (make-hash-table :test 'equal))
        (hactar::*hidden-commands* (make-hash-table :test 'equal))
        (hactar::*tui-hidden-commands* (make-hash-table :test 'equal))
        (hactar::*editor-hidden-commands* (make-hash-table :test 'equal)))
    (eval '(hactar::defmode demo "Demo mode"
             :commands ((hactar::defmode-command demo greet (args)
                          (declare (ignore args))
                          (format t "hello-from-demo")))))
    ;; Not active -> the bare command is not registered.
    (is-false (gethash "/greet" hactar::*commands*))
    ;; Active -> command registered and runnable.
    (hactar::activate-mode "demo")
    (is-true (gethash "/greet" hactar::*commands*))
    (is (search "hello-from-demo" (run-registered-command "/greet")))
    ;; Deactivated -> command removed.
    (hactar::deactivate-mode "demo")
    (is-false (gethash "/greet" hactar::*commands*))))

(test refactor-mode-shared-command-names
  "Two modes can declare the same command name; the active mode's body wins."
  (let ((hactar::*modes* (make-hash-table :test 'equal))
        (hactar::*active-modes* '())
        (hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*commands* (make-hash-table :test 'equal))
        (hactar::*sub-commands* (make-hash-table :test 'equal))
        (hactar::*command-completions* (make-hash-table :test 'equal))
        (hactar::*acp-commands* (make-hash-table :test 'equal))
        (hactar::*hidden-commands* (make-hash-table :test 'equal))
        (hactar::*tui-hidden-commands* (make-hash-table :test 'equal))
        (hactar::*editor-hidden-commands* (make-hash-table :test 'equal)))
    (eval '(hactar::defmode railsmode "Rails"
             :commands ((hactar::defmode-command railsmode route.create (args)
                          (declare (ignore args))
                          (format t "rails-route")))))
    (eval '(hactar::defmode reactmode "React"
             :commands ((hactar::defmode-command reactmode route.create (args)
                          (declare (ignore args))
                          (format t "react-route")))))
    ;; Neither registered until activation.
    (is-false (gethash "/route.create" hactar::*commands*))
    ;; Activate rails -> rails body is live.
    (hactar::activate-mode "railsmode")
    (is (search "rails-route" (run-registered-command "/route.create")))
    ;; Activate react -> most recently activated wins.
    (hactar::activate-mode "reactmode")
    (is (search "react-route" (run-registered-command "/route.create")))
    ;; Deactivate react -> rails implementation is restored.
    (hactar::deactivate-mode "reactmode")
    (is (search "rails-route" (run-registered-command "/route.create")))
    ;; Deactivate rails -> command gone entirely.
    (hactar::deactivate-mode "railsmode")
    (is-false (gethash "/route.create" hactar::*commands*))))

(test refactor-mode-command-listing
  "/mode list outputs registered modes and marks the active one."
  (let ((hactar::*modes* (make-hash-table :test 'equal))
        (hactar::*active-modes* '())
        (hactar::*active-rules* (make-hash-table :test 'equal)))
    (eval '(hactar::defmode listmode "Listed mode"))
    (hactar::activate-mode "listmode")
    (let ((out (with-output-to-string (*standard-output*)
                 (hactar::mode '("list")))))
      (is (search "listmode" out))
      (is (search "ACTIVE" out)))))
