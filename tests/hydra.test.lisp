(in-package :hactar-tests)

(def-suite hydra-tests
  :description "Tests for the transient/hydra menu system")

(in-suite hydra-tests)

(defvar *test-form-executed* nil)
(defvar *test-func-executed* nil)
(defvar *test-cmd-executed* nil)
(defvar *test-inner-executed* nil)

(test define-hydra-registers-properly
  "Test that defhydra registers the hydra structure and command."
  (hactar::defhydra test-dummy-hydra (:title "Test Title" :exit nil)
    "A test dummy hydra"
    ("a" (format t "A executed") "Execute A" :exit t)
    ("b" "skills.help" "Execute B" :exit nil))

  (let ((h (gethash "test-dummy-hydra" hactar::*hydras*)))
    (is-true h)
    (is (eq 'test-dummy-hydra (hactar::hydra-name h)))
    (is (string= "Test Title" (getf (hactar::hydra-options h) :title)))
    (is (string= "A test dummy hydra" (hactar::hydra-docstring h)))
    (is (= 2 (length (hactar::hydra-heads h))))

    (let ((head1 (first (hactar::hydra-heads h)))
          (head2 (second (hactar::hydra-heads h))))
      (is (string= "a" (hactar::hydra-head-key head1)))
      (is (string= "Execute A" (hactar::hydra-head-hint head1)))
      (is-true (hactar::hydra-head-exit head1))

      (is (string= "b" (hactar::hydra-head-key head2)))
      (is (string= "Execute B" (hactar::hydra-head-hint head2)))
      (is-false (hactar::hydra-head-exit head2)))

    ;; Verify that slash command was registered
    (is-true (gethash "/test-dummy-hydra" hactar::*commands*))))

(test execute-hydra-subcommands
  "Test running a hydra and dispatching its subcommands (forms, functions, strings)."
  (setf *test-form-executed* nil
        *test-func-executed* nil
        *test-cmd-executed* nil)

  ;; Mock function
  (setf (fdefinition 'test-mock-func)
        (lambda () (setf *test-func-executed* t)))

  ;; Mock command
  (setf (gethash "/test-mock-cmd" hactar::*commands*)
        (list (lambda (args)
                (declare (ignore args))
                (setf *test-cmd-executed* t))
              "Mock command" nil nil))

  (hactar::defhydra test-run-hydra (:title "Run Title" :exit nil)
    "Runner test hydra"
    ("a" (setf *test-form-executed* t) "Execute form" :exit t)
    ("b" test-mock-func "Execute function" :exit nil)
    ("c" "test-mock-cmd" "Execute command" :exit nil))

  (let ((h (gethash "test-run-hydra" hactar::*hydras*)))
    ;; 1. Test executing form and exiting
    (let* ((hactar::*hydra-input* (make-string-input-stream (format nil "a~%")))
           (hactar::*hydra-output* (make-string-output-stream)))
      (hactar::run-hydra h)
      (let ((output (get-output-stream-string hactar::*hydra-output*)))
        (format t "~&--- FORM TEST OUTPUT ---~%~A~%------------------------~%" output)
        (is-true *test-form-executed*)
        (is-true (search "Run Title" output))))

    ;; 2. Test executing function, command, and then quit
    (let* ((hactar::*hydra-input* (make-string-input-stream (format nil "b~%c~%q~%")))
           (hactar::*hydra-output* (make-string-output-stream)))
      (hactar::run-hydra h)
      (let ((output (get-output-stream-string hactar::*hydra-output*)))
        (format t "~&--- FUNC TEST OUTPUT ---~%~A~%------------------------~%" output)
        (is-true *test-func-executed*)
        (is-true *test-cmd-executed*)
        (is-true (search "Execute function" output))))))

(test nested-hydra-execution
  "Test nesting one hydra within another."
  (setf *test-inner-executed* nil)

  (hactar::defhydra test-inner-hydra (:title "Inner Hydra" :exit t)
    "Inner"
    ("x" (setf *test-inner-executed* t) "Execute inner" :exit t))

  (hactar::defhydra test-outer-hydra (:title "Outer Hydra" :exit nil)
    "Outer"
    ("i" test-inner-hydra "Goto inner" :exit nil))

  (let ((outer (gethash "test-outer-hydra" hactar::*hydras*)))
    ;; Input 'i' to go to inner, 'x' inside inner, then outer loops and we input 'q' to quit outer.
    (let* ((hactar::*hydra-input* (make-string-input-stream (format nil "i~%x~%q~%")))
           (hactar::*hydra-output* (make-string-output-stream)))
      (hactar::run-hydra outer)
      (let ((output (get-output-stream-string hactar::*hydra-output*)))
        (format t "~&--- NESTED TEST OUTPUT ---~%~A~%--------------------------~%" output)
        (is-true *test-inner-executed*)
        (is-true (search "Outer Hydra" output))
        (is-true (search "Inner Hydra" output))))))
