;; tests for state.lisp
(in-package :hactar-tests)
(in-suite hactar-tests)

(test state-macro-registration
  "Test that defstate registers variables correctly."
  (hactar::defstate *test-state-var* 42 "A test var" :type "integer" :examples ("1" "2"))
  (let ((info (gethash "*test-state-var*" hactar::*state-registry*)))
    (is-true info)
    (is (eq (getf info :name) '*test-state-var*))
    (is (equal (getf info :doc) "A test var"))
    (is (equal (getf info :type) "integer"))
    (is (equal (getf info :examples) '("1" "2")))))

(test state-command-xml
  "Test state command XML output."
  (hactar::defstate *test-cmd-var* "foo" "A command test var" :type "string")
  (let* ((hactar::*in-editor* t)
         (output (with-output-to-string (*standard-output*)
                   (hactar::execute-command "/state" '("*test-cmd-var*")))))
    (is-true (search "<var name=\"*TEST-CMD-VAR*\" type=\"string\">" output))
    (is-true (search "<doc>A command test var</doc>" output))))

(test state-command-notfound
  "Test state command when variable is not found."
  (let ((output (with-output-to-string (*standard-output*)
                  (hactar::execute-command "/state" '("*nonexistent-var*")))))
    (is-true (search "<notfound/>" output))))
