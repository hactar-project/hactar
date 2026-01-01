(in-package :hactar-tests)
(def-suite agent-tests
	   :description "Tests for agent definition and execution.")

(in-suite agent-tests)
(hactar::defagent test-agent (param1)
  "A simple test agent."
  :stack ("test-stack")
  :init (setf (gethash 'init-called (hactar::agent-instance-state agent-instance)) t)
  :run (setf (gethash 'run-count (hactar::agent-instance-state agent-instance))
             (1+ (gethash 'run-count (hactar::agent-instance-state agent-instance) 0)))
  :stop-condition (>= (gethash 'run-count (hactar::agent-instance-state agent-instance) 0) 3)
  :cleanup (setf (gethash 'cleanup-called (hactar::agent-instance-state agent-instance)) t))

(test defagent-macro-test
  "Test that defagent correctly creates and registers an agent definition."
  (let ((agent-def (gethash (intern "TEST-AGENT" :hactar) hactar::*agent-definitions*)))
    (is-true agent-def)
    (is (eq (intern "TEST-AGENT" :hactar) (hactar::agent-definition-name agent-def)))
    (is (equal '(param1) (hactar::agent-definition-args agent-def)))
    (is (string= "A simple test agent." (hactar::agent-definition-docstring agent-def)))
    (is (equal '("test-stack") (hactar::agent-definition-stack agent-def)))
    (is-true (hactar::agent-definition-init-form agent-def))
    (is-true (hactar::agent-definition-run-form agent-def))
    (is-true (hactar::agent-definition-stop-condition-form agent-def))
    (is-true (hactar::agent-definition-cleanup-form agent-def))))

(test agent-lifecycle-test
  "Test the full lifecycle of an agent: start, run, stop, cleanup."
  (let ((hactar::*live-dangerously* t) ; Allow agent to run
        (hactar::*stack* '("test-stack" "another-tech")) ; Match agent's stack requirement
        (agent-def (gethash (intern "TEST-AGENT" :hactar) hactar::*agent-definitions*)))
    (is-true agent-def)

    ;; Start the agent
    (let ((instance (hactar::start-agent agent-def '("value1"))))
      (is-true instance)
      (is (eq :running (hactar::agent-instance-status instance)))

      ;; Wait for the agent's thread to complete
      (bt:join-thread (hactar::agent-instance-thread instance))

      ;; Check the final state of the agent instance
      (let ((state (hactar::agent-instance-state instance)))
        (is-true (gethash 'init-called state))
        (is (= 3 (gethash 'run-count state)))
        (is-true (gethash 'cleanup-called state)))

      ;; Check that the agent is no longer in the running agents list
      (is (null (gethash (hactar::agent-instance-id instance) hactar::*running-agents*))))))

(test agent-stack-mismatch-test
  "Test that an agent does not start if the stack does not match."
  (let ((hactar::*live-dangerously* t)
        (hactar::*stack* '("wrong-stack")) ; Mismatched stack
        (agent-def (gethash (intern "TEST-AGENT" :hactar) hactar::*agent-definitions*)))
    (is-true agent-def)
    ;; Attempt to start the agent - it should return nil
    (is (null (hactar::start-agent agent-def '("value1"))))))

(test agent-safety-check-test
  "Test that an agent does not start if safety flags are not set."
  (let ((hactar::*live-dangerously* nil)
        (hactar::*agent-safe-env* nil)
        (hactar::*stack* '("test-stack"))
        (agent-def (gethash (intern "TEST-AGENT" :hactar) hactar::*agent-definitions*)))
    (is-true agent-def)
    ;; Attempt to start the agent - it should return nil
    (is (null (hactar::start-agent agent-def '("value1"))))))

(test base-agent-test "agent def exists"
  (let ((hactar::*live-dangerously* t)
        (agent-def (gethash (intern "CMD" :hactar) hactar::*agent-definitions*)))
    (is-true agent-def)))

  (test cmd-test "Command succeeds on first try"
	(let ((hactar::*live-dangerously* t)
	      (agent-def (gethash (intern "CMD" :hactar) hactar::*agent-definitions*)))
	  (with-dynamic-stubs ((uiop:run-program (lambda (cmd &key &allow-other-keys)
						   (declare (ignore cmd))
						   (values "output" "error" 0)))
			       (hactar::get-llm-response (lambda (prompt &key &allow-other-keys)
							   (declare (ignore prompt))
							   (fail "get-llm-response should not be called")
							   "")))
	    (let ((instance (hactar::start-agent agent-def '("echo 'success'"))))
	      (is-true instance)
	      (bt:join-thread (hactar::agent-instance-thread instance))
	      (is (eql 1 (hactar::agent-instance-retry-count instance)))
	      (is (eql 0 (hactar::agent-instance-last-command-status instance)))))))

  (test cmd-fails-once "Command fails once, then succeeds"
	(let ((hactar::*live-dangerously* t)
	      (agent-def (gethash (intern "CMD" :hactar) hactar::*agent-definitions*))
	      (run-program-call-count 0)
              (get-llm-response-call-count 0))
          (with-dynamic-stubs ((uiop:run-program (lambda (cmd &key &allow-other-keys)
                                                   (declare (ignore cmd))
                                                   (incf run-program-call-count)
                                                   (if (= run-program-call-count 1)
                                                       (values "output" "error" 1)
                                                       (values "output" "error" 0))))
                               (hactar::get-llm-response (lambda (prompt &key &allow-other-keys)
                                                           (declare (ignore prompt))
                                                           (incf get-llm-response-call-count)
                                                           "continue")))
            (let ((instance (hactar::start-agent agent-def '("fail-then-succeed"))))
              (is-true instance)
              (bt:join-thread (hactar::agent-instance-thread instance))
              (is (eql 2 (hactar::agent-instance-retry-count instance)))
              (is (eql 0 (hactar::agent-instance-last-command-status instance)))
              (is (= 1 get-llm-response-call-count))))))

  (test cmd-fail-llm-stop "Command fails, LLM says stop"
	(let ((hactar::*live-dangerously* t)
	      (agent-def (gethash (intern "CMD" :hactar) hactar::*agent-definitions*)))
	  (with-dynamic-stubs ((uiop:run-program (lambda (cmd &key &allow-other-keys)
						   (declare (ignore cmd))
						   (values "output" "error" 1)))
			       (hactar::get-llm-response (lambda (prompt &key &allow-other-keys)
							   (declare (ignore prompt))
							   "stop")))
	    (let ((instance (hactar::start-agent agent-def '("fail-and-stop"))))
	      (is-true instance)
	      (bt:join-thread (hactar::agent-instance-thread instance))
	      (let ((state (hactar::agent-instance-state instance)))
		(is (eql 1 (hactar::agent-instance-retry-count instance)))
		(is (eql 1 (hactar::agent-instance-last-command-status instance)))
		(is (string-equal "stop" (gethash 'last-llm-response state))))))))

  (test cmd-retry-limit "Command fails 3 times (retry limit)"
	(let ((hactar::*live-dangerously* t)
	      (agent-def (gethash (intern "CMD" :hactar) hactar::*agent-definitions*))
	      (get-llm-response-call-count 0))
          (with-dynamic-stubs ((uiop:run-program (lambda (cmd &key &allow-other-keys)
                                                   (declare (ignore cmd))
                                                   (values "output" "error" 1)))
                               (hactar::get-llm-response (lambda (prompt &key &allow-other-keys)
                                                           (declare (ignore prompt))
                                                           (incf get-llm-response-call-count)
                                                           "continue")))
            (let ((instance (hactar::start-agent agent-def '("always-fail"))))
              (is-true instance)
              (bt:join-thread (hactar::agent-instance-thread instance))
              (is (eql 3 (hactar::agent-instance-retry-count instance)))
              (is (eql 1 (hactar::agent-instance-last-command-status instance)))
              (is (= 2 get-llm-response-call-count))))))

(test lint-agent-def-exists "lint agent definition exists"
  (let ((agent-def (gethash (intern "LINT" :hactar) hactar::*agent-definitions*)))
    (is-true agent-def)))

(test lint-succeeds-first-try "Lint agent succeeds on first try"
  (let ((hactar::*live-dangerously* t)
        (hactar::*lint-command* "echo 'lint success'")
        (agent-def (gethash (intern "LINT" :hactar) hactar::*agent-definitions*)))
    (with-dynamic-stubs ((uiop:run-program (lambda (cmd &key &allow-other-keys)
                                             (declare (ignore cmd))
                                             (values "output" "" 0)))
                         (hactar::get-llm-response (lambda (prompt &key &allow-other-keys)
                                                     (declare (ignore prompt))
                                                     (fail "get-llm-response should not be called")
                                                     "")))
      (let ((instance (hactar::start-agent agent-def nil)))
        (is-true instance)
        (bt:join-thread (hactar::agent-instance-thread instance))
        (is (= 1 (hactar::agent-instance-retry-count instance)))
        (is (= 0 (hactar::agent-instance-last-command-status instance)))))))

(test lint-fails-once-then-succeeds "Lint agent fails once, then succeeds"
  (let ((hactar::*live-dangerously* t)
        (hactar::*lint-command* "fail-then-succeed")
        (agent-def (gethash (intern "LINT" :hactar) hactar::*agent-definitions*))
        (run-program-call-count 0)
        (get-llm-response-call-count 0))
    (with-dynamic-stubs ((uiop:run-program (lambda (cmd &key &allow-other-keys)
                                             (declare (ignore cmd))
                                             (incf run-program-call-count)
                                             (if (= run-program-call-count 1)
                                                 (values "" "error" 1)
                                                 (values "" "" 0))))
                         (hactar::get-llm-response (lambda (prompt &key &allow-other-keys)
                                                     (declare (ignore prompt))
                                                     (incf get-llm-response-call-count)
                                                     "some SEARCH/REPLACE block")))
      (let ((instance (hactar::start-agent agent-def nil)))
        (is-true instance)
        (bt:join-thread (hactar::agent-instance-thread instance))
        (is (= 2 (hactar::agent-instance-retry-count instance)))
        (is (= 0 (hactar::agent-instance-last-command-status instance)))
        (is (= 1 get-llm-response-call-count))))))

(test lint-reaches-retry-limit "Lint agent fails until retry limit"
  (let ((hactar::*live-dangerously* t)
        (hactar::*agent-retry-limit* 3) ; Set a specific limit for this test
        (hactar::*lint-command* "always-fail")
        (agent-def (gethash (intern "LINT" :hactar) hactar::*agent-definitions*))
        (get-llm-response-call-count 0))
    (with-dynamic-stubs ((uiop:run-program (lambda (cmd &key &allow-other-keys)
                                             (declare (ignore cmd))
                                             (values "" "error" 1)))
                         (hactar::get-llm-response (lambda (prompt &key &allow-other-keys)
                                                     (declare (ignore prompt))
                                                     (incf get-llm-response-call-count)
                                                     "some SEARCH/REPLACE block")))
      (let ((instance (hactar::start-agent agent-def nil)))
        (is-true instance)
        (bt:join-thread (hactar::agent-instance-thread instance))
        (is (= 3 (hactar::agent-instance-retry-count instance)))
        (is (= 1 (hactar::agent-instance-last-command-status instance)))
        (is (= 3 get-llm-response-call-count))))))
