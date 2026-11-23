(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test session-capture-save-load-roundtrip
  (with-temp-project (root :files '(("src/app.lisp" . "(defun hi () :ok)")))
    (hactar-rebuild:clear-history)
    (setf hactar-rebuild:*context-files* nil
          hactar-rebuild:*active-presets* nil)
    (clrhash hactar-rebuild:*active-rules*)
    (hactar-rebuild:register-preset
     "lisp-dev"
     :model "hactar-mock"
     :rules '(("style" . "Use small functions"))
     :files '("src/app.lisp"))
    (hactar-rebuild:apply-preset "lisp-dev")
    (hactar-rebuild:append-history :user "hello")
    (hactar-rebuild:append-history :assistant "hi there")
    (is (search "Use small functions" (hactar-rebuild:render-system-prompt :root root)))
    (is (search "role: user" (hactar-rebuild:serialize-history-yaml
                              hactar-rebuild:*chat-history*)))
    (hactar-rebuild:save-session "alpha" root)
    (is (equal '("alpha") (hactar-rebuild:list-sessions root)))
    (hactar-rebuild:clear-history)
    (setf hactar-rebuild:*context-files* nil)
    (hactar-rebuild:load-session "alpha" root)
    (is (= 2 (length hactar-rebuild:*chat-history*)))
    (is (string= "hactar-mock" hactar-rebuild:*current-model*))
    (is (equal '("src/app.lisp")
               (mapcar #'car hactar-rebuild:*context-files*)))))

(test assistant-chat-turn-and-tool-loop
  (with-temp-project (root)
    (hactar-rebuild:clear-history)
    (let ((hactar-rebuild:*auto-update* nil)
          (hactar-rebuild:*mock-llm-output* "assistant reply"))
      (let ((res (hactar-rebuild:chat-turn "explain this" :model "hactar-mock")))
        (is (eq :ok (hactar-rebuild:result-status res)))
        (is (string= "assistant reply" (hactar-rebuild:result-message res)))
        (is (= 2 (length hactar-rebuild:*chat-history*)))))
    (hactar-rebuild:deftool add-nums (a b) "Add" (+ a b))
    (let* ((step 0)
           (hactar-llm:*llm-client*
             (hactar-llm:make-client
              :chat (lambda (model sys user &optional tools)
                      (declare (ignore model sys user tools))
                      (incf step)
                      (if (= step 1)
                          (list :tool-calls (list (list :name "add-nums" :arguments '(2 3))))
                          "sum is 5")))))
      (let ((res (hactar-rebuild:run-tool-loop "add 2 and 3" :model "hactar-mock")))
        (is (eq :ok (hactar-rebuild:result-status res)))
        (is (string= "sum is 5" (hactar-rebuild:result-message res)))
        (is (= 2 (getf (hactar-rebuild:result-data res) :steps)))))))

(test doctor-checks-and-synth-signal
  (with-temp-project (root)
    (let ((res (hactar-rebuild:run-doctor-checks root)))
      (is-true (hactar-rebuild:result-p res))
      (is (listp (getf (hactar-rebuild:result-data res) :checks))))
    (setf hactar-rebuild:*update-ledger* '((:kind :edit :path "a.lisp")))
    (let ((sig (hactar-rebuild:collect-signal)))
      (is-true (hactar-rebuild:signal-ready-p sig)))))
