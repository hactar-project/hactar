(in-package #:hactar-rebuild)

;;; Multi-turn assistant chat loop over hactar-llm + registered tools.

(defun chat-turn (user-message &key (model *current-model*)
                                    system-prompt
                                    tools
                                    (record-history t)
                                    (run-hooks-p t))
  "Execute one assistant turn: compose context prompt, call hactar-llm:chat,
record history, optionally run TURN.AFTER hook, and return a RESULT."
  (let* ((sys (or system-prompt (render-system-prompt)))
         (answer (llm-chat model sys user-message tools)))
    (when record-history
      (append-history :user user-message)
      (append-history :assistant answer))
    (let ((res (ok answer :data (list :model model :turn user-message))))
      (when (and run-hooks-p (gethash "turn.after" *hooks*))
        (run-hook 'turn.after user-message res))
      res)))

(defun run-tool-loop (user-message &key (model *current-model*)
                                        system-prompt
                                        tools
                                        (max-steps 5))
  "Run a multi-step tool loop. Supports structured responses returned as
(:text string :tool-calls list) or plain string answers."
  (let ((sys (or system-prompt (render-system-prompt)))
        (prompt user-message)
        (history-steps nil))
    (append-history :user user-message)
    (loop for step from 1 to max-steps
          for raw = (llm-chat model sys prompt tools)
          do (cond
               ((stringp raw)
                (append-history :assistant raw)
                (return-from run-tool-loop
                  (ok raw :data (list :steps step :tool-history (nreverse history-steps)))))
               ((and (listp raw) (getf raw :tool-calls))
                (let ((calls (hactar-llm:normalize-tool-calls (getf raw :tool-calls)))
                      (tool-outputs nil))
                  (dolist (tc calls)
                    (let* ((tname (getf tc :name))
                           (targs (getf tc :arguments))
                           (tresult (apply #'run-tool tname targs)))
                      (push (list :tool tname :result (result-message tresult)
                                  :status (result-status tresult))
                            tool-outputs)))
                  (let ((outputs (nreverse tool-outputs)))
                    (push outputs history-steps)
                    (setf prompt (format nil "Tool results:~%~{~S~%~}" outputs)))))
               (t
                (let ((text (princ-to-string (or (and (listp raw) (getf raw :text)) raw))))
                  (append-history :assistant text)
                  (return-from run-tool-loop
                    (ok text :data (list :steps step :tool-history (nreverse history-steps))))))))
    (err "Tool loop exceeded max steps" :data (list :steps max-steps))))
