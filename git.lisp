;; git utils
(in-package :hactar)
(defun generate-commit-message ()
  "Asks the LLM to generate a commit message based on the staged git diff."
  (unless *cheap-model*
    (format t "No LLM model selected. Cannot generate commit message.~%")
    (return-from generate-commit-message nil))
  (let ((model-config (find-model-by-name *cheap-model*)))
    (unless model-config
      (format t "Model '~A' not found.~%" *cheap-model*)
      (return-from generate-commit-message nil))
    (let* ((prompt-template (get-prompt 'git-commit "git-commit.mustache"))
           (diff (run-git-command '("diff" "--staged") :ignore-error nil))
           (prompt (mustache:render* prompt-template `((:diff . ,diff))))
           (messages `(((:role . "user") (:content . ,prompt))))
           (provider (intern (string-upcase (model-config-provider model-config)) :keyword)))
      (handler-case
          (multiple-value-bind (response _)
			       (llm:complete provider messages
					     :model (model-config-model-name model-config)
					     :max-tokens (model-config-max-output-tokens model-config)
					     :system-prompt "You generate commit messages."
					     :stream nil)
			       (declare (ignore _))
			       (when response
				 (let ((first-line (first (str:lines response))))
				   (string-trim '(#\Space #\Tab #\Newline #\Return) first-line))))
        (error (e)
               (format t "Error generating commit message: ~A~%" e)
               nil)))))
