;; WIP in-line completion endpoint
(in-package :hactar)
(define-command set-completion-model (args)
  "Set the model used for the /complete command and HTTP endpoint. Uses fuzzy-select if no model name is provided."
  (if args
      (let* ((model-name (first args))
             (model (find-model-by-name model-name)))
        (if model
            (progn
              (setf *completion-model* model)
              (format t "Completion model set to: ~A~%" model-name))
            (format t "Model not found: ~A~%" model-name)))
      (if *available-models*
          (let* ((items (loop for model in *available-models*
                              collect `((:item . ,(model-config-name model))
                                        (:preview . ,(format nil "Provider: ~A~%Model: ~A~%Max Tokens: ~A In / ~A Out"
                                                             (model-config-provider model)
                                                             (model-config-model-name model)
                                                             (model-config-max-input-tokens model)
                                                             (model-config-max-output-tokens model))))))
                 (selected-item (fuzzy-select items)))
            (if selected-item
                (let* ((selected-model-name (cdr (assoc :item selected-item)))
                       (model (find-model-by-name selected-model-name)))
                  (when model
                    (setf *completion-model* model)
                    (format t "Completion model set to: ~A~%" selected-model-name)))
                (format t "Model selection cancelled.~%")))
          (format t "No models available to select.~%"))))

(define-command complete (args)
                "Complete the provided text using the configured completion model."
                (let ((input-text (format nil "~{~A~^ ~}" args)))
                  (if (null *completion-model*)
                    nil
                    (if (string= input-text "")
                      (format t "Please provide text to complete.~%")
                      (handler-case
                          (let* ((mustache:*escape-tokens* nil)
				 (prompt-template (uiop:read-file-string (get-prompt-path "complete-text.mustache")))
                                 (prompt (mustache:render* prompt-template `((:text . ,input-text))))
                                 (provider-type (intern (string-upcase (model-config-provider *completion-model*)) :keyword))
                                 (completion (llm:complete provider-type
                                                           `(((:role . "user") (:content . ,prompt)))
                                                           :model (model-config-model-name *completion-model*)
                                                           :max-tokens (model-config-max-output-tokens *completion-model*)
                                                           :system-prompt ""
                                                           :stream nil)))
                            (format t "~%~A~%" completion))
                        (error (e)
                          (format t "Error getting completion: ~A~%" e)))))))
