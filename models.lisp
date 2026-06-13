(in-package :hactar)

(define-command model (args)
                "Switch to a new LLM. Uses fuzzy-select if no model name is provided."
                (if args
                  (let ((model-name (first args)))
                    (set-current-model model-name))
                  (cond
                    ((and (or *acp-mode* (not *in-repl*))
                          (not (and (boundp '*tui-running*) (symbol-value '*tui-running*))))
                     (if *available-models*
                       (progn
                         (format t "Current model: ~A~%" (if *current-model* (model-config-name *current-model*) "None"))
                         (format t "Available models (use /model <name> to switch):~%")
                         (dolist (model *available-models*)
                           (format t "  ~A~%" (model-config-name model))))
                       (format t "No models available.~%")))
                    ;; Interactive mode with fuzzy-select
                    (*available-models*
                     (let* ((items (loop for model in *available-models*
                                         collect `((:item . ,(model-config-name model))
                                                   (:preview . ,(format nil "Provider: ~A~%Model: ~A~%Max Tokens: ~A In / ~A Out~%Edit Format: ~A~%Cost (In/Out): $~8F / $~8F: Supports: ~A"
                                                                        (model-config-provider model)
                                                                        (model-config-model-name model)
                                                                        (model-config-max-input-tokens model)
                                                                        (model-config-max-output-tokens model)
                                                                        (model-config-edit-format model)
                                                                        (model-config-input-cost-per-token model)
                                                                        (model-config-output-cost-per-token model)
                                                                        (format nil "~{~A~^, ~}" (model-config-supports model)))))))
                            (selected-item (fuzzy-select items)))
                       (if selected-item
                         (let ((selected-model-name (cdr (assoc :item selected-item))))
                           (set-current-model selected-model-name))
                         (format t "Model selection cancelled.~%"))))
                    (t (format t "No models available to select.~%"))))
                :completions (lambda (text args)
                               (declare (ignore args))
                               (let ((names (mapcar #'model-config-name *available-models*)))
                                 (if (string= text "")
                                     names
                                     (remove-if-not
                                      (lambda (n)
                                        (str:starts-with-p text n :ignore-case t))
                                      names))))
                :acp (lambda (cmd-args)
                       (if cmd-args
                           (let ((model-name (first cmd-args)))
                             (set-current-model model-name)
                             `(("text" . ,(format nil "Model set to: ~A" model-name))
                               ("data" . (("model" . ,model-name)))))
                           `(("text" . ,(format nil "Current model: ~A"
                                                (if *current-model* (model-config-name *current-model*) "None")))
                             ("data" . (("currentModel" . ,(if *current-model* (model-config-name *current-model*) :null))
                                        ("availableModels" . ,(coerce
                                                               (mapcar (lambda (m)
                                                                         `(("name" . ,(model-config-name m))
                                                                           ("provider" . ,(model-config-provider m))
                                                                           ("modelName" . ,(model-config-model-name m))))
                                                                       *available-models*)
                                                               'vector))))))))

(define-command models (args)
                "Search the list of available models."
                (let ((search-term (if args (first args) "")))
                  (format t "Available models:~%")
                  (dolist (model *available-models*)
                    (when (or (string= search-term "")
                              (search search-term (model-config-name model) :test #'char-equal))
                      (format t "  ~A~%" (model-config-name model))
                      (format t "    Model name: ~A~%" (model-config-model-name model))
                      (format t "    Edit format: ~A~%" (model-config-edit-format model))
                      (format t "    Max tokens: ~A input / ~A output~%"
                              (model-config-max-input-tokens model)
                              (model-config-max-output-tokens model))
                      (format t "    Supports: ~{~A~^, ~}~%" (model-config-supports model)))))
                :acp (lambda (cmd-args)
                       (let ((search-term (if cmd-args (first cmd-args) "")))
                         (let ((matching (loop for model in *available-models*
                                               when (or (string= search-term "")
                                                        (search search-term (model-config-name model) :test #'char-equal))
                                               collect `(("name" . ,(model-config-name model))
                                                         ("modelName" . ,(model-config-model-name model))
                                                         ("provider" . ,(model-config-provider model))
                                                         ("editFormat" . ,(model-config-edit-format model))
                                                         ("maxInputTokens" . ,(model-config-max-input-tokens model))
                                                         ("maxOutputTokens" . ,(model-config-max-output-tokens model))
                                                         ("supports" . ,(coerce (model-config-supports model) 'vector))))))
                           `(("text" . ,(format nil "~A model(s) found." (length matching)))
                             ("data" . ,(coerce matching 'vector)))))))

(define-command cheap-model (args)
                "Set the cheap model to use when cost is a concern."
                (if args
                  (let ((model-name (first args)))
                    (setf *cheap-model* model-name)
                    (format t "Cheap model set to: ~A~%" model-name))
                  (cond
                    ((or *acp-mode* (not *in-repl*))
                     (if *available-models*
                       (progn
                         (format t "Current cheap model: ~A~%" (or *cheap-model* "None"))
                         (format t "Available models (use /cheap-model <name> to switch):~%")
                         (dolist (model *available-models*)
                           (format t "  ~A~%" (model-config-name model))))
                       (format t "No models available.~%")))
                    (*available-models*
                     (let* ((items (loop for model in *available-models*
                                         collect `((:item . ,(model-config-name model))
                                                   (:preview . ,(format nil "Provider: ~A~%Model: ~A~%Max Tokens: ~A In / ~A Out~%Edit Format: ~A~%Cost (In/Out): $~8F / $~8F: Supports: ~A"
                                                                        (model-config-provider model)
                                                                        (model-config-model-name model)
                                                                        (model-config-max-input-tokens model)
                                                                        (model-config-max-output-tokens model)
                                                                        (model-config-edit-format model)
                                                                        (model-config-input-cost-per-token model)
                                                                        (model-config-output-cost-per-token model)
                                                                        (format nil "~{~A~^, ~}" (model-config-supports model)))))))
                            (selected-item (fuzzy-select items)))
                       (if selected-item
                         (let ((selected-model-name (cdr (assoc :item selected-item))))
                           (setf *cheap-model* selected-model-name)
                           (format t "Cheap model set to: ~A~%" selected-model-name))
                         (format t "Model selection cancelled.~%"))))
                    (t (format t "No models available to select.~%")))))

(define-command docs-meta-model (args)
                "Set the model to use for generating documentation metadata."
                (if args
                  (let ((model-name (first args)))
                    (setf *docs-meta-model* model-name)
                    (format t "Docs meta model set to: ~A~%" model-name))
                  (cond
                    ((or *acp-mode* (not *in-repl*))
                     (if *available-models*
                       (progn
                         (format t "Current docs meta model: ~A~%" (or *docs-meta-model* "None"))
                         (format t "Available models (use /docs-meta-model <name> to switch):~%")
                         (dolist (model *available-models*)
                           (format t "  ~A~%" (model-config-name model))))
                       (format t "No models available.~%")))
                    (*available-models*
                     (let* ((items (loop for model in *available-models*
                                         collect `((:item . ,(model-config-name model))
                                                   (:preview . ,(format nil "Provider: ~A~%Model: ~A~%Max Tokens: ~A In / ~A Out~%Edit Format: ~A~%Cost (In/Out): $~8F / $~8F: Supports: ~A"
                                                                        (model-config-provider model)
                                                                        (model-config-model-name model)
                                                                        (model-config-max-input-tokens model)
                                                                        (model-config-max-output-tokens model)
                                                                        (model-config-edit-format model)
                                                                        (model-config-input-cost-per-token model)
                                                                        (model-config-output-cost-per-token model)
                                                                        (format nil "~{~A~^, ~}" (model-config-supports model)))))))
                            (selected-item (fuzzy-select items)))
                       (if selected-item
                         (let ((selected-model-name (cdr (assoc :item selected-item))))
                           (setf *docs-meta-model* selected-model-name)
                           (format t "Docs meta model set to: ~A~%" selected-model-name))
                         (format t "Model selection cancelled.~%"))))
                    (t (format t "No models available to select.~%")))))

(defun find-model-by-name (name)
  "Find a model configuration by name."
  (find name *available-models* :key #'model-config-name :test #'string=))

(defun set-current-model (model-name)
  "Set the current model by name."
  (let ((model (find-model-by-name model-name)))
    (unless model
      ;; Dynamically create model-config if it has a known provider
      (let* ((slash-pos (position #\/ model-name))
             (provider (if slash-pos
                           (subseq model-name 0 slash-pos)
                           "ollama"))
             (actual-model-name (if slash-pos
                                    (subseq model-name (1+ slash-pos))
                                    model-name)))
        (when (member provider '("ollama" "openai" "anthropic" "gemini" "openrouter" "deepseek" "kimi" "glm" "google") :test #'string-equal)
          (setf model (make-model-config
                       :name model-name
                       :provider provider
                       :model-name actual-model-name
                       :edit-format "diff"
                       :use-repo-map t
                       :max-input-tokens 32000
                       :max-output-tokens 4096
                       :input-cost-per-token 0.0
                       :output-cost-per-token 0.0
                       :supports '("chat" "edit")))
          (push model *available-models*))))
    (if model
        (let ((old-model *current-model*))
          (setf *current-model* model)
          (nhooks:run-hook *model-changed-hook* model old-model)
          (unless *silent*
            (if *lisp-rpc-mode*
                (rpc-model-changed model-name)
                (format t "Switched to model: ~A~%" model-name))))
        (unless *silent*
          (if *lisp-rpc-mode*
              (rpc-model-not-found model-name)
              (format t "Model not found: ~A~%" model-name))))))

(defun switch-model-by-provider (provider args)
  (let ((provider-models (remove-if-not (lambda (m) (string-equal (model-config-provider m) provider)) *available-models*)))
    (cond
      ((null provider-models)
       (format t "No models found for provider: ~A~%" provider))
      (args
       (let* ((arg (first args))
              (full-name (if (search "/" arg) arg (format nil "~A/~A" provider arg)))
              (model (or (find-model-by-name full-name)
                         (find-model-by-name arg)
                         (find-if (lambda (m) (search arg (model-config-name m) :test #'char-equal)) provider-models))))
         (if model
             (set-current-model (model-config-name model))
             (format t "Model ~S not found for provider ~A.~%" arg provider))))
      (t
       (cond
         ((or *acp-mode* (not *in-repl*))
          (format t "Current model: ~A~%" (if *current-model* (model-config-name *current-model*) "None"))
          (format t "Available ~A models:~%" provider)
          (dolist (model provider-models)
            (format t "  ~A~%" (model-config-name model))))
         (t
          (let* ((items (loop for model in provider-models
                              collect `((:item . ,(model-config-name model))
                                        (:preview . ,(format nil "Provider: ~A~%Model: ~A~%Max Tokens: ~A In / ~A Out~%Edit Format: ~A~%Cost (In/Out): $~8F / $~8F: Supports: ~A"
                                                             (model-config-provider model)
                                                             (model-config-model-name model)
                                                             (model-config-max-input-tokens model)
                                                             (model-config-max-output-tokens model)
                                                             (model-config-edit-format model)
                                                             (model-config-input-cost-per-token model)
                                                             (model-config-output-cost-per-token model)
                                                             (format nil "~{~A~^, ~}" (model-config-supports model)))))))
                 (selected-item (fuzzy-select items)))
            (if selected-item
                (let ((selected-model-name (cdr (assoc :item selected-item))))
                  (set-current-model selected-model-name))
                (format t "Model selection cancelled.~%")))))))))

(define-command copilot (args)
  "Switch to a Copilot model."
  (switch-model-by-provider "copilot" args))

(define-command openrouter (args)
  "Switch to an OpenRouter model."
  (switch-model-by-provider "openrouter" args))

(define-command gemini (args)
  "Switch to a Gemini model."
  (switch-model-by-provider "gemini" args))

(define-command anthropic (args)
  "Switch to an Anthropic model."
  (switch-model-by-provider "anthropic" args))

(define-command openai (args)
  "Switch to an OpenAI model."
  (switch-model-by-provider "openai" args))

(define-command deepseek (args)
  "Switch to a DeepSeek model."
  (switch-model-by-provider "deepseek" args))

(defun calculate-model-cost (model &key (input-tokens 0) (output-tokens 0))
  "Calculate the estimated cost for the given INPUT-TOKENS and OUTPUT-TOKENS.
   MODEL can be a model-config struct or a string (model name)."
  (let ((config (if (stringp model)
                    (find-model-by-name model)
                    model)))
    (if config
        (+ (* input-tokens (model-config-input-cost-per-token config))
           (* output-tokens (model-config-output-cost-per-token config)))
        0.0)))
