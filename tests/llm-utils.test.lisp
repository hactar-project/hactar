(in-package :hactar-tests)

(def-suite llm-utils-tests
  :description "Tests for llm-utils.lisp functions.")

(in-suite llm-utils-tests)

;;* ask tests

(test ask-returns-content-from-llm
  "ask should return the content string from llm:complete."
  (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama"
                                                :model-name "test-model"
                                                :max-input-tokens 4096)))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream &allow-other-keys)
                                         (declare (ignore provider messages model system-prompt stream))
                                         (values "Hello, world!" nil nil))))
      (let ((result (hactar::ask "What is your name?")))
        (is (string= result "Hello, world!"))))))

(test ask-passes-correct-provider-and-model
  "ask should resolve model config and pass provider/model to llm:complete."
  (let ((mock-model (hactar::make-model-config :name "my-model" :provider "anthropic"
                                                :model-name "claude-3-haiku"
                                                :max-input-tokens 4096))
        (captured-provider nil)
        (captured-model nil))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream &allow-other-keys)
                                         (declare (ignore messages system-prompt stream))
                                         (setf captured-provider provider
                                               captured-model model)
                                         (values "response" nil nil))))
      (hactar::ask "question" :model-name "my-model")
      (is (eq captured-provider :anthropic))
      (is (string= captured-model "claude-3-haiku")))))

(test ask-falls-back-to-ollama-when-model-not-found
  "ask should fall back to :ollama when model is not found."
  (let ((captured-provider nil)
        (hactar::*debug* nil))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       nil))
                         (llm:complete (lambda (provider messages &key model system-prompt stream &allow-other-keys)
                                         (declare (ignore messages model system-prompt stream))
                                         (setf captured-provider provider)
                                         (values "fallback response" nil nil))))
      (let ((result (hactar::ask "question" :model-name "nonexistent")))
        (is (eq captured-provider :ollama))
        (is (string= result "fallback response"))))))

(test ask-returns-nil-when-llm-returns-nil
  "ask should return nil when llm:complete returns nil content."
  (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama"
                                                :model-name "test-model"
                                                :max-input-tokens 4096)))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream &allow-other-keys)
                                         (declare (ignore provider messages model system-prompt stream))
                                         (values nil nil nil))))
      (is (null (hactar::ask "question"))))))

(test ask-passes-custom-system-prompt
  "ask should forward the :system-prompt keyword to llm:complete."
  (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama"
                                                :model-name "test-model"
                                                :max-input-tokens 4096))
        (captured-system-prompt nil))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream &allow-other-keys)
                                         (declare (ignore provider messages model stream))
                                         (setf captured-system-prompt system-prompt)
                                         (values "ok" nil nil))))
      (hactar::ask "question" :system-prompt "You are a pirate.")
      (is (string= captured-system-prompt "You are a pirate.")))))

;;* moderate tests

(test moderate-returns-flagged-true-plist
  "moderate should return (:flagged t :reason ...) when LLM flags content."
  (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama"
                                                :model-name "test-model"
                                                :max-input-tokens 4096)))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream response-format &allow-other-keys)
                                         (declare (ignore provider messages model system-prompt stream response-format))
                                         (values "{\"flagged\": true, \"reason\": \"violence\"}" nil nil))))
      (let ((result (hactar::moderate "some violent text")))
        (is (eq (getf result :flagged) t))
        (is (string= (getf result :reason) "violence"))))))

(test moderate-returns-flagged-false-plist
  "moderate should return (:flagged nil :reason nil) when LLM does not flag content."
  (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama"
                                                :model-name "test-model"
                                                :max-input-tokens 4096)))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream response-format &allow-other-keys)
                                         (declare (ignore provider messages model system-prompt stream response-format))
                                         (values "{\"flagged\": false, \"reason\": null}" nil nil))))
      (let ((result (hactar::moderate "perfectly fine text")))
        (is (eq (getf result :flagged) nil))
        (is (null (getf result :reason)))))))

(test moderate-handles-code-fenced-json
  "moderate should strip markdown code fences from the LLM response."
  (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama"
                                                :model-name "test-model"
                                                :max-input-tokens 4096))
        (fenced-response (format nil "```json~%{\"flagged\": true, \"reason\": \"spam\"}~%```")))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream response-format &allow-other-keys)
                                         (declare (ignore provider messages model system-prompt stream response-format))
                                         (values fenced-response nil nil))))
      (let ((result (hactar::moderate "spam text")))
        (is (eq (getf result :flagged) t))
        (is (string= (getf result :reason) "spam"))))))

(test moderate-handles-nil-content
  "moderate should return (:flagged nil :reason nil) when LLM returns nil."
  (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama"
                                                :model-name "test-model"
                                                :max-input-tokens 4096))
        (hactar::*debug* nil))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream response-format &allow-other-keys)
                                         (declare (ignore provider messages model system-prompt stream response-format))
                                         (values nil nil nil))))
      (let ((result (hactar::moderate "text")))
        (is (eq (getf result :flagged) nil))
        (is (null (getf result :reason)))))))

(test moderate-handles-malformed-json
  "moderate should return (:flagged nil :reason nil) on unparseable JSON."
  (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama"
                                                :model-name "test-model"
                                                :max-input-tokens 4096))
        (hactar::*debug* nil))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream response-format &allow-other-keys)
                                         (declare (ignore provider messages model system-prompt stream response-format))
                                         (values "not valid json {{{" nil nil))))
      (let ((result (hactar::moderate "text")))
        (is (eq (getf result :flagged) nil))
        (is (null (getf result :reason)))))))

(test moderate-sends-json-response-format
  "moderate should request json_object response format from llm:complete."
  (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama"
                                                :model-name "test-model"
                                                :max-input-tokens 4096))
        (captured-response-format nil))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (declare (ignore name))
                                                       mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream response-format &allow-other-keys)
                                         (declare (ignore provider messages model system-prompt stream))
                                         (setf captured-response-format response-format)
                                         (values "{\"flagged\": false, \"reason\": null}" nil nil))))
      (hactar::moderate "text")
      (is (string= captured-response-format "json_object")))))

;;* embed tests

(test embed-delegates-to-ollama-embed
  "embed should call llm:ollama-embed and return its result."
  (let ((expected-embedding '(0.1 0.2 0.3 0.4 0.5)))
    (with-dynamic-stubs ((llm:ollama-embed (lambda (text &key model endpoint)
                                             (declare (ignore text endpoint))
                                             (when (string= model "nomic-embed-text")
                                               expected-embedding))))
      (let ((result (hactar::embed "hello world")))
        (is (equal result expected-embedding))))))

(test embed-passes-custom-model
  "embed should forward the :model keyword to llm:ollama-embed."
  (let ((captured-model nil))
    (with-dynamic-stubs ((llm:ollama-embed (lambda (text &key model endpoint)
                                             (declare (ignore text endpoint))
                                             (setf captured-model model)
                                             '(0.1 0.2))))
      (hactar::embed "text" :model "mxbai-embed-large")
      (is (string= captured-model "mxbai-embed-large")))))

(test embed-passes-custom-endpoint
  "embed should forward the :endpoint keyword to llm:ollama-embed."
  (let ((captured-endpoint nil))
    (with-dynamic-stubs ((llm:ollama-embed (lambda (text &key model endpoint)
                                             (declare (ignore text model))
                                             (setf captured-endpoint endpoint)
                                             '(0.1))))
      (hactar::embed "text" :endpoint "http://custom:11434/api/embeddings")
      (is (string= captured-endpoint "http://custom:11434/api/embeddings")))))

(test embed-returns-nil-on-failure
  "embed should return nil when ollama-embed returns nil."
  (with-dynamic-stubs ((llm:ollama-embed (lambda (text &key model endpoint)
                                           (declare (ignore text model endpoint))
                                           nil)))
    (is (null (hactar::embed "text")))))

(test embed-uses-default-model
  "embed should use nomic-embed-text as the default model."
  (let ((captured-model nil))
    (with-dynamic-stubs ((llm:ollama-embed (lambda (text &key model endpoint)
                                             (declare (ignore text endpoint))
                                             (setf captured-model model)
                                             '(0.5))))
      (hactar::embed "text")
      (is (string= captured-model "nomic-embed-text")))))

;;* cost calculator tests

(test calculate-model-cost-with-struct
  "calculate-model-cost should correctly calculate cost when passed a model-config struct."
  (let ((mock-model (hactar::make-model-config :name "test-paid" :provider "openai"
                                                :model-name "test-paid"
                                                :input-cost-per-token 0.000002
                                                :output-cost-per-token 0.000008)))
    (is (= (hactar::calculate-model-cost mock-model :input-tokens 100 :output-tokens 50)
           (+ (* 100 0.000002) (* 50 0.000008))))))

(test calculate-model-cost-with-name-string
  "calculate-model-cost should look up model config by name and calculate cost."
  (let ((mock-model (hactar::make-model-config :name "my-paid-model" :provider "openai"
                                                :model-name "gpt-4o"
                                                :input-cost-per-token 0.0000025
                                                :output-cost-per-token 0.00001)))
    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name)
                                                       (if (string= name "my-paid-model")
                                                           mock-model
                                                           nil))))
      (is (= (hactar::calculate-model-cost "my-paid-model" :input-tokens 1000 :output-tokens 500)
             (+ (* 1000 0.0000025) (* 500 0.00001)))))))

(test validate-model-config-cost-defaults
  "validate-model-config should assign smart defaults: 0.0 for ollama/copilot/free, non-zero for paid."
  ;; Test Ollama (free)
  (let* ((ollama-data (make-hash-table :test 'equal))
         (validated (progn
                      (setf (gethash "name" ollama-data) "ollama/some-model"
                            (gethash "model_name" ollama-data) "some-model")
                      (hactar::validate-model-config ollama-data))))
    (is (= 0.0 (gethash "input_cost_per_token" validated)))
    (is (= 0.0 (gethash "output_cost_per_token" validated))))

  ;; Test Copilot (free)
  (let* ((copilot-data (make-hash-table :test 'equal))
         (validated (progn
                      (setf (gethash "name" copilot-data) "copilot/gpt-4o"
                            (gethash "model_name" copilot-data) "gpt-4o")
                      (hactar::validate-model-config copilot-data))))
    (is (= 0.0 (gethash "input_cost_per_token" validated)))
    (is (= 0.0 (gethash "output_cost_per_token" validated))))

  ;; Test OpenRouter free model
  (let* ((free-data (make-hash-table :test 'equal))
         (validated (progn
                      (setf (gethash "name" free-data) "openrouter/meta-llama/llama-3-8b:free"
                            (gethash "model_name" free-data) "llama-3-8b")
                      (hactar::validate-model-config free-data))))
    (is (= 0.0 (gethash "input_cost_per_token" validated)))
    (is (= 0.0 (gethash "output_cost_per_token" validated))))

  ;; Test Paid model default (e.g. standard openai/anthropic default when not specified)
  (let* ((paid-data (make-hash-table :test 'equal))
         (validated (progn
                      (setf (gethash "name" paid-data) "openai/custom-gpt"
                            (gethash "model_name" paid-data) "custom-gpt")
                      (hactar::validate-model-config paid-data))))
    (is (= 0.0000011 (gethash "input_cost_per_token" validated)))
    (is (= 0.0000044 (gethash "output_cost_per_token" validated)))))
