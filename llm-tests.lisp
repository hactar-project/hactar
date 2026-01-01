(defpackage :llm-tests
  (:use :cl :fiveam :llm :mockingbird :shasht)
  (:export #:run-tests))

(in-package :llm-tests)

;; Define the test suite
(def-suite llm-tests
  :description "Tests for the llm library")

(in-suite llm-tests)

;; Helper function to read all chunks from a stream reader
(defun read-all-chunks (reader)
  (with-output-to-string (s)
    (loop for chunk = (read-next-chunk reader)
          while chunk
          do (write-string chunk s))))

;; Test the stream reader functionality with different providers
(test stream-reader-parsing
  "Test parsing logic within read-next-chunk for various providers"
  ; OpenAI/OpenRouter Format
  (let* ((mock-lines (format nil "data: {\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}~%data: {\"choices\":[{\"delta\":{\"content\":\" world\"}}]}~%data: [DONE]~%"))
         (mock-binary-stream (flexi-streams:make-in-memory-input-stream (babel:string-to-octets mock-lines :encoding :utf-8)))
         (reader (make-stream-reader mock-binary-stream :openai)))
    (is (string= "Hello world" (read-all-chunks reader)))
    (is (llm-stream-reader-closed-p reader)))

  ; Ollama Format
  (let* ((mock-lines (format nil "{\"message\":{\"content\":\"Ollama\"},\"done\":false}~%{\"message\":{\"content\":\" stream\"},\"done\":false}~%{\"done\":true,\"message\":{\"content\":\"\"}}~%"))
         (mock-binary-stream (flexi-streams:make-in-memory-input-stream (babel:string-to-octets mock-lines :encoding :utf-8)))
         (reader (make-stream-reader mock-binary-stream :ollama)))
    (is (string= "Ollama stream" (read-all-chunks reader)))
    (is (llm-stream-reader-closed-p reader)))

  ; Anthropic Format
  (let* ((mock-lines (format nil "event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"text\":\"Anthropic\"}}~%~%event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"text\":\" rocks\"}}~%~%event: message_stop~%data: {\"type\":\"message_stop\"}~%"))
         (mock-binary-stream (flexi-streams:make-in-memory-input-stream (babel:string-to-octets mock-lines :encoding :utf-8)))
         (reader (make-stream-reader mock-binary-stream :anthropic)))
    (is (string= "Anthropic rocks" (read-all-chunks reader)))
    (is (llm-stream-reader-closed-p reader)))

  ; Gemini Format
  (let* ((mock-lines (format nil "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Gemini\"}]}}]}~%data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\" stream\"}]}}]}~%")) ; Gemini doesn't have an explicit DONE marker in this format
         (mock-binary-stream (flexi-streams:make-in-memory-input-stream (babel:string-to-octets mock-lines :encoding :utf-8)))
         (reader (make-stream-reader mock-binary-stream :gemini)))
    (is (string= "Gemini stream" (read-all-chunks reader)))
    ;; Reading past the end should close it
    (is (null (read-next-chunk reader)))
    (is (llm-stream-reader-closed-p reader)))
  )

;; Test the convert-byte-array-to-utf8 function (still potentially useful)
(test convert-byte-array-to-utf8-test
  "Test conversion of byte arrays to UTF-8 strings"
  (let ((test-string "Hello, world!"))
    (let ((byte-array (babel:string-to-octets test-string :encoding :utf-8)))
      (is (string= test-string (llm::convert-byte-array-to-utf8 byte-array))))))

;; Test the prepare-messages function
(test prepare-messages-test
  "Test the prepare-messages function for formatting messages"
  ;; Test with a string message
  (let ((result (llm::prepare-messages "Hello" "System prompt")))
    (is (= 2 (length result)))
    (is (string= "system" (cdr (assoc :role (first result)))))
    (is (string= "System prompt" (cdr (assoc :content (first result)))))
    (is (string= "user" (cdr (assoc :role (second result)))))
    (is (string= "Hello" (cdr (assoc :content (second result))))))
  
  ;; Test with a message array
  (let (
	(result (llm::prepare-messages '(((:role . "user") (:content . "Hello"))) "System prompt")))
    (is (= 2 (length result)))
    (is (string= "system" (cdr (assoc :role (first result)))))
    (is (string= "System prompt" (cdr (assoc :content (first result)))))
    (is (string= "user" (cdr (assoc :role (second result)))))
    (is (string= "Hello" (cdr (assoc :content (second result)))))))

;; Test the complete function with mocks for specific implementations
(test complete-dispatch-test
  "Test the complete function dispatches to the correct completer"
  (let ((test-message "Test message")
        (mock-response "This is a mock response"))
    
    ;; Test OpenAI dispatch
    (with-dynamic-stubs ((llm:openai-complete 
                          (lambda (messages &rest args)
                            (declare (ignore args))
                            (is (equal test-message messages))
                            mock-response)))
      (is (equal mock-response (llm:complete :openai test-message))))
    
    ;; Test Ollama dispatch
    (with-dynamic-stubs ((llm:ollama-complete 
                          (lambda (messages &rest args)
                            (declare (ignore args))
                            (is (equal test-message messages))
                            mock-response)))
      (is (equal mock-response (llm:complete :ollama test-message))))
    
    ;; Test Anthropic dispatch
    (with-dynamic-stubs ((llm:anthropic-complete 
                          (lambda (messages &rest args)
                            (declare (ignore args))
                            (is (equal test-message messages))
                            mock-response)))
      (is (equal mock-response (llm:complete :anthropic test-message))))
    
    ;; Test OpenRouter dispatch
    (with-dynamic-stubs ((llm:openrouter-complete 
                          (lambda (messages &rest args)
                            (declare (ignore args))
                            (is (equal test-message messages))
                            mock-response)))
      (is (equal mock-response (llm:complete :openrouter test-message))))
    
    ;; Test Gemini dispatch
    (with-dynamic-stubs ((llm:gemini-complete 
                          (lambda (messages &rest args)
                            (declare (ignore args))
                            (is (equal test-message messages))
                            mock-response)))
      (is (equal mock-response (llm:complete :gemini test-message))))
    
    ;; Test error on unknown type
    (signals error 
      (llm:complete :unknown test-message))))

;; Test OpenAI complete function with mocked API
(test ollama-complete-test
  "Test the ollama-complete function"
  (let ((messages "Ollama test")
        (model "llama3")
        (response-content "Ollama response content"))

    ;; Test non-streaming case (using :message format)
    (let ((mock-response-body (babel:string-to-octets (format nil "{\"model\":\"~A\",\"created_at\":\"2023-08-04T19:23:18.67927Z\",\"message\":{\"role\":\"assistant\",\"content\":~S},\"done\":true}" model response-content))))
      (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content &allow-other-keys)
						  (declare (ignore endpoint method content))
						  (values mock-response-body 200 nil nil nil nil nil))))
        (multiple-value-bind (content tool-calls updated-messages)
            (llm:ollama-complete messages :model model)
          (is (string= response-content content))
          (is (equal `((:role . "assistant") (:content . ,response-content))
                     (car (last updated-messages)))))))

    ;; Test streaming case
  (let ((messages "Ollama stream test")
        (model "llama3-stream")
        (mock-stream-content (format nil "{\"message\":{\"content\":\"Streaming\"},\"done\":false}~%{\"message\":{\"content\":\" response\"},\"done\":false}~%{\"done\":true,\"message\":{\"content\":\"\"}}~%")))
    (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content want-stream &allow-other-keys)
						 (declare (ignore endpoint method content want-stream))
						(values (flexi-streams:make-in-memory-input-stream
							 (babel:string-to-octets mock-stream-content :encoding :utf-8)) 200 nil nil nil nil))))
      (multiple-value-bind (reader processed-messages)
          (llm:ollama-complete messages :model model :stream t)
        (is (typep reader 'llm-stream-reader))
        (is (eq :ollama (llm-stream-reader-provider reader)))
        (is (string= "Streaming response" (read-all-chunks reader)))
        (is (llm-stream-reader-closed-p reader))
        (is (equal `(((:role . "system") (:content . ,llm:*default-system-prompt*))
                     ((:role . "user") (:content . ,messages)))
                   processed-messages)))))))

(test openai-complete-test
  "Test the openai-complete function"
  (let ((messages "Hello, world!")
        (model "gpt-3.5-turbo")
        (api-key "test-api-key")
        (response-content "This is a test response from OpenAI"))
    
    ;; Test non-streaming case
    (let ((mock-response (babel:string-to-octets 
                          (format nil "{\"choices\": [{\"message\": {\"content\": ~S}}]}" 
                                  response-content))))
      (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content content-type additional-headers &allow-other-keys)
						  (declare (ignore endpoint method content content-type additional-headers))
						  (values mock-response 200 nil nil nil nil nil))))
        (multiple-value-bind (content tool-calls updated-messages)
            (llm:openai-complete messages :model model :api-key api-key)
          (is (string= response-content content))
          (is (equal `((:role . "assistant") (:content . ,response-content))
                     (car (last updated-messages))))
	  )))

    ;; Test streaming case
    (let ((messages "Stream test")
          (model "gpt-4")
          (api-key "stream-key")
          (mock-stream-content (format nil "data: {\"choices\":[{\"delta\":{\"content\":\"Streaming response\"}}]}~%data: [DONE]~%")))
      (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content-type content additional-headers want-stream &allow-other-keys)
						  (declare (ignore endpoint method content content-type additional-headers want-stream))
						  (values (flexi-streams:make-in-memory-input-stream
							 (babel:string-to-octets mock-stream-content :encoding :utf-8)) 200 nil nil nil nil)
						  )))
	(multiple-value-bind (reader processed-messages)
            (llm:openai-complete messages :model model :api-key api-key :stream t)
          (is (typep reader 'llm-stream-reader))
          (is (eq :openai (llm-stream-reader-provider reader)))
          (is (string= "Streaming response" (read-all-chunks reader)))
          (is (llm-stream-reader-closed-p reader))
          (is (equal `(((:role . "system") (:content . ,llm:*default-system-prompt*))
                       ((:role . "user") (:content . ,messages)))
                     processed-messages)))))
    ))

(test copilot-complete-test
  "Test the copilot-complete function"
  (let ((messages "Hello from Copilot!")
        (model "gpt-4o")
        (api-token "copilot-token")
        (response-content "Copilot says hi"))

    ;; Non-streaming case
    (let ((mock-response (babel:string-to-octets
                          (format nil "{\"choices\": [{\"message\": {\"content\": ~S}}]}"
                                  response-content))))
      (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content content-type additional-headers &allow-other-keys)
                                                  (declare (ignore endpoint method content content-type additional-headers))
                                                  (values mock-response 200 nil nil nil nil nil))))
        (multiple-value-bind (content tool-calls updated-messages)
            (llm:copilot-complete messages :model model :api-token api-token)
          (declare (ignore tool-calls))
          (is (string= response-content content))
          (is (equal `((:role . "assistant") (:content . ,response-content))
                     (car (last updated-messages)))))))

    ;; Streaming case
    (let ((stream-messages "Copilot stream test")
          (stream-model "gpt-4o")
          (stream-api-token "copilot-stream-token")
          (mock-stream-content (format nil "data: {\"choices\":[{\"delta\":{\"content\":\"Streaming Copilot\"}}]}~%data: [DONE]~%")))
      (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content-type content additional-headers want-stream &allow-other-keys)
                                                  (declare (ignore endpoint method content-type content additional-headers want-stream))
                                                  (values (flexi-streams:make-in-memory-input-stream
                                                           (babel:string-to-octets mock-stream-content :encoding :utf-8)) 200 nil nil nil nil))))
        (multiple-value-bind (reader processed-messages)
            (llm:copilot-complete stream-messages :model stream-model :api-token stream-api-token :stream t)
          (is (typep reader 'llm-stream-reader))
          ;; Copilot uses OpenAI-compatible streaming so provider is :openai
          (is (eq :openai (llm-stream-reader-provider reader)))
          (is (string= "Streaming Copilot" (read-all-chunks reader)))
          (is (llm-stream-reader-closed-p reader))
          (is (equal `(((:role . "system") (:content . ,llm:*default-system-prompt*))
                       ((:role . "user") (:content . ,stream-messages)))
                     processed-messages)))))))

;; Test Anthropic complete function with mocked API
(test anthropic-complete-test
  "Test the anthropic-complete function"
  (let ((messages "Anthropic test")
        (model "claude-3-haiku-20240307")
        (api-key "anthropic-key")
        (system-prompt "Anthropic system prompt")
        (response-content "Anthropic response content"))

    ;; Test non-streaming case
    (let ((mock-response-body (babel:string-to-octets (format nil "{\"id\":\"msg_123\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":~S}],\"model\":\"~A\",\"stop_reason\":\"end_turn\",\"stop_sequence\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":20}}" response-content model))))
      (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content content-type additional-headers &allow-other-keys)
						  (declare (ignore endpoint method content content-type additional-headers))
						  (values mock-response-body 200 nil nil nil nil nil))))
        (multiple-value-bind (content tool-calls updated-messages)
            (llm:anthropic-complete messages :model model :api-key api-key :system-prompt system-prompt)
          (is (string= response-content content))
          (is (equal `((:role . "assistant") (:content . ,response-content))
                     (car (last updated-messages)))))))

  ;; Test streaming case
  (let ((messages "Anthropic stream test")
        (model "claude-3-sonnet-20240229")
        (api-key "anthropic-stream-key")
        (mock-stream-content (format nil "event: message_start~%data: {\"type\":\"message_start\"}~%~%event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Streaming\"}}~%~%event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\" response\"}}~%~%event: message_stop~%data: {\"type\":\"message_stop\"}~%")))
    (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content content-type additional-headers want-stream &allow-other-keys)
						(declare (ignore endpoint method content content-type additional-headers want-stream))
						(values (flexi-streams:make-in-memory-input-stream
							 (babel:string-to-octets mock-stream-content :encoding :utf-8)) 200 nil nil nil nil)
						)))
      (multiple-value-bind (reader processed-messages)
          (llm:anthropic-complete messages :model model :api-key api-key :stream t)
        (is (typep reader 'llm-stream-reader))
        (is (eq :anthropic (llm-stream-reader-provider reader)))
        (is (string= "Streaming response" (read-all-chunks reader)))
        (is (llm-stream-reader-closed-p reader))
        ;; Anthropic doesn't add system prompt to processed messages internally
        (is (equal `(((:role . "user") (:content . ,messages)))
                   processed-messages)))))))

;; Test OpenRouter complete function with mocked API
(test openrouter-complete-test
  "Test the openrouter-complete function"
  (let ((messages "OpenRouter test")
        (model "mistralai/mistral-7b-instruct:free")
        (api-key "openrouter-key")
        (response-content "OpenRouter response content"))

    ;; Test non-streaming case
    (let ((mock-response-body (babel:string-to-octets (format nil "{\"id\":\"gen-123\",\"model\":~S,\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":~S},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":20,\"total_tokens\":30}}" model response-content))))
      (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content content-type additional-headers &allow-other-keys)
						  (declare (ignore endpoint method content content-type additional-headers))
						  (values mock-response-body 200 nil nil nil nil nil))))
        (multiple-value-bind (content tool-calls updated-messages)
            (llm:openrouter-complete messages :model model :api-key api-key)
          (is (string= response-content content))
          (is (equal `((:role . "assistant") (:content . ,response-content))
                     (car (last updated-messages)))))))

  ;; Test streaming case
  (let ((messages "OpenRouter stream test")
        (model "google/gemini-flash-1.5")
        (api-key "openrouter-stream-key")
        (mock-stream-content (format nil "data: {\"id\":\"comp-123\",\"model\":\"google/gemini-flash-1.5\",\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"Streaming\"},\"finish_reason\":null}]}~%data: {\"id\":\"comp-123\",\"model\":\"google/gemini-flash-1.5\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\" response\"},\"finish_reason\":null}]}~%data: [DONE]~%")))
    (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content content-type additional-headers want-stream &allow-other-keys)
						(declare (ignore endpoint method content content-type additional-headers want-stream))
						(values (flexi-streams:make-in-memory-input-stream
							 (babel:string-to-octets mock-stream-content :encoding :utf-8)) 200 nil nil nil nil)
						)))
      (multiple-value-bind (reader processed-messages)
          (llm:openrouter-complete messages :model model :api-key api-key :stream t)
        (is (typep reader 'llm-stream-reader))
        ;; OpenRouter uses OpenAI format for streaming
        (is (eq :openrouter (llm-stream-reader-provider reader)))
        (is (string= "Streaming response" (read-all-chunks reader)))
        (is (llm-stream-reader-closed-p reader))
        (is (equal `(((:role . "system") (:content . ,llm:*default-system-prompt*))
                     ((:role . "user") (:content . ,messages)))
                   processed-messages)))))))

;; Test Gemini complete function with mocked API
(test gemini-complete-test
  "Test the gemini-complete function"
  (let ((messages "Gemini test")
        (model "gemini-1.5-flash")
        (api-key "gemini-key")
        (system-prompt "Gemini system prompt")
        (response-content "Gemini response content"))

    ;; Test non-streaming case
    (let ((mock-response-body (babel:string-to-octets (format nil "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": ~S}],\"role\":\"model\"},\"finishReason\":\"STOP\",\"index\":0,\"safetyRatings\":[]}],\"usageMetadata\":{\"promptTokenCount\":10,\"candidatesTokenCount\":20,\"totalTokenCount\":30}}" response-content))))
      (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content content-type &allow-other-keys)
						  (declare (ignore endpoint method content content-type))
						  (values mock-response-body 200 nil nil nil nil nil))))
        (multiple-value-bind (content tool-calls updated-messages)
            (llm:gemini-complete messages :model model :api-key api-key :system-prompt system-prompt)
          (is (string= response-content content))
          (is (equal `((:role . "assistant") (:content . ,response-content))
                     (car (last updated-messages)))))))

  ;; Test streaming case
  (let ((messages "Gemini stream test")
        (model "gemini-pro-stream")
        (api-key "gemini-stream-key")
        (mock-stream-content (format nil "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Streaming\"}]}}]}~%data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\" response\"}]}}]}~%")))
    (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content content-type want-stream &allow-other-keys)
						(declare (ignore endpoint method content content-type want-stream))
						(values (flexi-streams:make-in-memory-input-stream
							 (babel:string-to-octets mock-stream-content :encoding :utf-8)) 200 nil nil nil nil)
						)))
      (multiple-value-bind (reader processed-messages)
          (llm:gemini-complete messages :model model :api-key api-key :stream t)
        (is (typep reader 'llm-stream-reader))
        (is (eq :gemini (llm-stream-reader-provider reader)))
        (is (string= "Streaming response" (read-all-chunks reader)))
        (is (llm-stream-reader-closed-p reader))
        ;; Gemini doesn't add system prompt to processed messages internally
        (is (equal `(((:role . "user") (:content . ,messages)))
                   processed-messages)))))))

;; Test Ollama Embeddings
(test ollama-embed-test
  "Test the ollama-embed function"
  (let ((text "Embed this text")
        (model "nomic-embed-text")
        (expected-embedding '(0.1 0.2 0.3)))
    (let ((mock-response-body (babel:string-to-octets (format nil "{\"embedding\": ~A}" (cl-json:encode-json-to-string expected-embedding)))))
      (with-dynamic-stubs ((drakma:http-request (lambda (endpoint &key method content content-type &allow-other-keys)
						  (declare (ignore endpoint method content content-type))
                                                                                                      (values mock-response-body 200 nil nil nil nil nil))))
        (let ((embedding (llm:ollama-embed text :model model)))
          (is (equal expected-embedding embedding)))))))

;;; --- Tool Calling Tests ---

(test openai-complete-with-tools
  "Test openai-complete with tools parameter."
  (let ((messages "What's the weather?")
        (model "gpt-4")
        (api-key "test-key")
        (tools #(((:type . "function")
                  (:function . ((:name . "get_weather")
                               (:description . "Get weather")
                               (:parameters . ((:type . "object")
                                              (:properties . ((:location . ((:type . "string")))))
                                              (:required . #("location"))))))))))
    
    ;; Test that tools are included in the payload
    (let ((captured-payload nil))
      (with-dynamic-stubs ((drakma:http-request 
                            (lambda (endpoint &key content &allow-other-keys)
                              (declare (ignore endpoint))
                              (setf captured-payload content)
                              (values (babel:string-to-octets 
                                       "{\"choices\": [{\"message\": {\"content\": \"test\"}}]}")
                                      200 nil nil nil nil nil))))
        (llm:openai-complete messages :model model :api-key api-key :tools tools)
        (is-true captured-payload)
        (is (search "get_weather" captured-payload))
        (is (search "tool_choice" captured-payload))))))

(test openai-complete-extracts-tool-calls
  "Test that openai-complete extracts tool calls from response."
  (let ((messages "What's the weather in NYC?")
        (model "gpt-4")
        (api-key "test-key")
        (mock-response (babel:string-to-octets 
                        "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": null, \"tool_calls\": [{\"id\": \"call_123\", \"type\": \"function\", \"function\": {\"name\": \"get_weather\", \"arguments\": \"{\\\"location\\\": \\\"NYC\\\"}\"}}]}}]}")))
    
    (with-dynamic-stubs ((drakma:http-request 
                          (lambda (endpoint &key &allow-other-keys)
                            (declare (ignore endpoint))
                            (values mock-response 200 nil nil nil nil nil))))
      (multiple-value-bind (content tool-calls updated-messages)
          (llm:openai-complete messages :model model :api-key api-key)
        (declare (ignore content updated-messages))
        (is-true tool-calls)
        (is (= 1 (length tool-calls)))
        (let* ((tc (first tool-calls))
               (func (cdr (assoc :function tc))))
          (is (string= "get_weather" (cdr (assoc :name func)))))))))

(test anthropic-complete-with-tools
  "Test anthropic-complete with tools parameter."
  (let ((messages "What's the weather?")
        (model "claude-3-haiku-20240307")
        (api-key "test-key")
        (tools #(((:type . "function")
                  (:function . ((:name . "get_weather")
                               (:description . "Get weather")
                               (:parameters . ((:type . "object")
                                              (:properties . ((:location . ((:type . "string")))))))))))))
    
    (let ((captured-payload nil))
      (with-dynamic-stubs ((drakma:http-request 
                            (lambda (endpoint &key content &allow-other-keys)
                              (declare (ignore endpoint))
                              (setf captured-payload content)
                              (values (babel:string-to-octets 
                                       "{\"content\": [{\"type\": \"text\", \"text\": \"test\"}]}")
                                      200 nil nil nil nil nil))))
        (llm:anthropic-complete messages :model model :api-key api-key :tools tools)
        (is-true captured-payload)
        (is (search "get_weather" captured-payload))))))

;; Function to run all tests
(defun run-tests ()
  (run! 'llm-tests))
