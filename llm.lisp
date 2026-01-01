;; The LLM lib
;; Supports anthropic, openai, gemini, ollama, github copilot, and openrouter
(defpackage #:llm
  (:use #:cl :str :cl-json :chunga :cl-base64)
  (:export #:complete
           #:anthropic-complete
           #:gemini-complete
           #:ollama-complete
           #:openai-complete
           #:openrouter-complete
           #:copilot-complete
           #:*read-timeout*
           #:*debug-stream*
           #:make-stream-reader
           #:read-next-chunk
           #:llm-stream-reader
           #:llm-stream-reader-closed-p
           #:close-reader
           #:dump-api-keys
	   #:anthropic-completer
           #:gemini-completer
           #:ollama-completer
           #:openai-completer
           #:openrouter-completer
           #:copilot-completer
           #:llm-stream-reader-provider
           #:ollama-embed
           #:*default-system-prompt*
	   #:*debug*
           #:*openai-api-key*
           #:*anthropic-api-key*
           #:*gemini-api-key*
           #:*openrouter-api-key*
           #:*github-copilot-token*
	   #:get-copilot-token
	   #:list-copilot-models
	   #:copilot-complete-text))

(in-package :llm)

;;* state
;; Set this to an output stream to see debug logs.
(defvar *debug-stream* nil)
(defvar *debug* nil
  "Enable debug output.")

;; 120 second read timeout by default
(defvar *read-timeout* 120)

;; Default system prompt
(defvar *default-system-prompt* "You are a helpful assistant.")

;; API Keys - Loaded from environment variables
(defvar *openai-api-key* (uiop:getenv "OPENAI_API_KEY")
                         "API key for OpenAI services.")
(defvar *anthropic-api-key* (uiop:getenv "ANTHROPIC_API_KEY")
                            "API key for Anthropic services.")
(defvar *gemini-api-key* (uiop:getenv "GEMINI_API_KEY")
                         "API key for Google Gemini services.")
(defvar *openrouter-api-key* (uiop:getenv "OPENROUTER_API_KEY")
                             "API key for OpenRouter services.")
(defvar *ollama-endpoint* (or (uiop:getenv "OLLAMA_ENDPOINT")
			     "http://localhost:11434/api/chat")
  "Ollama Endpoint uri")

(defun read-copilot-token-from-file ()
  "Read GitHub access token (Personal Access Token) from hosts.json config file."
  (let* ((path (merge-pathnames ".config/github-copilot/hosts.json" (user-homedir-pathname))))
    (when (probe-file path)
      (handler-case
          (let* ((json-str (uiop:read-file-string path))
                 (config (cl-json:decode-json-from-string json-str))
                 (github-com (or (cdr (assoc :github.com config))
                                 (cdr (assoc :|github.com| config))
                                 (cdr (assoc :*github.com config))
                                 (cdr (assoc :github config)))))
            (when github-com
              (or (cdr (assoc :oauth_token github-com))
                  (cdr (assoc :oauth-token github-com))
                  (cdr (assoc :token github-com)))))
        (error (e)
          (debug-log (format nil "~&Error reading Copilot hosts.json: ~A~%" e))
          nil)))))

(defun get-copilot-token ()
  "Return a short-lived GitHub Copilot bearer token, refreshing it if needed.
Requires the environment variable GITHUB_COPILOT_ACCESS_TOKEN to be set to a GitHub access token.
The Copilot token is refreshed every ~25 minutes."
  (let* ((now (get-universal-time))
         (needs-refresh (or (null *github-copilot-token*)
                            (null *github-copilot-token-last-refresh*)
                            (>= (- now *github-copilot-token-last-refresh*)
                                (* 25 60)))))
    (if (not needs-refresh)
        *github-copilot-token*
        (let ((access-token (or (uiop:getenv "GITHUB_COPILOT_ACCESS_TOKEN")
                                (read-copilot-token-from-file)
				(uiop:getenv "GITHUB_COPILOT_TOKEN"))))
          (unless access-token
            (error "No GitHub Copilot access token found. Please set GITHUB_COPILOT_ACCESS_TOKEN."))
          (let ((new-token (fetch-copilot-token-from-access-token access-token)))
            (unless new-token
              (error "Failed to exchange access token for a GitHub Copilot token."))
            (setf *github-copilot-token* new-token
                  *github-copilot-token-last-refresh* now)
            *github-copilot-token*)))))

(defvar *github-copilot-token* nil
  "GitHub Copilot bearer token (derived from GITHUB_COPILOT_ACCESS_TOKEN).")

(defvar *github-copilot-token-last-refresh* nil
  "Timestamp (seconds since 1900 via get-universal-time) when Copilot token was last refreshed.")

(defun fetch-copilot-token-from-access-token (access-token)
  "Exchange a GitHub access token for a short-lived Copilot token."
  (let* ((endpoint "https://api.github.com/copilot_internal/v2/token")
         (headers `(("authorization" . ,(format nil "token ~A" access-token)))))
    (multiple-value-bind (body status)
        (drakma:http-request endpoint
                             :method :get
                             :additional-headers headers
                             :external-format-in :utf-8
                             :external-format-out :utf-8)
      (if (= status 200)
          (let* ((response (cl-json:decode-json-from-string
                            (if (stringp body) body (convert-byte-array-to-utf8 body))))
                 (token (cdr (assoc :token response))))
            token)
          (progn
            (debug-log (format nil "~&Error getting Copilot token: HTTP ~A~%" status))
            nil)))))

(defun copilot-api-request (endpoint &key (method :get) payload)
  "Make a request to the GitHub Copilot API."
  (let* ((token (get-copilot-token))
         (base-url "https://api.githubcopilot.com")
         (full-url (format nil "~A~A" base-url endpoint))
         (headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                    ("Copilot-Integration-Id" . "vscode-chat")
		    ("Content-Type" . "application/json"))))
    (unless token
      (error "No GitHub Copilot token found. Run 'hactar copilotapi authorize' first."))
    (multiple-value-bind (body status)
        (drakma:http-request full-url
                            :method method
                            :content payload
                            :additional-headers headers
                            :external-format-in :utf-8
                            :external-format-out :utf-8)
      (if (>= status 400)
          (progn
            (format t "~&API Error: HTTP ~A~%" status)
            (format t "~&Response: ~A~%" (if (stringp body) body (babel:octets-to-string body :encoding :utf-8)))
            nil)
          (cl-json:decode-json-from-string
           (if (stringp body) body (babel:octets-to-string body :encoding :utf-8)))))))

(defun list-copilot-models ()
  "List available GitHub Copilot models."
  (let ((response (copilot-api-request "/models")))
    (when response
      (let ((models (cdr (assoc :data response))))
        (if (vectorp models)
            (loop for model across models
                  collect (cdr (assoc :id model)))
            (loop for model in models
                  collect (cdr (assoc :id model))))))))

(defun copilot-complete-text (prompt &key (model "gpt-4o") (max-tokens 1000) (temperature 0.3))
  "Generate a completion using GitHub Copilot API.
Returns NIL if no choices or content are present."
  (let* ((messages `(((:role . "user") (:content . ,prompt))))
         (payload-alist `((:messages . ,(make-array (length messages) :initial-contents messages))
                          (:model . ,model)
                          ("max_tokens" . ,max-tokens)
                          (:temperature . ,temperature)
                          (:stream . :false)))
         (payload (payload-to-json payload-alist))
         (response (copilot-api-request "/chat/completions" :method :post :payload payload)))
    (when response
      (let* ((choices (cdr (assoc :choices response)))
             (first-choice (cond
                             ((and (vectorp choices) (> (length choices) 0))
                              (aref choices 0))
                             ((and (listp choices) (first choices))
                              (first choices))
                             (t nil)))
             (message (when first-choice (cdr (assoc :message first-choice))))
             (content (when message (cdr (assoc :content message)))))
        content))))

;;* debugging
(defun dump-api-keys ()
  "Dump the API keys to the current output stream."
  (with-output-to-string (str)
			 (format str "OpenAI API key: ~A~%" *openai-api-key*)
			 (format str "Anthropic API key: ~A~%" *anthropic-api-key*)
			 (format str "Gemini API key: ~A~%" *gemini-api-key*)
			 (format str "OpenRouter API key: ~A~%" *openrouter-api-key*)
			 (format str "GitHub Copilot token: ~A~%" *github-copilot-token*)))

(defun debug-log (&rest args)
  "Log messages to the debug stream. Accepts multiple arguments like format."
  (when *debug*
    (let ((message (if (and (= (length args) 1) (stringp (first args)))
                       (first args)
                       (apply #'format nil "~{~A~^ ~}" args))))
      (format t "~A~%" message)
      (when *debug-stream*
        (format *debug-stream* "~A~%" message)))))

;;* stream reading
(defstruct llm-stream-reader
  (http-stream nil :read-only t)     ; The raw Chunga stream
  (provider nil :read-only t)	     ; Keyword like :openai, :ollama
  (closed-p nil)
  (tool-call-buffer (make-hash-table :test 'equal) :read-only t) ; Buffer for assembling tool calls
  (in-think-block-p nil))       ; For Ollama: true if inside <think>...</think>

(defun make-stream-reader (http-stream provider)
  "Creates a new llm-stream-reader instance."
  (make-llm-stream-reader :http-stream (flexi-streams:make-flexi-stream http-stream :external-format :utf-8)
                          :provider provider
                          :tool-call-buffer (make-hash-table :test 'equal)
                          :in-think-block-p nil))

(defun close-reader (reader)
  "Closes the underlying HTTP stream and marks the reader as closed."
  (unless (llm-stream-reader-closed-p reader)
    (when (llm-stream-reader-http-stream reader)
      (ignore-errors (close (llm-stream-reader-http-stream reader))))
    (setf (llm-stream-reader-closed-p reader) t)))

(defun read-next-chunk (reader)
  "Reads the next text chunk from the LLM stream reader.
   Returns the text chunk as a string, or NIL if the stream is exhausted.
   Handles provider-specific parsing and stream closure."
  (when (or (null reader) (llm-stream-reader-closed-p reader))
    (return-from read-next-chunk nil))
  (let* ((stream (llm-stream-reader-http-stream reader))
         (provider (llm-stream-reader-provider reader)))

    (handler-case
        (loop
          (let ((line (read-line stream nil))) ; Returns nil on EOF
            (when (or (eq line 'eof) (null line) (string= line "data: [DONE]"))
              (close-reader reader)
              (return nil))
            ;; Provider-specific parsing
            (let ((chunk (case provider
                           (:openai (parse-openai-chunk line))
                           (:ollama (parse-ollama-chunk line reader))
                           (:anthropic (parse-anthropic-chunk line))
                           (:openrouter (parse-openai-chunk line))
                           (:gemini (parse-gemini-chunk line))
                           (otherwise (error "Unsupported provider for streaming: ~A" provider)))))
              (cond
                ((eq chunk :done)
                 (close-reader reader)
                 (return nil))
                ((eq chunk :skip)
                 )
                ((stringp chunk)
                 (when (> (length chunk) 0)
                   (return chunk)))
                (t )))))
      (end-of-file ()
        (close-reader reader)
       nil)
      (error (e)
        (format t "~&Error reading/parsing stream chunk (~A): ~A~%" provider e)
        (close-reader reader)
       nil))))

(defun parse-openai-chunk (line)
  (when (str:starts-with? "data: " line)
    (let ((data-str (subseq line 6)))
      (when (string= data-str "[DONE]")
        (return-from parse-openai-chunk :done))
      (handler-case
          (let* ((json (cl-json:decode-json-from-string data-str))
		 (choices (cdr (assoc :choices json)))
                 (first-choice (when choices (car choices)))
                 (delta (when first-choice (cdr (assoc :delta first-choice))))
                 (content (when delta (cdr (assoc :content delta)))))
            (if content content :skip))
        (error (e)
	       (debug-log (format nil "~&OpenAI JSON parse error: ~A on ~S~%" e data-str))
          nil)))))

(defun parse-ollama-chunk (line reader)
  (handler-case
      (let* ((json (cl-json:decode-json-from-string line))
             (done (cdr (assoc :done json)))
             (message (cdr (assoc :message json)))
             (content (cdr (assoc :content message))))
        (cond
          (done :done)
          (content
           (let ((trimmed-content (string-trim '(#\Space #\Tab #\Newline #\Return) content)))
             (if (llm-stream-reader-in-think-block-p reader)
                 (if (string= trimmed-content "</think>")
                     (progn
                       (setf (llm-stream-reader-in-think-block-p reader) nil)
                       :skip)
                     :skip) ; Still inside <think> block
                 (if (string= trimmed-content "<think>")
                     (progn
                       (setf (llm-stream-reader-in-think-block-p reader) t)
                       :skip)
                     content)))) ; Not in think block, and not starting one
          (t :skip)))
    (error (e)
	   (debug-log (format nil "~&Ollama JSON parse error: ~A on ~S~%" e line))
      nil)))

(defun parse-anthropic-chunk (line)
  ;; Anthropic uses Server-Sent Events (SSE)
  (cond
    ((str:starts-with? "event: message_stop" line)
     :done)
    ((str:starts-with? "event: content_block_delta" line)
     :skip)
    ((str:starts-with? "data: " line)
     (handler-case
         (let* ((json-str (subseq line 6))
                (json (cl-json:decode-json-from-string json-str))
                (type (cdr (assoc :type json))))
	   ;; We are primarily interested in content_block_delta data
           (if (equal type "content_block_delta")
               (let* ((delta (cdr (assoc :delta json)))
                      (text (when delta (cdr (assoc :text delta)))))
                 (if text text :skip))
               :skip))	      ; Ignore other data types like message_start, ping
       (error (e)
              (debug-log (format nil "~&Anthropic JSON parse error: ~A on ~S~%" e line))
         nil)))	  ; Return nil (which becomes :skip in read-next-chunk) on error
    (t :skip))) ; Ignore lines not starting with event: or data:

(defun parse-gemini-chunk (line)
  "Parses a line from the Gemini streaming API (SSE-like format).
   Expects lines like 'data: {...}'. Returns text chunk, :skip, or nil on error."
  ;; Gemini uses an SSE-like format. We only care about 'data:' lines.
  (if (str:starts-with? "data: " line)
    (handler-case
        (let* ((json-str (subseq line 6))
               (json (cl-json:decode-json-from-string json-str))
               ;; Gemini structure: candidates -> content -> parts -> text
               (candidates (cdr (assoc :candidates json)))
               (text-parts (when candidates
                             (loop for candidate in candidates
                                   append (let* ((content (cdr (assoc :content candidate)))
                                                 (parts (when content (cdr (assoc :parts content)))))
                                            (when parts
                                              (loop for part in parts
                                                    for text = (cdr (assoc :text part))
                                                    when text collect text))))))
               (joined-text (str:join  "" text-parts)))
          (if text-parts
            joined-text
            :skip))
      (error (e)
        (format t "~&Gemini JSON parse error: ~A " e)
       :skip))
    :skip))

;;* Utils
(defun convert-byte-array-to-utf8 (byte-array)
  "Converts a byte array to a UTF-8 string.
   Assumes Drakma might sometimes return raw octets even when UTF-8 is requested."
  (handler-case
      (babel:octets-to-string byte-array :encoding :utf-8 :errorp t)
    (error (e)
	   (debug-log (format nil "Failed to decode byte array as UTF-8: ~A. Trying latin-1 as fallback." e))
      ;; Fallback attempt, though ideally Drakma's :external-format-in handles this.
      (handler-case
          (babel:octets-to-string byte-array :encoding :iso-8859-1 :errorp nil) ; Allow errors on fallback
        (error (e2)
               (debug-log (format nil "Fallback decoding as latin-1 also failed: ~A" e2))
          nil)))))

(defun chunga-read-line (stream)
  "chunga:read-line* doesnt work, so use this."
  (read-line stream nil))
;;** messages
(defun prepare-messages (messages system-prompt &key (add-system-prompt t))
  "Prepare messages by ensuring proper format and optionally adding system prompt."
  (when (stringp messages)
    (setf messages `(((:role . "user") (:content . ,messages)))))
  ;; Handle if messages is already a single message map
  (unless (listp (car messages))
    (setf messages (list messages)))

  (when (and add-system-prompt system-prompt (not (string= system-prompt "")))
    ;; Ensure system prompt is first if added
    (unless (equal (cdr (assoc :role (car messages))) "system")
      (push `((:role . "system") (:content . ,system-prompt)) messages)))
  messages)

(defun extract-messages (provider response)
  "Extract message content from provider-specific NON-STREAMING response formats.
   PROVIDER is one of :openai, :anthropic, :ollama, :openrouter, or :gemini.
   RESPONSE is the parsed JSON response from the API.
   Returns two values: text content and tool calls."
  (case provider
    (:openai
     (let* ((choices (cdr (assoc :choices response)))
            (message (when choices (cdr (assoc :message (car choices)))))
            (text-content (when message (cdr (assoc :content message))))
            (tool-calls (when message (cdr (assoc :tool_calls message)))))
       (values text-content tool-calls)))
    (:anthropic
     ;; Anthropic V1 non-streaming response structure
     (let* ((content-blocks (cdr (assoc :content response)))
            (text-content
              (when content-blocks
                ;; Find the first text block
                (loop for block in content-blocks
                      when (equal (cdr (assoc :type block)) "text")
			return (cdr (assoc :text block)))))
            (tool-calls
              (when content-blocks
                ;; Find tool use blocks
                (loop for block in content-blocks
                      when (equal (cdr (assoc :type block)) "tool_use")
			collect block))))
       (values text-content tool-calls)))
    (:ollama
     (let* ((message (cdr (assoc :message response)))
            (text-content (when message (cdr (assoc :content message))))
            (tool-calls nil))
       (values text-content tool-calls)))
    (:openrouter
     (let* ((choices (cdr (assoc :choices response)))
            (message (when choices (cdr (assoc :message (car choices)))))
            (text-content (when message (cdr (assoc :content message))))
            (tool-calls (when message (cdr (assoc :tool_calls message)))))
       (values text-content tool-calls)))
    (:gemini
     ;; Simplified extraction for non-streaming - assumes first candidate/part
     (let* ((candidates (cdr (assoc :candidates response)))
            (content (when candidates (cdr (assoc :content (car candidates)))))
            (parts (when content (cdr (assoc :parts content))))
            (text-content (when parts (cdr (assoc :text (car parts)))))
            (function-calls
              (when parts
                (loop for part in parts
                      when (cdr (assoc :function_call part))
			collect (cdr (assoc :function_call part))))))
       (values text-content function-calls)))
    (otherwise
     (error "Unknown provider type for message extraction: ~A" provider))))

;;* completions
(defun complete (type messages &rest args &key (max-context 32000) images tools tool-choice &allow-other-keys)
  "Central completion function that dispatches to the appropriate completer.
   TYPE can be :openai, :ollama, :anthropic, :openrouter, :gemini, or :copilot.
   MESSAGES is the list of messages (alists).
   IMAGES is an optional list of image plists.
   TOOLS is an optional list/vector of tool definitions for the LLM.
   TOOL-CHOICE controls tool selection behavior (\"auto\", \"none\", or specific tool).
   If STREAM is T, returns an llm-stream-reader object.
   Otherwise, returns (values response-text tool-calls-list full-message-history).
   MAX-CONTEXT specifies the context window size."
  (let ((completer-fn (case type
                        (:openai #'openai-complete)
                        (:ollama #'ollama-complete)
                        (:anthropic #'anthropic-complete)
                        (:openrouter #'openrouter-complete)
                        (:gemini #'gemini-complete)
                        (:copilot #'copilot-complete)
                        (otherwise (error "Unknown completer type: ~A" type)))))
    ;; Ensure max-context, images, tools, and tool-choice are passed along with other args
    (apply completer-fn messages 
           :max-context max-context 
           :images images 
           :tools tools 
           :tool-choice tool-choice
           args)))

(defun payload-to-json (payload)
  (let* ((shasht:*write-alist-as-object* t)
         (shasht:*write-plist-as-object* t)
	  (shasht:*symbol-name-function* (lambda (sym)
                                          (string-downcase (symbol-name sym))))
         (result
           (shasht:write-json payload nil)))
    result))

(defun execute-llm-request (endpoint payload headers stream)
  (if stream
    ;; Streaming: Return the Chunga stream directly
    (multiple-value-bind (stream status-code headers uri stream-uri must-close reason-phrase)
                         (drakma:http-request endpoint
                                              :method :post
                                              :content-type "application/json"
                                              :content payload
                                              :additional-headers headers
                                              :connection-timeout *read-timeout*
                                              :external-format-out :utf-8
                                              :external-format-in :utf-8
                                              :want-stream t)
      (declare (ignore status-code headers uri stream-uri must-close reason-phrase))
      stream)
    ;; Non-streaming: Get response body, decode, parse
    (multiple-value-bind (response-body status-code headers uri stream-uri must-close reason-phrase)
                         (drakma:http-request endpoint
                                              :method :post
                                              :content-type "application/json"
                                              :content payload
                                              :additional-headers headers
                                              :connection-timeout *read-timeout*
                                              :external-format-out :utf-8
                                              :external-format-in :utf-8)
      (declare (ignore headers uri stream-uri must-close))
      (debug-log (format nil "~&API Response Status: ~A~%" status-code))
      (if (>= status-code 400)
        (let ((response-string (if (stringp response-body)
                                   response-body
                                 (convert-byte-array-to-utf8 response-body))))
          (debug-log (format nil "~&API Error: HTTP ~A - ~A~%" status-code reason-phrase))
          (debug-log (format nil "~&API Response (Raw): ~A~%" response-string))
          nil)
        (let ((response-string (if (stringp response-body)
                                 response-body
                                 (convert-byte-array-to-utf8 response-body))))
          (debug-log (format nil "~&API Response (Raw): ~A~%" response-string))
          (if (or (null response-string) (string= response-string ""))
            (progn
              (debug-log (format nil "~&API Error: Empty response received.~%"))
              nil)
            (handler-case (cl-json:decode-json-from-string response-string)
              (error (e)
                     (debug-log (format nil "~&API JSON Parse Error: ~A~%Response: ~A~%" e response-string))
               nil))))))))

(defun openai-complete (messages &key
                        (api-key *openai-api-key*)
                        (model "gpt-4o-mini")
                        (endpoint "https://api.openai.com/v1/chat/completions")
                        (system-prompt *default-system-prompt*)
                        (max-tokens 1024)
                        (max-context 32000)
                        (stream nil)
                        (response-format nil)
                        (images nil)
                        (tools nil)
                        (tool-choice "auto")
                        (extra-headers nil)
                        &allow-other-keys)
 (declare (ignore max-context))
  (let* ((processed-messages (prepare-messages messages system-prompt))
         ;; OpenAI/OpenRouter format: Combine text and images within the last user message content
         (final-messages (if images
                           (let* ((last-user-message (car (last processed-messages))) ; Assuming last message is user
                                  (user-content (cdr (assoc :content last-user-message)))
                                  (image-parts (loop for img in images
                                                     for base64-data = (cdr (assoc :base64-data img))
                                                     for mime-type = (cdr (assoc :mime-type img))
                                                     ;; OpenAI uses data URI format for base64 images
                                                     for data-uri = (format nil "data:~A;base64,~A" mime-type base64-data)
                                                     collect `((:type . "image_url")
                                                               (:image_url . ((:url . ,data-uri))))))
                                  (new-content (coerce (cons `((:type . "text") (:text . ,user-content)) image-parts) 'vector)))
                             ;; Replace the last message's content with the combined list
                             (append (butlast processed-messages)
                                     (list `((:role . "user") (:content . ,new-content)))))
                           processed-messages))
         (messages-array (make-array (length final-messages) :initial-contents final-messages))
         ;; Convert tools to array if it's a list
         (tools-array (when tools 
                        (if (vectorp tools) tools (coerce tools 'vector))))
         (payload-alist `((:model . ,model)
                          (:messages . ,messages-array)
                          ("max_tokens" . ,max-tokens)
                          (:stream . ,(if stream t :false))
                          ,@(when response-format `(("response_format" . (("type" . ,response-format)))))
                          ,@(when tools-array `((:tools . ,tools-array) 
                                                (:tool_choice . ,tool-choice)))))
         (payload-string (payload-to-json payload-alist))
         (headers (append `(("Content-Type" . "application/json")
                            ("Authorization" . ,(concatenate 'string "Bearer " api-key)))
                          extra-headers)))
    (debug-log (format nil "OpenAI Payload: ~A~%" payload-string))
    (if stream
      (let ((http-stream (execute-llm-request endpoint payload-string headers t)))
        (values (make-stream-reader http-stream :openai)
                processed-messages))
      (let ((response-json (execute-llm-request endpoint payload-string headers nil)))
        (if response-json
            (multiple-value-bind (content tool-calls) (extract-messages :openai response-json)
              (values content
                      tool-calls
                      (append processed-messages
                              (list `((:role . "assistant")
                                      (:content . ,(or content ""))
                                      ,@(when tool-calls `((:tool_calls . ,tool-calls))))))))
            (values nil nil processed-messages))))))

(defun ollama-complete (messages &key
				   (model "llama3")
				   (endpoint *ollama-endpoint*)
				   (system-prompt *default-system-prompt*)
				   (stream nil)
				   (response-format nil)
				   (max-tokens 1024) ; Ollama uses num_predict for max output tokens
				   (max-context 32000) ; Ollama uses num_ctx for context window size
				   (images nil)
				   (tools nil)
				   (extra-headers nil)
                        &allow-other-keys)
  (declare (ignore max-tokens))
  (let* ((processed-messages (prepare-messages messages system-prompt))
         ;; Ollama format: Add images array to the last user message
         (final-messages (if images
                             (let* ((last-user-message (car (last processed-messages)))
                                    (image-data (loop for img in images
                                                      collect (cdr (assoc :base64-data img)))))
			       ;; Add the images key to the last message plist
                               (append (butlast processed-messages)
                                       (list (append last-user-message `((:images . ,image-data))))))
                             processed-messages))
         (messages-array (make-array (length final-messages) :initial-contents final-messages))
         (payload-alist `(("model" . ,model)
                          ("messages" . ,messages-array)
                          ("stream" . ,(if stream t :false))
                          ;; Options map for Ollama specific parameters
                          ("options" . (("num_ctx" . ,max-context)
                                        ("num_predict" . ,max-tokens)))
                          ,@(when response-format `(("format" . ,response-format)))
                          ;; ,@(when tools `((:tools . ,tools)))
			  ))
         (payload-string (payload-to-json payload-alist))
         (headers (append `(("Content-Type" . "application/json")) extra-headers)))
    (debug-log (format nil "Ollama Payload:~A~%" payload-string))
    (if stream
	(let ((http-stream (execute-llm-request endpoint payload-string headers t)))
          (values (make-stream-reader http-stream :ollama)
                  processed-messages))
	(let ((response-json (execute-llm-request endpoint payload-string headers nil)))
          (if response-json
              (multiple-value-bind (content tool-calls) (extract-messages :ollama response-json)
		(let ((final-content (if content
					 (cl-ppcre:regex-replace-all "(?s)<think>.*?</think>" content "")
					 nil)))
                  (values final-content
                          tool-calls
                          (append processed-messages
                                  (list `((:role . "assistant")
                                          (:content . ,(or final-content ""))
                                          ,@(when tool-calls `((:tool_calls . ,tool-calls)))))))))
              (values nil nil processed-messages))))))

(defun anthropic-complete (messages &key
                           (api-key *anthropic-api-key*)
                           (model "claude-3-haiku-20240307")
                           (endpoint "https://api.anthropic.com/v1/messages")
                           (system-prompt *default-system-prompt*)
                           (max-tokens 1024)
                           (max-context 32000) ; ignored by Anthropic API
                           (stream nil)
                           (images nil)
                           (tools nil)
                           (extra-headers nil)
                           &allow-other-keys)
 (declare (ignore max-context))
  (let* ((processed-messages (prepare-messages messages system-prompt :add-system-prompt nil))
         ;; Anthropic format: Combine text and images within the last user message content
         (final-messages (if images
                           (let* ((last-user-message (car (last processed-messages)))
                                  (user-text (cdr (assoc :content last-user-message)))
                                  (image-parts (loop for img in images
                                                     for base64-data = (cdr (assoc :base64-data img))
                                                     for mime-type = (cdr (assoc :mime-type img))
                                                     collect `((:type . "image")
                                                               (:source . ((:type . "base64")
                                                                           (:media_type . ,mime-type)
                                                                           (:data . ,base64-data))))))
                                  ;; Content is a list containing image parts and the text part
                                  (new-content (append image-parts (list `((:type . "text") (:text . ,user-text))))))
                             ;; Replace the last message's content with the combined list
                             (append (butlast processed-messages)
                                     (list `((:role . "user") (:content . ,new-content)))))
                           processed-messages))
         (messages-array (make-array (length final-messages) :initial-contents final-messages))
         (payload-alist `(("model" . ,model)
                          ("messages" . ,messages-array)
                          ("max_tokens" . ,max-tokens)
                          ("stream" . ,(if stream t nil))
                          ,@(when (and system-prompt (> (length system-prompt) 0))
                              `(("system" . ,system-prompt)))
                          ,@(when tools `((:tools . ,tools) ("tool_choice" . (:type . "auto"))))))
         (payload-string (payload-to-json payload-alist))
         (headers (append `(("Content-Type" . "application/json")
                            ("x-api-key" . ,api-key)
                            ("anthropic-version" . "2023-06-01")
                            ("anthropic-beta" . "tools-2024-04-04"))
                          extra-headers)))
    (debug-log (format nil "Anthropic Payload: ~A~%" payload-string))
    (if stream
      (let ((http-stream (execute-llm-request endpoint payload-string headers t)))
        (values (make-stream-reader http-stream :anthropic)
                processed-messages))
      (let ((response-json (execute-llm-request endpoint payload-string headers nil)))
        (if response-json
            (multiple-value-bind (content tool-calls) (extract-messages :anthropic response-json)
              (values content
                      tool-calls
                      (append processed-messages
                              (list `((:role . "assistant")
                                      (:content . ,(or content ""))
                                      ,@(when tool-calls `((:tool_calls . ,tool-calls))))))))
            (values nil nil processed-messages))))))

(defun openrouter-complete (messages &key
                            (api-key *openrouter-api-key*)
                            (model "mistralai/mistral-7b-instruct:free")
                            (endpoint "https://openrouter.ai/api/v1/chat/completions")
                            (system-prompt *default-system-prompt*)
                            (max-tokens 1024)
                            (max-context 32000)
                            (stream nil)
                            (response-format nil)
                            (images nil)
                            (tools nil)
                            (extra-headers nil)
                            &allow-other-keys)
 (declare (ignore max-context))
  (let* ((processed-messages (prepare-messages messages system-prompt))
         ;; OpenRouter uses the OpenAI format for images
         (final-messages (if images
                           (let* ((last-user-message (car (last processed-messages))) ; Assuming last message is user
                                  (user-content (cdr (assoc :content last-user-message)))
                                  (image-parts (loop for img in images
                                                     for base64-data = (cdr (assoc :base64-data img))
                                                     for mime-type = (cdr (assoc :mime-type img))
                                                     ;; Use data URI format
                                                     for data-uri = (format nil "data:~A;base64,~A" mime-type base64-data)
                                                     collect `((:type . "image_url")
                                                               (:image_url . ((:url . ,data-uri))))))
                                  (new-content (cons `((:type . "text") (:content . ,user-content)) image-parts)))
                             ;; Replace the last message's content with the combined list
                             (append (butlast processed-messages)
                                     (list `((:role . "user") (:content . ,new-content)))))
                           processed-messages))
         (messages-array (make-array (length final-messages) :initial-contents final-messages))
         (payload-alist `(("model" . ,model)
                          ("messages" . ,messages-array)
                          ("max_tokens" . ,max-tokens)
                          ("stream" . ,(if stream t :false))
                          ,@(when response-format `(("response_format" . (("type" . ,response-format)))))
                          ,@(when tools `((:tools . ,tools) (:tool_choice . "auto")))))
         (payload-string (payload-to-json payload-alist))
         (headers (append `(("Content-Type" . "application/json")
                            ("Authorization" . ,(concatenate 'string "Bearer " api-key)))
                          extra-headers)))
    (debug-log (format nil "OpenRouter Payload: ~A~%" payload-string))
    (if stream
      (let ((http-stream (execute-llm-request endpoint payload-string headers t)))
        (values (make-stream-reader http-stream :openrouter)
                processed-messages))
      (let ((response-json (execute-llm-request endpoint payload-string headers nil)))
        (if response-json
            (multiple-value-bind (content tool-calls) (extract-messages :openrouter response-json)
              (values content
                      tool-calls
                      (append processed-messages
                              (list `((:role . "assistant")
                                      (:content . ,(or content ""))
                                      ,@(when tool-calls `((:tool_calls . ,tool-calls))))))))
            (values nil nil processed-messages))))))

(defun copilot-complete (messages &key
				  (api-token (get-copilot-token))
				  (model "gpt-4o")
				  (endpoint "https://api.githubcopilot.com/chat/completions")
				  (system-prompt *default-system-prompt*)
				  (max-tokens 1024)
				  (max-context 32000) ; ignored by Copilot API
				  (stream nil)
				  (response-format nil)
				  (images nil)
				  (tools nil)
				  (extra-headers nil)
				  &allow-other-keys)
 (declare (ignore max-context response-format tools))
  (let* ((processed-messages (prepare-messages messages system-prompt))
         ;; Copilot uses OpenAI format for images
         (final-messages (if images
                           (let* ((last-user-message (car (last processed-messages)))
                                  (user-content (cdr (assoc :content last-user-message)))
                                  (image-parts (loop for img in images
                                                     for base64-data = (cdr (assoc :base64-data img))
                                                     for mime-type = (cdr (assoc :mime-type img))
                                                     for data-uri = (format nil "data:~A;base64,~A" mime-type base64-data)
                                                     collect `((:type . "image_url")
                                                               (:image_url . ((:url . ,data-uri))))))
				  (new-content (coerce (cons `((:type . "text") (:text . ,user-content)) image-parts) 'vector))
                                  )
                             (append (butlast processed-messages)
                                     (list `((:role . "user") (:content . ,new-content)))))
                           processed-messages))
         (messages-array (make-array (length final-messages) :initial-contents final-messages))
         (payload-alist `((:model . ,model)
                          (:messages . ,messages-array)
                          ("max_tokens" . ,max-tokens)
                          (:stream . ,(if stream t :false))))
         (payload-string (payload-to-json payload-alist))
         (headers (append `(("Content-Type" . "application/json")
                            ("Authorization" . ,(concatenate 'string "Bearer " api-token))
                            ("Copilot-Integration-Id" . "vscode-chat")
                            ,@(when images `(("Copilot-Vision-Request" . "true"))))
                          extra-headers)))
    (unless api-token
      (error "No GitHub Copilot token available. Set GITHUB_COPILOT_ACCESS_TOKEN and try again."))
    (debug-log (format nil "Copilot Payload: ~A~%" payload-string))
    (if stream
      (let ((http-stream (execute-llm-request endpoint payload-string headers t)))
        (values (make-stream-reader http-stream :openai) ; Copilot uses OpenAI-compatible streaming
                processed-messages))
      (let ((response-json (execute-llm-request endpoint payload-string headers nil)))
        (if response-json
            (multiple-value-bind (content tool-calls) (extract-messages :openai response-json)
              (values content
                      tool-calls
                      (append processed-messages
                              (list `((:role . "assistant")
                                      (:content . ,(or content ""))
                                      ,@(when tool-calls `((:tool_calls . ,tool-calls))))))))
            (values nil nil processed-messages))))))

(defun gemini-complete (messages &key
                        (api-key *gemini-api-key*)
                        (model "gemini-1.5-flash")
                        (endpoint "https://generativelanguage.googleapis.com/v1beta/models")
                        (system-prompt *default-system-prompt*)
                        (max-tokens 2048)
                        (stream nil)
                        (max-context 32000) ; ignored by Gemini API
                        (images nil)
                        (tools nil)
                        (extra-headers nil)
                        &allow-other-keys)
  (declare (ignore extra-headers max-context))
  (let* ((processed-messages (prepare-messages messages system-prompt :add-system-prompt nil))
         ;; Gemini format: Images go into the 'parts' array of the last user message
         (contents-list (loop for msg in processed-messages
                              for last-p = (eq msg (car (last processed-messages))) ; Check if it's the last message
                              collect (let* ((role (cdr (assoc :role msg)))
                                             (content (cdr (assoc :content msg)))
                                             (parts (list `(("text" . ,content)))))
                                        ;; If it's the last message and there are images, add them
                                        (when (and last-p images)
                                          (setf parts (append
                                                        (loop for img in images
                                                              for base64-data = (cdr (assoc :base64-data img))
                                                              for mime-type = (cdr (assoc :mime-type img))
                                                              collect `(("inline_data" . (("mime_type" . ,mime-type)
                                                                                          ("data" . ,base64-data)))))
                                                        parts))) ; Add image parts before text part
                                        `(("role" . ,(if (string= role "assistant") "model" "user"))
                                          ("parts" . ,(coerce parts 'vector)))))) ; Convert parts list to vector/array
         ;; 2. Convert the list to an array
         (gemini-contents-array (make-array (length contents-list) :initial-contents contents-list))
         (api-action (if stream "streamGenerateContent" "generateContent"))
         (api-url (format nil "~A/~A:~A?key=~A~A"
                          endpoint model api-action api-key
                          (if stream "&alt=sse" "")))
         ;; 3. Use the array in the payload
         (payload-alist `(("contents" . ,gemini-contents-array)
                          ,@(when (and system-prompt (> (length system-prompt) 0))
                              `(("system_instruction" . (("parts" . #((("text" . ,system-prompt))))))))
                          ("generationConfig" .
                           (("maxOutputTokens" . ,max-tokens)))
                          ,@(when tools `((:tools . ,tools)))))
         (payload-string (payload-to-json payload-alist))
         (headers '(("Content-Type" . "application/json"))))
    (debug-log (format nil "Gemini Payload: ~A~%" payload-string))
    (if stream
      (let ((http-stream (execute-llm-request api-url payload-string headers t)))
        (values (make-stream-reader http-stream :gemini)
                processed-messages))
      (let ((response-json (execute-llm-request api-url payload-string headers nil)))
        (if response-json
            (multiple-value-bind (content tool-calls) (extract-messages :gemini response-json)
              (values content
                      tool-calls
                      (append processed-messages
                              (list `((:role . "assistant")
                                      (:content . ,(or content ""))
                                      ,@(when tool-calls `((:tool_calls . ,tool-calls))))))))
            (values nil nil processed-messages))))))

;;* embeddings
(defun ollama-embed (text &key
                     (model "nomic-embed-text")
                     (endpoint "http://localhost:11434/api/embeddings")
                     (extra-headers nil))
  "Generates an embedding for the given text using the Ollama API.
   Returns the embedding vector (list of numbers) on success, NIL on error."
  (let* ((payload-alist `(("model" . ,model)
                          ("prompt" . ,text)))
         (payload-string (cl-json:encode-json-alist-to-string payload-alist))
         (headers (append `(("Content-Type" . "application/json")) extra-headers)))
    (handler-case
        (multiple-value-bind (response-body status-code response-headers uri stream must-close reason)
            (drakma:http-request endpoint
                                 :method :post
                                 :content-type "application/json"
                                 :content payload-string
                                 :additional-headers headers
                                 :connection-timeout *read-timeout*)
          (declare (ignore response-headers uri stream must-close))

          (debug-log "~&Ollama Embeddings Response Status: ~A (~A)~%" status-code reason)

          (if (>= status-code 400)
              (progn
		(debug-log (format nil "~&Ollama Embeddings Error: HTTP ~A - ~A~%Response Body: ~A~%"
				   status-code reason (convert-byte-array-to-utf8 response-body)))
		nil)
              (let* ((response-string (convert-byte-array-to-utf8 response-body))
                     (response-json (cl-json:decode-json-from-string response-string)))
		(cdr (assoc :embedding response-json)))))
      (error (e)
             (debug-log (format nil "~&Error during Ollama embedding request: ~A~%" e))
	nil))))
