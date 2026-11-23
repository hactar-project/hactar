(in-package #:hactar-llm-live)

;;; Live HTTP/SSE provider implementation for hactar-llm.
;;; All HTTP requests route through *HTTP-REQUEST-FN* so tests can exercise
;;; request construction, response extraction, and SSE streaming offline.

(defvar *default-model* "anthropic/claude-sonnet-4-6")
(defvar *ollama-endpoint* (or (uiop:getenv "OLLAMA_ENDPOINT")
                              "http://localhost:11434/api/chat"))
(defvar *ollama-embed-endpoint* "http://localhost:11434/api/embeddings")
(defvar *read-timeout* 120)

(defvar *http-request-fn* nil
  "When non-nil, called as (url &key method headers content want-stream timeout)
returning (values body-or-stream status).")

(defun %default-http-request (url &key (method :get) headers content want-stream
                                       (timeout *read-timeout*))
  (handler-case
      (multiple-value-bind (body status)
          (dex:request url
                       :method method
                       :headers headers
                       :content content
                       :want-stream want-stream
                       :read-timeout timeout
                       :connect-timeout timeout)
        (values body status))
    (dex:http-request-failed (e)
      (values (dex:response-body e) (dex:response-status e)))))

(defun %http-request (url &key (method :get) headers content want-stream)
  (let ((fn (or *http-request-fn* #'%default-http-request)))
    (funcall fn url :method method :headers headers :content content
                    :want-stream want-stream :timeout *read-timeout*)))

(defun %ensure-string (body)
  (cond
    ((stringp body) body)
    ((typep body '(vector (unsigned-byte 8)))
     (babel:octets-to-string body :encoding :utf-8))
    ((streamp body)
     (with-output-to-string (out)
       (loop for line = (read-line body nil nil)
             while line do (write-line line out))))
    (t (princ-to-string body))))

(defun payload->json (payload)
  (let ((shasht:*write-alist-as-object* t)
        (shasht:*write-plist-as-object* nil)
        (shasht:*write-false-values* '(:false nil))
        (shasht:*symbol-name-function*
          (lambda (sym) (string-downcase (symbol-name sym)))))
    (shasht:write-json payload nil)))

(defun json->alist (text)
  (let ((shasht:*read-default-object-format* :alist)
        (shasht:*read-default-array-format* :list))
    (shasht:read-json text)))

(defun %field (key alist)
  (when (listp alist)
    (cdr (assoc (string key) alist :test #'string-equal :key #'string))))

;;; ---- Provider configuration table -----------------------------------------

(defvar *provider-configs*
  '((:openai      :family :openai :env "OPENAI_API_KEY"
                  :endpoint "https://api.openai.com/v1/chat/completions"
                  :default-model "gpt-4o-mini")
    (:openrouter  :family :openai :env "OPENROUTER_API_KEY"
                  :endpoint "https://openrouter.ai/api/v1/chat/completions"
                  :default-model "mistralai/mistral-7b-instruct:free")
    (:mistral     :family :openai :env "MISTRAL_API_KEY"
                  :endpoint "https://api.mistral.ai/v1/chat/completions"
                  :default-model "mistral-small-latest")
    (:deepseek    :family :openai :env "DEEPSEEK_API_KEY"
                  :endpoint "https://api.deepseek.com/chat/completions"
                  :default-model "deepseek-v4-pro")
    (:kimi        :family :openai :env "MOONSHOT_API_KEY"
                  :endpoint "https://api.moonshot.ai/v1/chat/completions"
                  :default-model "kimi-k2.6")
    (:opencode    :family :openai :env "OPENCODE_API_KEY"
                  :endpoint "https://opencode.ai/zen/go/v1/chat/completions"
                  :default-model "kimi-k2.6")
    (:nvidia-nim  :family :openai :env "NVIDIA_NIM_API_KEY"
                  :endpoint "https://integrate.api.nvidia.com/v1/chat/completions"
                  :default-model "meta/llama3-70b-instruct")
    (:copilot     :family :copilot :env "GITHUB_COPILOT_ACCESS_TOKEN"
                  :endpoint "https://api.githubcopilot.com/chat/completions"
                  :default-model "gpt-4o")
    (:anthropic   :family :anthropic :env "ANTHROPIC_API_KEY"
                  :endpoint "https://api.anthropic.com/v1/messages"
                  :default-model "claude-sonnet-4-6")
    (:ollama      :family :ollama
                  :endpoint nil
                  :default-model "llama3")
    (:gemini      :family :gemini :env "GEMINI_API_KEY"
                  :endpoint "https://generativelanguage.googleapis.com/v1beta/models"
                  :default-model "gemini-2.5-flash")
    (:bedrock     :family :bedrock :env "AWS_BEARER_TOKEN_BEDROCK"
                  :default-model "anthropic.claude-3-5-sonnet-20241022-v2:0")))

(defun %provider-config (provider)
  (let ((key (intern (string-upcase (string provider)) :keyword)))
    (cdr (assoc key *provider-configs*))))

;;; ---- GitHub Copilot token exchange ----------------------------------------

(defvar *github-copilot-config-path*
  (merge-pathnames ".config/github-copilot/hosts.json" (user-homedir-pathname)))
(defvar *copilot-token-cache* nil)
(defvar *copilot-token-fetched-at* 0)

(defun save-copilot-token (token &optional (path *github-copilot-config-path*))
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
    (write-string (payload->json `(("github.com" . (("oauth_token" . ,token)
                                                    ("user" . "copilot")))))
                  stream))
  path)

(defun fetch-copilot-token (access-token)
  (multiple-value-bind (body status)
      (%http-request "https://api.github.com/copilot_internal/v2/token"
                     :method :get
                     :headers `(("Authorization" . ,(format nil "token ~A" access-token))
                                ("Accept" . "application/json")))
    (when (and status (< status 400))
      (%field "token" (json->alist (%ensure-string body))))))

(defun get-copilot-token ()
  (when (and *copilot-token-cache*
             (< (- (get-universal-time) *copilot-token-fetched-at*) 1500))
    (return-from get-copilot-token *copilot-token-cache*))
  (let ((access (or (uiop:getenv "GITHUB_COPILOT_ACCESS_TOKEN")
                    (hactar-llm:oauth-access-token "github-copilot"))))
    (when access
      (let ((tok (fetch-copilot-token access)))
        (when tok
          (setf *copilot-token-cache* tok
                *copilot-token-fetched-at* (get-universal-time))
          tok)))))

;;; ---- Pure request construction --------------------------------------------

(defun %msg-alist (msg)
  `(("role" . ,(%field :role msg))
    ("content" . ,(%field :content msg))))

(defun %url-encode (s)
  (with-output-to-string (out)
    (loop for ch across (princ-to-string s)
          for code = (char-code ch)
          do (if (or (<= (char-code #\A) code (char-code #\Z))
                     (<= (char-code #\a) code (char-code #\z))
                     (<= (char-code #\0) code (char-code #\9))
                     (member ch '(#\- #\_ #\. #\~)))
                 (write-char ch out)
                 (format out "%~2,'0X" code)))))

(defun build-provider-request (provider messages &key model system-prompt
                                                    (max-tokens 2048)
                                                    (max-context 32000)
                                                    stream tools api-key
                                                    endpoint)
  "Build (values url headers json-payload family) for PROVIDER."
  (let* ((cfg (%provider-config provider))
         (family (or (getf cfg :family)
                     (error "Unknown LLM provider: ~A" provider)))
         (model-id (or model (getf cfg :default-model)))
         (key (or api-key
                  (and (getf cfg :env) (uiop:getenv (getf cfg :env)))))
         (sys (or system-prompt hactar-llm:*default-system-prompt*)))
    (ecase family
      ((:openai :copilot)
       (let* ((tok (if (eq family :copilot)
                       (or api-key (get-copilot-token))
                       key))
              (msgs (mapcar #'%msg-alist
                            (hactar-llm:prepare-messages messages sys)))
              (payload `(("model" . ,model-id)
                         ("messages" . ,(coerce msgs 'vector))
                         ("max_tokens" . ,max-tokens)
                         ("stream" . ,(if stream t :false))
                         ,@(when tools `(("tools" . ,(coerce tools 'vector))))))
              (headers `(("Content-Type" . "application/json")
                         ("Authorization" . ,(format nil "Bearer ~A" (or tok "")))
                         ,@(when (eq family :copilot)
                             '(("Copilot-Integration-Id" . "vscode-chat"))))))
         (values (or endpoint (getf cfg :endpoint))
                 headers (payload->json payload) :openai)))
      (:anthropic
       (let* ((msgs (mapcar #'%msg-alist
                            (hactar-llm:prepare-messages messages sys :add-system-prompt nil)))
              (payload `(("model" . ,model-id)
                         ("messages" . ,(coerce msgs 'vector))
                         ("max_tokens" . ,max-tokens)
                         ("stream" . ,(if stream t :false))
                         ,@(when (and sys (plusp (length sys)))
                             `(("system" . ,sys)))
                         ,@(when tools `(("tools" . ,(coerce tools 'vector))))))
              (headers `(("Content-Type" . "application/json")
                         ("x-api-key" . ,(or key ""))
                         ("anthropic-version" . "2023-06-01"))))
         (values (or endpoint (getf cfg :endpoint))
                 headers (payload->json payload) :anthropic)))
      (:ollama
       (let* ((msgs (mapcar #'%msg-alist
                            (hactar-llm:prepare-messages messages sys)))
              (payload `(("model" . ,model-id)
                         ("messages" . ,(coerce msgs 'vector))
                         ("stream" . ,(if stream t :false))
                         ("options" . (("num_ctx" . ,max-context)
                                       ("num_predict" . ,max-tokens))))))
         (values (or endpoint *ollama-endpoint*)
                 '(("Content-Type" . "application/json"))
                 (payload->json payload) :ollama)))
      (:gemini
       (let* ((msgs (hactar-llm:prepare-messages messages sys :add-system-prompt nil))
              (contents (mapcar (lambda (m)
                                  `(("role" . ,(if (string= (%field :role m) "assistant")
                                                   "model" "user"))
                                    ("parts" . #((("text" . ,(%field :content m)))))))
                                msgs))
              (base (or endpoint (getf cfg :endpoint)))
              (action (if stream "streamGenerateContent" "generateContent"))
              (url (format nil "~A/~A:~A?key=~A~A"
                           base model-id action (or key "")
                           (if stream "&alt=sse" "")))
              (payload `(("contents" . ,(coerce contents 'vector))
                         ,@(when (and sys (plusp (length sys)))
                             `(("system_instruction" . (("parts" . #((("text" . ,sys))))))))
                         ("generationConfig" . (("maxOutputTokens" . ,max-tokens))))))
         (values url '(("Content-Type" . "application/json"))
                 (payload->json payload) :gemini)))
      (:bedrock
       (let* ((region (or (uiop:getenv "AWS_REGION") "us-east-1"))
              (msgs (remove-if (lambda (m) (string= (%field :role m) "system"))
                               (hactar-llm:prepare-messages messages sys :add-system-prompt nil)))
              (converse (mapcar (lambda (m)
                                  `(("role" . ,(%field :role m))
                                    ("content" . #((("text" . ,(%field :content m)))))))
                                msgs))
              (url (or endpoint
                       (format nil "https://bedrock-runtime.~A.amazonaws.com/model/~A/converse"
                               region (%url-encode model-id))))
              (payload `(("messages" . ,(coerce converse 'vector))
                         ,@(when (and sys (plusp (length sys)))
                             `(("system" . #((("text" . ,sys))))))
                         ("inferenceConfig" . (("maxTokens" . ,max-tokens))))))
         (values url `(("Content-Type" . "application/json")
                       ("Authorization" . ,(format nil "Bearer ~A" (or key ""))))
                 (payload->json payload) :bedrock))))))

;;; ---- Pure response & SSE extraction ---------------------------------------

(defun extract-provider-response (family response)
  "Extract (values text-content tool-calls) from parsed JSON RESPONSE."
  (ecase family
    (:openai
     (let* ((choices (%field "choices" response))
            (msg (and choices (%field "message" (first choices)))))
       (values (and msg (%field "content" msg))
               (and msg (%field "tool_calls" msg)))))
    (:anthropic
     (let ((blocks (%field "content" response)))
       (values
        (loop for b in blocks
              when (equal (%field "type" b) "text")
                return (%field "text" b))
        (loop for b in blocks
              when (equal (%field "type" b) "tool_use")
                collect b))))
    (:ollama
     (let ((msg (%field "message" response)))
       (values (and msg (%field "content" msg)) nil)))
    (:gemini
     (let* ((cands (%field "candidates" response))
            (content (and cands (%field "content" (first cands))))
            (parts (and content (%field "parts" content))))
       (values (and parts (%field "text" (first parts)))
               (loop for p in parts
                     for fc = (%field "function_call" p)
                     when fc collect fc))))
    (:bedrock
     (let* ((output (%field "output" response))
            (msg (and output (%field "message" output)))
            (blocks (and msg (%field "content" msg))))
       (values
        (loop for b in blocks for txt = (%field "text" b) when txt return txt)
        (loop for b in blocks for tu = (%field "tool_use" b) when tu collect tu))))))

(defstruct llm-stream-reader
  http-stream provider closed-p in-think-block-p)

(defun make-stream-reader (http-stream provider)
  (let ((text-stream
          (if (subtypep (stream-element-type http-stream) '(unsigned-byte 8))
              (flexi-streams:make-flexi-stream http-stream :external-format :utf-8)
              http-stream)))
    (make-llm-stream-reader :http-stream text-stream :provider provider)))

(defun close-reader (reader)
  (unless (llm-stream-reader-closed-p reader)
    (when (llm-stream-reader-http-stream reader)
      (ignore-errors (close (llm-stream-reader-http-stream reader))))
    (setf (llm-stream-reader-closed-p reader) t)))

(defun parse-stream-line (family line &optional reader)
  "Parse one SSE or NDJSON line for FAMILY (:openai/:anthropic/:ollama/:gemini).
Returns a text chunk string, :DONE, or :SKIP."
  (cond
    ((or (null line) (string= line "") (string= line "data: [DONE]"))
     (if (string= line "data: [DONE]") :done :skip))
    ((eq family :ollama)
     (handler-case
         (let* ((json (json->alist line))
                (done (%field "done" json))
                (msg (%field "message" json))
                (content (and msg (%field "content" msg))))
           (cond
             (done :done)
             ((stringp content)
              (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) content)))
                (cond
                  ((and reader (llm-stream-reader-in-think-block-p reader))
                   (when (string= trimmed "</think>")
                     (setf (llm-stream-reader-in-think-block-p reader) nil))
                   :skip)
                  ((string= trimmed "<think>")
                   (when reader (setf (llm-stream-reader-in-think-block-p reader) t))
                   :skip)
                  (t content))))
             (t :skip)))
       (error () :skip)))
    ((eq family :anthropic)
     (cond
       ((uiop:string-prefix-p "event: message_stop" line) :done)
       ((uiop:string-prefix-p "data: " line)
        (handler-case
            (let ((json (json->alist (subseq line 6))))
              (if (equal (%field "type" json) "content_block_delta")
                  (let* ((delta (%field "delta" json))
                         (text (and delta (%field "text" delta))))
                    (or text :skip))
                  :skip))
          (error () :skip)))
       (t :skip)))
    ((eq family :gemini)
     (if (uiop:string-prefix-p "data: " line)
         (handler-case
             (let* ((json (json->alist (subseq line 6)))
                    (cands (%field "candidates" json))
                    (parts (loop for c in cands
                                 for content = (%field "content" c)
                                 append (%field "parts" content)))
                    (texts (loop for p in parts
                                 for txt = (%field "text" p)
                                 when txt collect txt)))
               (if texts (apply #'concatenate 'string texts) :skip))
           (error () :skip))
         :skip))
    (t ;; :openai and OpenAI-compatible
     (if (uiop:string-prefix-p "data: " line)
         (handler-case
             (let* ((json (json->alist (subseq line 6)))
                    (choices (%field "choices" json))
                    (delta (and choices (%field "delta" (first choices))))
                    (content (and delta (%field "content" delta))))
               (or content :skip))
           (error () :skip))
         :skip))))

(defun read-next-chunk (reader)
  (when (or (null reader) (llm-stream-reader-closed-p reader))
    (return-from read-next-chunk nil))
  (let* ((stream (llm-stream-reader-http-stream reader))
         (cfg (%provider-config (llm-stream-reader-provider reader)))
         (family (or (getf cfg :family) (llm-stream-reader-provider reader)))
         (eff-family (if (eq family :copilot) :openai family)))
    (loop
      (let ((line (read-line stream nil nil)))
        (unless line (close-reader reader) (return nil))
        (let ((chunk (parse-stream-line eff-family (string-right-trim '(#\Return) line) reader)))
          (cond
            ((eq chunk :done) (close-reader reader) (return nil))
            ((and (stringp chunk) (plusp (length chunk))) (return chunk))))))))

;;; ---- High-level live calls ------------------------------------------------

(defun %resolve-provider-and-model (model-spec)
  (let* ((spec (or model-spec *default-model*))
         (registered (hactar-llm:find-model spec)))
    (if registered
        (values (or (hactar-llm:model-provider registered) :openai)
                (or (hactar-llm:model-id registered) spec))
        (multiple-value-bind (prov id) (hactar-llm:parse-model-spec spec)
          (values (or prov :ollama) id)))))

(defun provider-complete (messages &key model system-prompt (max-tokens 2048)
                                        stream tools api-key endpoint)
  (multiple-value-bind (provider model-id) (%resolve-provider-and-model model)
    (multiple-value-bind (url headers payload family)
        (build-provider-request provider messages
                                :model model-id
                                :system-prompt system-prompt
                                :max-tokens max-tokens
                                :stream stream
                                :tools tools
                                :api-key api-key
                                :endpoint endpoint)
      (if stream
          (multiple-value-bind (http-stream status)
              (%http-request url :method :post :headers headers
                                 :content payload :want-stream t)
            (when (and status (>= status 400))
              (error "LLM stream error HTTP ~A" status))
            (make-stream-reader http-stream provider))
          (multiple-value-bind (body status)
              (%http-request url :method :post :headers headers :content payload)
            (let ((text (%ensure-string body)))
              (when (and status (>= status 400))
                (error "LLM HTTP ~A error: ~A" status text))
              (extract-provider-response family (json->alist text))))))))

(defun ollama-embed (input &key (model "nomic-embed-text")
                                (endpoint *ollama-embed-endpoint*))
  (multiple-value-bind (body status)
      (%http-request endpoint
                     :method :post
                     :headers '(("Content-Type" . "application/json"))
                     :content (payload->json `(("model" . ,model) ("prompt" . ,input))))
    (let ((text (%ensure-string body)))
      (when (and status (>= status 400))
        (error "Ollama embedding HTTP ~A error: ~A" status text))
      (coerce (%field "embedding" (json->alist text)) 'vector))))

(defun %oauth-http-adapter (url &key (method :get) content content-type headers)
  (let ((all-headers (if content-type
                         (cons (cons "Content-Type" content-type) headers)
                         headers)))
    (multiple-value-bind (body status)
        (%http-request url :method method :headers all-headers :content content)
      (let ((str (%ensure-string body)))
        (when (and status (>= status 400))
          (error "OAuth HTTP ~A error: ~A" status str))
        str))))

(defun make-live-client ()
  "Create a hactar-llm client value backed by live HTTP providers."
  (hactar-llm:make-client
   :complete (lambda (prompt &key model)
               (values (provider-complete prompt :model model)))
   :chat (lambda (model system user &optional tools)
           (values (provider-complete user :model model
                                           :system-prompt system
                                           :tools tools)))
   :stream (lambda (prompt callback &key model)
             (let ((reader (provider-complete prompt :model model :stream t))
                   (acc (make-string-output-stream)))
               (unwind-protect
                    (loop for chunk = (read-next-chunk reader)
                          while chunk
                          do (write-string chunk acc)
                             (funcall callback chunk))
                 (close-reader reader))
               (get-output-stream-string acc)))
   :diffuse (lambda (prompt &key model)
              (values (provider-complete
                       prompt
                       :model (or model *default-model*)
                       :system-prompt "Return ONLY valid updated text or Lisp forms.")))
   :embed (lambda (input &key model)
            (ollama-embed input :model (or model "nomic-embed-text")))))

(defun enable-live-client! ()
  "Install the live HTTP client, OAuth HTTP transport, and default providers."
  (setf hactar-llm:*llm-client* (make-live-client)
        hactar-llm:*oauth-http-fn* #'%oauth-http-adapter)
  (hactar-llm:register-default-oauth-providers)
  (dolist (entry *provider-configs*)
    (let ((name (car entry)))
      (hactar-llm:register-provider
       (hactar-llm:make-provider
        :name name
        :complete (lambda (prompt &key model)
                    (provider-complete prompt :model (or model (format nil "~A/" name))))
        :embed (when (eq name :ollama) #'ollama-embed)))))
  hactar-llm:*llm-client*)
