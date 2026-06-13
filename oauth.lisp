;; Subscription OAuth support
;; Device/PKCE OAuth flows for subscription-based LLM APIs:
;;   anthropic, openai-codex, github-copilot, google-antigravity
(in-package :hactar)

;;* Constants
(defparameter +anthropic-client-id+ (uiop:getenv "ANTHROPIC_CLIENT_ID"))
(defparameter +anthropic-authorize-url+ "https://claude.ai/oauth/authorize")
(defparameter +anthropic-token-url+ "https://console.anthropic.com/v1/oauth/token")
(defparameter +anthropic-redirect-uri+ "https://console.anthropic.com/oauth/code/callback")
(defparameter +anthropic-scopes+ "org:create_api_key user:profile user:inference")

(defparameter +codex-client-id+ (uiop:getenv "CODEX_CLIENT_ID"))
(defparameter +codex-authorize-url+ "https://auth.openai.com/oauth/authorize")
(defparameter +codex-token-url+ "https://auth.openai.com/oauth/token")
(defparameter +codex-redirect-uri+ "http://localhost:1455/auth/callback")
(defparameter +codex-scope+ "openid profile email offline_access")

(defparameter +copilot-client-id+ (or (uiop:getenv "GITHUB_COPILOT_CLIENT_ID") (uiop:getenv "COPILOT_CLIENT_ID")))
(defparameter +copilot-headers+
  '(("User-Agent" . "GitHubCopilotChat/0.35.0")
    ("Editor-Version" . "vscode/1.107.0")
    ("Editor-Plugin-Version" . "copilot-chat/0.35.0")
    ("Copilot-Integration-Id" . "vscode-chat")))

(defparameter +antigravity-client-id+ (uiop:getenv "ANTIGRAVITY_CLIENT_ID"))
(defparameter +antigravity-client-secret+ (uiop:getenv "ANTIGRAVITY_CLIENT_SECRET"))
(defparameter +antigravity-redirect-uri+ "http://localhost:51121/oauth-callback")
(defparameter +antigravity-auth-url+ "https://accounts.google.com/o/oauth2/v2/auth")
(defparameter +antigravity-token-url+ "https://oauth2.googleapis.com/token")
(defparameter +antigravity-default-project+ "default-project")
(defparameter +antigravity-scopes+
  (concatenate 'string
               "https://www.googleapis.com/auth/cloud-platform "
               "https://www.googleapis.com/auth/userinfo.email "
               "https://www.googleapis.com/auth/userinfo.profile "
               "https://www.googleapis.com/auth/cclog "
               "https://www.googleapis.com/auth/experimentsandconfigs"))

(defparameter +unix-epoch-offset+ 2208988800
  "Seconds between the Unix epoch (1970) and the Lisp universal-time epoch (1900).")

;;* Randomness / encoding helpers
(defvar *oauth-random-state* (make-random-state t))

(defun oauth-random-bytes (n)
  "Return N random bytes as an (unsigned-byte 8) array."
  (let ((a (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n a)
      (setf (aref a i) (random 256 *oauth-random-state*)))))

(defun oauth-random-hex (n)
  "Return a hex string of N random bytes."
  (with-output-to-string (s)
    (loop repeat n do (format s "~2,'0X" (random 256 *oauth-random-state*)))))

(defun b64-standard->url (s)
  "Convert standard base64 to base64url and strip padding."
  (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+ s))))

(defun base64url-encode-bytes (bytes)
  "Base64url-encode a byte array."
  (b64-standard->url (cl-base64:usb8-array-to-base64-string bytes)))

(defun oauth-b64url-decode-string (s)
  "Decode a base64url string into a UTF-8 string."
  (let* ((std (substitute #\/ #\_ (substitute #\+ #\- s)))
         (pad (mod (- 4 (mod (length std) 4)) 4))
         (padded (concatenate 'string std (make-string pad :initial-element #\=)))
         (bytes (cl-base64:base64-string-to-usb8-array padded)))
    (babel:octets-to-string bytes :encoding :utf-8)))

;;* PKCE
(defun oauth-sha256-base64url (str)
  "Return (values base64url-of-sha256 t) using openssl, or (values nil nil)."
  (handler-case
      (let* ((cmd (format nil "printf %s ~A | openssl dgst -sha256 -binary | openssl base64 -A" str))
             (out (uiop:run-program cmd :output :string :force-shell t)))
        (values (b64-standard->url (string-trim '(#\Newline #\Return #\Space) out)) t))
    (error () (values nil nil))))

(defun generate-pkce ()
  "Generate a PKCE (verifier challenge method). Falls back to the plain method."
  (let ((verifier (base64url-encode-bytes (oauth-random-bytes 32))))
    (multiple-value-bind (challenge ok) (oauth-sha256-base64url verifier)
      (if ok
          (values verifier challenge "S256")
          (values verifier verifier "plain")))))

;;* JSON field extraction (regex based to avoid key mangling)
(defun json-string-field (json key)
  "Extract a top-level JSON string field by KEY, or NIL."
  (multiple-value-bind (m r)
      (cl-ppcre:scan-to-strings (format nil "\"~A\"\\s*:\\s*\"([^\"]*)\"" key) json)
    (declare (ignore m))
    (when r (aref r 0))))

(defun json-number-field (json key)
  "Extract a top-level JSON integer field by KEY, or NIL."
  (multiple-value-bind (m r)
      (cl-ppcre:scan-to-strings (format nil "\"~A\"\\s*:\\s*(-?[0-9]+)" key) json)
    (declare (ignore m))
    (when r (parse-integer (aref r 0)))))

;;* HTTP helpers
(defun oauth-encode-params (alist)
  "URL-encode an alist of (key . value) into a query/form string."
  (format nil "~{~A~^&~}"
          (mapcar (lambda (kv)
                    (format nil "~A=~A"
                            (drakma:url-encode (car kv) :utf-8)
                            (drakma:url-encode (cdr kv) :utf-8)))
                  alist)))

(defun oauth-request (url &key (method :get) content content-type headers)
  "Perform an HTTP request and return the body string. Signals on >=400."
  (multiple-value-bind (body status)
      (drakma:http-request url
                           :method method
                           :content content
                           :content-type content-type
                           :additional-headers headers
                           :external-format-in :utf-8
                           :external-format-out :utf-8)
    (let ((str (if (stringp body) body (babel:octets-to-string body :encoding :utf-8))))
      (when (and status (>= status 400))
        (error "OAuth HTTP ~A error: ~A" status str))
      str)))

(defun oauth-http-post-json (url json &optional extra-headers)
  (oauth-request url :method :post :content json
                 :content-type "application/json" :headers extra-headers))

(defun oauth-http-post-form (url params)
  (oauth-request url :method :post :content (oauth-encode-params params)
                 :content-type "application/x-www-form-urlencoded"))

;;* Misc helpers
(defun oauth-parse-code-input (input)
  "Extract an authorization code from a pasted code, code#state, or redirect URL."
  (let ((value (string-trim '(#\Space #\Tab #\Return #\Newline) (or input ""))))
    (cond
      ((string= value "") nil)
      ((cl-ppcre:scan "code=" value)
       (multiple-value-bind (m r) (cl-ppcre:scan-to-strings "code=([^&#]+)" value)
         (declare (ignore m))
         (if r (aref r 0) value)))
      ((find #\# value) (first (uiop:split-string value :separator "#")))
      (t value))))

(defun oauth-jwt-account-id (token)
  "Extract chatgpt_account_id from a JWT access token, or NIL."
  (handler-case
      (let* ((parts (uiop:split-string token :separator "."))
             (payload (second parts)))
        (when payload
          (json-string-field (oauth-b64url-decode-string payload) "chatgpt_account_id")))
    (error () nil)))

(defun oauth-creds-from-token-response (resp)
  "Build a credentials plist from a token endpoint response string."
  (let ((access (json-string-field resp "access_token"))
        (refresh (json-string-field resp "refresh_token"))
        (expires-in (or (json-number-field resp "expires_in") 3600)))
    (unless access
      (error "Token response missing access_token: ~A" resp))
    (list :access access
          :refresh refresh
          :expires (+ (get-universal-time) (max 0 (- expires-in 300))))))

;;* Credential persistence
(defvar *oauth-credentials* (make-hash-table :test 'equal)
  "Provider-id -> credentials plist.")

(defun oauth-credentials-path ()
  (uiop:subpathname *hactar-config-path* "credentials.lisp"))

(defun save-oauth-credentials ()
  "Persist *oauth-credentials* to the credentials file."
  (let ((path (oauth-credentials-path)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (let ((alist '()))
        (maphash (lambda (k v) (push (cons k v) alist)) *oauth-credentials*)
        (prin1 alist s)
        (terpri s)))
    path))

(defun load-oauth-credentials ()
  "Load credentials from the credentials file into *oauth-credentials*."
  (let ((path (oauth-credentials-path)))
    (when (probe-file path)
      (with-open-file (s path :direction :input)
        (let ((alist (ignore-errors (read s nil nil))))
          (clrhash *oauth-credentials*)
          (dolist (pair alist)
            (setf (gethash (car pair) *oauth-credentials*) (cdr pair)))))))
  *oauth-credentials*)

(defun get-oauth-credentials (id) (gethash id *oauth-credentials*))
(defun set-oauth-credentials (id plist) (setf (gethash id *oauth-credentials*) plist))

(defun oauth-expired-p (creds)
  (let ((exp (getf creds :expires)))
    (or (null exp) (>= (get-universal-time) exp))))

;;* Provider-specific login flows
(defun oauth-login-anthropic (cb)
  "Anthropic (Claude Pro/Max) paste-code PKCE flow."
  (unless +anthropic-client-id+
    (error "Anthropic OAuth client ID is not configured. Set the ANTHROPIC_CLIENT_ID environment variable."))
  (multiple-value-bind (verifier challenge method) (generate-pkce)
    (let* ((params (oauth-encode-params
                    `(("code" . "true")
                      ("client_id" . ,+anthropic-client-id+)
                      ("response_type" . "code")
                      ("redirect_uri" . ,+anthropic-redirect-uri+)
                      ("scope" . ,+anthropic-scopes+)
                      ("code_challenge" . ,challenge)
                      ("code_challenge_method" . ,method)
                      ("state" . ,verifier))))
           (url (format nil "~A?~A" +anthropic-authorize-url+ params)))
      (funcall (getf cb :on-auth) url nil)
      (let* ((input (funcall (getf cb :on-prompt) "Paste the authorization code (code#state):"))
             (cleaned (string-trim '(#\Space #\Tab #\Return #\Newline) (or input "")))
             (splits (uiop:split-string cleaned :separator "#"))
             (code (first splits))
             (state (or (second splits) verifier))
             (resp (oauth-http-post-json
                    +anthropic-token-url+
                    (to-json `(("grant_type" . "authorization_code")
                               ("client_id" . ,+anthropic-client-id+)
                               ("code" . ,code)
                               ("state" . ,state)
                               ("redirect_uri" . ,+anthropic-redirect-uri+)
                               ("code_verifier" . ,verifier))))))
        (oauth-creds-from-token-response resp)))))

(defun oauth-refresh-anthropic (creds)
  (unless +anthropic-client-id+
    (error "Anthropic OAuth client ID is not configured. Set the ANTHROPIC_CLIENT_ID environment variable."))
  (oauth-creds-from-token-response
   (oauth-http-post-json
    +anthropic-token-url+
    (to-json `(("grant_type" . "refresh_token")
               ("client_id" . ,+anthropic-client-id+)
               ("refresh_token" . ,(getf creds :refresh)))))))

(defun oauth-login-codex (cb)
  "ChatGPT Plus/Pro (Codex) PKCE flow with redirect/code paste."
  (unless +codex-client-id+
    (error "OpenAI Codex OAuth client ID is not configured. Set the CODEX_CLIENT_ID environment variable."))
  (multiple-value-bind (verifier challenge method) (generate-pkce)
    (let* ((state (oauth-random-hex 16))
           (params (oauth-encode-params
                    `(("response_type" . "code")
                      ("client_id" . ,+codex-client-id+)
                      ("redirect_uri" . ,+codex-redirect-uri+)
                      ("scope" . ,+codex-scope+)
                      ("code_challenge" . ,challenge)
                      ("code_challenge_method" . ,method)
                      ("state" . ,state)
                      ("id_token_add_organizations" . "true")
                      ("codex_cli_simplified_flow" . "true")
                      ("originator" . "hactar"))))
           (url (format nil "~A?~A" +codex-authorize-url+ params)))
      (funcall (getf cb :on-auth) url "Sign in, then paste the redirect URL or code.")
      (let* ((input (funcall (getf cb :on-prompt) "Paste the authorization code or redirect URL:"))
             (code (oauth-parse-code-input input))
             (resp (oauth-http-post-form
                    +codex-token-url+
                    `(("grant_type" . "authorization_code")
                      ("client_id" . ,+codex-client-id+)
                      ("code" . ,code)
                      ("code_verifier" . ,verifier)
                      ("redirect_uri" . ,+codex-redirect-uri+))))
             (creds (oauth-creds-from-token-response resp)))
        (setf (getf creds :account-id) (oauth-jwt-account-id (getf creds :access)))
        creds))))

(defun oauth-refresh-codex (creds)
  (unless +codex-client-id+
    (error "OpenAI Codex OAuth client ID is not configured. Set the CODEX_CLIENT_ID environment variable."))
  (let ((r (oauth-creds-from-token-response
            (oauth-http-post-form
             +codex-token-url+
             `(("grant_type" . "refresh_token")
               ("refresh_token" . ,(getf creds :refresh))
               ("client_id" . ,+codex-client-id+))))))
    (setf (getf r :account-id) (oauth-jwt-account-id (getf r :access)))
    r))

(defun oauth-github-start-device-flow (domain)
  "Begin the GitHub device-code flow; return a plist."
  (unless +copilot-client-id+
    (error "GitHub Copilot OAuth client ID is not configured. Set the COPILOT_CLIENT_ID environment variable."))
  (let* ((url (format nil "https://~A/login/device/code" domain))
         (resp (oauth-http-post-json
                url
                (to-json `(("client_id" . ,+copilot-client-id+)
                           ("scope" . "read:user")))
                '(("Accept" . "application/json")
                  ("User-Agent" . "GitHubCopilotChat/0.35.0")))))
    (list :device-code (json-string-field resp "device_code")
          :user-code (json-string-field resp "user_code")
          :verification-uri (json-string-field resp "verification_uri")
          :interval (or (json-number-field resp "interval") 5)
          :expires-in (or (json-number-field resp "expires_in") 900))))

(defun oauth-github-poll (domain device-code interval expires-in)
  "Poll for the GitHub OAuth access token. Returns the token string."
  (unless +copilot-client-id+
    (error "GitHub Copilot OAuth client ID is not configured. Set the COPILOT_CLIENT_ID environment variable."))
  (let ((url (format nil "https://~A/login/oauth/access_token" domain))
        (deadline (+ (get-universal-time) expires-in))
        (wait (max 1 interval)))
    (loop
      (when (> (get-universal-time) deadline)
        (error "Device flow timed out"))
      (sleep wait)
      (let* ((resp (oauth-http-post-json
                    url
                    (to-json `(("client_id" . ,+copilot-client-id+)
                               ("device_code" . ,device-code)
                               ("grant_type" . "urn:ietf:params:oauth:grant-type:device_code")))
                    '(("Accept" . "application/json")
                      ("User-Agent" . "GitHubCopilotChat/0.35.0"))))
             (token (json-string-field resp "access_token"))
             (err (json-string-field resp "error")))
        (cond
          (token (return token))
          ((equal err "authorization_pending") nil)
          ((equal err "slow_down") (incf wait 5))
          (err (error "Device flow failed: ~A" err)))))))

(defun oauth-github-refresh-copilot-token (gh-token &optional domain)
  "Exchange a GitHub OAuth token for a short-lived Copilot token."
  (let* ((d (or domain "github.com"))
         (url (format nil "https://api.~A/copilot_internal/v2/token" d))
         (resp (oauth-request url :method :get
                              :headers (append `(("Authorization" . ,(format nil "Bearer ~A" gh-token))
                                                 ("Accept" . "application/json"))
                                               +copilot-headers+)))
         (token (json-string-field resp "token"))
         (expires-at (json-number-field resp "expires_at")))
    (unless token (error "Invalid Copilot token response: ~A" resp))
    (list :refresh gh-token
          :access token
          :expires (+ (- (or expires-at (get-universal-time)) 300) +unix-epoch-offset+)
          :enterprise-url (unless (string= d "github.com") d))))

(defun oauth-login-github-copilot (cb)
  "GitHub Copilot device-code login flow."
  (let* ((domain-input (funcall (getf cb :on-prompt)
                                "GitHub Enterprise domain (blank for github.com):"))
         (d (string-trim '(#\Space #\Tab #\Return #\Newline) (or domain-input "")))
         (domain (if (string= d "") "github.com" d))
         (device (oauth-github-start-device-flow domain)))
    (funcall (getf cb :on-auth)
             (getf device :verification-uri)
             (format nil "Enter code: ~A" (getf device :user-code)))
    (let ((gh-token (oauth-github-poll domain
                                       (getf device :device-code)
                                       (getf device :interval)
                                       (getf device :expires-in))))
      (oauth-github-refresh-copilot-token
       gh-token (unless (string= domain "github.com") domain)))))

(defun oauth-refresh-github-copilot (creds)
  (oauth-github-refresh-copilot-token (getf creds :refresh) (getf creds :enterprise-url)))

(defun oauth-google-user-email (access-token)
  (handler-case
      (json-string-field
       (oauth-request "https://www.googleapis.com/oauth2/v1/userinfo?alt=json"
                      :headers `(("Authorization" . ,(format nil "Bearer ~A" access-token))))
       "email")
    (error () nil)))

(defun oauth-antigravity-discover-project (access-token)
  (handler-case
      (let* ((resp (oauth-request
                    "https://cloudcode-pa.googleapis.com/v1internal:loadCodeAssist"
                    :method :post
                    :content (to-json `(("metadata" . (("ideType" . "IDE_UNSPECIFIED")
                                                        ("platform" . "PLATFORM_UNSPECIFIED")
                                                        ("pluginType" . "GEMINI")))))
                    :content-type "application/json"
                    :headers `(("Authorization" . ,(format nil "Bearer ~A" access-token)))))
             (proj (json-string-field resp "cloudaicompanionProject")))
        (or proj +antigravity-default-project+))
    (error () +antigravity-default-project+)))

(defun oauth-login-antigravity (cb)
  "Antigravity PKCE flow with redirect-URL paste."
  (unless (and +antigravity-client-id+ +antigravity-client-secret+)
    (error "Antigravity OAuth client ID or secret is not configured. Set the ANTIGRAVITY_CLIENT_ID and ANTIGRAVITY_CLIENT_SECRET environment variables."))
  (multiple-value-bind (verifier challenge method) (generate-pkce)
    (let* ((params (oauth-encode-params
                    `(("client_id" . ,+antigravity-client-id+)
                      ("response_type" . "code")
                      ("redirect_uri" . ,+antigravity-redirect-uri+)
                      ("scope" . ,+antigravity-scopes+)
                      ("code_challenge" . ,challenge)
                      ("code_challenge_method" . ,method)
                      ("state" . ,verifier)
                      ("access_type" . "offline")
                      ("prompt" . "consent"))))
           (url (format nil "~A?~A" +antigravity-auth-url+ params)))
      (funcall (getf cb :on-auth) url
               "Sign in, then paste the full redirect URL (localhost:51121/...).")
      (let* ((input (funcall (getf cb :on-prompt) "Paste the redirect URL:"))
             (code (oauth-parse-code-input input))
             (resp (oauth-http-post-form
                    +antigravity-token-url+
                    `(("client_id" . ,+antigravity-client-id+)
                      ("client_secret" . ,+antigravity-client-secret+)
                      ("code" . ,code)
                      ("grant_type" . "authorization_code")
                      ("redirect_uri" . ,+antigravity-redirect-uri+)
                      ("code_verifier" . ,verifier))))
             (creds (oauth-creds-from-token-response resp)))
        (setf (getf creds :project-id)
              (oauth-antigravity-discover-project (getf creds :access)))
        (setf (getf creds :email)
              (oauth-google-user-email (getf creds :access)))
        creds))))

(defun oauth-refresh-antigravity (creds)
  (unless (and +antigravity-client-id+ +antigravity-client-secret+)
    (error "Antigravity OAuth client ID or secret is not configured. Set the ANTIGRAVITY_CLIENT_ID and ANTIGRAVITY_CLIENT_SECRET environment variables."))
  (let ((r (oauth-creds-from-token-response
            (oauth-http-post-form
             +antigravity-token-url+
             `(("client_id" . ,+antigravity-client-id+)
               ("client_secret" . ,+antigravity-client-secret+)
               ("refresh_token" . ,(getf creds :refresh))
               ("grant_type" . "refresh_token"))))))
    (setf (getf r :project-id) (getf creds :project-id))
    (unless (getf r :refresh)
      (setf (getf r :refresh) (getf creds :refresh)))
    r))

;;* Provider registry
(defstruct oauth-provider
  id name login-fn refresh-fn api-key-fn (warn-tos nil))

(defvar *oauth-providers* (make-hash-table :test 'equal))

(defun get-oauth-provider (id) (gethash id *oauth-providers*))
(defun oauth-provider-list ()
  (loop for v being the hash-values of *oauth-providers* collect v))
(defun register-oauth-provider (p)
  (setf (gethash (oauth-provider-id p) *oauth-providers*) p))

(register-oauth-provider
 (make-oauth-provider :id "anthropic"
                      :name "Anthropic (Claude Pro/Max)"
                      :login-fn #'oauth-login-anthropic
                      :refresh-fn #'oauth-refresh-anthropic
                      :api-key-fn (lambda (c) (getf c :access))
                      :warn-tos t))
(register-oauth-provider
 (make-oauth-provider :id "openai-codex"
                      :name "ChatGPT Plus/Pro (Codex Subscription)"
                      :login-fn #'oauth-login-codex
                      :refresh-fn #'oauth-refresh-codex
                      :api-key-fn (lambda (c) (getf c :access))))
(register-oauth-provider
 (make-oauth-provider :id "github-copilot"
                      :name "GitHub Copilot"
                      :login-fn #'oauth-login-github-copilot
                      :refresh-fn #'oauth-refresh-github-copilot
                      :api-key-fn (lambda (c) (getf c :access))))
(register-oauth-provider
 (make-oauth-provider :id "google-antigravity"
                      :name "Antigravity (Gemini 3, Claude, GPT-OSS)"
                      :login-fn #'oauth-login-antigravity
                      :refresh-fn #'oauth-refresh-antigravity
                      :api-key-fn (lambda (c)
                                    (to-json `(("token" . ,(getf c :access))
                                               ("projectId" . ,(getf c :project-id)))))
                      :warn-tos t))

;;* Token access (with env override + auto-refresh)
(defun oauth-env-token (provider-id)
  "Return an environment-variable token for PROVIDER-ID, or NIL."
  (cond
    ((string= provider-id "anthropic") llm:*anthropic-oauth-token*)
    ((string= provider-id "github-copilot") llm:*github-copilot-token*)
    ((string= provider-id "google-antigravity")
     (when (and llm:*antigravity-token* llm:*antigravity-project-id*)
       (to-json `(("token" . ,llm:*antigravity-token*)
                  ("projectId" . ,llm:*antigravity-project-id*)))))
    (t nil)))

(defun oauth-refresh (provider-id creds)
  (funcall (oauth-provider-refresh-fn (get-oauth-provider provider-id)) creds))

(defun oauth-access-token (provider-id)
  "Return a usable token/api-key for PROVIDER-ID.
   Honors env overrides and refreshes expired stored credentials."
  (let ((env (oauth-env-token provider-id)))
    (when env (return-from oauth-access-token env)))
  (load-oauth-credentials)
  (let ((creds (get-oauth-credentials provider-id)))
    (unless creds
      (error "Not logged in to ~A. Use /login to authenticate." provider-id))
    (when (oauth-expired-p creds)
      (setf creds (oauth-refresh provider-id creds))
      (set-oauth-credentials provider-id creds)
      (save-oauth-credentials))
    (funcall (oauth-provider-api-key-fn (get-oauth-provider provider-id)) creds)))

;;* Interactive flow
(defun default-oauth-callbacks ()
  "Standard REPL/stdout callbacks for OAuth flows."
  (list :on-auth (lambda (url &optional instructions)
                   (format t "~&Open this URL in your browser to authenticate:~%~A~%" url)
                   (when instructions (format t "~A~%" instructions))
                   (force-output))
        :on-prompt (lambda (message)
                     (format t "~&~A " message)
                     (force-output)
                     (string-trim '(#\Space #\Tab #\Return #\Newline)
                                  (or (read-line *standard-input* nil "") "")))
        :on-progress (lambda (msg)
                       (format t "~&~A~%" msg)
                       (force-output))))

(defun select-oauth-provider ()
  "Pick a provider via fuzzy-select."
  (let* ((items (loop for p in (oauth-provider-list)
                      collect `((:item . ,(oauth-provider-name p))
                                (:preview . ,(format nil "Provider ID: ~A~@[~%~A~]"
                                                     (oauth-provider-id p)
                                                     (when (oauth-provider-warn-tos p)
                                                       "WARNING: using this may violate the provider's Terms of Service."))))))
         (sel (fuzzy-select items)))
    (when sel
      (find (cdr (assoc :item sel)) (oauth-provider-list)
            :key #'oauth-provider-name :test #'string=))))

(defun run-oauth-login-flow (&optional provider-id)
  "Run the interactive OAuth login flow, saving credentials on success."
  (load-oauth-credentials)
  (let ((provider (if provider-id
                      (get-oauth-provider provider-id)
                      (select-oauth-provider))))
    (cond
      ((null provider)
       (format t "~&Login cancelled.~%")
       nil)
      (t
       (when (oauth-provider-warn-tos provider)
         (format t "~&WARNING: Logging in to ~A may violate the provider's Terms of Service.~%"
                 (oauth-provider-name provider))
         (unless (confirm-action "Continue anyway?")
           (format t "~&Login cancelled.~%")
           (return-from run-oauth-login-flow nil)))
       (handler-case
           (let ((creds (funcall (oauth-provider-login-fn provider)
                                 (default-oauth-callbacks))))
             (set-oauth-credentials (oauth-provider-id provider) creds)
             (save-oauth-credentials)
             (format t "~&Logged in to ~A. Credentials saved to ~A~%"
                     (oauth-provider-name provider)
                     (uiop:native-namestring (oauth-credentials-path)))
             creds)
         (error (e)
           (format t "~&Login failed: ~A~%" e)
           nil))))))

;;* Commands
(define-command login (args)
  "Log in to a subscription LLM provider via OAuth (anthropic, openai-codex, github-copilot, google-antigravity)."
  (if args
      (run-oauth-login-flow (first args))
      (run-oauth-login-flow))
  :completions (lambda (text args)
                 (declare (ignore args))
                 (let ((ids (mapcar #'oauth-provider-id (oauth-provider-list))))
                   (if (string= text "")
                       ids
                       (remove-if-not (lambda (id) (str:starts-with-p text id :ignore-case t)) ids)))))

(define-command logout (args)
  "Remove saved OAuth credentials for a provider (or all providers)."
  (load-oauth-credentials)
  (if args
      (progn
        (remhash (first args) *oauth-credentials*)
        (save-oauth-credentials)
        (format t "Logged out of ~A.~%" (first args)))
      (progn
        (clrhash *oauth-credentials*)
        (save-oauth-credentials)
        (format t "Logged out of all providers.~%"))))
