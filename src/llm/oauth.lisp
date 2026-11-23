(in-package #:hactar-llm)

;;; Pure OAuth PKCE/device helpers, credential store, and provider flow registry.
;;; HTTP requests go through *OAUTH-HTTP-FN* so tests run offline and live HTTP
;;; transports plug in cleanly from hactar-llm-live.

(defparameter +unix-epoch-offset+ 2208988800)

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

(defparameter +copilot-client-id+
  (or (uiop:getenv "GITHUB_COPILOT_CLIENT_ID")
      (uiop:getenv "COPILOT_CLIENT_ID")
      "Iv1.b507a08c87ecfe98"))

(defparameter +antigravity-client-id+ (uiop:getenv "ANTIGRAVITY_CLIENT_ID"))
(defparameter +antigravity-client-secret+ (uiop:getenv "ANTIGRAVITY_CLIENT_SECRET"))
(defparameter +antigravity-redirect-uri+ "http://localhost:51121/oauth-callback")
(defparameter +antigravity-auth-url+ "https://accounts.google.com/o/oauth2/v2/auth")
(defparameter +antigravity-token-url+ "https://oauth2.googleapis.com/token")
(defparameter +antigravity-scopes+
  "https://www.googleapis.com/auth/cloud-platform https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile")

(defvar *oauth-http-fn* nil
  "Transport fn: (url &key method content content-type headers) -> body string.")

(defvar *oauth-sha256-fn* nil
  "Optional (bytes-or-string) -> base64url-sha256 string hook.")

(defvar *oauth-random-state* (make-random-state t))

;;; ---- Pure base64 / base64url / URL encoding -------------------------------

(defparameter +b64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun %b64-encode-bytes (bytes)
  (let* ((len (length bytes))
         (out (make-string-output-stream)))
    (loop for i from 0 below len by 3
          for b0 = (aref bytes i)
          for b1 = (if (< (1+ i) len) (aref bytes (1+ i)) 0)
          for b2 = (if (< (+ i 2) len) (aref bytes (+ i 2)) 0)
          for n = (logior (ash b0 16) (ash b1 8) b2)
          do (write-char (char +b64-alphabet+ (ldb (byte 6 18) n)) out)
             (write-char (char +b64-alphabet+ (ldb (byte 6 12) n)) out)
             (write-char (if (< (1+ i) len)
                             (char +b64-alphabet+ (ldb (byte 6 6) n))
                             #\=)
                         out)
             (write-char (if (< (+ i 2) len)
                             (char +b64-alphabet+ (ldb (byte 6 0) n))
                             #\=)
                         out))
    (get-output-stream-string out)))

(defun %b64url-encode-bytes (bytes)
  (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+ (%b64-encode-bytes bytes)))))

(defun %b64url-decode-string (s)
  (let* ((std (substitute #\/ #\_ (substitute #\+ #\- s)))
         (clean (remove #\= std))
         (bytes (make-array (floor (* (length clean) 3) 4)
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0)))
    (loop for i from 0 below (length clean) by 4
          for c0 = (position (char clean i) +b64-alphabet+)
          for c1 = (and (< (+ i 1) (length clean)) (position (char clean (+ i 1)) +b64-alphabet+))
          for c2 = (and (< (+ i 2) (length clean)) (position (char clean (+ i 2)) +b64-alphabet+))
          for c3 = (and (< (+ i 3) (length clean)) (position (char clean (+ i 3)) +b64-alphabet+))
          when (and c0 c1)
            do (let ((n (logior (ash c0 18) (ash c1 12) (ash (or c2 0) 6) (or c3 0))))
                 (vector-push-extend (ldb (byte 8 16) n) bytes)
                 (when c2 (vector-push-extend (ldb (byte 8 8) n) bytes))
                 (when c3 (vector-push-extend (ldb (byte 8 0) n) bytes))))
    (map 'string #'code-char bytes)))

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

(defun %oauth-encode-params (alist)
  (format nil "~{~A~^&~}"
          (mapcar (lambda (kv) (format nil "~A=~A" (%url-encode (car kv)) (%url-encode (cdr kv))))
                  alist)))

;;; ---- Pure SHA-256 for PKCE S256 -------------------------------------------

(defparameter +sha256-k+
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defun %u32 (x) (logand x #xffffffff))
(defun %rotr32 (x n) (logior (ash (%u32 x) (- n)) (%u32 (ash (%u32 x) (- 32 n)))))

(defun %sha256-bytes (string)
  (let* ((msg (map 'vector #'char-code string))
         (bit-len (* 8 (length msg)))
         (padded (make-array (* 64 (ceiling (+ (length msg) 9) 64))
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (replace padded msg)
    (setf (aref padded (length msg)) #x80)
    (loop for i from 0 below 8
          do (setf (aref padded (- (length padded) 1 i))
                   (ldb (byte 8 (* 8 i)) bit-len)))
    (let ((h (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                     #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
      (loop for chunk from 0 below (length padded) by 64 do
        (let ((w (make-array 64 :initial-element 0)))
          (dotimes (i 16)
            (let ((off (+ chunk (* i 4))))
              (setf (aref w i)
                    (logior (ash (aref padded off) 24)
                            (ash (aref padded (+ off 1)) 16)
                            (ash (aref padded (+ off 2)) 8)
                            (aref padded (+ off 3))))))
          (loop for i from 16 below 64 do
            (let* ((w15 (aref w (- i 15)))
                   (w2 (aref w (- i 2)))
                   (s0 (logxor (%rotr32 w15 7) (%rotr32 w15 18) (ash w15 -3)))
                   (s1 (logxor (%rotr32 w2 17) (%rotr32 w2 19) (ash w2 -10))))
              (setf (aref w i) (%u32 (+ (aref w (- i 16)) s0 (aref w (- i 7)) s1)))))
          (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
                (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
            (dotimes (i 64)
              (let* ((s1 (logxor (%rotr32 e 6) (%rotr32 e 11) (%rotr32 e 25)))
                     (ch (logxor (logand e f) (logand (lognot e) g)))
                     (t1 (%u32 (+ hh s1 ch (aref +sha256-k+ i) (aref w i))))
                     (s0 (logxor (%rotr32 a 2) (%rotr32 a 13) (%rotr32 a 22)))
                     (maj (logxor (logand a b) (logand a c) (logand b c)))
                     (t2 (%u32 (+ s0 maj))))
                (setf hh g g f f e e (%u32 (+ d t1)) d c c b b a a (%u32 (+ t1 t2)))))
            (loop for idx from 0 below 8
                  for val in (list a b c d e f g hh)
                  do (setf (aref h idx) (%u32 (+ (aref h idx) val)))))))
      (let ((digest (make-array 32 :element-type '(unsigned-byte 8))))
        (dotimes (i 8 digest)
          (dotimes (j 4)
            (setf (aref digest (+ (* i 4) j))
                  (ldb (byte 8 (* 8 (- 3 j))) (aref h i)))))))))

(defun generate-pkce ()
  "Generate (values verifier challenge \"S256\")."
  (let* ((bytes (let ((a (make-array 32 :element-type '(unsigned-byte 8))))
                  (dotimes (i 32 a)
                    (setf (aref a i) (random 256 *oauth-random-state*)))))
         (verifier (%b64url-encode-bytes bytes))
         (challenge (if *oauth-sha256-fn*
                        (funcall *oauth-sha256-fn* verifier)
                        (%b64url-encode-bytes (%sha256-bytes verifier)))))
    (values verifier challenge "S256")))

;;; ---- Minimal JSON field helpers (zero external deps) ----------------------

(defun %json-escape (str)
  (with-output-to-string (out)
    (loop for ch across (princ-to-string str) do
      (case ch
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (t (write-char ch out))))))

(defun %alist->json (alist)
  (format nil "{~{\"~A\":\"~A\"~^,~}}"
          (loop for (k . v) in alist
                collect (%json-escape k)
                collect (%json-escape v))))

(defun %json-string-field (json key)
  (let* ((needle (format nil "\"~A\"" key))
         (pos (search needle json)))
    (when pos
      (let ((colon (position #\: json :start (+ pos (length needle)))))
        (when colon
          (let ((q1 (position #\" json :start (1+ colon))))
            (when q1
              (let ((q2 (position #\" json :start (1+ q1))))
                (when q2 (subseq json (1+ q1) q2))))))))))

(defun %json-number-field (json key)
  (let* ((needle (format nil "\"~A\"" key))
         (pos (search needle json)))
    (when pos
      (let ((colon (position #\: json :start (+ pos (length needle)))))
        (when colon
          (let* ((tail (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                         (subseq json (1+ colon))))
                 (end (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\-)))
                                       tail)))
            (when (plusp (or end (length tail)))
              (ignore-errors (parse-integer (subseq tail 0 end))))))))))

;;; ---- Credential store & parsing -------------------------------------------

(defvar *oauth-credentials* (make-hash-table :test 'equal)
  "Provider-id -> credentials plist (:access :refresh :expires ...).")

(defun oauth-credentials-path ()
  (merge-pathnames ".config/hactar/credentials.lisp" (user-homedir-pathname)))

(defun save-oauth-credentials (&optional (path (oauth-credentials-path)))
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((alist '()))
      (maphash (lambda (k v) (push (cons k v) alist)) *oauth-credentials*)
      (prin1 alist s)
      (terpri s)))
  path)

(defun load-oauth-credentials (&optional (path (oauth-credentials-path)))
  (when (probe-file path)
    (with-open-file (s path :direction :input)
      (let ((*read-eval* nil))
        (let ((alist (ignore-errors (read s nil nil))))
          (clrhash *oauth-credentials*)
          (dolist (pair alist)
            (setf (gethash (car pair) *oauth-credentials*) (cdr pair)))))))
  *oauth-credentials*)

(defun get-oauth-credentials (id) (gethash (string-downcase (string id)) *oauth-credentials*))
(defun set-oauth-credentials (id plist)
  (setf (gethash (string-downcase (string id)) *oauth-credentials*) plist))

(defun oauth-expired-p (creds)
  (let ((exp (getf creds :expires)))
    (or (null exp) (>= (get-universal-time) exp))))

(defun oauth-parse-code-input (input)
  "Extract an authorization code from a pasted code, code#state, or redirect URL."
  (let ((value (string-trim '(#\Space #\Tab #\Return #\Newline) (or input ""))))
    (cond
      ((string= value "") nil)
      ((search "code=" value)
       (let* ((start (+ (search "code=" value) 5))
              (end (position-if (lambda (c) (member c '(#\& #\#))) value :start start)))
         (subseq value start end)))
      ((find #\# value) (first (uiop:split-string value :separator "#")))
      (t value))))

(defun oauth-jwt-account-id (token)
  "Extract chatgpt_account_id from a JWT access token, or NIL."
  (handler-case
      (let* ((parts (uiop:split-string token :separator "."))
             (payload (second parts)))
        (when payload
          (%json-string-field (%b64url-decode-string payload) "chatgpt_account_id")))
    (error () nil)))

(defun oauth-creds-from-token-response (resp)
  "Build a credentials plist from a token endpoint JSON response string."
  (let ((access (%json-string-field resp "access_token"))
        (refresh (%json-string-field resp "refresh_token"))
        (expires-in (or (%json-number-field resp "expires_in") 3600)))
    (unless access
      (error "Token response missing access_token: ~A" resp))
    (list :access access
          :refresh refresh
          :expires (+ (get-universal-time) (max 0 (- expires-in 300))))))

;;; ---- Pluggable HTTP helper & provider flows -------------------------------

(defun %oauth-request (url &key (method :get) content content-type headers)
  (unless *oauth-http-fn*
    (error "No OAuth HTTP transport configured (*OAUTH-HTTP-FN* is NIL); load hactar-llm-live."))
  (funcall *oauth-http-fn* url :method method :content content
                              :content-type content-type :headers headers))

(defun oauth-login-anthropic (&key on-auth on-prompt)
  (unless +anthropic-client-id+
    (error "ANTHROPIC_CLIENT_ID is not set"))
  (multiple-value-bind (verifier challenge method) (generate-pkce)
    (let ((url (format nil "~A?~A" +anthropic-authorize-url+
                       (%oauth-encode-params
                        `(("code" . "true")
                          ("client_id" . ,+anthropic-client-id+)
                          ("response_type" . "code")
                          ("redirect_uri" . ,+anthropic-redirect-uri+)
                          ("scope" . ,+anthropic-scopes+)
                          ("code_challenge" . ,challenge)
                          ("code_challenge_method" . ,method)
                          ("state" . ,verifier))))))
      (when on-auth (funcall on-auth url nil))
      (let* ((input (if on-prompt (funcall on-prompt "Paste authorization code (code#state):") ""))
             (cleaned (string-trim '(#\Space #\Tab #\Return #\Newline) input))
             (splits (uiop:split-string cleaned :separator "#"))
             (code (first splits))
             (state (or (second splits) verifier))
             (resp (%oauth-request +anthropic-token-url+
                                   :method :post
                                   :content-type "application/json"
                                   :content (%alist->json
                                             `(("grant_type" . "authorization_code")
                                               ("client_id" . ,+anthropic-client-id+)
                                               ("code" . ,code)
                                               ("state" . ,state)
                                               ("redirect_uri" . ,+anthropic-redirect-uri+)
                                               ("code_verifier" . ,verifier))))))
        (let ((creds (oauth-creds-from-token-response resp)))
          (set-oauth-credentials "anthropic" creds)
          creds)))))

(defun oauth-login-codex (&key on-auth on-prompt)
  (unless +codex-client-id+
    (error "CODEX_CLIENT_ID is not set"))
  (multiple-value-bind (verifier challenge method) (generate-pkce)
    (let* ((state (%b64url-encode-bytes (let ((a (make-array 16 :element-type '(unsigned-byte 8))))
                                          (dotimes (i 16 a) (setf (aref a i) (random 256 *oauth-random-state*))))))
           (url (format nil "~A?~A" +codex-authorize-url+
                        (%oauth-encode-params
                         `(("response_type" . "code")
                           ("client_id" . ,+codex-client-id+)
                           ("redirect_uri" . ,+codex-redirect-uri+)
                           ("scope" . ,+codex-scope+)
                           ("code_challenge" . ,challenge)
                           ("code_challenge_method" . ,method)
                           ("state" . ,state))))))
      (when on-auth (funcall on-auth url nil))
      (let* ((input (if on-prompt (funcall on-prompt "Paste redirect URL or code:") ""))
             (code (or (oauth-parse-code-input input) (error "Authorization code required")))
             (resp (%oauth-request +codex-token-url+
                                   :method :post
                                   :content-type "application/x-www-form-urlencoded"
                                   :content (%oauth-encode-params
                                             `(("grant_type" . "authorization_code")
                                               ("client_id" . ,+codex-client-id+)
                                               ("code" . ,code)
                                               ("code_verifier" . ,verifier)
                                               ("redirect_uri" . ,+codex-redirect-uri+)))))
             (creds (oauth-creds-from-token-response resp)))
        (let ((acct (oauth-jwt-account-id (getf creds :access))))
          (when acct (setf (getf creds :account-id) acct)))
        (set-oauth-credentials "openai-codex" creds)
        creds))))

(defun oauth-login-copilot (&key on-auth (domain "github.com") (sleep-fn #'sleep))
  (let* ((device-url (format nil "https://~A/login/device/code" domain))
         (resp (%oauth-request device-url
                               :method :post
                               :content-type "application/json"
                               :headers '(("Accept" . "application/json"))
                               :content (%alist->json `(("client_id" . ,+copilot-client-id+)
                                                        ("scope" . "read:user")))))
         (device-code (%json-string-field resp "device_code"))
         (user-code (%json-string-field resp "user_code"))
         (verify-uri (%json-string-field resp "verification_uri"))
         (interval (or (%json-number-field resp "interval") 5)))
    (when on-auth (funcall on-auth verify-uri user-code))
    (loop repeat 60 do
      (funcall sleep-fn interval)
      (let* ((token-resp (%oauth-request (format nil "https://~A/login/oauth/access_token" domain)
                                         :method :post
                                         :content-type "application/json"
                                         :headers '(("Accept" . "application/json"))
                                         :content (%alist->json
                                                   `(("client_id" . ,+copilot-client-id+)
                                                     ("device_code" . ,device-code)
                                                     ("grant_type" . "urn:ietf:params:oauth:grant-type:device_code")))))
             (token (%json-string-field token-resp "access_token"))
             (err (%json-string-field token-resp "error")))
        (when token
          (let ((creds (list :access token :refresh token :expires (+ (get-universal-time) 86400))))
            (set-oauth-credentials "github-copilot" creds)
            (return creds)))
        (cond ((string= err "slow_down") (incf interval 5))
              ((or (null err) (string= err "authorization_pending")) nil)
              (t (error "GitHub device flow failed: ~A" err)))))))

(defun oauth-refresh (provider-id creds)
  (let* ((id (string-downcase (string provider-id)))
         (refresh (getf creds :refresh)))
    (unless refresh (error "No refresh token for ~A" provider-id))
    (let ((new-creds
            (cond
              ((string= id "anthropic")
               (oauth-creds-from-token-response
                (%oauth-request +anthropic-token-url+
                                :method :post
                                :content-type "application/json"
                                :content (%alist->json
                                          `(("grant_type" . "refresh_token")
                                            ("client_id" . ,+anthropic-client-id+)
                                            ("refresh_token" . ,refresh))))))
              ((string= id "openai-codex")
               (oauth-creds-from-token-response
                (%oauth-request +codex-token-url+
                                :method :post
                                :content-type "application/x-www-form-urlencoded"
                                :content (%oauth-encode-params
                                          `(("grant_type" . "refresh_token")
                                            ("client_id" . ,+codex-client-id+)
                                            ("refresh_token" . ,refresh))))))
              (t creds))))
      (set-oauth-credentials id new-creds)
      new-creds)))

(defun oauth-access-token (provider-id)
  (let* ((id (string-downcase (string provider-id)))
         (creds (get-oauth-credentials id)))
    (when creds
      (if (oauth-expired-p creds)
          (getf (oauth-refresh id creds) :access)
          (getf creds :access)))))

(defun register-default-oauth-providers ()
  "Register built-in OAuth provider entries in *PROVIDERS*."
  (register-provider (make-provider :name :anthropic-oauth :oauth #'oauth-login-anthropic))
  (register-provider (make-provider :name :openai-codex :oauth #'oauth-login-codex))
  (register-provider (make-provider :name :github-copilot :oauth #'oauth-login-copilot))
  t)
