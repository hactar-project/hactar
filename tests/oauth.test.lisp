(in-package :hactar-tests)

(def-suite oauth-tests
  :description "Tests for the oauth stuff")

(in-suite oauth-tests)

(test oauth-pkce-generation
  "PKCE verifier/challenge/method are produced"
  (multiple-value-bind (verifier challenge method) (hactar::generate-pkce)
    (is (stringp verifier))
    (is (> (length verifier) 20))
    (is (stringp challenge))
    (is (member method '("S256" "plain") :test #'string=))))

(test oauth-base64url
  "base64url encoding strips padding and url-unsafe chars"
  (let ((enc (hactar::base64url-encode-bytes
              (make-array 3 :element-type '(unsigned-byte 8)
                            :initial-contents '(251 255 191)))))
    (is (stringp enc))
    (is (not (find #\+ enc)))
    (is (not (find #\/ enc)))
    (is (not (find #\= enc)))))

(test oauth-base64url-roundtrip
  "base64url decode reverses encode for UTF-8 strings"
  (let* ((s "hello-world_123")
         (enc (hactar::base64url-encode-bytes (babel:string-to-octets s :encoding :utf-8)))
         (dec (hactar::oauth-b64url-decode-string enc)))
    (is (string= s dec))))

(test oauth-json-field-extraction
  "JSON string/number fields are extracted by key"
  (let ((json "{\"access_token\":\"abc123\",\"expires_in\":3600,\"refresh_token\":\"r1\"}"))
    (is (string= "abc123" (hactar::json-string-field json "access_token")))
    (is (= 3600 (hactar::json-number-field json "expires_in")))
    (is (string= "r1" (hactar::json-string-field json "refresh_token")))
    (is (null (hactar::json-string-field json "missing")))))

(test oauth-creds-from-response
  "Credentials plist is built from a token response"
  (let ((creds (hactar::oauth-creds-from-token-response
                "{\"access_token\":\"a\",\"refresh_token\":\"b\",\"expires_in\":3600}")))
    (is (string= "a" (getf creds :access)))
    (is (string= "b" (getf creds :refresh)))
    (is (integerp (getf creds :expires)))))

(test oauth-parse-code-input
  "Authorization code is parsed from raw, code#state, and redirect URLs"
  (is (string= "abc" (hactar::oauth-parse-code-input "abc")))
  (is (string= "abc" (hactar::oauth-parse-code-input "abc#state123")))
  (is (string= "abc" (hactar::oauth-parse-code-input
                      "http://localhost:1455/auth/callback?code=abc&state=xyz"))))

(test oauth-jwt-account-id
  "chatgpt_account_id is extracted from a JWT access token"
  (let* ((payload "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct_123\"}}")
         (b64 (hactar::base64url-encode-bytes
               (babel:string-to-octets payload :encoding :utf-8)))
         (token (format nil "header.~A.sig" b64)))
    (is (string= "acct_123" (hactar::oauth-jwt-account-id token)))))

(test oauth-provider-registry
  "All four providers are registered, with TOS warnings where expected"
  (is (hactar::get-oauth-provider "anthropic"))
  (is (hactar::get-oauth-provider "openai-codex"))
  (is (hactar::get-oauth-provider "github-copilot"))
  (is (hactar::get-oauth-provider "google-antigravity"))
  (is-true (hactar::oauth-provider-warn-tos (hactar::get-oauth-provider "anthropic")))
  (is-true (hactar::oauth-provider-warn-tos (hactar::get-oauth-provider "google-antigravity")))
  (is-false (hactar::oauth-provider-warn-tos (hactar::get-oauth-provider "github-copilot"))))

(test oauth-credential-store-roundtrip
  "Credentials persist to and load from the config file"
  (let* ((dir (ensure-directories-exist
               (merge-pathnames (format nil "hactar-oauth-~A/" (random 1000000))
                                (uiop:temporary-directory))))
         (hactar::*hactar-config-path* dir)
         (hactar::*oauth-credentials* (make-hash-table :test 'equal)))
    (hactar::set-oauth-credentials "anthropic" (list :access "x" :refresh "y" :expires 123))
    (hactar::save-oauth-credentials)
    (clrhash hactar::*oauth-credentials*)
    (hactar::load-oauth-credentials)
    (let ((c (hactar::get-oauth-credentials "anthropic")))
      (is (string= "x" (getf c :access)))
      (is (string= "y" (getf c :refresh)))
      (is (= 123 (getf c :expires))))
    (ignore-errors (delete-file (hactar::oauth-credentials-path)))))

(test oauth-expired-p
  "Expiry is detected against universal-time"
  (is-true (hactar::oauth-expired-p (list :access "a" :expires (- (get-universal-time) 10))))
  (is-false (hactar::oauth-expired-p (list :access "a" :expires (+ (get-universal-time) 1000))))
  (is-true (hactar::oauth-expired-p (list :access "a"))))

(test oauth-env-token
  "Environment-variable token overrides are honored"
  (let ((llm:*anthropic-oauth-token* "env-token"))
    (is (string= "env-token" (hactar::oauth-env-token "anthropic"))))
  (let ((llm:*antigravity-token* "tok")
        (llm:*antigravity-project-id* "proj"))
    (let ((json (hactar::oauth-env-token "google-antigravity")))
      (is (search "tok" json))
      (is (search "proj" json))))
  (let ((llm:*antigravity-token* nil)
        (llm:*antigravity-project-id* nil))
    (is (null (hactar::oauth-env-token "google-antigravity")))))
