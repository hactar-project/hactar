;; github copilot api commands and web interface
(in-package :hactar)

;;* Configuration
(defvar *github-copilot-config-path* 
  (merge-pathnames ".config/github-copilot/hosts.json" (user-homedir-pathname))
  "Path to GitHub Copilot configuration file.")
;; NOTE: this client-id is not a private thing. Just a GH copilot api thing
(defvar *github-copilot-client-id* (or (uiop:getenv "GITHUB_COPILOT_CLIENT_ID")
                                       "Iv1.b507a08c87ecfe98")
  "GitHub Copilot public application client ID.")

(defvar *github-copilot-token* nil
  "Cached GitHub Copilot token.")

;;* Token Management
(defun get-copilot-token ()
  "Get the short-lived GitHub Copilot bearer token from the llm package."
  (llm:get-copilot-token))

(defun save-copilot-token (token)
  "Save GitHub Copilot token to hosts.json config file."
  (ensure-directories-exist *github-copilot-config-path*)
  (let ((config `((:*github.com . ((:oauth_token . ,token)
                                   (:user . "copilot"))))))
    (with-open-file (stream *github-copilot-config-path*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((shasht:*write-alist-as-object* t))
        (write-string (shasht:write-json config nil) stream)))
    (setf *github-copilot-token* token)
    token))

;;* Authorization Flow
(defun request-copilot-device-code ()
  "Request a device code for GitHub Copilot authorization."
  (let* ((endpoint "https://github.com/login/device/code")
         (payload (format nil "{\"client_id\":\"~A\",\"scope\":\"read:user\"}" 
                         *github-copilot-client-id*))
         (headers '(("accept" . "application/json")
                   ("editor-version" . "Neovim/0.6.1")
                   ("editor-plugin-version" . "copilot.vim/1.16.0")
                   ("content-type" . "application/json")
                   ("user-agent" . "GithubCopilot/1.155.0"))))
    (multiple-value-bind (body status)
        (drakma:http-request endpoint
                            :method :post
                            :content payload
                            :additional-headers headers
                            :external-format-in :utf-8
                            :external-format-out :utf-8)
      (if (= status 200)
          (cl-json:decode-json-from-string 
           (if (stringp body) body (babel:octets-to-string body :encoding :utf-8)))
          (progn
            (format t "~&Error requesting device code: HTTP ~A~%" status)
            nil)))))

(defun poll-for-access-token (device-code interval)
  "Poll for access token after user authorization."
  (let* ((endpoint "https://github.com/login/oauth/access_token")
         (payload (format nil "{\"client_id\":\"~A\",\"device_code\":\"~A\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}"
                         *github-copilot-client-id* device-code))
         (headers '(("accept" . "application/json")
                   ("content-type" . "application/json"))))
    (loop
      (sleep interval)
      (multiple-value-bind (body status)
          (drakma:http-request endpoint
                              :method :post
                              :content payload
                              :additional-headers headers
                              :external-format-in :utf-8
                              :external-format-out :utf-8)
        (when (= status 200)
          (let* ((response (cl-json:decode-json-from-string 
                           (if (stringp body) body (babel:octets-to-string body :encoding :utf-8))))
                 (error-msg (cdr (assoc :error response)))
                 (access-token (cdr (assoc :access_token response))))
            (cond
              ((string= error-msg "authorization_pending")
               (format t ".") (force-output))
              ((string= error-msg "slow_down")
               (incf interval 5)
               (format t "~%Slowing down polling...~%"))
              (access-token
               (return access-token))
              (t
               (format t "~&Error: ~A~%" error-msg)
               (return nil)))))))))

(defun get-copilot-token-from-access-token (access-token)
  "Exchange access token for Copilot token."
  (let* ((endpoint "https://api.github.com/copilot_internal/v2/token")
         (headers `(("authorization" . ,(format nil "token ~A" access-token)))))
    (multiple-value-bind (body status)
        (drakma:http-request endpoint
                            :method :get
                            :additional-headers headers
                            :external-format-in :utf-8
                            :external-format-out :utf-8)
      (if (= status 200)
          (let ((response (cl-json:decode-json-from-string 
                          (if (stringp body) body (babel:octets-to-string body :encoding :utf-8)))))
            (cdr (assoc :token response)))
          (progn
            (format t "~&Error getting Copilot token: HTTP ~A~%" status)
            nil)))))

(defun authorize-copilot ()
  "Authorization is now managed via a GitHub access token.
Set the environment variable GITHUB_COPILOT_ACCESS_TOKEN to a valid GitHub Personal Access Token.
A short-lived Copilot token will be fetched automatically when needed and refreshed every ~25 minutes.")

;;* Utility
(defun payload-to-json (payload)
  "Convert payload to JSON string (local copy for copilot.lisp)."
  (let* ((shasht:*write-alist-as-object* t)
         (shasht:*write-plist-as-object* t)
	  (shasht:*symbol-name-function* (lambda (sym)
                                          (string-downcase (symbol-name sym))))
         (result
           (shasht:write-json payload nil)))
    result))

;;* Subcommand
(define-sub-command copilotapi (args)
  "GitHub Copilot API commands: models, authorize, complete"
  (if (null args)
      (format t "Usage: hactar copilotapi <command>~%Commands:~%  models     - List available models~%  authorize  - Authorize GitHub Copilot~%  complete   - Generate completion~%")
      (let ((command (first args)))
        (cond
          ((string= command "models")
           (format t "~&Fetching models...~%")
           (let ((models (llm:list-copilot-models)))
             (if models
                 (progn
                   (format t "~&Available models:~%")
                   (dolist (model models)
                     (format t "| ~A~%" model)))
                 (format t "~&Failed to fetch models~%"))))
	  ((string= command "authorize")
           (authorize-copilot))
	  ((string= command "complete")
           (if (rest args)
               (let* ((query (format nil "~{~A~^ ~}" (rest args)))
                      (result (llm:copilot-complete-text query :model "gpt-4o")))
                 (if result
                     (format t "~A~%" result)
                     (format t "~&Failed to generate completion~%")))
               (format t "~&Usage: hactar copilotapi complete <query>~%")))
          
          (t
           (format t "~&Unknown command: ~A~%" command))))))
