(in-package :hactar-tests)

(def-suite formats-tests
  :description "Tests for Command Formats infrastructure")

(in-suite formats-tests)

(test parse-format-keyword
  "Test parsing format strings into keywords."
  (is (eq :json (hactar::parse-format-keyword "json")))
  (is (eq :markdown (hactar::parse-format-keyword "markdown")))
  (is (null (hactar::parse-format-keyword "unsupported-format"))))

(test extract-format-string
  "Test extracting the format string from CLI args."
  (is (string= "json" (hactar::extract-format-string '("--format=json"))))
  (is (string= "yaml" (hactar::extract-format-string '("arg1" "--format" "yaml" "arg2"))))
  (is (string= "json" (hactar::extract-format-string '("arg1" "--json" "arg2"))))
  (is (string= "json" (hactar::extract-format-string '("-json"))))
  (is (string= "markdown" (hactar::extract-format-string '("-md"))))
  (is (string= "markdown" (hactar::extract-format-string '("--markdown"))))
  (is (string= "xml" (hactar::extract-format-string '("--xml"))))
  (is (string= "yaml" (hactar::extract-format-string '("-yml"))))
  (is (string= "org-mode" (hactar::extract-format-string '("--org"))))
  (is (string= "org-mode" (hactar::extract-format-string '("-org"))))
  (is (string= "org-mode" (hactar::extract-format-string '("--org-mode"))))
  (is (null (hactar::extract-format-string '("arg1" "arg2")))))

(test wrap-formatted-output
  "Test formatting wrappers output the expected markup."
  (let ((hactar::*format-wrapper-style* :xml-tags))
    (is (string= (format nil "<json>~%{\"test\":true}~%</json>")
                 (hactar::wrap-formatted-output "{\"test\":true}" :json))))
  
  (let ((hactar::*format-wrapper-style* :markdown))
    (is (string= (format nil "```json~%{\"test\":true}~%```")
                 (hactar::wrap-formatted-output "{\"test\":true}" :json)))))

(test format-tag-name
  "Test formatting tag names from keywords."
  (is (string= "json" (hactar::format-tag-name :json)))
  (is (string= "org-mode" (hactar::format-tag-name :org-mode))))

(test supported-format-names
  "Test supported format names string generation."
  (let ((names (hactar::supported-format-names)))
    (is (member "json" names :test #'string=))
    (is (member "markdown" names :test #'string=))
    (is (member "xml" names :test #'string=))))

(test format-handler-registry
  "Test registering, retrieving, and unregistering format handlers."
  (let ((cmd "/test-registry")
        (handler (lambda (args) (declare (ignore args)) "ok")))
    ;; Ensure clean state
    (hactar::unregister-format-handler cmd :json)
    (is (null (hactar::get-format-handler cmd :json)))

    ;; Register
    (hactar::register-format-handler cmd :json handler)
    (is (eq handler (hactar::get-format-handler cmd :json)))

    ;; Unregister
    (hactar::unregister-format-handler cmd :json)
    (is (null (hactar::get-format-handler cmd :json)))))

(test execute-format-command
  "Test executing a format command captures and wraps output."
  (let ((cmd "/test-exec")
        (handler (lambda (args) (declare (ignore args)) "{\"data\": 123}"))
        (hactar::*format-wrapper-style* :xml-tags))
    (hactar::register-format-handler cmd :json handler)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((result (hactar::execute-format-command cmd :json nil)))
                      (is (string= "{\"data\": 123}" result))))))
      (is (search "<json>" output))
      (is (search "{\"data\": 123}" output))
      (is (search "</json>" output)))
    (hactar::unregister-format-handler cmd :json)))
