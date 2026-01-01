(in-package :hactar-tests)
(def-suite hyperfractal-tests
  :description "Tests for Hyperfractal plaintext web browser.")

(in-suite hyperfractal-tests)

;;* Configuration
(test hyperfractal-config-defaults-test
  "Test that Hyperfractal has sensible defaults."
  (is (= hactar::*hyperfractal-cache-size* 50))
  (is (string= hactar::*hyperfractal-default-format* "markdown"))
  (is (string= hactar::*hyperfractal-browser* "curl")))

(test hyperfractal-load-env-config-test
  "Test loading configuration from environment variables."
  (let ((hactar::*hyperfractal-default-model* nil)
        (hactar::*hyperfractal-cache-size* 50)
        (hactar::*hyperfractal-default-format* "markdown")
        (hactar::*hyperfractal-browser* "curl"))
    (with-dynamic-stubs ((uiop:getenv (lambda (name)
                                        (cond
                                          ((string= name "HYPERFRACTAL_MODEL") "gpt-4")
                                          ((string= name "HYPERFRACTAL_CACHE_SIZE") "100")
                                          ((string= name "HYPERFRACTAL_FORMAT") "org")
                                          ((string= name "HYPERFRACTAL_BROWSER") "chrome")
                                          (t nil)))))
      (hactar::hyperfractal-load-env-config)
      (is (string= hactar::*hyperfractal-default-model* "gpt-4"))
      (is (= hactar::*hyperfractal-cache-size* 100))
      (is (string= hactar::*hyperfractal-default-format* "org"))
      (is (string= hactar::*hyperfractal-browser* "chrome")))))

;;* Netrc Parsing
(test hyperfractal-parse-netrc-test
  "Test parsing netrc-format authentication file."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "machine example.com login myuser password mypass~%")
      (format s "machine api.example.org login apiuser password apikey~%"))
    (let ((entries (hactar::hyperfractal-parse-netrc p)))
      (is (= 2 (length entries)))
      (let ((example-entry (cdr (assoc "example.com" entries :test #'string-equal))))
        (is-true example-entry)
        (is (string= (cdr (assoc :user example-entry)) "myuser"))
        (is (string= (cdr (assoc :password example-entry)) "mypass")))
      (let ((api-entry (cdr (assoc "api.example.org" entries :test #'string-equal))))
        (is-true api-entry)
        (is (string= (cdr (assoc :user api-entry)) "apiuser"))))))

(test hyperfractal-parse-netrc-missing-file-test
  "Test parsing netrc when file doesn't exist."
  (let ((result (hactar::hyperfractal-parse-netrc #P"/nonexistent/file")))
    (is (null result))))

(test hyperfractal-get-auth-test
  "Test getting authentication for a host."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "machine testhost.com login testuser password testpass~%"))
    (let ((hactar::*hyperfractal-auth-file* p))
      (let ((auth (hactar::hyperfractal-get-auth "testhost.com")))
        (is-true auth)
        (is (string= (cdr (assoc :user auth)) "testuser"))
        (is (string= (cdr (assoc :password auth)) "testpass")))
      ;; Non-existent host
      (is (null (hactar::hyperfractal-get-auth "unknown.com"))))))

;;* Caching
(test hyperfractal-cache-key-test
  "Test cache key generation."
  (is (string= (hactar::hyperfractal-cache-key "https://example.com" "markdown")
               "https://example.com::markdown"))
  (is (string= (hactar::hyperfractal-cache-key "https://test.org/page" "org")
               "https://test.org/page::org")))

(test hyperfractal-cache-operations-test
  "Test cache put, get, and clear operations."
  (hactar::hyperfractal-cache-clear)
  
  (is (null (hactar::hyperfractal-cache-get "https://test.com" "markdown")))
  (hactar::hyperfractal-cache-put "https://test.com" "markdown" "# Test Content")
  (is (string= (hactar::hyperfractal-cache-get "https://test.com" "markdown") "# Test Content"))
  (is (null (hactar::hyperfractal-cache-get "https://test.com" "org")))
  (hactar::hyperfractal-cache-clear)
  (is (null (hactar::hyperfractal-cache-get "https://test.com" "markdown"))))

(test hyperfractal-cache-lru-test
  "Test that cache maintains LRU order."
  (hactar::hyperfractal-cache-clear)
  
  (hactar::hyperfractal-cache-put "https://first.com" "markdown" "first")
  (hactar::hyperfractal-cache-put "https://second.com" "markdown" "second")
  (hactar::hyperfractal-cache-put "https://third.com" "markdown" "third")
  (hactar::hyperfractal-cache-get "https://first.com" "markdown")
  (is (string= (first hactar::*hyperfractal-cache-order*)
               "https://first.com::markdown")))

(test hyperfractal-cache-size-bytes-test
  "Test calculating cache size."
  (hactar::hyperfractal-cache-clear)
  (is (= 0 (hactar::hyperfractal-cache-size-bytes)))
  
  (hactar::hyperfractal-cache-put "https://test.com" "md" "hello")  ; 5 bytes
  (is (= 5 (hactar::hyperfractal-cache-size-bytes)))
  
  (hactar::hyperfractal-cache-put "https://test2.com" "md" "world!")  ; 6 bytes
  (is (= 11 (hactar::hyperfractal-cache-size-bytes))))

;;* URL Extraction
(test hyperfractal-extract-host-test
  "Test extracting host from URL."
  (is (string= (hactar::hyperfractal-extract-host "https://example.com/page") "example.com"))
  (is (string= (hactar::hyperfractal-extract-host "http://test.org:8080/path") "test.org"))
  (is (string= (hactar::hyperfractal-extract-host "https://api.github.com/repos") "api.github.com"))
  (is (null (hactar::hyperfractal-extract-host "not-a-url"))))

;;* Link Extraction
(test hyperfractal-extract-links-markdown-test
  "Test extracting links from Markdown content."
  (let ((content "Check out [Example](https://example.com) and [Test Site](https://test.org/page)."))
    (let ((links (hactar::hyperfractal-extract-links-markdown content)))
      (is (= 2 (length links)))
      (is (string= (car (first links)) "Example"))
      (is (string= (cdr (first links)) "https://example.com"))
      (is (string= (car (second links)) "Test Site"))
      (is (string= (cdr (second links)) "https://test.org/page")))))

(test hyperfractal-extract-links-org-test
  "Test extracting links from Org-mode content."
  (let ((content "Visit [[https://example.com][Example]] or [[https://test.org][Test]]."))
    (let ((links (hactar::hyperfractal-extract-links-org content)))
      (is (= 2 (length links)))
      (is (string= (car (first links)) "Example"))
      (is (string= (cdr (first links)) "https://example.com"))
      (is (string= (car (second links)) "Test"))
      (is (string= (cdr (second links)) "https://test.org")))))

(test hyperfractal-extract-links-dispatch-test
  "Test that extract-links dispatches based on format."
  (let ((md-content "[Link](https://example.com)"))
    (is (= 1 (length (hactar::hyperfractal-extract-links md-content :format "markdown")))))
  (let ((org-content "[[https://example.com][Link]]"))
    (is (= 1 (length (hactar::hyperfractal-extract-links org-content :format "org"))))))

;;* URL Resolution
(test hyperfractal-resolve-url-absolute-test
  "Test that absolute URLs are returned unchanged."
  (is (string= (hactar::hyperfractal-resolve-url "https://other.com/page" "https://example.com/base")
               "https://other.com/page"))
  (is (string= (hactar::hyperfractal-resolve-url "http://test.org" "https://example.com")
               "http://test.org")))

(test hyperfractal-resolve-url-relative-test
  "Test resolving relative URLs against base."
  (is (string= (hactar::hyperfractal-resolve-url "/about" "https://example.com/page")
               "https://example.com/about"))
  (is (string= (hactar::hyperfractal-resolve-url "other.html" "https://example.com/dir/page.html")
               "https://example.com/dir/other.html")))

;;* Navigation History
(test hyperfractal-history-push-test
  "Test pushing URLs to navigation history."
  (setf hactar::*hyperfractal-history* '())
  (setf hactar::*hyperfractal-history-position* 0)
  
  (hactar::hyperfractal-history-push "https://first.com")
  (is (= 1 (length hactar::*hyperfractal-history*)))
  (is (string= (first hactar::*hyperfractal-history*) "https://first.com"))
  
  (hactar::hyperfractal-history-push "https://second.com")
  (is (= 2 (length hactar::*hyperfractal-history*)))
  (is (string= (first hactar::*hyperfractal-history*) "https://second.com")))

(test hyperfractal-history-back-forward-test
  "Test navigating back and forward in history."
  (setf hactar::*hyperfractal-history* '("https://third.com" "https://second.com" "https://first.com"))
  (setf hactar::*hyperfractal-history-position* 0)
  
  (let ((url (hactar::hyperfractal-history-back)))
    (is (string= url "https://second.com"))
    (is (= hactar::*hyperfractal-history-position* 1)))
  (let ((url (hactar::hyperfractal-history-back)))
    (is (string= url "https://first.com"))
    (is (= hactar::*hyperfractal-history-position* 2)))
  (is (null (hactar::hyperfractal-history-back)))
  (let ((url (hactar::hyperfractal-history-forward)))
    (is (string= url "https://second.com"))
    (is (= hactar::*hyperfractal-history-position* 1)))
  (let ((url (hactar::hyperfractal-history-forward)))
    (is (string= url "https://third.com"))
    (is (= hactar::*hyperfractal-history-position* 0)))
  (is (null (hactar::hyperfractal-history-forward))))

;;* Fetching
(test hyperfractal-fetch-with-curl-test
  "Test fetching URL with curl (mocked)."
  (with-dynamic-stubs ((uiop:run-program
                        (lambda (args &key output error-output ignore-error-status)
                          (declare (ignore output error-output ignore-error-status))
                          (if (member "https://example.com" args :test #'string=)
                              (values "<html><body>Hello</body></html>" "" 0)
                              (values "" "Not found" 1)))))
    (is (search "Hello" (hactar::hyperfractal-fetch-with-curl "https://example.com")))
    (is (null (hactar::hyperfractal-fetch-with-curl "https://notfound.com")))))

(test hyperfractal-fetch-with-curl-auth-test
  "Test fetching URL with authentication."
  (let ((called-args nil))
    (with-dynamic-stubs ((uiop:run-program
                          (lambda (args &key output error-output ignore-error-status)
                            (declare (ignore output error-output ignore-error-status))
                            (setf called-args args)
                            (values "content" "" 0))))
      (hactar::hyperfractal-fetch-with-curl "https://example.com" 
                                            :auth '((:user . "testuser") (:password . "testpass")))
      (is (member "-u" called-args :test #'string=))
      (is (member "testuser:testpass" called-args :test #'string=)))))

;;* HTML Conversion
(test hyperfractal-html-to-markdown-test
  "Test converting HTML to Markdown (mocked LLM)."
  (let ((hactar::*current-model* (hactar::make-model-config 
                                   :name "test" 
                                   :provider "ollama" 
                                   :model-name "test"
                                   :max-output-tokens 4096)))
    (with-dynamic-stubs ((llm:complete
                          (lambda (provider messages &key model max-tokens system-prompt response-format stream)
                            (declare (ignore provider messages model max-tokens system-prompt response-format stream))
                            "# Converted Content

This is the main content extracted from the page.")))
      (let ((result (hactar::hyperfractal-html-to-markdown "<html><body><h1>Title</h1><p>Content</p></body></html>" "https://example.com")))
        (is (search "Converted Content" result))))))

(test hyperfractal-html-to-org-test
  "Test converting HTML to Org-mode (mocked LLM)."
  (let ((hactar::*current-model* (hactar::make-model-config 
                                   :name "test" 
                                   :provider "ollama" 
                                   :model-name "test"
                                   :max-output-tokens 4096)))
    (with-dynamic-stubs ((llm:complete
                          (lambda (provider messages &key model max-tokens system-prompt response-format stream)
                            (declare (ignore provider messages model max-tokens system-prompt response-format stream))
                            "* Converted Content

This is the main content.")))
      (let ((result (hactar::hyperfractal-html-to-org "<html><body><h1>Title</h1></body></html>" "https://example.com")))
        (is (search "Converted Content" result))))))

(test hyperfractal-convert-html-dispatch-test
  "Test that convert-html dispatches based on format."
  (let ((hactar::*current-model* (hactar::make-model-config 
                                   :name "test" 
                                   :provider "ollama" 
                                   :model-name "test"
                                   :max-output-tokens 4096)))
    (with-dynamic-stubs ((llm:complete
                          (lambda (provider messages &key model max-tokens system-prompt response-format stream)
                            (declare (ignore provider messages model max-tokens response-format stream))
                            (if (search "Markdown" system-prompt)
                                "# Markdown"
                                "* Org"))))
      (let ((md-result (hactar::hyperfractal-convert-html "<html></html>" "https://test.com" :format "markdown")))
        (is (search "Markdown" md-result)))
      
      (let ((org-result (hactar::hyperfractal-convert-html "<html></html>" "https://test.com" :format "org")))
        (is (search "Org" org-result))))))

(test hyperfractal-convert-html-unknown-format-test
  "Test that unknown format signals an error."
  (signals error (hactar::hyperfractal-convert-html "<html></html>" "https://test.com" :format "unknown")))

;;* Browse Function
(test hyperfractal-browse-cached-test
  "Test that browse returns cached content."
  (hactar::hyperfractal-cache-clear)
  (setf hactar::*hyperfractal-history* '())
  
  (hactar::hyperfractal-cache-put "https://cached.com" "markdown" "# Cached Content")
  (let ((result (hactar::hyperfractal-browse "https://cached.com" :format "markdown")))
    (is (string= result "# Cached Content"))
    (is (member "https://cached.com" hactar::*hyperfractal-history* :test #'string=))))

(test hyperfractal-browse-no-cache-test
  "Test browsing without using cache."
  (hactar::hyperfractal-cache-clear)
  (setf hactar::*hyperfractal-history* '())
  
  (hactar::hyperfractal-cache-put "https://cached.com" "markdown" "# Old Content")
  
  (let ((hactar::*current-model* (hactar::make-model-config 
                                   :name "test" 
                                   :provider "ollama" 
                                   :model-name "test"
                                   :max-output-tokens 4096)))
    (with-dynamic-stubs ((hactar::hyperfractal-fetch-url
                          (lambda (url)
                            (declare (ignore url))
                            "<html><body>Fresh</body></html>"))
                         (llm:complete
                          (lambda (provider messages &key model max-tokens system-prompt response-format stream)
                            (declare (ignore provider messages model max-tokens system-prompt response-format stream))
                            "# Fresh Content")))
      (let ((result (hactar::hyperfractal-browse "https://cached.com" :format "markdown" :use-cache nil)))
        (is (search "Fresh Content" result))))))

(test hyperfractal-browse-fetch-failure-test
  "Test browse returns nil when fetch fails."
  (hactar::hyperfractal-cache-clear)
  
  (with-dynamic-stubs ((hactar::hyperfractal-fetch-url
                        (lambda (url)
                          (declare (ignore url))
                          nil)))
    (let ((result (hactar::hyperfractal-browse "https://fails.com")))
      (is (null result)))))

;;* Slash Commands
(test hyperfractal-commands-exist-test
  "Test that Hyperfractal slash commands are registered."
  (is-true (gethash "/hf" hactar::*commands*))
  (is-true (gethash "/hf-back" hactar::*commands*))
  (is-true (gethash "/hf-forward" hactar::*commands*))
  (is-true (gethash "/hf-links" hactar::*commands*))
  (is-true (gethash "/hf-goto" hactar::*commands*))
  (is-true (gethash "/hf-cache-clear" hactar::*commands*)))

(test hyperfractal-hf-command-test
  "Test /hf command with mocked browse."
  (with-dynamic-stubs ((hactar::hyperfractal-init (lambda () nil))
                       (hactar::hyperfractal-browse-and-print
                        (lambda (url &key format)
                          (format t "Browsing ~A with format ~A" url format))))
    (let* ((output (make-string-output-stream))
           (*standard-output* output)
           (cmd-fn (first (gethash "/hf" hactar::*commands*))))
      (funcall cmd-fn '("https://example.com"))
      (let ((out (get-output-stream-string output)))
        (is (search "https://example.com" out))))))

(test hyperfractal-hf-links-command-test
  "Test /hf-links command displays links."
  (setf hactar::*hyperfractal-history* '("https://test.com"))
  (hactar::hyperfractal-cache-clear)
  (hactar::hyperfractal-cache-put "https://test.com" "markdown" 
                                   "[Link 1](https://link1.com) and [Link 2](https://link2.com)")
  
  (let* ((output (make-string-output-stream))
         (*standard-output* output)
         (hactar::*hyperfractal-default-format* "markdown")
         (cmd-fn (first (gethash "/hf-links" hactar::*commands*))))
    (funcall cmd-fn '())
    (let ((out (get-output-stream-string output)))
      (is (search "Link 1" out))
      (is (search "Link 2" out))
      (is (search "link1.com" out))
      (is (search "link2.com" out)))))

(test hyperfractal-hf-cache-clear-command-test
  "Test /hf-cache-clear command."
  (hactar::hyperfractal-cache-put "https://test.com" "md" "content")
  (is (> (hactar::hyperfractal-cache-size-bytes) 0))
  
  (let* ((output (make-string-output-stream))
         (*standard-output* output)
         (cmd-fn (first (gethash "/hf-cache-clear" hactar::*commands*))))
    (funcall cmd-fn '())
    (is (= 0 (hactar::hyperfractal-cache-size-bytes)))
    (is (search "cleared" (get-output-stream-string output)))))

(test hyperfractal-hf-back-no-history-test
  "Test /hf-back when there's no history."
  (setf hactar::*hyperfractal-history* '())
  (setf hactar::*hyperfractal-history-position* 0)
  
  (let* ((output (make-string-output-stream))
         (*standard-output* output)
         (cmd-fn (first (gethash "/hf-back" hactar::*commands*))))
    (funcall cmd-fn '())
    (is (search "No previous page" (get-output-stream-string output)))))

(test hyperfractal-hf-forward-no-history-test
  "Test /hf-forward when there's no forward history."
  (setf hactar::*hyperfractal-history* '("https://current.com"))
  (setf hactar::*hyperfractal-history-position* 0)
  
  (let* ((output (make-string-output-stream))
         (*standard-output* output)
         (cmd-fn (first (gethash "/hf-forward" hactar::*commands*))))
    (funcall cmd-fn '())
    (is (search "No next page" (get-output-stream-string output)))))

(test hyperfractal-hf-goto-invalid-number-test
  "Test /hf-goto with invalid number."
  (setf hactar::*hyperfractal-history* '("https://test.com"))
  (hactar::hyperfractal-cache-clear)
  (hactar::hyperfractal-cache-put "https://test.com" "markdown" "[Link](https://link.com)")
  
  (let* ((output (make-string-output-stream))
         (*standard-output* output)
         (hactar::*hyperfractal-default-format* "markdown")
         (cmd-fn (first (gethash "/hf-goto" hactar::*commands*))))
    (funcall cmd-fn '("999"))
    (is (search "Invalid link number" (get-output-stream-string output)))))

;;* Web Command Registration
(test hyperfractal-browse-webcommand-test
  "Test that browse web command is registered."
  (let ((web-cmd (gethash "browse" hactar::*web-commands*)))
    (is-true web-cmd)
    (is (string= (hactar::web-command-name web-cmd) "browse"))))
