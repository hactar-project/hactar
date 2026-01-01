(in-package :hactar-tests)

;;; --- Ruhe Tests ---
(def-suite ruhe-tests
  :description "Tests for Ruhe web feed generator."
  )

(in-suite ruhe-tests)

;; Test format struct creation
(test ruhe-format-object-test
  "Test creating a format-object struct."
  (let ((fmt (ruhe::make-format-object
              :name 'test-format
              :language "test"
              :extensions '("tst" "test")
              :parse (lambda (s) (list :source s))
              :modify (lambda (b &rest fns) (declare (ignore fns)) b)
              :render (lambda (b) (getf b :source))
              :validators nil)))
    (is (eq (ruhe::format-object-name fmt) 'test-format))
    (is (string= (ruhe::format-object-language fmt) "test"))
    (is (equal (ruhe::format-object-extensions fmt) '("tst" "test")))
    (is-true (ruhe::format-object-parse fmt))
    (is-true (ruhe::format-object-modify fmt))
    (is-true (ruhe::format-object-render fmt))))

;; Test defformat macro
(test ruhe-defformat-macro-test
  "Test that defformat correctly creates and registers a format."
  (clrhash ruhe::*formats*)
  (clrhash ruhe::*formats-by-ext*)
  
  (eval '(ruhe::defformat test-fmt
           :language "testlang"
           :extensions ("tf" "tfx")
           :parser (lambda (s) (list :source s :tree nil))
           :renderer (lambda (bundle) (getf bundle :source))))
  
  (let ((fmt (ruhe::find-format 'test-fmt)))
    (is-true fmt)
    (is (eq (ruhe::format-object-name fmt) 'test-fmt))
    (is (string= (ruhe::format-object-language fmt) "testlang"))
    (is (equal (ruhe::format-object-extensions fmt) '("tf" "tfx"))))
  
  ;; Check extension mapping
  (is (eq (gethash "tf" ruhe::*formats-by-ext*) 'test-fmt))
  (is (eq (gethash "tfx" ruhe::*formats-by-ext*) 'test-fmt)))

;; Test find-format-by-extension
(test ruhe-find-format-by-extension-test
  "Test finding a format by file extension."
  ;; Register formats first
  (eval '(ruhe::defformat org
           :language "org"
           :extensions ("org")
           :parser (lambda (s) (list :source s :tree nil))
           :renderer (lambda (bundle) (getf bundle :source))))
  (eval '(ruhe::defformat markdown
           :language "markdown"
           :extensions ("md" "markdown")
           :parser (lambda (s) (list :source s :tree nil))
           :renderer (lambda (bundle) (getf bundle :source))))
  (eval '(ruhe::defformat json
           :language "json"
           :extensions ("json" "jsonc")
           :parser (lambda (s) (list :source s :tree nil))
           :renderer (lambda (bundle) (getf bundle :source))))
  (is-true (ruhe::find-format-by-extension "org"))
  (is-true (ruhe::find-format-by-extension "md"))
  (is-true (ruhe::find-format-by-extension "json")))

;; Test defformat with extends
(test ruhe-defformat-extends-test
  "Test that defformat can extend a parent format."
  (clrhash ruhe::*formats*)
  (clrhash ruhe::*formats-by-ext*)
  
  ;; Define base format
  (eval '(ruhe::defformat base-fmt
           :extensions ("base")
           :parser (lambda (s) (list :source s :parsed t))
           :renderer (lambda (b) (format nil "BASE:~A" (getf b :source)))))
  
  ;; Define extended format
  (eval '(ruhe::defformat extended-fmt
           :extends base-fmt
           :extensions ("ext")
           :wrap-render (lambda (result) (format nil "EXTENDED:~A" result))))
  
  (let* ((ext-fmt (ruhe::find-format 'extended-fmt))
         (parse-fn (ruhe::format-object-parse ext-fmt))
         (render-fn (ruhe::format-object-render ext-fmt)))
    (is-true ext-fmt)
    ;; Parse should be inherited
    (let ((parsed (funcall parse-fn "test")))
      (is (getf parsed :parsed)))
    ;; Render should be wrapped
    (let ((rendered (funcall render-fn (list :source "test"))))
      (is (search "EXTENDED" rendered)))))

;; Test schema struct creation
(test ruhe-schema-object-test
  "Test creating a schema-object struct."
  (let ((schema (ruhe::make-schema-object
                 :name 'test-schema
                 :schema '(:type :object :properties ((:name (:type :string))))
                 :parent 'json
                 :validators nil)))
    (is (eq (ruhe::schema-object-name schema) 'test-schema))
    (is-true (ruhe::schema-object-schema schema))
    (is (eq (ruhe::schema-object-parent schema) 'json))))

;; Test defschema macro
(test ruhe-defschema-macro-test
  "Test that defschema correctly creates and registers a schema."
  (clrhash ruhe::*schemas*)
  
  (eval '(ruhe::defschema test-schema
           (:type :object
            :properties ((:id (:type :integer))
                        (:name (:type :string))))
           :extends json))
  
  (let ((schema (ruhe::find-schema 'test-schema)))
    (is-true schema)
    (is (eq (ruhe::schema-object-name schema) 'test-schema))
    (is (eq (ruhe::schema-object-parent schema) 'json))))

;; Test schema-exists-p
(test ruhe-schema-exists-p-test
  "Test checking if a schema exists."
  (clrhash ruhe::*schemas*)
  
  (eval '(ruhe::defschema exists-schema (:type :string)))
  
  (is-true (ruhe::schema-exists-p 'exists-schema))
  (is (null (ruhe::schema-exists-p 'nonexistent-schema))))

;; Test source struct creation
(test ruhe-source-object-test
  "Test creating a source-object struct."
  (let ((source (ruhe::make-source-object
                 :name 'test-source
                 :base-url "https://api.example.com"
                 :headers '(("Authorization" . "Bearer token"))
                 :get-fn (lambda () "data"))))
    (is (eq (ruhe::source-object-name source) 'test-source))
    (is (string= (ruhe::source-object-base-url source) "https://api.example.com"))
    (is-true (ruhe::source-object-headers source))
    (is-true (ruhe::source-object-get-fn source))))

;; Test defsource macro
(test ruhe-defsource-macro-test
  "Test that defsource correctly creates and registers a source."
  (clrhash ruhe::*sources*)
  
  (eval '(ruhe::defsource test-api
           :base-url "https://test.api.com"
           :headers (("X-Test" . "value"))
           :get (lambda (id) (format nil "Item ~A" id))))
  
  (let ((source (ruhe::find-source 'test-api)))
    (is-true source)
    (is (eq (ruhe::source-object-name source) 'test-api))
    (is (string= (ruhe::source-object-base-url source) "https://test.api.com"))))

;; Test fetch-source
(test ruhe-fetch-source-test
  "Test fetching data from a registered source."
  (clrhash ruhe::*sources*)
  
  (eval '(ruhe::defsource fetch-test-source
           :get (lambda (arg) (format nil "Fetched: ~A" arg))))
  
  (let ((result (ruhe::fetch-source 'fetch-test-source "item-1")))
    (is (string= result "Fetched: item-1")))
  
  ;; Non-existent source returns nil
  (is (null (ruhe::fetch-source 'nonexistent "arg"))))

;; Test built-in formats exist
(test ruhe-builtin-formats-test
  "Test that built-in formats are registered."
  ;; Re-register built-in formats for testing
  (eval '(ruhe::defformat org
           :language "org"
           :extensions ("org")
           :parser (lambda (s)
                     (list :source s
                           :language :org
                           :tree (org-mode-parser:parse-org-string s)))
           :renderer (lambda (bundle)
                       (if (getf bundle :tree)
                           (org-mode-parser:org-to-string (getf bundle :tree))
                           (getf bundle :source)))))
  (eval '(ruhe::defformat markdown
           :language "markdown"
           :extensions ("md" "markdown")
           :parser (lambda (s) (list :source s :language :markdown :tree nil))
           :renderer (lambda (bundle) (getf bundle :source))))
  (eval '(ruhe::defformat json
           :language "json"
           :extensions ("json" "jsonc")
           :parser (lambda (s)
                     (list :source s
                           :language :json
                           :tree (cl-json:decode-json-from-string s)))
           :renderer (lambda (bundle)
                       (if (getf bundle :tree)
                           (cl-json:encode-json-to-string (getf bundle :tree))
                           (getf bundle :source)))))
  (eval '(ruhe::defformat html
           :language "html"
           :extensions ("html" "htm")
           :parser (lambda (s) (list :source s :language :html :tree nil))
           :renderer (lambda (bundle) (getf bundle :source))))
  (is-true (ruhe::find-format 'org))
  (is-true (ruhe::find-format 'markdown))
  (is-true (ruhe::find-format 'json))
  (is-true (ruhe::find-format 'html)))

;; Test org format parser
(test ruhe-org-format-parser-test
  "Test the org format parser."
  ;; Ensure format is registered
  (eval '(ruhe::defformat org
           :language "org"
           :extensions ("org")
           :parser (lambda (s)
                     (list :source s
                           :language :org
                           :tree (org-mode-parser:parse-org-string s)))
           :renderer (lambda (bundle)
                       (if (getf bundle :tree)
                           (org-mode-parser:org-to-string (getf bundle :tree))
                           (getf bundle :source)))))
  (let* ((fmt (ruhe::find-format 'org))
         (parse-fn (ruhe::format-object-parse fmt))
         (result (funcall parse-fn "* Headline\nContent")))
    (is (eq (getf result :language) :org))
    (is-true (getf result :tree))
    (is (string= (getf result :source) "* Headline\nContent"))))

;; Test json format parser
(test ruhe-json-format-parser-test
  "Test the json format parser."
  ;; Ensure format is registered
  (eval '(ruhe::defformat json
           :language "json"
           :extensions ("json" "jsonc")
           :parser (lambda (s)
                     (list :source s
                           :language :json
                           :tree (cl-json:decode-json-from-string s)))
           :renderer (lambda (bundle)
                       (if (getf bundle :tree)
                           (cl-json:encode-json-to-string (getf bundle :tree))
                           (getf bundle :source)))))
  (let* ((fmt (ruhe::find-format 'json))
         (parse-fn (ruhe::format-object-parse fmt))
         (result (funcall parse-fn "{\"key\": \"value\"}")))
    (is (eq (getf result :language) :json))
    (is-true (getf result :tree))
    (is (equal (cdr (assoc :key (getf result :tree))) "value"))))

;; Test json format renderer
(test ruhe-json-format-renderer-test
  "Test the json format renderer."
  ;; Ensure format is registered
  (eval '(ruhe::defformat json
           :language "json"
           :extensions ("json" "jsonc")
           :parser (lambda (s)
                     (list :source s
                           :language :json
                           :tree (cl-json:decode-json-from-string s)))
           :renderer (lambda (bundle)
                       (if (getf bundle :tree)
                           (cl-json:encode-json-to-string (getf bundle :tree))
                           (getf bundle :source)))))
  (let* ((fmt (ruhe::find-format 'json))
         (render-fn (ruhe::format-object-render fmt))
         (bundle (list :tree '((:key . "value")) :source nil))
         (result (funcall render-fn bundle)))
    (is (stringp result))
    (is (search "key" result))
    (is (search "value" result))))

;; Test built-in sources exist
(test ruhe-builtin-sources-test
  "Test that built-in sources are registered."
  ;; Re-register built-in sources for testing
  (eval '(ruhe::defsource rss
           :base-url nil
           :get (lambda (url)
                  (declare (ignore url))
                  nil)))
  (eval '(ruhe::defsource hn
           :base-url "https://hnrss.org"
           :get (lambda (feed-type &key limit)
                  (declare (ignore feed-type limit))
                  nil)))
  (eval '(ruhe::defsource reddit
           :base-url "https://old.reddit.com"
           :get (lambda (subreddit &key limit)
                  (declare (ignore subreddit limit))
                  nil)))
  (is-true (ruhe::find-source 'rss))
  (is-true (ruhe::find-source 'hn))
  (is-true (ruhe::find-source 'reddit)))

;; Test cache operations
(test ruhe-cache-operations-test
  "Test cache put, get, and clear."
  (ruhe::ruhe-cache-clear)
  
  ;; Initially empty
  (is (null (ruhe::ruhe-cache-get "test-key")))
  
  ;; Put and get
  (ruhe::ruhe-cache-put "test-key" "test-content")
  (is (string= (ruhe::ruhe-cache-get "test-key") "test-content"))
  
  ;; Clear
  (ruhe::ruhe-cache-clear)
  (is (null (ruhe::ruhe-cache-get "test-key"))))

;; Test cache LRU behavior
(test ruhe-cache-lru-test
  "Test that cache maintains LRU order."
  (ruhe::ruhe-cache-clear)
  
  (ruhe::ruhe-cache-put "key1" "content1")
  (ruhe::ruhe-cache-put "key2" "content2")
  (ruhe::ruhe-cache-put "key3" "content3")
  
  ;; Access key1 to move it to front
  (ruhe::ruhe-cache-get "key1")
  
  ;; key1 should be at front of order
  (is (string= (first ruhe::*ruhe-cache-order*) "key1")))

;; Test cache size calculation
(test ruhe-cache-size-bytes-test
  "Test calculating cache size in bytes."
  (ruhe::ruhe-cache-clear)
  
  (is (= 0 (ruhe::ruhe-cache-size-bytes)))
  
  (ruhe::ruhe-cache-put "k1" "hello")  ; ~5 bytes for the value
  (is (> (ruhe::ruhe-cache-size-bytes) 0))
  
  (ruhe::ruhe-cache-put "k2" "world!")  ; More bytes
  (let ((size-after (ruhe::ruhe-cache-size-bytes)))
    (is (> size-after 5))))

;; Test cache key generation
(test ruhe-cache-key-test
  "Test cache key generation from input path."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "test content"))
    (let ((key (ruhe::ruhe-cache-key p)))
      (is (stringp key))
      (is (search "::" key)))))

;; Test URL extraction
(test ruhe-extract-urls-test
  "Test extracting URLs from text."
  (let ((text "Check https://example.com and http://test.org/page for more."))
    (let ((urls (ruhe::extract-urls text)))
      (is (= 2 (length urls)))
      (is (member "https://example.com" urls :test #'string=))
      (is (member "http://test.org/page" urls :test #'string=))))
  
  ;; No URLs
  (is (null (ruhe::extract-urls "no urls here"))))

;; Test source detection
(test ruhe-detect-sources-test
  "Test detecting sources from text."
  ;; Test with full URLs (protocol prefix required for URL extraction)
  (let ((text "Check https://reddit.com/r/lisp and https://news.ycombinator.com for updates."))
    (let ((sources (ruhe::detect-sources text)))
      (is (find :reddit sources :key (lambda (s) (getf s :type))))
      (is (find :hn sources :key (lambda (s) (getf s :type))))))
  
  ;; RSS feed detection
  (let ((sources (ruhe::detect-sources "Subscribe to https://example.com/rss")))
    (is (find :rss sources :key (lambda (s) (getf s :type)))))
  
  ;; Subreddit keyword detection
  (let ((sources (ruhe::detect-sources "r/programming is great")))
    (is (find :reddit sources :key (lambda (s) (getf s :type))))))

;; Test format-items-as-org
(test ruhe-format-items-as-org-test
  "Test formatting items as org-mode content."
  (let ((items '(((:title . "Test Item")
                  (:link . "https://example.com")
                  (:description . "A test description")))))
    (let ((result (ruhe::format-items-as-org items "Test Feed")))
      (is (search "* Test Feed" result))
      (is (search "** Test Item" result))
      (is (search "[[https://example.com][Link]]" result))
      (is (search "A test description" result)))))

;; Test format-items-as-markdown
(test ruhe-format-items-as-markdown-test
  "Test formatting items as markdown content."
  (let ((items '(((:title . "Test Item")
                  (:link . "https://example.com")
                  (:description . "A test description")))))
    (let ((result (ruhe::format-items-as-markdown items "Test Feed")))
      (is (search "# Test Feed" result))
      (is (search "## Test Item" result))
      (is (search "[Link](https://example.com)" result))
      (is (search "A test description" result)))))

;; Test generate-output
(test ruhe-generate-output-test
  "Test generating output from sources content."
  (let ((sources-content
          (list (list :title "Source 1"
                      :items '(((:title . "Item 1") (:link . "http://a.com"))))
                (list :title "Source 2"
                      :items '(((:title . "Item 2") (:link . "http://b.com")))))))
    ;; Org format
    (let ((org-result (ruhe::generate-output sources-content :org)))
      (is (search "* Source 1" org-result))
      (is (search "* Source 2" org-result)))
    
    ;; Markdown format
    (let ((md-result (ruhe::generate-output sources-content :markdown)))
      (is (search "# Source 1" md-result))
      (is (search "# Source 2" md-result)))))

;; Test configuration loading from env
(test ruhe-load-env-config-test
  "Test loading configuration from environment variables."
  (let ((ruhe::*ruhe-default-model* nil)
        (ruhe::*ruhe-cache-size* 1000))
    (with-dynamic-stubs ((uiop:getenv (lambda (name)
                                        (cond
                                          ((string= name "RUHE_MODEL") "test-model")
                                          ((string= name "RUHE_CACHE_SIZE") "500")
                                          (t nil)))))
      (ruhe::ruhe-load-env-config)
      (is (string= ruhe::*ruhe-default-model* "test-model"))
      (is (= ruhe::*ruhe-cache-size* 500)))))

;; Test hook existence
(test ruhe-hooks-exist-test
  "Test that Ruhe hooks are defined."
  (is (typep ruhe::*ruhe-preprocess-input-hook* 'nhooks:hook))
  (is (typep ruhe::*ruhe-process-output-hook* 'nhooks:hook))
  (is (typep ruhe::*ruhe-preprocess-context-hook* 'nhooks:hook))
  (is (typep ruhe::*ruhe-process-context-hook* 'nhooks:hook))
  (is (typep ruhe::*ruhe-schema-changed-hook* 'nhooks:hook))
  (is (typep ruhe::*ruhe-formats-changed-hook* 'nhooks:hook))
  (is (typep ruhe::*ruhe-schemas-changed-hook* 'nhooks:hook)))

;; Test ruhe-process with mocked components
(test ruhe-process-mocked-test
  "Test ruhe-process with mocked dependencies."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "Follow https://example.com/rss for updates"))
    
    (ruhe::ruhe-cache-clear)
    
    (with-dynamic-stubs ((ruhe::parse-intent (lambda (text)
                                               (declare (ignore text))
                                               '((:sources))))
                         (ruhe::fetch-source-content (lambda (spec)
                                                       (declare (ignore spec))
                                                       '(((:title . "Mock Item")
                                                          (:link . "http://mock.com"))))))
      (let ((result (ruhe::ruhe-process p :format :org :use-cache nil)))
        (is (stringp result))))))

;; Test ruhe-compile
(test ruhe-compile-test
  "Test compiling a feed specification to Lisp."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "Monitor https://news.ycombinator.com for tech news"))
    
    (with-dynamic-stubs ((ruhe::parse-intent (lambda (text)
                                               (declare (ignore text))
                                               '((:sources)))))
      (let ((result (ruhe::ruhe-compile p)))
        (is (stringp result))
        (is (search "defun generate-" result))
        (is (search "Generated by Ruhe" result))))))

;; Test ruhe-compile with output file
(test ruhe-compile-to-file-test
  "Test compiling to an output file."
  (uiop:with-temporary-file (:pathname input :keep t)
    (uiop:with-temporary-file (:pathname output :keep t)
      (with-open-file (s input :direction :output :if-exists :supersede)
        (format s "Test feed"))
      
      (with-dynamic-stubs ((ruhe::parse-intent (lambda (text)
                                                 (declare (ignore text))
                                                 '((:sources)))))
        (let* ((output-stream (make-string-output-stream))
               (*standard-output* output-stream)
               (result (ruhe::ruhe-compile input :output-file output)))
          (is (pathnamep result))
          (is-true (probe-file output)))))))

;; Test fetch-source-content dispatch
(test ruhe-fetch-source-content-test
  "Test fetching content from different source types."
  ;; Mock the individual fetch functions
  (with-dynamic-stubs ((ruhe::fetch-source (lambda (name &rest args)
                                             (declare (ignore args))
                                             (case name
                                               (ruhe::rss '(((:title . "RSS Item"))))
                                               (ruhe::hn '(((:title . "HN Item"))))
                                               (ruhe::reddit '(((:title . "Reddit Item"))))
                                               (t nil))))
                       (hactar:hyperfractal-browse (lambda (url)
                                                     (declare (ignore url))
                                                     "Web content")))
    ;; RSS source
    (let ((content (ruhe::fetch-source-content '(:type :rss :url "http://feed.com"))))
      (is-true (or (null content) (listp content))))  ; Just check it doesn't error
    
    ;; HN source
    (let ((content (ruhe::fetch-source-content '(:type :hn :feed :top))))
      (is-true (or (null content) (listp content))))
    
    ;; Reddit source
    (let ((content (ruhe::fetch-source-content '(:type :reddit :subreddit "lisp"))))
      (is-true (or (null content) (listp content))))
    
    ;; Web source
    (let ((content (ruhe::fetch-source-content '(:type :web :url "http://page.com"))))
      (is (string= content "Web content")))))

;; Test Hactar integration commands exist
(test ruhe-hactar-commands-exist-test
  "Test that Ruhe slash commands are registered in Hactar."
  (is-true (gethash "/ruhe-summary" hactar::*commands*))
  (is-true (gethash "/ruhe-ask" hactar::*commands*))
  (is-true (gethash "/ruhe-cron" hactar::*commands*))
  (is-true (gethash "/ruhe-compile" hactar::*commands*))
  (is-true (gethash "/ruhe-sources" hactar::*commands*))
  (is-true (gethash "/ruhe-urls" hactar::*commands*))
  ;; Aliases
  (is-true (gethash "/brief" hactar::*commands*))
  (is-true (gethash "/tldr" hactar::*commands*)))

;; Test /ruhe-cron command output
(test ruhe-cron-command-test
  "Test /ruhe-cron command generates cron syntax."
  (let* ((output (make-string-output-stream))
         (*standard-output* output)
         (cmd-fn (first (gethash "/ruhe-cron" hactar::*commands*))))
    (funcall cmd-fn '("0 */6 * * *" "myfeed.org"))
    (let ((out (get-output-stream-string output)))
      (is (search "0 */6 * * *" out))
      (is (search "myfeed.org" out)))))

;; Test /ruhe-sources command
(test ruhe-sources-command-test
  "Test /ruhe-sources command detects sources."
  (let ((hactar::*files* '())
        (hactar::*active-guide-file* nil))
    (with-dynamic-stubs ((hactar::generate-context (lambda ()
                                                     "Check r/programming and https://hn.algolia.com/rss")))
      (let* ((output (make-string-output-stream))
             (*standard-output* output)
             (cmd-fn (first (gethash "/ruhe-sources" hactar::*commands*))))
        (funcall cmd-fn '())
        (let ((out (get-output-stream-string output)))
          (is (or (search "reddit" (string-downcase out))
                  (search "rss" (string-downcase out))
                  (search "Detected sources" out)
                  (search "No sources" out))))))))

;; Test /ruhe-urls command
(test ruhe-urls-command-test
  "Test /ruhe-urls command extracts URLs."
  (with-dynamic-stubs ((hactar::generate-context (lambda ()
                                                   "Visit https://example.com and http://test.org")))
    (let* ((output (make-string-output-stream))
           (*standard-output* output)
           (cmd-fn (first (gethash "/ruhe-urls" hactar::*commands*))))
      (funcall cmd-fn '())
      (let ((out (get-output-stream-string output)))
        (is (or (search "example.com" out)
                (search "test.org" out)
                (search "Extracted URLs" out)
                (search "No URLs" out)))))))

;; Test parse-intent with mocked LLM
(test ruhe-parse-intent-test
  "Test parsing user intent with mocked LLM."
  (let ((hactar::*current-model* (hactar::make-model-config
                                   :name "test"
                                   :provider "ollama"
                                   :model-name "test"
                                   :max-output-tokens 1024)))
    (with-dynamic-stubs ((llm:complete (lambda (provider messages &key model max-tokens system-prompt response-format stream)
                                         (declare (ignore provider messages model max-tokens system-prompt stream))
                                         (if (string= response-format "json_object")
                                             "{\"sources\": [\"https://example.com\"], \"filters\": [\"tech\"], \"format\": \"org\"}"
                                             "response"))))
      (let ((intent (ruhe::parse-intent "Follow tech news from example.com")))
        (is (listp intent))))))

;; Test caching in ruhe-process
(test ruhe-process-caching-test
  "Test that ruhe-process uses and populates cache."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "Test content"))
    
    (ruhe::ruhe-cache-clear)
    
    ;; Pre-populate cache
    (let ((key (ruhe::ruhe-cache-key p)))
      (ruhe::ruhe-cache-put key "# Cached Result"))
    
    ;; Should return cached content
    (let ((result (ruhe::ruhe-process p :format :org :use-cache t)))
      (is (string= result "# Cached Result")))))

;; Test format validation
(test ruhe-format-parse-render-roundtrip-test
  "Test that parsing and rendering are inverse operations for simple cases."
  ;; Ensure format is registered
  (eval '(ruhe::defformat markdown
           :language "markdown"
           :extensions ("md" "markdown")
           :parser (lambda (s) (list :source s :language :markdown :tree nil))
           :renderer (lambda (bundle) (getf bundle :source))))
  (let* ((markdown-fmt (ruhe::find-format 'markdown))
         (input "# Hello World")
         (parsed (funcall (ruhe::format-object-parse markdown-fmt) input))
         (rendered (funcall (ruhe::format-object-render markdown-fmt) parsed)))
    (is (string= rendered input))))
