(in-package :hactar-tests)
(def-suite utils-tests
	   :description "Tests for utils.lisp functions.")

(in-suite utils-tests)

(test get-free-args-test
  "Extract free args after a subcommand, skipping known options and flags."
  (with-dynamic-stubs ((uiop:command-line-arguments (lambda ()
                                                      '("hactar" "agent" "--model" "m"
                                                        "-p" "4005" "--sonnet" "run" "free1" "free2"))))
    (let ((res (hactar::get-free-args "agent")))
      (is (equal res '("run" "free1" "free2"))))))

(test push-end-test
  "Append item to end of list."
  (is (equal (hactar::push-end 3 '(1 2)) '(1 2 3)))
  (is (equal (hactar::push-end 1 '()) '(1))))

(test split-join-lines-test
  "Split and join lines preserving empties."
  (let* ((s "a\n\nb")
         (lines (hactar::split-lines s)))
    (is (equal lines '("a" "" "b")))
    (is (string= (hactar::join-lines lines) "a
b"))))

(test prefix-utils-test
  "Test remove-prefix and string-prefix-p."
  (is-true (hactar::string-prefix-p "pre" "prefix"))
  (is (string= (hactar::remove-prefix "pre" "prefix") "fix"))
  (is (string= (hactar::remove-prefix "x" "prefix") "prefix"))
  (is (null (hactar::string-prefix-p "longer" "short"))))

(test join-strings-test
  "Join strings with custom separator."
  (is (string= (hactar::join-strings ", " '("a" "b" "c")) "a, b, c"))
  (is (string= (hactar::join-strings "" '("x" "y")) "xy"))
  (is (string= (hactar::join-strings " / " '("only")) "only")))

(test extract-md-fenced-code-block-test
  "Extract language, optional filename, and contents from a fenced code block."
  (let* ((text "Intro
```python src/file.py
(print \"x\")
```
Outro")
         (blk (hactar::extract-md-fenced-code-block text)))
    (is (string= (cdr (assoc :lang blk)) "python"))
    (is (string= (cdr (assoc :filename blk)) "src/file.py"))
    (is (string= (cdr (assoc :contents blk)) "(print \"x\")"))))

(test file-io-utils-test
  "Write and read file content with UTF-8."
  (uiop:with-temporary-file (:pathname p :keep t)
    (let ((content "héllo UTF-8 ✓"))
      (is-true (hactar::write-file-content p content))
      (is (string= (hactar::read-file-content p) content))
      (is (string= (hactar::get-file-content p) content)))))

(test to-json-test
  "Encode an alist to JSON with downcased keys."
  (let ((json (hactar::to-json '((:foo . 1) (:bar . "baz")))))
    (is-true (stringp json))
    (is-true (str:containsp "\"foo\"" json))
    (is-true (str:containsp "\"bar\"" json))
    (is-true (str:containsp "1" json))
    (is-true (str:containsp "\"baz\"" json))))

(test models-and-prompts-path-test
  "Paths derived from XDG config directory."
  (let ((tmp (make-temp-dir)))
    (with-dynamic-stubs ((hactar::get-xdg-config-dir (lambda () (uiop:parse-native-namestring tmp))))
      (let* ((models (hactar::get-models-config-path))
             (prompt (hactar::get-prompt-path "summary.org"))
             (models-str (uiop:native-namestring models))
             (prompt-str (uiop:native-namestring prompt)))
        (is-true (str:ends-with? "/hactar/models.yaml" models-str))
        (is-true (str:ends-with? "/hactar/prompts/summary.org" prompt-str))))))

(test mime-utils-test
  "MIME type detection and image file predicate."
  (is (string= (hactar::get-mime-type (uiop:parse-native-namestring "/x/y/a.JPG")) "image/jpeg"))
  (is (string= (hactar::get-mime-type (uiop:parse-native-namestring "img.png")) "image/png"))
  (is (null (hactar::get-mime-type (uiop:parse-native-namestring "file.unknown"))))
  (is-true (hactar::is-image-file? (uiop:parse-native-namestring "p.webp")))
  (is (null (hactar::is-image-file? (uiop:parse-native-namestring "doc.txt")))))

(test calculate-target-dimensions-test
  "Ensure aspect-ratio-based target dimensions."
  ;; 1:1
  (is (equal (hactar::calculate-target-dimensions 1000 1000) '(1092 1092)))
  ;; 3:4
  (is (equal (hactar::calculate-target-dimensions 900 1200) '(951 1268)))
  ;; Default branch preserves ratio approximately
  (let* ((dims (hactar::calculate-target-dimensions 400 4000))
         (w (first dims))
         (h (second dims))
         (ratio (if (zerop h) 0 (/ (float w) h))))
    (is (< (abs (- ratio 0.1)) 0.02))))

(test split-content-test
  "Split content into chunks preferring paragraph breaks."
  (let* ((content "Para1 line1\n\nPara2 line1\nPara2 line2\n\nPara3")
         (chunks (hactar::split-content content 20)))
    (is-true (listp chunks))
    (is (> (length chunks) 1))
    (is (string= (str:join "" chunks) content))))

(test resolve-starter-path-test
  "Resolve starter path from env or default under *hactar-data-path*."
  ;; Env var wins
  (with-dynamic-stubs ((uiop:getenv (lambda (name)
                                      (if (string= name "HACTAR_STARTERS_REACT_PATH")
                                          "/custom/React.org" nil))))
    (is (string= (hactar::resolve-starter-path "react") "/custom/React.org")))
  ;; Fallback to *hactar-data-path*/starters/React.org
  (let ((tmp (make-temp-dir)))
    (let ((hactar::*hactar-data-path* (uiop:parse-native-namestring tmp)))
      (with-dynamic-stubs ((uiop:getenv (lambda (name) (declare (ignore name)) nil)))
        (let* ((p (hactar::resolve-starter-path "react"))
               (s (uiop:native-namestring p)))
          (is-true (str:ends-with? "/starters/React.org" s)))))))

(test normalize-completion-test
  "Extract code block content or strip Completion: prefix."
  (let ((with-code "text
```bash
echo hi
```")
        (with-prefix "Completion: do the thing"))
    (is (string= (hactar::normalize-completion with-code) "echo hi"))
    (is (string= (hactar::normalize-completion with-prefix) "do the thing"))
    (is (string= (hactar::normalize-completion "raw text") "raw text"))))

(test path-and-dir-utils-test
  "Test %to-pathname, %ensure-dir, and %dir-writable-p."
  (let ((tmp (make-temp-dir)))
    (let* ((dir (format nil "~A/sub/dir/" tmp))
           (ens (hactar::%ensure-dir dir)))
      (is-true (typep (hactar::%to-pathname dir) 'pathname))
      (is-true (probe-file ens))
      (is-true (hactar::%dir-writable-p ens)))))

(test copy-to-clipboard-test
  "Prefer wl-copy when available; ensure run-program invoked."
  (let ((calls 0))
    (with-dynamic-stubs ((hactar::find-executable (lambda (name) (string= name "wl-copy")))
                         (uiop:run-program (lambda (args &key &allow-other-keys)
                                             (declare (ignore args))
                                             (incf calls)
                                             (values "" "" 0))))
      (hactar::copy-to-clipboard "Hello")
      (is (= calls 1)))))

(test debug-log-test
  "When *debug* is true, writes to *debug-stream*."
  (let ((hactar::*debug* t)
        (hactar::*debug-stream* (make-string-output-stream)))
    (hactar::debug-log "Hello" 123 "world")
    (let ((out (get-output-stream-string hactar::*debug-stream*)))
      (is-true (str:containsp "Hello 123 world" out)))))

(test colorize-and-logs-test
  "ANSI colorization and log helpers output."
  ;; colorize returns string with ESC codes
  (let ((c (hactar::colorize "X" :red)))
    (is-true (str:containsp "[31m" c)))
  ;; Capture output of log-good and log-warning
  (let* ((buf (make-string-output-stream))
         (*standard-output* buf))
    (hactar::log-good "It worked ~A" 42)
    (hactar::log-warning "Uh oh ~A" "nope")
    (let ((s (get-output-stream-string buf)))
      (is-true (str:containsp "Good:" s))
      (is-true (str:containsp "Warning:" s)))))

(test case-conversion-test
  "Test kebab-case, pascal-case, and camel-case conversions."
  ;; kebab-case tests
  (is (string= (hactar::kebab-case "MyComponent") "my-component"))
  (is (string= (hactar::kebab-case "XMLParser") "xmlparser"))
  (is (string= (hactar::kebab-case "getUserData") "get-user-data"))
  (is (string= (hactar::kebab-case "simple") "simple"))
  (is (string= (hactar::kebab-case "ALLCAPS") "allcaps"))
  (is (string= (hactar::kebab-case "") ""))
  
  ;; pascal-case tests
  (is (string= (hactar::pascal-case "my-component") "MyComponent"))
  (is (string= (hactar::pascal-case "get_user_data") "GetUserData"))
  (is (string= (hactar::pascal-case "hello world") "HelloWorld"))
  (is (string= (hactar::pascal-case "simple") "Simple"))
  (is (string= (hactar::pascal-case "already-kebab") "AlreadyKebab"))
  (is (string= (hactar::pascal-case "") ""))
  
  ;; camel-case tests
  (is (string= (hactar::camel-case "my-component") "myComponent"))
  (is (string= (hactar::camel-case "get_user_data") "getUserData"))
  (is (string= (hactar::camel-case "hello world") "helloWorld"))
  (is (string= (hactar::camel-case "simple") "simple"))
  (is (string= (hactar::camel-case "already-kebab") "alreadyKebab"))
  (is (null (hactar::camel-case ""))))

(test chunk-for-llm-test
  "Test chunking file content based on token limits."
  (uiop:with-temporary-file (:pathname p :keep t)
    (let ((content (make-string 5000 :initial-element #\a))
          (hactar::*chunking-llm* "test-model"))
      (hactar::write-file-content p content)

      ;; Test with explicit size
      (let ((chunks (hactar::chunk-for-llm p :size-of-chunk 100)))
        ;; 100 tokens * 3 chars/token = 300 chars per chunk.
        ;; 5000 / 300 = 16.66 -> 17 chunks approx.
        (is (> (length chunks) 10))
        (is (<= (length (first chunks)) 300)))

      ;; Test with default (mocking model lookup failure -> 2048 tokens -> 6144 chars)
      ;; 5000 chars fits in one chunk of 6144 chars.
      (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name) (declare (ignore name)) nil)))
        (let ((chunks (hactar::chunk-for-llm p)))
          (is (= (length chunks) 1))
          (is (= (length (first chunks)) 5000))))

      ;; Test with model config
      (let ((mock-model (hactar::make-model-config :name "test" :provider "ollama" :model-name "test" :max-input-tokens 100)))
        (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name) (declare (ignore name)) mock-model)))
          ;; 100 max input -> 50 tokens chunk -> 150 chars.
          ;; 5000 / 150 = 33.3 -> 34 chunks approx.
          (let ((chunks (hactar::chunk-for-llm p)))
             (is (> (length chunks) 30))
             (is (<= (length (first chunks)) 150))))))))

(test process-in-chunks-test
  "Test processing chunks with LLM."
  (let ((chunks '("chunk1" "chunk2"))
        (prompt "Summarize")
        (hactar::*chunking-llm* "test-model")
        (mock-model (hactar::make-model-config :name "test" :provider "ollama" :model-name "test" :max-input-tokens 1000)))

    (with-dynamic-stubs ((hactar::find-model-by-name (lambda (name) (declare (ignore name)) mock-model))
                         (llm:complete (lambda (provider messages &key model system-prompt stream)
                                         (declare (ignore provider model system-prompt stream))
                                         ;; messages is `(((:role . "user") (:content . ,chunk)))
                                         (let* ((msg (car messages))
                                                (content (cdr (assoc :content msg))))
                                           (format nil "Processed: ~A" content)))))
      (let ((result (hactar::process-in-chunks chunks prompt)))
        (is (search "Processed: chunk1" result))
        (is (search "Processed: chunk2" result))
        (is (search (string #\Newline) result))))))

(test process-with-llm-test
  "Test orchestrating chunking and processing."
  (uiop:with-temporary-file (:pathname p :keep t)
    (hactar::write-file-content p "file content")

    (with-dynamic-stubs ((hactar::chunk-for-llm (lambda (file &key size-of-chunk)
                                                  (declare (ignore file size-of-chunk))
                                                  '("chunk")))
                         (hactar::process-in-chunks (lambda (chunks prompt)
                                                      (declare (ignore chunks prompt))
                                                      "result")))
      (is (string= (hactar::process-with-llm p "prompt") "result")))))

(test context-file-sync-test
  "Test that context file is synchronized with add/drop operations."
  (let* ((temp-dir (make-temp-dir))
         (context-file (merge-pathnames "test-context.org" temp-dir)))
    (unwind-protect
        (let ((hactar::*exposed-context-file* context-file)
              (hactar::*files* '())
              (hactar::*repo-root* temp-dir)
              (hactar::*name* "TestProject")
              (hactar::*author* "TestAuthor")
              (hactar::*language* "lisp")
              (hactar::*shell* "bash")
              (hactar::*stack* '("tech1" "tech2")))
          ;; Initialize context file
          (hactar::context-expose-upsert-project-details)
          (hactar::context-expose-upsert-files-section)

          ;; Verify project details are in file
          (let ((content (uiop:read-file-string context-file)))
            (is-true (search "Project Details" content))
            (is-true (search "TestProject" content))
            (is-true (search "TestAuthor" content)))

          ;; Add a file
          (let ((test-file (merge-pathnames "test-file.lisp" temp-dir)))
            (with-open-file (s test-file :direction :output :if-exists :supersede :if-does-not-exist :create)
              (format s "test content"))
            (hactar::add-file-to-context (uiop:native-namestring test-file))

            ;; Verify Files section exists and contains the file
            (let ((content (uiop:read-file-string context-file)))
              (is-true (search "* Files" content))
              (is-true (search (file-namestring test-file) content)))

            ;; Drop the file
            (hactar::drop-file-from-context (uiop:native-namestring test-file))

            ;; Verify file is removed from Files section
            (let ((content (uiop:read-file-string context-file)))
              (is-true (search "* Files" content))
              (is (null (search (file-namestring test-file) content))))))
      ;; Cleanup
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t)))))

(test context-variable-sync-test
  "Test that project variables are synced to context file."
  (let* ((temp-dir (make-temp-dir))
         (context-file (merge-pathnames "test-context-vars.org" temp-dir)))
    (unwind-protect
        (let ((hactar::*exposed-context-file* context-file)
              (hactar::*context-expose-hooks-installed* nil))
          ;; Install hooks
          (hactar::context-expose-install-hooks-if-needed)

          ;; Initialize with default values
          (hactar::set-context-variable :name "InitialName")
          (hactar::set-context-variable :author "InitialAuthor")

          ;; Verify initial values are in file
          (let ((content (uiop:read-file-string context-file)))
            (is-true (search "InitialName" content))
            (is-true (search "InitialAuthor" content)))

          ;; Change values
          (hactar::set-context-variable :name "UpdatedName")
          (hactar::set-context-variable :author "UpdatedAuthor")

          ;; Verify updated values are in file
          (let ((content (uiop:read-file-string context-file)))
            (is-true (search "UpdatedName" content))
            (is-true (search "UpdatedAuthor" content))))
      ;; Cleanup
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t)))))
