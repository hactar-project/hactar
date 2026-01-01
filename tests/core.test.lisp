(in-package :hactar-tests)
(in-suite hactar-tests)

;;; Helper Functions for Hactar Core Tests
;; Helper to fetch a doc directly for verification using the current connection settings
(defun get-doc-by-id (id)
  (car (hactar::docs-find :id id)))

;;; Model Changed Hook Tests
(test model-changed-hook-fires
  "Test that *model-changed-hook* fires when set-current-model is called."
  (let ((hactar::*available-models*
          (list (hactar::make-model-config :name "test/model-a"
                                           :provider "test"
                                           :model-name "model-a"
                                           :max-input-tokens 4096
                                           :max-output-tokens 1024)
                (hactar::make-model-config :name "test/model-b"
                                           :provider "test"
                                           :model-name "model-b"
                                           :max-input-tokens 4096
                                           :max-output-tokens 1024)))
        (hactar::*current-model* nil)
        (hactar::*silent* t)
        (hook-called nil)
        (received-new nil)
        (received-old nil))
    ;; Save and replace hook
    (let ((original-hook hactar::*model-changed-hook*))
      (setf hactar::*model-changed-hook* (make-instance 'hactar::hook-model-changed))
      (nhooks:add-hook hactar::*model-changed-hook*
                       (make-instance 'nhooks:handler
                                      :fn (lambda (new-model old-model)
                                            (setf hook-called t
                                                  received-new new-model
                                                  received-old old-model))
                                      :name 'test-hook))
      (unwind-protect
           (progn
             (hactar::set-current-model "test/model-a")
             (is-true hook-called "Hook should have been called")
             (is (string= "test/model-a" (hactar::model-config-name received-new)))
             (is (null received-old) "Old model should be nil for first switch")

             ;; Switch again
             (setf hook-called nil)
             (hactar::set-current-model "test/model-b")
             (is-true hook-called "Hook should fire on second switch")
             (is (string= "test/model-b" (hactar::model-config-name received-new)))
             (is (string= "test/model-a" (hactar::model-config-name received-old))))
        (setf hactar::*model-changed-hook* original-hook)))))

(test model-changed-hook-not-fired-on-invalid-model
  "Test that *model-changed-hook* does NOT fire when model is not found."
  (let ((hactar::*available-models* nil)
        (hactar::*current-model* nil)
        (hactar::*silent* t)
        (hook-called nil))
    (let ((original-hook hactar::*model-changed-hook*))
      (setf hactar::*model-changed-hook* (make-instance 'hactar::hook-model-changed))
      (nhooks:add-hook hactar::*model-changed-hook*
                       (make-instance 'nhooks:handler
                                      :fn (lambda (new-model old-model)
                                            (declare (ignore new-model old-model))
                                            (setf hook-called t))
                                      :name 'test-hook))
      (unwind-protect
           (progn
             (hactar::set-current-model "nonexistent/model")
             (is (null hook-called) "Hook should NOT fire for invalid model"))
        (setf hactar::*model-changed-hook* original-hook)))))

;;; GPT-5-mini Rule Tests
(test gpt-5-mini-model-p-detection
  "Test that gpt-5-mini-model-p correctly identifies gpt-5-mini variants."
  (let ((mini-model (hactar::make-model-config :name "copilot/gpt-5-mini"
                                                :provider "copilot"
                                                :model-name "gpt-5-mini"
                                                :max-input-tokens 4096
                                                :max-output-tokens 1024))
        (other-model (hactar::make-model-config :name "anthropic/claude-3"
                                                 :provider "anthropic"
                                                 :model-name "claude-3"
                                                 :max-input-tokens 4096
                                                 :max-output-tokens 1024))
        (mini-variant (hactar::make-model-config :name "openai/gpt-5-mini-latest"
                                                  :provider "openai"
                                                  :model-name "gpt-5-mini-latest"
                                                  :max-input-tokens 4096
                                                  :max-output-tokens 1024)))
    (is-true (hactar::gpt-5-mini-model-p mini-model))
    (is-true (hactar::gpt-5-mini-model-p mini-variant))
    (is (null (hactar::gpt-5-mini-model-p other-model)))
    (is (null (hactar::gpt-5-mini-model-p nil)))))

(test gpt-5-mini-read-file-rule-activates
  "Test that switching to gpt-5-mini activates the read_file rule."
  (let ((hactar::*available-models*
          (list (hactar::make-model-config :name "copilot/gpt-5-mini"
                                           :provider "copilot"
                                           :model-name "gpt-5-mini"
                                           :max-input-tokens 4096
                                           :max-output-tokens 1024)
                (hactar::make-model-config :name "anthropic/claude-3"
                                           :provider "anthropic"
                                           :model-name "claude-3"
                                           :max-input-tokens 4096
                                           :max-output-tokens 1024)))
        (hactar::*current-model* nil)
        (hactar::*silent* t)
        (hactar::*active-rules* (make-hash-table :test 'equal)))
    ;; Switch to gpt-5-mini
    (hactar::set-current-model "copilot/gpt-5-mini")
    ;; Rule should now be active
    (let ((rule-text (gethash 'hactar::gpt-5-mini-read-file-rule hactar::*active-rules*)))
      (is-true rule-text "Rule should be set in *active-rules*")
      (is (search "read_file" rule-text) "Rule text should mention read_file")
      (is (search "MUST" rule-text) "Rule text should contain MUST"))))

(test gpt-5-mini-read-file-rule-deactivates
  "Test that switching away from gpt-5-mini removes the read_file rule."
  (let ((hactar::*available-models*
          (list (hactar::make-model-config :name "copilot/gpt-5-mini"
                                           :provider "copilot"
                                           :model-name "gpt-5-mini"
                                           :max-input-tokens 4096
                                           :max-output-tokens 1024)
                (hactar::make-model-config :name "anthropic/claude-3"
                                           :provider "anthropic"
                                           :model-name "claude-3"
                                           :max-input-tokens 4096
                                           :max-output-tokens 1024)))
        (hactar::*current-model* nil)
        (hactar::*silent* t)
        (hactar::*active-rules* (make-hash-table :test 'equal)))
    ;; First activate the rule
    (hactar::set-current-model "copilot/gpt-5-mini")
    (is-true (gethash 'hactar::gpt-5-mini-read-file-rule hactar::*active-rules*)
             "Rule should be active after switching to gpt-5-mini")
    ;; Now switch to a different model
    (hactar::set-current-model "anthropic/claude-3")
    (is (null (gethash 'hactar::gpt-5-mini-read-file-rule hactar::*active-rules*))
        "Rule should be removed after switching away from gpt-5-mini")))

;;; Hactar Core Tests
(test add-file-to-context-test
  "Test adding a file to the context window."
  (let ((hactar::*files* '())
        (hactar::*current-model* nil)
        (hactar::*exposed-context-file* nil))
    (hactar::add-file-to-context "/path/to/file1.txt")
    (is (member "/path/to/file1.txt" hactar::*files* :test #'string=))
    (hactar::add-file-to-context "/path/to/file2.txt")
    (is (= 2 (length hactar::*files*)))
    (hactar::add-file-to-context "/path/to/file1.txt")
    (is (= 2 (length hactar::*files*)))))

(test drop-file-from-context-test
  "Test removing a file from the context window."
  (let ((hactar::*files* '("/path/to/file1.txt" "/path/to/file2.txt"))
        (hactar::*exposed-context-file* nil))
    (hactar::drop-file-from-context "/path/to/file1.txt")
    (is (= 1 (length hactar::*files*)))
    (is (not (member "/path/to/file1.txt" hactar::*files* :test #'string=)))
    (hactar::drop-file-from-context "/path/to/file2.txt")
    (is (null hactar::*files*))))

;; Test get-language-hint-from-extension
(test language-hint-test
  "Test mapping file extensions to language hints."
  (is (string= (hactar::get-language-hint-from-extension "lisp") "lisp"))
  (is (string= (hactar::get-language-hint-from-extension "py") "python"))
  (is (string= (hactar::get-language-hint-from-extension "js") "javascript"))
  (is (string= (hactar::get-language-hint-from-extension "rs") "rust"))
  (is (string= (hactar::get-language-hint-from-extension "md") "markdown"))
  (is (string= (hactar::get-language-hint-from-extension "json") "json"))
  (is (string= (hactar::get-language-hint-from-extension "yaml") "yaml"))
  (is (string= (hactar::get-language-hint-from-extension "yml") "yaml"))
  (is (string= (hactar::get-language-hint-from-extension "sql") "sql"))
  (is (string= (hactar::get-language-hint-from-extension "toml") "toml"))
  (is (string= (hactar::get-language-hint-from-extension "org") "org"))
  (is (string= (hactar::get-language-hint-from-extension "mustache") "mustache"))
  (is (string= (hactar::get-language-hint-from-extension "html") "html"))
  (is (string= (hactar::get-language-hint-from-extension "css") "css"))
  (is (string= (hactar::get-language-hint-from-extension "unknown") "unknown"))
  (is (null (hactar::get-language-hint-from-extension nil))))

;; Test get-github-raw-url (basic cases, doesn't probe URLs)
(test github-raw-url-test
  "Test converting GitHub URLs to raw content URLs."
  (is (string= (hactar::get-github-raw-url "https://github.com/user/repo/blob/main/path/to/file.txt")
               "https://raw.githubusercontent.com/user/repo/refs/heads/main/path/to/file.txt"))
  (is (string= (hactar::get-github-raw-url "https://github.com/user/repo/blob/dev-branch/src/main.rs")
               "https://raw.githubusercontent.com/user/repo/refs/heads/dev-branch/src/main.rs"))
  ;; Tree URL defaults to README.md
  (is (string= (hactar::get-github-raw-url "https://github.com/user/repo/tree/main/docs/")
               "https://raw.githubusercontent.com/user/repo/refs/heads/main/docs/README.md"))
  (is (string= (hactar::get-github-raw-url "https://github.com/user/repo/tree/main/docs")
               "https://raw.githubusercontent.com/user/repo/refs/heads/main/docs/README.md"))
  ;; Repo root URL needs probing, so we test the pattern recognition part only
  (is (null (hactar::get-github-raw-url "https://example.com"))))

;; Test parse-url-from-text
(test parse-url-test
  "Test extracting the first URL from text."
  (is (string= (hactar::parse-url-from-text "Check this out: https://example.com/page?q=1")
               "https://example.com/page?q=1"))
  (is (string= (hactar::parse-url-from-text "Another one http://test.org/path.")
               "http://test.org/path."))
  (is (null (hactar::parse-url-from-text "No URL here.")))
  (is (string= (hactar::parse-url-from-text "First URL https://first.com then https://second.com")
               "https://first.com"))
  (is (string= (hactar::parse-url-from-text "URL with fragment http://site.com#section")
               "http://site.com#section")))

;;; Test parse-metadata-args
(test parse-metadata-args-test
  "Test parsing metadata arguments from a list of strings."
  ;; Valid input with tags and covers
  (is (equalp (hactar::parse-metadata-args '("(" ":tags" "'(\"tag1\" \"tag2\")" ":covers" "'(\"coverA\")" ")"))
              '(:tags '("tag1" "tag2") :covers '("coverA")))) ; Expect quoted lists
  ;; Valid input with only tags
  (is (equalp (hactar::parse-metadata-args '("(" ":tags" "'(\"api\" \"backend\")" ")"))
              '(:tags '("api" "backend")))) ; Expect quoted list
  ;; Valid input with only covers
  (is (equalp (hactar::parse-metadata-args '("(" ":covers" "'(\"react@18\")" ")"))
              '(:covers '("react@18")))) ; Expect quoted list
  ;; Valid input with meta (JSON string) - JSON string is read as a string, no change needed

  ;; Valid input with mixed symbols and strings in lists
  (is (equalp (hactar::parse-metadata-args '("(" ":tags" "'(tag-symbol \"tag-string\")" ")"))
              '(:tags '(tag-symbol "tag-string")))) ; Expect quoted list
  ;; Input without metadata args (just the source) - should return nil
  (is (null (hactar::parse-metadata-args '("source-file.txt"))))
  ;; Input list doesn't start with "("
  (is (null (hactar::parse-metadata-args '(":tags" "'(\"a\")" ")"))))
  ;; Input list doesn't end with ")"
  (is (null (hactar::parse-metadata-args '("(" ":tags" "'(\"a\")"))))
  ;; Empty input list
  (is (null (hactar::parse-metadata-args '())))
  ;; Input with invalid Lisp syntax inside

  ;; Input with non-keyword symbols
  (is (equalp (hactar::parse-metadata-args '("(" "not-a-keyword" "'(1 2)" ")"))
              '(not-a-keyword '(1 2)))) ; Expect quoted list
  ;; Input that is not a list representation
  (is (null (hactar::parse-metadata-args '("just" "some" "strings")))))

(test parse-file-blocks-test
  "Test parsing of file creation blocks from text."
  ;; Test case 1: Markdown style
  (let ((text "Here is a file to create:
```python app/main.py
```
And some other text."))
    (is (equal (hactar::parse-file-blocks text) '(((:filename . "app/main.py") (:content . ""))))))

  ;; Test case 2: Org-mode style
  (let ((text "Please make this file:
#+begin_src markdown
  ```lisp src/utils.lisp
  ```
#+end_src
Thanks."))
    (is (equal (hactar::parse-file-blocks text) '(((:filename . "src/utils.lisp") (:content . ""))))))

  ;; Test case 3: Mix of both
  (let ((text "Create these files:
```go cmd/server/main.go
```
And also:
#+begin_src text
  ```rust src/lib.rs
  ```
#+end_src
"))
    (is (equal (hactar::parse-file-blocks text) '(((:filename . "cmd/server/main.go") (:content . ""))
                                                  ((:filename . "src/lib.rs") (:content . ""))))))

  ;; Test case 4: Invalid block (markdown with content)
  (let ((text "This is not a file block:
```python
print('hello')
```"))
    (is (null (hactar::parse-file-blocks text))))

  ;; Test case 5: Invalid block (org-mode with content)
  (let ((text "This is also not a file block:
#+begin_src markdown
  ```
  some content here
  ```
#+end_src
"))
    (is (null (hactar::parse-file-blocks text))))

  ;; Test case 6: No blocks
  (let ((text "Just some regular text without any file blocks."))
    (is (null (hactar::parse-file-blocks text))))

  ;; Test case 7: Empty string
  (is (null (hactar::parse-file-blocks "")))

  ;; Test case 8: Multiple markdown blocks
  (let ((text "File one:
```file1.txt
```
File two:
```file2.txt
```"))
    (is (equal (hactar::parse-file-blocks text) '(((:filename . "file1.txt") (:content . ""))
                                                  ((:filename . "file2.txt") (:content . ""))))))

  ;; Test case 9: Block with extra whitespace
  (let ((text "
    ```  spaced/file.js
    ```
  "))
    (is (equal (hactar::parse-file-blocks text) '(((:filename . "spaced/file.js") (:content . "")))))))

;; Helper to create a dummy embedding vector of the correct size
(defun make-dummy-embedding (&optional (value 0.1) (dimensions 768))
  "Creates a list of floats of the specified dimension."
  (make-list dimensions :initial-element (float value)))

;;; Test docs-create
(test docs-create-test
  "Test creating documents in the database (uses connection bound in run-tests)."
  ;; NOTE: Table is cleared once before all tests in run-tests

  ;; Mock embedding function locally for this test using with-dynamic-stubs
  (with-dynamic-stubs ((llm:ollama-embed (lambda (text &rest args)
                                           (declare (ignore text args))
                                           (make-dummy-embedding 0.1)))) ; Use dummy embedding
    ;; Basic creation
    (let ((ids (hactar::docs-create :source "test-source-1" :title "Test Title 1" :content "Test content 1" :tags '("tagA") :covers '("coverX"))))
      (is (= 1 (length ids)))
      (let ((doc (get-doc-by-id (car ids))))
        (is (string= (cdr (assoc :source doc)) "test-source-1"))
        (is (string= (cdr (assoc :title doc)) "Test Title 1"))
        (is (string= (cdr (assoc :content doc)) "Test content 1"))
        ;; Tags and Covers are returned as JSON strings, parse them for comparison
        (is (equal (cdr (assoc :tags doc)) '("tagA")))
        (is (equal (cdr (assoc :covers doc)) '("coverX")))))

    ;; Creation with metadata
    (let* ((ids (hactar::docs-create :source "test-source-2" :title "Test Title 2" :content "Test content 2" :meta '((:dogs . "are awesome"))))
           (doc (get-doc-by-id (car ids)))
           (meta-alist (cdr (assoc :meta doc))))
      (is (= 1 (length ids)))
      (is (string= (cdr (assoc :source doc)) "test-source-2"))
      (is (string= (cdr (assoc :dogs meta-alist)) "are awesome")))))

;;; Test docs-find
(test docs-find-test
  "Test finding documents from the database (uses connection bound in run-tests)."
  ;; NOTE: Assumes data from docs-create-test might still be present if run in sequence,
  ;; or relies on setup within run-tests if run independently.
  ;; For isolation, ideally, each test block would manage its own data,
  ;; but for now, we rely on the pre-run clearing and data creation.

  ;; Add specific data needed *only* for find tests and mock embeddings
  (with-dynamic-stubs ((llm:ollama-embed (lambda (text &rest args)
                                           (declare (ignore args))
                                           (cond ((string= text "Content A") (make-dummy-embedding 0.1)) ; Use dummy embedding
                                                 ((string= text "Content B") (make-dummy-embedding 0.2)) ; Use dummy embedding
                                                 ((string= text "Content C") (make-dummy-embedding 0.3)) ; Use dummy embedding
                                                 ((string= text "find me") (make-dummy-embedding 0.15)) ; Use dummy embedding for search text
                                                 (t (make-dummy-embedding 0.9)))))) ; Default dummy embedding
    ;; Create necessary documents for testing find
    (hactar::docs-create :source "src1" :title "Doc 1" :content "Content A" :tags '("t1" "t2") :covers '("c1") :type "typeA" )
    (hactar::docs-create :source "src2" :title "Doc 2" :content "Content B" :tags '("t2" "t3") :covers '("c1" "c2") :type "typeB" )
    (hactar::docs-create :source "src3" :title "Doc 3" :content "Content C" :tags '("t1") :covers '("c3") :type "typeA")

    ;; Find by ID
    (let* ((all-docs (hactar::docs-find)) ; Get all docs to find the ID of Doc 2
           (doc2-id (cdr (assoc :id (find "src2" all-docs :key (lambda (d) (cdr (assoc :source d))) :test #'string=))))
           (results (hactar::docs-find :id doc2-id)))
      (is (= 1 (length results)))
      (is (string= (cdr (assoc :title (first results))) "Doc 2")))

    ;; Find by tags (all)
    (let ((results (hactar::docs-find :tags '("t2"))))
      (is (= 2 (length results))) ; Doc 1 and Doc 2 have "t2"
      (is (every (lambda (doc) (member "t2" (cdr (assoc :tags doc)) :test #'string=)) results)))

    ;; Find by covers (all)
    (let ((results (hactar::docs-find :covers '("c1"))))
      (is (= 2 (length results)))	; Doc 1 and Doc 2 have "c1"
      (is (every (lambda (doc) (member "c1" (cdr (assoc :covers doc)) :test #'string=)) results)))

    ;; Find by source
    (let ((results (hactar::docs-find :sources '("src1" "src3"))))
      (is (= 2 (length results)))
      (is (every (lambda (doc) (member (cdr (assoc :source doc)) '("src1" "src3") :test #'string=)) results)))

    ;; Test limit
    (let ((results (hactar::docs-find :limit 1)))
      (is (= 1 (length results))))))

;;; Test errors-create
(test errors-create-test
  "Test creating errors in the database."
  (with-dynamic-stubs ((llm:ollama-embed (lambda (text &rest args)
                                           (declare (ignore text args))
                                           (make-dummy-embedding 0.1))))
    (let ((id (hactar::errors-create :code "ERR001"
				     :stack "test"
                                     :title "Something went wrong"
                                     :message "Something went wrong message"
                                     :cause "Bad input"
                                     :solution "Fix input"
                                     :tags '("ui" "input"))))
      (is (integerp id))
      (let ((err (car (hactar::errors-find :code "ERR001"))))
        (is (string= (cdr (assoc :code err)) "ERR001"))
        (is (string= (cdr (assoc :title err)) "Something went wrong"))
        (is (string= (cdr (assoc :message err)) "Something went wrong message"))
        (is (string= (cdr (assoc :cause err)) "Bad input"))
        (is (string= (cdr (assoc :solution err)) "Fix input"))
        (is (equal (cdr (assoc :tags err)) '("ui" "input")))))))

;;; Test errors-find
(test errors-find-test
  "Test finding errors in the database."
  (with-dynamic-stubs ((llm:ollama-embed (lambda (text &rest args)
                                           (declare (ignore args))
                                           (cond ((string= text "search query") (make-dummy-embedding 0.1))
                                                 (t (make-dummy-embedding 0.9))))))
    (hactar::errors-create :stack "test" :code "E1" :title "T1" :message "M1" :cause "C1" :solution "S1" :tags '("t1"))
    (hactar::errors-create :stack "test" :code "E2" :title "T2" :message "M2" :cause "C2" :solution "S2" :tags '("t2"))

    ;; Find by code
    (let ((res (hactar::errors-find :code "E1")))
      (is (= 1 (length res)))
      (is (string= (cdr (assoc :code (first res))) "E1")))

    ;; Find by tag
    (let ((res (hactar::errors-find :tags '("t1"))))
      (is (= 1 (length res)))
      (is (string= (cdr (assoc :code (first res))) "E1")))

    ;; Find by text
    (let ((res (hactar::errors-find :text "search query")))
      (is (listp res)))))

;;; Test add command helper functions
(test parse-add-args-test
  "Test parsing of add command arguments."
  (multiple-value-bind (files descriptions)
      (hactar::parse-add-args '("file1.txt" "-descriptions=desc1,desc2" "file2.jpg"))
    (is (equal files '("file1.txt" "file2.jpg")))
    (is (equal descriptions '("desc1" "desc2"))))
  
  (multiple-value-bind (files descriptions)
      (hactar::parse-add-args '("src/*.lisp"))
    (is (equal files '("src/*.lisp")))
    (is (null descriptions))))

(test expand-file-pattern-test
  "Test expansion of globular patterns."
  ;; Create temporary directory and files with predictable names
  (let* ((temp-dir (uiop:temporary-directory))
         (test-subdir (merge-pathnames (format nil "hactar-test-~A/" (get-universal-time)) temp-dir))
         (nested-subdir (merge-pathnames "nested/" test-subdir))
         (p1 (merge-pathnames "test_file_1.txt" test-subdir))
         (p2 (merge-pathnames "test_file_2.txt" test-subdir))
         (p3 (merge-pathnames "other.lisp" test-subdir))
         (p4 (merge-pathnames "nested_file.txt" nested-subdir)))
    (unwind-protect
        (progn
          ;; Create the test directories and files
          (ensure-directories-exist test-subdir)
          (ensure-directories-exist nested-subdir)
          (with-open-file (s p1 :direction :output :if-exists :supersede)
            (write-string "test1" s))
          (with-open-file (s p2 :direction :output :if-exists :supersede)
            (write-string "test2" s))
          (with-open-file (s p3 :direction :output :if-exists :supersede)
            (write-string "test3" s))
          (with-open-file (s p4 :direction :output :if-exists :supersede)
            (write-string "test4" s))
          
          ;; Test exact match (absolute path)
          (let ((res (hactar::expand-file-pattern (uiop:native-namestring p1) test-subdir)))
            (is (member (uiop:native-namestring p1) res :test #'string=)))
          
          ;; Test glob match with wildcard in filename (relative pattern)
          (let* ((res (hactar::expand-file-pattern "test_file_*.txt" test-subdir)))
            (is (member (uiop:native-namestring p1) res :test #'string=))
            (is (member (uiop:native-namestring p2) res :test #'string=))
            ;; Should NOT include other.lisp
            (is (not (member (uiop:native-namestring p3) res :test #'string=))))
          
          ;; Test directory glob pattern like "*" from root
          (let* ((res (hactar::expand-file-pattern "*" test-subdir)))
            ;; Should include all files in the directory
            (is (member (uiop:native-namestring p1) res :test #'string=))
            (is (member (uiop:native-namestring p2) res :test #'string=))
            (is (member (uiop:native-namestring p3) res :test #'string=))
            ;; Should NOT include files in nested subdirectory (single * doesn't recurse)
            (is (not (member (uiop:native-namestring p4) res :test #'string=))))
          
          ;; Test recursive glob pattern like "**/*.txt"
          (let* ((res (hactar::expand-file-pattern "**/*.txt" test-subdir)))
            ;; Should include all .txt files including nested
            (is (member (uiop:native-namestring p1) res :test #'string=))
            (is (member (uiop:native-namestring p2) res :test #'string=))
            (is (member (uiop:native-namestring p4) res :test #'string=))
            ;; Should NOT include .lisp files
            (is (not (member (uiop:native-namestring p3) res :test #'string=))))
          
          ;; Test nested directory glob like "nested/*"
          (let* ((res (hactar::expand-file-pattern "nested/*" test-subdir)))
            ;; Should only include files in nested directory
            (is (member (uiop:native-namestring p4) res :test #'string=))
            (is (not (member (uiop:native-namestring p1) res :test #'string=))))
          
          ;; Test non-existent exact file returns nil
          (let ((res (hactar::expand-file-pattern "nonexistent.txt" test-subdir)))
            (is (null res))))
      ;; Cleanup
      (ignore-errors (delete-file p1))
      (ignore-errors (delete-file p2))
      (ignore-errors (delete-file p3))
      (ignore-errors (delete-file p4))
      (ignore-errors (uiop:delete-directory-tree test-subdir :validate t)))))
