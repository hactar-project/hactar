(in-package :hactar-tests)
(def-suite context-tests
	   :description "Test context handling")
(in-suite context-tests)

(test language-test
  "Test the language function returns the current language."
  (let ((hactar::*language* "Common Lisp"))
    (is (string= (hactar::language) "Common Lisp")))
  (let ((hactar::*language* "Python"))
    (is (string= (hactar::language) "Python")))
  (let ((hactar::*language* nil))
    (is (null (hactar::language)))))

(test images-context-string-test
  "Test generation of images context string."
  (let ((hactar::*images* nil))
    (is (string= (hactar::images-context-string) "(No images added to context)")))
  
  (let ((hactar::*images* '((:path #P"/path/to/image1.png" :text nil)
                            (:path #P"/path/to/image2.jpg" :text "A screenshot"))))
    (let ((result (hactar::images-context-string)))
      (is (search "Images in Context:" result))
      (is (search "image1.png" result))
      (is (search "image2.jpg" result))
      (is (search "A screenshot" result)))))

(test docs-context-string-test
  "Test generation of documentation context string."
  (let ((hactar::*docs-context* nil))
    (is (string= (hactar::docs-context-string) "(No documentation added to context)")))
  
  (let ((hactar::*docs-context* '(((:title . "API Guide")
                                   (:source . "https://example.com/api")
                                   (:covers . ("api" "rest"))
                                   (:tags . ("documentation"))
                                   (:content . "API documentation content here")))))
    (let ((result (hactar::docs-context-string)))
      (is (search "Title: API Guide" result))
      (is (search "Source: https://example.com/api" result))
      (is (search "API documentation content here" result)))))

(test errors-context-string-test
  "Test generation of errors context string."
  (let ((hactar::*errors-context* nil))
    (is (string= (hactar::errors-context-string) "(No errors added to context)")))
  
  (let ((hactar::*errors-context* '(((:title . "Connection Error")
                                     (:code . "CONN-001")
                                     (:message . "Failed to connect")
                                     (:cause . "Network timeout")
                                     (:solution . "Check network connection")))))
    (let ((result (hactar::errors-context-string)))
      (is (search "Title: Connection Error" result))
      (is (search "Code: CONN-001" result))
      (is (search "Message: Failed to connect" result))
      (is (search "Cause: Network timeout" result))
      (is (search "Solution: Check network connection" result)))))

(test add-doc-to-context-test
  "Test adding documentation to context."
  (let ((hactar::*docs-context* nil)
        (hactar::*exposed-context-file* nil)
        (hactar::*silent* t))
    (hactar::add-doc-to-context '((:id . 1) (:title . "Doc 1") (:content . "Content 1")))
    (is (= 1 (length hactar::*docs-context*)))
    
    (hactar::add-doc-to-context '((:id . 2) (:title . "Doc 2") (:content . "Content 2")))
    (is (= 2 (length hactar::*docs-context*)))
    
    (hactar::add-doc-to-context '((:id . 1) (:title . "Doc 1 Updated") (:content . "Content 1 Updated")))
    (is (= 2 (length hactar::*docs-context*)))))

(test remove-doc-from-context-test
  "Test removing documentation from context."
  (let ((hactar::*docs-context* '(((:id . 1) (:title . "Doc 1"))
                                  ((:id . 2) (:title . "Doc 2"))))
        (hactar::*exposed-context-file* nil))
    (hactar::remove-doc-from-context 1)
    (is (= 1 (length hactar::*docs-context*)))
    (is (= 2 (cdr (assoc :id (first hactar::*docs-context*)))))
    
    (hactar::remove-doc-from-context 999)
    (is (= 1 (length hactar::*docs-context*)))))

(test add-error-to-context-test
  "Test adding errors to context."
  (let ((hactar::*errors-context* nil)
        (hactar::*exposed-context-file* nil)
        (hactar::*silent* t))
    (hactar::add-error-to-context '((:code . "ERR-001") (:title . "Error 1")))
    (is (= 1 (length hactar::*errors-context*)))
    
    (hactar::add-error-to-context '((:code . "ERR-002") (:title . "Error 2")))
    (is (= 2 (length hactar::*errors-context*)))
    
    (hactar::add-error-to-context '((:code . "ERR-001") (:title . "Error 1 Updated")))
    (is (= 2 (length hactar::*errors-context*)))))

(test remove-error-from-context-test
  "Test removing errors from context."
  (let ((hactar::*errors-context* '(((:code . "ERR-001") (:title . "Error 1"))
                                    ((:code . "ERR-002") (:title . "Error 2"))))
        (hactar::*exposed-context-file* nil))
    (hactar::remove-error-from-context "ERR-001")
    (is (= 1 (length hactar::*errors-context*)))
    (is (string= "ERR-002" (cdr (assoc :code (first hactar::*errors-context*)))))
    
    (hactar::remove-error-from-context "ERR-999")
    (is (= 1 (length hactar::*errors-context*)))))

(test add-image-to-context-test
  "Test adding images to context."
  (let ((hactar::*images* nil)
        (hactar::*silent* t))
    (with-dynamic-stubs ((hactar::check-image-size (lambda (path) (declare (ignore path)) nil)))
      (hactar::add-image-to-context "/path/to/image1.png" nil)
      (is (= 1 (length hactar::*images*)))
      
      (hactar::add-image-to-context "/path/to/image2.jpg" "A screenshot")
      (is (= 2 (length hactar::*images*)))
      
      (let ((img2 (find "/path/to/image2.jpg" hactar::*images* 
                        :key (lambda (img) (getf img :path)) :test #'equal)))
        (is (string= "A screenshot" (getf img2 :text)))))))

(test drop-image-from-context-test
  "Test removing images from context."
  (let ((hactar::*images* '((:path "/path/to/image1.png" :text nil)
                            (:path "/path/to/image2.jpg" :text "A screenshot"))))
    (hactar::drop-image-from-context "/path/to/image1.png")
    (is (= 1 (length hactar::*images*)))
    (is (string= "/path/to/image2.jpg" (getf (first hactar::*images*) :path)))
    
    (hactar::drop-image-from-context "/nonexistent.png")
    (is (= 1 (length hactar::*images*)))))

(test set-context-variable-test
  "Test setting context variables."
  (let ((hactar::*name* nil)
        (hactar::*author* nil)
        (hactar::*language* nil)
        (hactar::*shell* nil)
        (hactar::*stack* nil)
        (hactar::*exposed-context-file* nil)
        (hactar::*context-variable-changed-hook* (make-instance 'hactar::hook-context-variable-changed)))
    (hactar::set-context-variable :name "Test Project")
    (is (string= hactar::*name* "Test Project"))
    
    (hactar::set-context-variable :author "Test Author")
    (is (string= hactar::*author* "Test Author"))
    
    (hactar::set-context-variable :language "Common Lisp")
    (is (string= hactar::*language* "Common Lisp"))
    
    (hactar::set-context-variable :shell "bash")
    (is (string= hactar::*shell* "bash"))
    
    (hactar::set-context-variable :stack '("SBCL" "Quicklisp"))
    (is (equal hactar::*stack* '("SBCL" "Quicklisp")))))

(test context-expose-file-path-test
  "Test generation of exposed context file path."
  (let ((hactar::*repo-root* #P"/home/user/project/"))
    (with-dynamic-stubs ((hactar::current-pid (lambda () 12345)))
      (let ((path (hactar::context-expose-file-path)))
        (is (search ".hactar.12345.context.org" (namestring path)))
        (is (search "/home/user/project/" (namestring path)))))))

(test context-expose-generate-project-details-test
  "Test generation of project details section."
  (let ((hactar::*name* "My Project")
        (hactar::*author* "John Doe")
        (hactar::*language* "Common Lisp")
        (hactar::*shell* "bash")
        (hactar::*stack* '("SBCL" "Quicklisp")))
    (let ((result (hactar::context-expose-generate-project-details)))
      (is (search "Name: My Project" result))
      (is (search "Author: John Doe" result))
      (is (search "Language: Common Lisp" result))
      (is (search "Shell: bash" result))
      (is (search "SBCL" result))
      (is (search "Quicklisp" result)))))

(test files-context-test
  "Test files-context respects token limits."
  (uiop:with-temporary-file (:pathname p1 :stream s1 :direction :output :keep t)
    (write-string "Short content" s1)
    (finish-output s1)
    (let ((hactar::*files* (list (uiop:native-namestring p1)))
          (hactar::*repo-root* (uiop:pathname-directory-pathname p1)))
      (multiple-value-bind (content skipped included)
          (hactar::files-context nil)
        (is (search "Short content" content))
        (is (null skipped))
        (is (= 1 (length included))))
      
      (multiple-value-bind (content skipped included)
          (hactar::files-context 1)
        (is (= 1 (length skipped)))
        (is (null included))))
    ;; Cleanup
    (ignore-errors (delete-file p1))))

(test generate-context-without-files-test
  "Test generating context string without file contents."
  (let ((hactar::*repo-map* "repo map content")
        (hactar::*stack* '("SBCL"))
        (hactar::*shell* "bash")
        (hactar::*language* "Common Lisp")
        (hactar::*name* "Test")
        (hactar::*author* "Tester")
        (hactar::*docs-context* nil)
        (hactar::*errors-context* nil)
        (hactar::*images* nil))
    (with-dynamic-stubs ((hactar::get-prompt-path 
                          (lambda (name) 
                            (declare (ignore name))
                            ;; Return a simple template path that will be mocked
                            "/tmp/test-prompt.org"))
                         (uiop:read-file-string 
                          (lambda (path)
                            (declare (ignore path))
                            "Name: {{name}}, Language: {{language}}")))
      (let ((result (hactar::generate-context-without-files)))
        (is (search "Name: Test" result))
        (is (search "Language: Common Lisp" result))))))

(test context-hooks-defined-test
  "Test that context hooks are properly defined."
  (is (boundp 'hactar::*context-file-added-hook*))
  (is (boundp 'hactar::*context-file-dropped-hook*))
  (is (boundp 'hactar::*context-variable-changed-hook*)))

(test read-context-from-file-test
  "Test reading context from exposed file."
  (let ((hactar::*exposed-context-file* nil))
    (is (null (hactar::read-context-from-file))))
  
  (uiop:with-temporary-file (:pathname p :stream s :direction :output :keep t)
    (write-string "* Files\nSome context content" s)
    (finish-output s)
    (let ((hactar::*exposed-context-file* p))
      (let ((result (hactar::read-context-from-file)))
        (is (search "Files" result))
        (is (search "Some context content" result))))
    (ignore-errors (delete-file p)))
  
  (let ((hactar::*exposed-context-file* #P"/nonexistent/path/file.org"))
    (is (null (hactar::read-context-from-file)))))

;;* default-system-prompt

(test default-system-prompt-without-tools-test
  "Test default-system-prompt when tools are disabled."
  (let ((hactar::*tool-use-enabled* nil)
        (hactar::*tools-in-system-prompt* t)
        (hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*active-guide-file* nil)
        (hactar::*language* "Common Lisp")
        (hactar::*repo-map* nil)
        (hactar::*stack* '())
        (hactar::*shell* "bash")
        (hactar::*name* "TestProject")
        (hactar::*author* "Tester")
        (hactar::*files* nil)
        (hactar::*docs-context* nil)
        (hactar::*errors-context* nil)
        (hactar::*images* nil)
        (hactar::*exposed-context-file* nil))
    (clrhash hactar::*defined-tools*)
    (with-dynamic-stubs ((hactar::get-prompt-path 
                          (lambda (name) 
                            (declare (ignore name))
                            "/tmp/test-system-prompt.org"))
                         (uiop:read-file-string 
                          (lambda (path)
                            (declare (ignore path))
                            "{{#tools_enabled}}TOOLS:{{tools}}{{/tools_enabled}}Context:{{context}}")))
      (let ((result (hactar::default-system-prompt)))
        (is (not (search "TOOLS:" result)))
        (is (search "Context:" result))))))

(test default-system-prompt-with-tools-test
  "Test default-system-prompt when tools are enabled and defined."
  (let ((hactar::*tool-use-enabled* t)
        (hactar::*tools-in-system-prompt* t)
        (hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*active-guide-file* nil)
        (hactar::*language* "Common Lisp")
        (hactar::*repo-map* nil)
        (hactar::*stack* '())
        (hactar::*shell* "bash")
        (hactar::*name* "TestProject")
        (hactar::*author* "Tester")
        (hactar::*files* nil)
        (hactar::*docs-context* nil)
        (hactar::*errors-context* nil)
        (hactar::*images* nil)
        (hactar::*exposed-context-file* nil))
    (clrhash hactar::*defined-tools*)
    (setf (gethash "test_tool" hactar::*defined-tools*)
          (hactar::make-tool-definition
           :name "test_tool"
           :description "A test tool"
           :parameters nil
           :permissions :auto
           :function (lambda (args) (declare (ignore args)) "result")))
    (with-dynamic-stubs ((hactar::get-prompt-path 
                          (lambda (name) 
                            (declare (ignore name))
                            "/tmp/test-system-prompt.org"))
                         (uiop:read-file-string 
                          (lambda (path)
                            (declare (ignore path))
                            "{{#tools_enabled}}TOOLS:{{tools}}{{/tools_enabled}}Context:{{context}}")))
      (let ((result (hactar::default-system-prompt)))
        (is (search "TOOLS:" result))
        (is (search "test_tool" result))
        (is (search "Context:" result))))
    (clrhash hactar::*defined-tools*)))

(test default-system-prompt-tools-in-api-mode-test
  "Test default-system-prompt when tools are in API mode (not system prompt)."
  (let ((hactar::*tool-use-enabled* t)
        (hactar::*tools-in-system-prompt* nil) ;; API mode
        (hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*active-guide-file* nil)
        (hactar::*language* "Common Lisp")
        (hactar::*repo-map* nil)
        (hactar::*stack* '())
        (hactar::*shell* "bash")
        (hactar::*name* "TestProject")
        (hactar::*author* "Tester")
        (hactar::*files* nil)
        (hactar::*docs-context* nil)
        (hactar::*errors-context* nil)
        (hactar::*images* nil)
        (hactar::*exposed-context-file* nil))
    (clrhash hactar::*defined-tools*)
    (setf (gethash "api_tool" hactar::*defined-tools*)
          (hactar::make-tool-definition
           :name "api_tool"
           :description "An API mode tool"
           :parameters nil
           :permissions :auto
           :function (lambda (args) (declare (ignore args)) "result")))
    (with-dynamic-stubs ((hactar::get-prompt-path 
                          (lambda (name) 
                            (declare (ignore name))
                            "/tmp/test-system-prompt.org"))
                         (uiop:read-file-string 
                          (lambda (path)
                            (declare (ignore path))
                            "{{#tools_enabled}}TOOLS:{{tools}}{{/tools_enabled}}Context:{{context}}")))
      (let ((result (hactar::default-system-prompt)))
        (is (not (search "TOOLS:" result)))
        (is (search "Context:" result))))
    (clrhash hactar::*defined-tools*)))

(test generate-commit-message-test
  "Test generation of commit message from diff."
  (let ((hactar::*cheap-model* nil)
        (*standard-output* (make-broadcast-stream)))
    (is (null (hactar::generate-commit-message))))
  
  (let ((hactar::*cheap-model* "test-model"))
    (with-dynamic-stubs ((hactar::get-prompt-path 
                          (lambda (name) (declare (ignore name)) "dummy/path"))
                         (uiop:read-file-string 
                          (lambda (path) (declare (ignore path)) "Diff: {{diff}}"))
                         (hactar::run-git-command 
                          (lambda (args &key ignore-error) 
                            (declare (ignore args ignore-error)) 
                            "mock git diff"))
                         (hactar::find-model-by-name
                          (lambda (name)
                            (declare (ignore name))
                            (hactar::make-model-config :provider "test-provider" 
                                                       :model-name "test-model" 
                                                       :max-output-tokens 100)))
                         (llm:complete 
                          (lambda (provider messages &key model max-tokens system-prompt stream) 
                            (declare (ignore provider messages max-tokens system-prompt stream))
                            (if (equal model "test-model")
                                (format nil "feat: changes~%~%Body text")
                                nil))))
      (is (string= "feat: changes" (hactar::generate-commit-message))))))
