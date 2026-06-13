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

(test parse-add-args-test
  "parse-add-args separates files from -descriptions=."
  (multiple-value-bind (files descs)
      (hactar::parse-add-args '("a.lisp" "-descriptions=d1,d2" "b.png"))
    (is (equal '("a.lisp" "b.png") files))
    (is (equal '("d1" "d2") descs)))
  (multiple-value-bind (files descs)
      (hactar::parse-add-args '("only.lisp"))
    (is (equal '("only.lisp") files))
    (is (null descs))))

(test add-file-to-context-basic-test
  "add-file-to-context adds a file and fires the added hook."
  (let ((hactar::*files* nil)
        (hactar::*current-model* nil)
        (hactar::*exposed-context-file* nil)
        (hactar::*litmode-active* nil)
        (fired nil)
        (hactar::*context-file-added-hook*
          (make-instance 'hactar::hook-context-file-added)))
    (nhooks:add-hook hactar::*context-file-added-hook*
                     (make-instance 'nhooks:handler
                                    :fn (lambda (f) (setf fired f))
                                    :name 'test-added-hook))
    (hactar::add-file-to-context "/tmp/somefile.lisp")
    (is (member "/tmp/somefile.lisp" hactar::*files* :test #'string=))
    (is (string= "/tmp/somefile.lisp" fired))
    ;; Adding again does not duplicate.
    (hactar::add-file-to-context "/tmp/somefile.lisp")
    (is (= 1 (length hactar::*files*)))))

(test drop-file-from-context-basic-test
  "drop-file-from-context removes a file and fires the dropped hook."
  (let ((hactar::*files* (list "/tmp/a.lisp" "/tmp/b.lisp"))
        (hactar::*exposed-context-file* nil)
        (hactar::*litmode-active* nil)
        (fired nil)
        (hactar::*context-file-dropped-hook*
          (make-instance 'hactar::hook-context-file-dropped)))
    (nhooks:add-hook hactar::*context-file-dropped-hook*
                     (make-instance 'nhooks:handler
                                    :fn (lambda (f) (setf fired f))
                                    :name 'test-dropped-hook))
    (hactar::drop-file-from-context "/tmp/a.lisp")
    (is (not (member "/tmp/a.lisp" hactar::*files* :test #'string=)))
    (is (member "/tmp/b.lisp" hactar::*files* :test #'string=))
    (is (string= "/tmp/a.lisp" fired))))

(test expand-file-pattern-exact-test
  "expand-file-pattern resolves an exact relative path to an absolute native path."
  (uiop:with-temporary-file (:pathname p :stream s :direction :output :keep t)
    (write-string "x" s) (finish-output s)
    (let* ((hactar::*repo-root* (uiop:pathname-directory-pathname p))
           (rel (uiop:native-namestring (uiop:enough-pathname p hactar::*repo-root*)))
           (result (hactar::expand-file-pattern rel)))
      (is (member (uiop:native-namestring p) result :test #'string=)))
    (ignore-errors (delete-file p))))

(test expand-file-pattern-missing-test
  "expand-file-pattern returns NIL for a non-existent exact path."
  (let ((hactar::*repo-root* (uiop:temporary-directory)))
    (is (null (hactar::expand-file-pattern "definitely-not-here-12345.lisp")))))

(test ls-command-plaintext-test
  "Test that the /ls command lists files, images, and docs in plaintext."
  (let ((hactar::*files* (list "/tmp/test-file.lisp"))
        (hactar::*images* (list (list :path #P"/tmp/test-image.png" :text "test-desc")))
        (hactar::*docs-context* (list (list (cons :id "doc1")
                                            (cons :title "Doc Title")
                                            (cons :source "docs/test-doc.md")))))
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::slash-cmd/ls nil))))
      (is (search "Files in context:" output))
      (is (search "/tmp/test-file.lisp" output))
      (is (search "Images in context:" output))
      (is (search "test-image.png" output))
      (is (search "test-desc" output))
      (is (search "Documentation in context:" output))
      (is (search "Doc Title" output))
      (is (search "doc1" output))
      (is (search "docs/test-doc.md" output)))))

(test ls-command-formats-test
  "Test format handlers for /ls with docs in context."
  (let ((hactar::*files* (list "/tmp/test-file.lisp"))
        (hactar::*images* (list (list :path #P"/tmp/test-image.png" :text "test-desc")))
        (hactar::*docs-context* (list (list (cons :id "doc1")
                                            (cons :title "Doc Title")
                                            (cons :source "docs/test-doc.md")))))
    ;; Test JSON format
    (let* ((json-handler (hactar::get-format-handler "/ls" :json))
           (json-res (funcall json-handler nil)))
      (is (search "\"path\": \"/tmp/test-file.lisp\"" json-res))
      (is (search "\"type\": \"file\"" json-res))
      (is (search "\"type\": \"image\"" json-res))
      (is (search "test-image.png" json-res))
      (is (search "\"type\": \"doc\"" json-res))
      (is (search "\"id\": \"doc1\"" json-res))
      (is (search "\"title\": \"Doc Title\"" json-res)))

    ;; Test YAML format
    (let* ((yaml-handler (hactar::get-format-handler "/ls" :yaml))
           (yaml-res (funcall yaml-handler nil)))
      (is (search "type: file" yaml-res))
      (is (search "type: image" yaml-res))
      (is (search "type: doc" yaml-res))
      (is (search "id: doc1" yaml-res))
      (is (search "title: Doc Title" yaml-res)))

    ;; Test XML format
    (let* ((xml-handler (hactar::get-format-handler "/ls" :xml))
           (xml-res (funcall xml-handler nil)))
      (is (search "<item type=\"file\">" xml-res))
      (is (search "<item type=\"image\">" xml-res))
      (is (search "<item type=\"doc\">" xml-res))
      (is (search "<id>doc1</id>" xml-res))
      (is (search "<title>Doc Title</title>" xml-res)))

    ;; Test Markdown format
    (let* ((md-handler (hactar::get-format-handler "/ls" :markdown))
           (md-res (funcall md-handler nil)))
      (is (search "## Files in context" md-res))
      (is (search "## Images in context" md-res))
      (is (search "## Documentation in context" md-res))
      (is (search "Doc Title" md-res))
      (is (search "doc1" md-res)))

    ;; Test Org-mode format
    (let* ((org-handler (hactar::get-format-handler "/ls" :org-mode))
           (org-res (funcall org-handler nil)))
      (is (search "* Files in context" org-res))
      (is (search "* Images in context" org-res))
      (is (search "* Documentation in context" org-res))
      (is (search "Doc Title" org-res))
      (is (search "doc1" org-res)))

    ;; Test ACP format
    (let* ((acp-handler (gethash "/ls" hactar::*acp-commands*))
           (acp-res (funcall acp-handler nil)))
      (is (string= "1 file(s), 1 image(s), and 1 doc(s) in context." (cdr (assoc "text" acp-res :test #'string=))))
      (let ((data (cdr (assoc "data" acp-res :test #'string=))))
        (is (= 3 (length data)))
        (is (equal '(("path" . "/tmp/test-file.lisp") ("type" . "file")) (aref data 0)))
        (is (equal `(("path" . ,(uiop:native-namestring #P"/tmp/test-image.png")) ("description" . "test-desc") ("type" . "image")) (aref data 1)))
        (is (equal '(("id" . "doc1") ("title" . "Doc Title") ("source" . "docs/test-doc.md") ("type" . "doc")) (aref data 2)))))))
