(in-package :hactar-tests)

(def-suite hactar-mode-tests
  :description "Tests for API documentation commands in hactar-mode.lisp")

(in-suite hactar-mode-tests)

(test extract-functions-from-file-test
  "Test that extract-functions-from-file successfully parses defun and defmacro forms."
  (let ((temp-content "(in-package :hactar)
(defun test-func (a b)
  \"This is a test docstring.\"
  (+ a b))
(defmacro test-macro (x &key y)
  (let ((res x)) res))"))
    (with-dynamic-stubs ((uiop:read-file-string (lambda (path &key external-format)
                                                  (declare (ignore path external-format))
                                                  temp-content)))
      (let ((funcs (hactar::extract-functions-from-file #P"mock-file.lisp")))
        (is (= 2 (length funcs)))
        (let ((f1 (first funcs))
              (f2 (second funcs)))
          (is (string= "test-func" (cdr (assoc :name f1))))
          (is (string= "(a b)" (cdr (assoc :signature f1))))
          (is (string= "This is a test docstring." (cdr (assoc :docstring f1))))
          (is (string= "test-macro" (cdr (assoc :name f2))))
          (is (string= "(x &key y)" (cdr (assoc :signature f2))))
          (is (string= "" (cdr (assoc :docstring f2)))))))))

(test serialize-apidocs-to-yaml-test
  "Test that YAML serialization outputs formatted keys and values."
  (let ((spec '((( :file . "file1.lisp")
                 (:functions . (((:name . "func1") (:signature . "(x)") (:docstring . "doc1"))
                                ((:name . "func2") (:signature . "()") (:docstring . "line1
line2"))))))))
    (let ((yaml (hactar::serialize-apidocs-to-yaml spec)))
      (is (search "- file: file1.lisp" yaml))
      (is (search "functions:" yaml))
      (is (search "name: func1" yaml))
      (is (search "signature: \"(x)\"" yaml))
      (is (search "docstring: \"doc1\"" yaml))
      (is (search "docstring: |" yaml))
      (is (search "        line1" yaml))
      (is (search "        line2" yaml)))))

(test serialize-apidocs-to-org-test
  "Test that Org-mode serialization outputs correct headlines."
  (let ((spec '((( :file . "file1.lisp")
                 (:functions . (((:name . "func1") (:signature . "(x)") (:docstring . "doc1"))
                                ((:name . "func2") (:signature . "()") (:docstring . ""))))))))
    (let ((org (hactar::serialize-apidocs-to-org spec)))
      (is (search "* file1.lisp" org))
      (is (search "** func1" org))
      (is (search "- Signature: =(x)=" org))
      (is (search "- Docstring:" org))
      (is (search "doc1" org))
      (is (search "** func2" org))
      (is (search "- Signature: =()=" org))
      (is (search "- Docstring: None" org)))))

(test apidocs-update-test
  "Test that apidocs-update reads existing documentation, prompts LLM, and writes back updated API docs."
  (let* ((temp-dir (merge-pathnames "tests/temp-apidoc-test/" (uiop:getcwd))))
    (ensure-directories-exist temp-dir)
    (unwind-protect
         (progn
           (with-dynamic-stubs ((hactar::resolve-repo-root (lambda () temp-dir))
                                (hactar::get-repo-lisp-files (lambda (root) (declare (ignore root)) '(#P"f1.lisp")))
                                (hactar::extract-functions-from-file (lambda (path)
                                                                       (declare (ignore path))
                                                                       '(((:name . "func1") (:signature . "()") (:docstring . "doc")))))
                                (hactar::get-prompt (lambda (name &optional fallback)
                                                      (declare (ignore name fallback))
                                                      "Mock System Prompt"))
                                (hactar::ask (lambda (prompt &key system-prompt model-name)
                                               (declare (ignore model-name))
                                               (is (search "func1" prompt))
                                               (is (string= "Mock System Prompt" system-prompt))
                                               "```org\nUpdated Org Content\n```"))
                                (hactar::extract-response-content (lambda (resp format)
                                                                    (is (search "Updated Org Content" resp))
                                                                    (is (eq format :org-mode))
                                                                    "Updated Org Content")))
             (let ((result (hactar::apidocs-update)))
               (is-true result)
               (let ((written (uiop:read-file-string (merge-pathnames "docs/API.org" temp-dir))))
                 (is (string= "Updated Org Content" (string-trim '(#\Space #\Tab #\Newline #\Return) written)))))))
      ;; Clean up temp directory
      (uiop:delete-directory-tree temp-dir :validate t))))
