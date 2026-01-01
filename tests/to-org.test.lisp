(in-package :hactar-tests)

(def-suite to-org-tests
  :description "Tests for the /to-org command")

(in-suite to-org-tests)

;;* Test Helpers

(defvar *test-to-org-root* nil
  "Temporary directory for to-org tests.")

(defun make-test-to-org-root ()
  "Create a fresh temp directory with a git repo for to-org tests."
  (let ((dir (merge-pathnames
              (format nil "hactar-to-org-test-~A/" (uuid:make-v4-uuid))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    ;; Initialize git repo
    (uiop:run-program (list "git" "-C" (namestring dir) "init")
                      :output nil :error-output nil :ignore-error-status t)
    ;; Configure git user for commits
    (uiop:run-program (list "git" "-C" (namestring dir) "config" "user.email" "test@test.com")
                      :output nil :error-output nil :ignore-error-status t)
    (uiop:run-program (list "git" "-C" (namestring dir) "config" "user.name" "Test")
                      :output nil :error-output nil :ignore-error-status t)
    dir))

(defun add-test-file (root relative-path content)
  "Create a file in the test repo, git add it."
  (let ((full-path (merge-pathnames relative-path root)))
    (ensure-directories-exist full-path)
    (with-open-file (s full-path :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :external-format :utf-8)
      (write-string content s))
    (uiop:run-program (list "git" "-C" (namestring root) "add" relative-path)
                      :output nil :error-output nil :ignore-error-status t)
    full-path))

(defun commit-test-files (root)
  "Commit all staged files in the test repo."
  (uiop:run-program (list "git" "-C" (namestring root) "commit" "-m" "test files")
                    :output nil :error-output nil :ignore-error-status t))

(defun cleanup-test-to-org ()
  "Clean up test directory."
  (when (and *test-to-org-root* (uiop:directory-exists-p *test-to-org-root*))
    (uiop:delete-directory-tree *test-to-org-root* :validate t :if-does-not-exist :ignore))
  (setf *test-to-org-root* nil))

(defmacro with-test-to-org-project (&body body)
  "Execute BODY with a fresh test project."
  `(let* ((*test-to-org-root* (make-test-to-org-root))
          (hactar::*repo-root* *test-to-org-root*)
          (hactar::*name* "test-project")
          (hactar::*language* "lisp")
          (hactar::*stack* '("common-lisp"))
          (hactar::*silent* t))
     (unwind-protect
          (progn ,@body)
       (cleanup-test-to-org))))

;;* Sanitize ID Tests

(test to-org-sanitize-id-simple
  "Test sanitize-id with a simple path."
  (let ((id (hactar::sanitize-id "src/main.lisp")))
    (is (string= "code-src-main-lisp" id))))

(test to-org-sanitize-id-nested
  "Test sanitize-id with nested paths."
  (let ((id (hactar::sanitize-id "src/pkg/deep/file.js")))
    (is (string= "code-src-pkg-deep-file-js" id))))

(test to-org-sanitize-id-special-chars
  "Test sanitize-id with special characters."
  (let ((id (hactar::sanitize-id "src/my_file.test.ts")))
    (is (string= "code-src-my-file-test-ts" id))))

(test to-org-sanitize-id-root-file
  "Test sanitize-id with a root-level file."
  (let ((id (hactar::sanitize-id "Makefile")))
    (is (string= "code-makefile" id))))

;;* File Ignore Tests

(test to-org-ignore-patterns-default
  "Test that default ignore patterns are present."
  (let ((patterns (hactar::to-org-ignore-patterns)))
    (is (member "*.min.js" patterns :test #'string=))
    (is (member "node_modules/*" patterns :test #'string=))
    (is (member ".hactar.org" patterns :test #'string=))))

(test to-org-ignore-patterns-with-extra
  "Test that extra ignore patterns are appended."
  (let ((hactar::*to-org-extra-ignore-patterns* '("*.custom" "custom-dir/*")))
    (let ((patterns (hactar::to-org-ignore-patterns)))
      (is (member "*.custom" patterns :test #'string=))
      (is (member "custom-dir/*" patterns :test #'string=))
      ;; Defaults still present
      (is (member "*.min.js" patterns :test #'string=)))))

(test to-org-file-ignored-p-matches
  "Test file ignore matching."
  (let ((patterns '("*.min.js" "node_modules/*" "dist/*")))
    (is-true (hactar::to-org-file-ignored-p "app.min.js" patterns))
    (is-true (hactar::to-org-file-ignored-p "node_modules/pkg/index.js" patterns))
    (is-true (hactar::to-org-file-ignored-p "node_modules/index.js" patterns))
    (is-true (hactar::to-org-file-ignored-p "src/app.min.js" patterns))
    (is (null (hactar::to-org-file-ignored-p "src/main.js" patterns)))
    (is (null (hactar::to-org-file-ignored-p "README.md" patterns)))))

;;* File Grouping Tests

(test to-org-group-name-known-dirs
  "Test group name mapping for known directories."
  (is (string= "Source" (hactar::to-org-group-name "src")))
  (is (string= "Tests" (hactar::to-org-group-name "tests")))
  (is (string= "Tests" (hactar::to-org-group-name "test")))
  (is (string= "Documentation" (hactar::to-org-group-name "docs")))
  (is (string= "Libraries" (hactar::to-org-group-name "lib")))
  (is (string= "Root" (hactar::to-org-group-name nil)))
  (is (string= "Root" (hactar::to-org-group-name ""))))

(test to-org-group-name-unknown-dirs
  "Test group name for unknown directories."
  (is (string= "Components" (hactar::to-org-group-name "components")))
  (is (string= "Utils" (hactar::to-org-group-name "utils"))))

(test to-org-top-directory
  "Test top directory extraction."
  (is (string= "src" (hactar::to-org-top-directory "src/main.lisp")))
  (is (string= "tests" (hactar::to-org-top-directory "tests/deep/file.lisp")))
  (is (null (hactar::to-org-top-directory "Makefile"))))

(test to-org-group-files-by-directory
  "Test file grouping."
  (let* ((files (list (list :relative-path "src/a.lisp")
                      (list :relative-path "src/b.lisp")
                      (list :relative-path "tests/test-a.lisp")
                      (list :relative-path "README.md")))
         (groups (hactar::group-files-by-directory files)))
    ;; Root should come first
    (is (string= "Root" (caar groups)))
    ;; Should have 3 groups
    (is (= 3 (length groups)))
    ;; Source group should have 2 files
    (let ((src-group (find "Source" groups :key #'car :test #'string=)))
      (is-true src-group)
      (is (= 2 (length (cdr src-group)))))))

;;* File Collection Tests

(test to-org-collect-project-files
  "Test collecting files from a test project."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (add-test-file *test-to-org-root* "README.md" "# Test Project")
    (commit-test-files *test-to-org-root*)
    (let ((files (hactar::collect-project-files)))
      (is (= 2 (length files)))
      (is (find "src/main.lisp" files :key (lambda (f) (getf f :relative-path)) :test #'string=))
      (is (find "README.md" files :key (lambda (f) (getf f :relative-path)) :test #'string=)))))

(test to-org-collect-respects-ignore
  "Test that file collection respects ignore patterns."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (add-test-file *test-to-org-root* "dist/bundle.min.js" "minified code")
    (commit-test-files *test-to-org-root*)
    (let ((files (hactar::collect-project-files)))
      (is (= 1 (length files)))
      (is (string= "src/main.lisp"
                    (getf (first files) :relative-path))))))

(test to-org-collect-respects-token-limit
  "Test that file collection respects the token limit."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "small.lisp" "(+ 1 2)")
    ;; Create a large file exceeding 100 tokens (400+ chars)
    (add-test-file *test-to-org-root* "large.lisp"
                   (make-string 500 :initial-element #\x))
    (commit-test-files *test-to-org-root*)
    (let ((files (hactar::collect-project-files :max-tokens 100)))
      (is (= 1 (length files)))
      (is (string= "small.lisp"
                    (getf (first files) :relative-path))))))

(test to-org-collect-respects-include
  "Test that file collection respects include patterns."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (add-test-file *test-to-org-root* "README.md" "# Test")
    (add-test-file *test-to-org-root* "src/util.lisp" "(defun util () :ok)")
    (commit-test-files *test-to-org-root*)
    (let ((files (hactar::collect-project-files :include-patterns '("*.lisp"))))
      (is (= 2 (length files)))
      (is (every (lambda (f) (str:ends-with-p ".lisp" (getf f :relative-path))) files)))))

(test to-org-collect-extra-ignore
  "Test that extra ignore patterns work."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (add-test-file *test-to-org-root* "src/generated.lisp" "(generated)")
    (commit-test-files *test-to-org-root*)
    (let ((files (hactar::collect-project-files :extra-ignore-patterns '("*generated*"))))
      (is (= 1 (length files))))))

;;* Org Generation Tests

(test to-org-generate-header
  "Test org header generation."
  (let* ((hactar::*name* "my-project")
         (hactar::*language* "lisp")
         (hactar::*stack* '("sbcl" "quicklisp"))
         (hactar::*author* "Test Author")
         (hactar::*current-model* nil)
         (header (hactar::generate-org-header)))
    (is (search "#+TITLE: my-project" header))
    (is (search "#+HACTAR_LANGUAGE: lisp" header))
    (is (search "#+HACTAR_STACK: sbcl, quicklisp" header))
    (is (search "#+HACTAR_AUTHOR: Test Author" header))))

(test to-org-generate-meta-zone
  "Test Meta zone generation."
  (let ((meta (hactar::generate-meta-zone)))
    (is (search ":meta:noctx:" meta))
    (is (search ":ID: headline-index" meta))
    (is (search ":ID: agent-locks" meta))
    (is (search ":ID: scope-profiles" meta))
    (is (search ":ID: tooling" meta))))

(test to-org-generate-meta-zone-with-scope
  "Test Meta zone generation with a scope profile."
  (let ((meta (hactar::generate-meta-zone :scope-name "dev")))
    (is (search "*** dev" meta))
    (is (search ":HACTAR_SCOPE_INCLUDE_TAGS: ctx,code" meta))
    (is (search ":HACTAR_SCOPE_MAX_TOKENS: 50000" meta))))

(test to-org-generate-code-zone
  "Test Code zone generation."
  (let* ((files (list (list :relative-path "src/main.lisp"
                            :content "(defun main () :ok)"
                            :language "lisp"
                            :tokens 5)))
         (groups (hactar::group-files-by-directory files))
         (code (hactar::generate-code-zone groups)))
    (is (search ":code:" code))
    (is (search ":ID: code-zone" code))
    (is (search "*** src/main.lisp" code))
    (is (search ":HACTAR_TANGLE: src/main.lisp" code))
    (is (search "#+begin_src lisp :tangle src/main.lisp" code))
    (is (search "defun main" code))
    (is (search "#+end_src" code))))

(test to-org-generate-tasks-zone
  "Test Tasks zone generation."
  (let ((tasks (hactar::generate-tasks-zone)))
    (is (search ":tasks:" tasks))
    (is (search ":ID: tasks-zone" tasks))))

(test to-org-generate-scratch-zone
  "Test Scratch zone generation."
  (let ((scratch (hactar::generate-scratch-zone)))
    (is (search ":scratch:noctx:" scratch))
    (is (search ":ID: scratch-zone" scratch))))

;;* Full to-org Integration Tests

(test to-org-full-generation
  "Test complete to-org generation."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (add-test-file *test-to-org-root* "src/util.lisp" "(defun util () :util)")
    (add-test-file *test-to-org-root* "tests/test-main.lisp" "(test main-test)")
    (add-test-file *test-to-org-root* "README.md" "# Test Project")
    (commit-test-files *test-to-org-root*)

    (let ((output-path (hactar::to-org :force t)))
      (is-true output-path)
      (is-true (probe-file output-path))
      (let ((content (uiop:read-file-string output-path)))
        ;; Has all zones
        (is (search "* Meta" content))
        (is (search "* Context" content))
        (is (search "* Tasks" content))
        (is (search "* Code" content))
        (is (search "* Scratch" content))
        ;; Has file content
        (is (search "defun main" content))
        (is (search "defun util" content))
        ;; Has tangle properties
        (is (search ":HACTAR_TANGLE: src/main.lisp" content))
        ;; Has tangle directive
        (is (search ":tangle src/main.lisp" content))))))

(test to-org-dry-run
  "Test dry-run mode."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (commit-test-files *test-to-org-root*)

    (let ((result (hactar::to-org :dry-run t)))
      ;; Should return the file list, not a path
      (is (listp result))
      (is (= 1 (length result)))
      ;; Should NOT have created the file
      (is (null (probe-file (merge-pathnames ".hactar.org" *test-to-org-root*)))))))

(test to-org-no-code-flag
  "Test --no-code flag omits Code zone."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (commit-test-files *test-to-org-root*)

    (let ((output-path (hactar::to-org :force t :no-code t)))
      (is-true output-path)
      (let ((content (uiop:read-file-string output-path)))
        ;; Should have Context but not Code zone
        (is (search "* Context" content))
        (is (null (search "* Code" content)))))))

(test to-org-custom-output-path
  "Test custom output path."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (commit-test-files *test-to-org-root*)

    (let* ((custom-path "custom-output.org")
           (output-path (hactar::to-org :output custom-path :force t)))
      (is-true output-path)
      (is (search "custom-output.org" (namestring output-path)))
      (is-true (probe-file output-path)))))

(test to-org-output-parseable-by-org-parser
  "Test that generated org is parseable by org-mode-parser."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (add-test-file *test-to-org-root* "README.md" "# Hello")
    (commit-test-files *test-to-org-root*)

    (let ((output-path (hactar::to-org :force t)))
      (is-true output-path)
      (let* ((content (uiop:read-file-string output-path))
             (doc (org-mode-parser:parse-org-string content)))
        (is (typep doc 'org-mode-parser:org-document))
        ;; Should have top-level headings
        (let ((headings (org-mode-parser:collect-all-headings doc)))
          (is (> (length headings) 0)))))))

(test to-org-output-works-with-litmode
  "Test that generated org file works with litmode init."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (commit-test-files *test-to-org-root*)

    (let ((output-path (hactar::to-org :force t)))
      (is-true output-path)
      ;; Init litmode with the generated file
      (let ((hactar::*litmode-active* nil)
            (hactar::*litmode-path* nil)
            (hactar::*litmode-content* nil)
            (hactar::*litmode-parsed* nil)
            (hactar::*litmode-db-path* nil)
            (hactar::*active-scope* nil)
            (hactar::*expanded-ids* '())
            (hactar::*focused-id* nil)
            (hactar::*headline-index-cache* nil)
            (hactar::*agent-id* nil))
        (clrhash hactar::*named-blocks*)
        (hactar::init-litmode *test-to-org-root*)
        (is-true hactar::*litmode-active*)
        (is (>= (length hactar::*headline-index-cache*) 3))
        ;; Cleanup litmode state
        (setf hactar::*litmode-active* nil)))))

(test to-org-with-scope
  "Test generating with a scope profile."
  (with-test-to-org-project
    (add-test-file *test-to-org-root* "src/main.lisp" "(defun main () :ok)")
    (commit-test-files *test-to-org-root*)

    (let ((output-path (hactar::to-org :force t :scope "development")))
      (is-true output-path)
      (let ((content (uiop:read-file-string output-path)))
        (is (search "*** development" content))
        (is (search ":HACTAR_SCOPE_INCLUDE_TAGS:" content))))))

(test to-org-escapes-org-content
  "Test that file content with org-mode special characters is escaped."
  (with-test-to-org-project
    ;; File with content that starts with * (would be parsed as heading)
    (add-test-file *test-to-org-root* "src/tricky.lisp"
                   "* This is not a heading
#+begin_src lisp
nested block
#+end_src")
    (commit-test-files *test-to-org-root*)

    (let ((output-path (hactar::to-org :force t)))
      (is-true output-path)
      ;; The file should still be parseable
      (let* ((content (uiop:read-file-string output-path))
             (doc (org-mode-parser:parse-org-string content)))
        (is (typep doc 'org-mode-parser:org-document))))))

(test to-org-empty-project
  "Test to-org with no files."
  (with-test-to-org-project
    ;; Empty git repo - need at least one commit
    (add-test-file *test-to-org-root* ".gitkeep" "")
    (commit-test-files *test-to-org-root*)
    ;; .gitkeep is tiny and should be collected but result in minimal output
    (let ((output-path (hactar::to-org :force t)))
      (is-true output-path))))
