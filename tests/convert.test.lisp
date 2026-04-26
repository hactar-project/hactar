;; conversion command tests
(in-package :hactar-tests)

(def-suite convert-tests :description "Tests for conversion commands.")
(in-suite convert-tests)

;;* redwood-collect-source-files tests

(test collect-source-files-basic
  "Collects ts/tsx/css/json/toml/mts files from a directory."
  (uiop:with-temporary-file (:pathname tmp-dir :keep nil)
    (let* ((dir (uiop:ensure-directory-pathname
                 (make-pathname :name nil :type nil :defaults tmp-dir)))
           (ts-file (merge-pathnames "src/worker.ts" dir))
           (tsx-file (merge-pathnames "src/app/Page.tsx" dir))
           (css-file (merge-pathnames "src/style.css" dir))
           (json-file (merge-pathnames "package.json" dir))
           (toml-file (merge-pathnames "wrangler.toml" dir))
           (txt-file (merge-pathnames "readme.txt" dir)))
      (ensure-directories-exist ts-file)
      (ensure-directories-exist tsx-file)
      (hactar::write-file-content (namestring ts-file) "export default {}")
      (hactar::write-file-content (namestring tsx-file) "export const Page = () => <div/>")
      (hactar::write-file-content (namestring css-file) "body { margin: 0; }")
      (hactar::write-file-content (namestring json-file) "{\"name\": \"test\"}")
      (hactar::write-file-content (namestring toml-file) "name = \"test\"")
      (hactar::write-file-content (namestring txt-file) "readme content")
      (let ((pairs (hactar::redwood-collect-source-files dir)))
        (is (= 5 (length pairs))
            "Should collect exactly 5 source files (ts, tsx, css, json, toml)")
        (is (not (member "readme.txt" pairs :test #'string= :key #'car))
            "Should not collect .txt files")
        (is (some (lambda (p) (search "worker.ts" (car p))) pairs)
            "Should include .ts files")
        (is (some (lambda (p) (search "Page.tsx" (car p))) pairs)
            "Should include .tsx files")
        (is (some (lambda (p) (search "style.css" (car p))) pairs)
            "Should include .css files")
        (is (some (lambda (p) (search "package.json" (car p))) pairs)
            "Should include .json files")
        (is (some (lambda (p) (search "wrangler.toml" (car p))) pairs)
            "Should include .toml files")))))

(test collect-source-files-skips-node-modules
  "Skips node_modules, .git, dist, .wrangler, .tmp directories."
  (uiop:with-temporary-file (:pathname tmp-dir :keep nil)
    (let* ((dir (uiop:ensure-directory-pathname
                 (make-pathname :name nil :type nil :defaults tmp-dir))))
      (dolist (skip-dir '("node_modules" ".git" "dist" ".wrangler" ".tmp"))
        (let ((skip-file (merge-pathnames (format nil "~A/index.ts" skip-dir) dir)))
          (ensure-directories-exist skip-file)
          (hactar::write-file-content (namestring skip-file) "export {}")))
      (let ((keep-file (merge-pathnames "src/worker.ts" dir)))
        (ensure-directories-exist keep-file)
        (hactar::write-file-content (namestring keep-file) "export default {}"))
      (let ((pairs (hactar::redwood-collect-source-files dir)))
        (is (= 1 (length pairs))
            "Should only collect the file outside skipped dirs")
        (is (search "worker.ts" (caar pairs))
            "Should collect worker.ts from src/")))))

(test collect-source-files-returns-content
  "File pairs contain relative path and file content."
  (uiop:with-temporary-file (:pathname tmp-dir :keep nil)
    (let* ((dir (uiop:ensure-directory-pathname
                 (make-pathname :name nil :type nil :defaults tmp-dir)))
           (ts-file (merge-pathnames "src/worker.ts" dir))
           (content "export default { fetch() {} }"))
      (ensure-directories-exist ts-file)
      (hactar::write-file-content (namestring ts-file) content)
      (let* ((pairs (hactar::redwood-collect-source-files dir))
             (pair (first pairs)))
        (is (stringp (car pair)) "Path should be a string")
        (is (string= (cdr pair) content) "Content should match file contents")))))

(test collect-source-files-empty-dir
  "Returns empty list for directory with no matching files."
  (uiop:with-temporary-file (:pathname tmp-dir :keep nil)
    (let* ((dir (uiop:ensure-directory-pathname
                 (make-pathname :name nil :type nil :defaults tmp-dir)))
           (txt-file (merge-pathnames "README.txt" dir)))
      (hactar::write-file-content (namestring txt-file) "hello")
      (let ((pairs (hactar::redwood-collect-source-files dir)))
        (is (null pairs) "Should return empty list when no matching files found")))))

;;* redwood-build-user-message tests

(test build-user-message-structure
  "User message starts with conversion request and includes file blocks."
  (let* ((pairs '(("src/worker.ts" . "export default {}")
                  ("src/style.css" . "body { margin: 0; }")))
         (msg (hactar::redwood-build-user-message pairs)))
    (is (stringp msg) "Message should be a string")
    (is (search "RedwoodSDK" msg) "Message should mention RedwoodSDK")
    (is (search "Hactar Lisp" msg) "Message should mention Hactar Lisp")
    (is (search "src/worker.ts" msg) "Message should include first file path")
    (is (search "src/style.css" msg) "Message should include second file path")
    (is (search "export default {}" msg) "Message should include first file content")
    (is (search "body { margin: 0; }" msg) "Message should include second file content")))

(test build-user-message-fenced-blocks
  "Each file is wrapped in a fenced code block with language hint and path."
  (let* ((pairs '(("src/worker.ts" . "const x = 1;")
                  ("src/App.tsx" . "<div/>")
                  ("wrangler.toml" . "name = \"app\"")))
         (msg (hactar::redwood-build-user-message pairs)))
    (is (search "```typescript src/worker.ts" msg)
        "TS file should have typescript language hint")
    (is (search "```tsx src/App.tsx" msg)
        "TSX file should have tsx language hint")
    (is (search "```toml wrangler.toml" msg)
        "TOML file should have toml language hint")))

(test build-user-message-empty-pairs
  "Empty file list produces only the intro text."
  (let ((msg (hactar::redwood-build-user-message nil)))
    (is (stringp msg) "Message should be a string even with no files")
    (is (search "RedwoodSDK" msg) "Message should still include the intro")))

(test build-user-message-ordering
  "Files appear in the message in the same order as the pairs list."
  (let* ((pairs '(("a.ts" . "first")
                  ("b.ts" . "second")
                  ("c.ts" . "third")))
         (msg (hactar::redwood-build-user-message pairs)))
    (let ((pos-a (search "a.ts" msg))
          (pos-b (search "b.ts" msg))
          (pos-c (search "c.ts" msg)))
      (is (< pos-a pos-b) "a.ts should appear before b.ts")
      (is (< pos-b pos-c) "b.ts should appear before c.ts"))))
