(in-package :hactar-tests)

(def-suite processors-tests
  :description "Tests for Processors")

(in-suite processors-tests)

(test parse-search-replace-blocks-parses-single-block
      "parse-search-replace-blocks should parse one search/replace block and preserve newlines."
      (let* ((text (format nil "src/file.txt~%```text src/file.txt~%<<<<<<< SEARCH~%old~%=======~%new~%>>>>>>> REPLACE~%```~%"))
             (blocks (hactar::parse-search-replace-blocks text)))
	(is (= 1 (length blocks)))
	(let* ((blk (first blocks)))
	  ;; filename should be the second token after the language
	  (is (string= (cdr (assoc :filename blk)) "src/file.txt"))
	  (is (string= (cdr (assoc :search blk)) "old"))
	  (is (string= (cdr (assoc :replace blk)) "new")))))

(test apply-search-replace-blocks-create-new-file
  "apply-search-replace-blocks should create a new file when search is empty and user confirms."
  (uiop:with-temporary-file (:pathname p :keep t)
    ;; point *repo-root* to temp dir so merge-pathnames works consistently
    (let* ((repo-root (uiop:pathname-directory-pathname p))
           (target "new-file.txt")
           (full (merge-pathnames target repo-root))
           (blocks (list `((:filename . ,target)
                           (:search . "")
                           (:replace . "hello\n")))))
      (when (probe-file full) (delete-file full))
      (let ((hactar::*repo-root* repo-root))
        (with-dynamic-stubs ((hactar::confirm-action (lambda (&rest args) (declare (ignore args)) t))
                             (hactar::generate-commit-message (lambda () "msg"))
                             (hactar::git-add (lambda (&rest args) (declare (ignore args)) nil))
                             (hactar::git-commit (lambda (&rest args) (declare (ignore args)) nil)))
          (hactar::apply-search-replace-blocks blocks :autocommit nil)
          (is-true (probe-file full))
          (is (string= (uiop:read-file-string full) "hello\n")))))))

(test apply-search-replace-blocks-modify-existing
  "apply-search-replace-blocks should replace the first occurrence of the search text."
  (uiop:with-temporary-file (:pathname p :keep t)
    (let* ((repo-root (uiop:pathname-directory-pathname p))
           (target "edit.txt")
           (full (merge-pathnames target repo-root)))
      (with-open-file (s full :direction :output :if-exists :supersede)
        (write-string "abc abc" s))
      (let ((hactar::*repo-root* repo-root)
            (blocks (list `((:filename . ,target)
                            (:search . "abc")
                            (:replace . "XYZ")))))
        (with-dynamic-stubs ((hactar::confirm-action (lambda (&rest args) (declare (ignore args)) t)))
          (hactar::apply-search-replace-blocks blocks :autocommit nil)
          (is (string= (uiop:read-file-string full) "XYZ abc")))))))
(test parse-file-blocks-ignores-search-replace-blocks
  "parse-file-blocks should NOT parse SEARCH/REPLACE blocks as file blocks.
   This tests the fix for double file creation prompts."
  ;; This is the exact format the LLM returns for a new file creation via SEARCH/REPLACE
  (let ((text "```typescript src/app/actions/hello.ts
<<<<<<< SEARCH
=======
\"use server\";

export function hello(name: string): string {
  return \"hello \" + name;
}
>>>>>>> REPLACE
```"))
    (is (null (hactar::parse-file-blocks text)))))

(test parse-file-blocks-still-parses-regular-file-blocks
  "parse-file-blocks should still parse regular file creation blocks."
  (let ((text "```typescript src/app/utils.ts
export function add(a: number, b: number): number {
  return a + b;
}
```"))
    (let ((blocks (hactar::parse-file-blocks text)))
      (is (= 1 (length blocks)))
      (is (string= (cdr (assoc :filename (first blocks))) "src/app/utils.ts"))
      (is (search "export function add" (cdr (assoc :content (first blocks))))))))

(test no-double-processing-of-new-file-blocks
  "Both processors should not both try to create the same file from a SEARCH/REPLACE block.
   Only search-replace-processor should handle SEARCH/REPLACE blocks."
  (let ((text "```typescript src/app/actions/hello.ts
<<<<<<< SEARCH
=======
\"use server\";

export function hello(name: string): string {
  return \"hello \" + name;
}
>>>>>>> REPLACE
```"))
    (let ((sr-blocks (hactar::parse-search-replace-blocks text)))
      (is (= 1 (length sr-blocks)))
      (is (string= (cdr (assoc :filename (first sr-blocks))) "src/app/actions/hello.ts"))
      (is (string= (cdr (assoc :search (first sr-blocks))) "")))
    (let ((file-blocks (hactar::parse-file-blocks text)))
      (is (null file-blocks)))))
