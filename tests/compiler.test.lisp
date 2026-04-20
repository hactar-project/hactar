(in-package :hactar-tests)

(def-suite compiler-tests :description "Core compiler tests.")
(in-suite compiler-tests)

(test read-all-forms-test
  "Test reading multiple forms from a string."
  (let ((forms (hactar::read-all-forms "(+ 1 2) (defun foo () nil)")))
    (is (= 2 (length forms)))
    (is (equal (first forms) '(+ 1 2)))
    (is (eq (car (second forms)) 'defun))))

(test emit-name-camel-case
  "Kebab-case symbols become camelCase."
  (let* ((hactar::*current-vfs* (hactar::make-vfs))
         (_ (hactar::vfs-select-file hactar::*current-vfs* "test.js")))
    (declare (ignore _))
    (hactar::emit-name 'my-function-name)
    (is (string= "myFunctionName"
                  (hactar::vfs-get-content hactar::*current-vfs* "test.js")))))

(test in-file-switches-output
  "The (in-file ...) form switches VFS output."
  (let* ((source "(in-file \"a.ts\") (defvar x 1) (in-file \"b.ts\") (defvar y 2)")
         (vfs (hactar::compile-string source :target :javascript :check nil)))
    (is (search "x" (hactar::vfs-get-content vfs "a.ts")))
    (is (search "y" (hactar::vfs-get-content vfs "b.ts")))
    (is (null (search "y" (or (hactar::vfs-get-content vfs "a.ts") ""))))))

(test compile-output-tags-test
  "compile-output-tags writes tag-formatted output to a stream."
  (let* ((source "(defvar x 1)")
         (vfs (hactar::compile-string source :target :javascript :check nil))
         (output (with-output-to-string (s)
                   (hactar::compile-output-tags vfs s))))
    (is (> (length output) 0))
    (is (search "x" output))))

(test compile-output-org-dry-test
  "compile-output-org with :dry t writes org format to stdout."
  (let* ((source "(defvar x 1)")
         (vfs (hactar::compile-string source :target :javascript :check nil))
         (output (with-output-to-string (*standard-output*)
                   (hactar::compile-output-org vfs :dry t))))
    (is (> (length output) 0))
    (is (search "x" output))))

(test compile-output-lisp-dry-test
  "compile-output-lisp with :dry t writes lisp format to stdout."
  (let* ((source "(defvar x 1)")
         (vfs (hactar::compile-string source :target :javascript :check nil))
         (output (with-output-to-string (*standard-output*)
                   (hactar::compile-output-lisp vfs :dry t))))
    (is (> (length output) 0))
    (is (search "x" output))))

(test compile-read-source-forms-test
  "compile-read-source-forms reads forms from files."
  (let* ((tmp-path (merge-pathnames "test-compile-input.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (s tmp-path :direction :output :if-exists :supersede)
             (write-string "(defvar x 1) (+ 2 3)" s))
           (let ((forms (hactar::compile-read-source-forms (list (namestring tmp-path)))))
             (is (= 2 (length forms)))
             (is (eq 'defvar (car (first forms))))))
      (when (probe-file tmp-path) (delete-file tmp-path)))))

(test compile-read-source-forms-missing-file
  "compile-read-source-forms returns NIL for missing files."
  (let ((output (with-output-to-string (*standard-output*)
                  (let ((result (hactar::compile-read-source-forms '("/nonexistent/file.lisp"))))
                    (is (null result))))))
    (is (search "not found" output))))

;; ACP support tests
(test compile-command-acp-registered
  "The /compile command is registered in *acp-commands*."
  (is (not (null (gethash "/compile" hactar::*acp-commands*)))))

(test compile-command-acp-returns-content
  "The /compile ACP handler returns structured file data."
  (let* ((tmp-path (merge-pathnames "test-acp-compile.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (s tmp-path :direction :output :if-exists :supersede)
             (write-string "(target :javascript) (defvar x 42)" s))
           (let* ((handler (gethash "/compile" hactar::*acp-commands*))
                  (result (funcall handler (list (namestring tmp-path)))))
             (is (not (null result)))
             (is (stringp (cdr (assoc "text" result :test #'string=))))
             (let ((data (cdr (assoc "data" result :test #'string=))))
               (is (not (null data)))
               (let ((files (cdr (assoc "files" data :test #'string=))))
                 (is (> (length files) 0))
                 (let ((first-file (elt files 0)))
                   (is (stringp (cdr (assoc "content" first-file :test #'string=))))
                   (is (search "x" (cdr (assoc "content" first-file :test #'string=)))))))))
      (when (probe-file tmp-path) (delete-file tmp-path)))))

(test compile-in-file-emitter-works-through-compile-form
  "in-file works as an emitter from macro-expanded code."
  (let* ((vfs (hactar::compile-string
               "(target :javascript) (progn (in-file \"a.js\") (defvar x 1) (in-file \"b.js\") (defvar y 2))"
               :check nil)))
    (is (search "var x = 1" (hactar::vfs-get-content vfs "a.js")))
    (is (search "var y = 2" (hactar::vfs-get-content vfs "b.js")))))
