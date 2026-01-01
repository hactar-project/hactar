(in-package :hactar-tests)

(def-suite ctags-tests
  :description "Tests for ctags integration.")

(in-suite ctags-tests)

(test parse-tag-line-test
  "Test parsing ctags lines."
  (let* ((line1 (format nil "main~Cmain.c~C/^int main(int argc, char **argv) {$/;\"~Cf~Cline:10" #\Tab #\Tab #\Tab #\Tab))
         (tag1 (hactar::parse-tag-line line1)))
    (is (string= (hactar::tag-name tag1) "main"))
    (is (string= (hactar::tag-file tag1) "main.c"))
    (is (string= (hactar::tag-kind tag1) "f"))
    (is (= (hactar::tag-line tag1) 10)))

  (let* ((line2 (format nil "MyClass~Csrc/MyClass.js~C/class MyClass {/;\"~Cc~Cline:5" #\Tab #\Tab #\Tab #\Tab))
         (tag2 (hactar::parse-tag-line line2)))
    (is (string= (hactar::tag-name tag2) "MyClass"))
    (is (string= (hactar::tag-file tag2) "src/MyClass.js"))
    (is (string= (hactar::tag-kind tag2) "c"))
    (is (= (hactar::tag-line tag2) 5)))

  (let* ((line3 (format nil "simple~Csimple.txt~C1" #\Tab #\Tab)) ; Basic format without extension fields
         (tag3 (hactar::parse-tag-line line3)))
    (is (string= (hactar::tag-name tag3) "simple"))
    (is (string= (hactar::tag-file tag3) "simple.txt"))))

(test extract-symbols-from-text-test
  "Test heuristic symbol extraction."
  (let ((text "def my_function():\n  x = ClassName()\n  return x.method()"))
    (let ((symbols (hactar::extract-symbols-from-text text)))
      (is (member "def" symbols :test #'string=)) ; Short tokens included if >= 3
      (is (member "my_function" symbols :test #'string=))
      (is (member "ClassName" symbols :test #'string=))
      (is (member "return" symbols :test #'string=))
      (is (member "method" symbols :test #'string=))
      ;; Check for non-symbols
      (is (not (member "()" symbols :test #'string=))))))

(test tags-search-functions-test
  "Test finding and filtering tags from cache."
  (let ((hactar::*tags-cache* (list
                               (hactar::make-tag :name "Alpha" :file "a.lisp" :kind "f" :line 1)
                               (hactar::make-tag :name "Beta" :file "b.lisp" :kind "v" :line 2)
                               (hactar::make-tag :name "AlphaBeta" :file "ab.lisp" :kind "c" :line 3))))

    ;; tags-find (fuzzy)
    (let ((res (hactar::tags-find "Alpha")))
      (is (= 2 (length res))) ; Alpha and AlphaBeta
      (is (find-if (lambda (t1) (string= (hactar::tag-name t1) "Alpha")) res))
      (is (find-if (lambda (t1) (string= (hactar::tag-name t1) "AlphaBeta")) res)))

    (let ((res (hactar::tags-find "beta")))
      (is (= 2 (length res))) ; Beta and AlphaBeta
      (is (find-if (lambda (t1) (string= (hactar::tag-name t1) "Beta")) res)))

    ;; tags-get (exact)
    (let ((res (hactar::tags-get "Alpha")))
      (is (= 1 (length res)))
      (is (string= (hactar::tag-name (first res)) "Alpha")))

    ;; tags-find-one
    (let ((res (hactar::tags-find-one "Alpha")))
      (is (string= (hactar::tag-name res) "Alpha")))))

(test tags-for-file-test
  "Test finding tags belonging to a file."
  (let ((hactar::*repo-root* (uiop:getcwd)) ; Not used for parsing here but needed for enough-pathname logic in tags-for
        (hactar::*tags-cache* (list
                               (hactar::make-tag :name "InFile" :file "src/target.lisp" :kind "f")
                               (hactar::make-tag :name "Other" :file "src/other.lisp" :kind "f"))))
    ;; Mocking enough-pathname behavior is tricky without messing with globals or file system.
    ;; tags-for uses (uiop:parse-native-namestring targ) and (uiop:enough-pathname p *repo-root*).
    ;; Let's assume input is relative path string which bypasses the absolute path logic if we are careful.

    (with-dynamic-stubs ((uiop:parse-native-namestring (lambda (p) p)) ; Mock path parsing to keep it simple strings
                         (uiop:absolute-pathname-p (lambda (p) (declare (ignore p)) nil))) ; Treat as relative

      (let ((res (hactar::tags-for "src/target.lisp")))
        (is (= 1 (length res)))
        (is (string= (hactar::tag-name (first res)) "InFile"))))))

(test load-tags-test
  "Test loading tags from a file."
  (uiop:with-temporary-file (:pathname tags-file :keep t)
    (let* ((repo-root (uiop:pathname-directory-pathname tags-file))
           (hactar::*repo-root* repo-root)
           (hactar::*ctags-file* (file-namestring tags-file)))

      (with-open-file (s tags-file :direction :output :if-exists :supersede)
        (format s "!_TAG_FILE_FORMAT	2	/extended format/~%!_TAG_PROGRAM_AUTHOR	Darren Hiebert	/dhiebert@users.sourceforge.net/~%foo	foo.c	/^void foo() {$/;\"	f~%bar	bar.c	/^int bar;$/;\"	v~%"))

      (let ((tags (hactar::load-tags)))
        (is (= 2 (length tags)))
        (is (find "foo" tags :key #'hactar::tag-name :test #'string=))
        (is (find "bar" tags :key #'hactar::tag-name :test #'string=))))))

(test tags-for-context-test
  "Test extracting tags relevant to context files."
  (let ((hactar::*files* '("test.lisp"))
        (hactar::*tags-cache* (list
                               (hactar::make-tag :name "used-function" :file "lib.lisp" :kind "f")
                               (hactar::make-tag :name "unused-function" :file "lib.lisp" :kind "f"))))

    (with-dynamic-stubs ((hactar::get-file-content (lambda (f)
                                                     (declare (ignore f))
                                                     "(defun my-code () (used-function))")))
      (let ((relevant-tags (hactar::tags-for-context)))
        (is (= 1 (length relevant-tags)))
        (is (string= (hactar::tag-name (first relevant-tags)) "used-function"))))))
