(in-package :hactar-tests)

(def-suite rules-tests
  :description "Tests for rules management commands")

(in-suite rules-tests)

(test rules-show
  "Test rules.show command output"
  (let ((hactar::*active-rules* (make-hash-table :test 'equal)))
    (is (string= (format nil "No active rules.~%")
                 (with-output-to-string (*standard-output*)
                   (hactar::run-rules-show))))
    (setf (gethash "test-rule" hactar::*active-rules*) "Do not harm humans.")
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::run-rules-show))))
      (is-true (search "Active Rules:" output))
      (is-true (search "test-rule" output))
      (is-true (search "Do not harm humans." output)))))

(test rules-list-and-available
  "Test listing available rules"
  (uiop:with-temporary-file (:pathname temp-file)
    (let* ((temp-dir (uiop:pathname-directory-pathname temp-file))
           (hactar::*hactar-rules-path* (merge-pathnames "hactar-test-rules/" temp-dir)))
      (ensure-directories-exist hactar::*hactar-rules-path*)
      (unwind-protect
           (progn
             (with-open-file (s (merge-pathnames "rule1.lisp" hactar::*hactar-rules-path*)
                                :direction :output :if-does-not-exist :create)
               (format s ";; rule1"))
             (with-open-file (s (merge-pathnames "rule2.lisp" hactar::*hactar-rules-path*)
                                :direction :output :if-does-not-exist :create)
               (format s ";; rule2"))

             (let ((available (hactar::list-available-rules)))
               (is (= 2 (length available)))
               (is-true (member "rule1" available :test #'string=))
               (is-true (member "rule2" available :test #'string=)))

             (let ((output (with-output-to-string (*standard-output*)
                             (hactar::run-rules-list))))
               (is-true (search "Available Rules:" output))
               (is-true (search "rule1" output))
               (is-true (search "rule2" output))))
        (uiop:delete-directory-tree hactar::*hactar-rules-path* :validate t)))))

(test rules-add
  "Test adding/loading a rule"
  (uiop:with-temporary-file (:pathname temp-file)
    (let* ((temp-dir (uiop:pathname-directory-pathname temp-file))
           (hactar::*hactar-rules-path* (merge-pathnames "hactar-test-rules-add/" temp-dir)))
      (ensure-directories-exist hactar::*hactar-rules-path*)
      (unwind-protect
           (progn
             (with-open-file (s (merge-pathnames "dummy-rule.lisp" hactar::*hactar-rules-path*)
                                :direction :output :if-does-not-exist :create)
               (format s "(defvar hactar::*dummy-rule-loaded* t)~%(setf (gethash \"dummy-rule\" hactar::*active-rules*) \"dummy content\")"))

             (let ((hactar::*active-rules* (make-hash-table :test 'equal)))
               (let ((output (with-output-to-string (*standard-output*)
                               (hactar::run-rules-add '("dummy-rule")))))
                 (is-true (search "Loaded rule: dummy-rule" output))
                 (is (boundp 'hactar::*dummy-rule-loaded*))
                 (is (eq t (symbol-value 'hactar::*dummy-rule-loaded*)))
                 (is (string= "dummy content" (gethash "dummy-rule" hactar::*active-rules*))))

               (let ((output (with-output-to-string (*standard-output*)
                               (hactar::run-rules-add '("non-existent-rule")))))
                 (is-true (search "not found" output)))))
        (uiop:delete-directory-tree hactar::*hactar-rules-path* :validate t)))))

(test rules-alias
  "Test that /rules command aliases /rules.list and examples are registered"
  (uiop:with-temporary-file (:pathname temp-file)
    (let* ((temp-dir (uiop:pathname-directory-pathname temp-file))
           (hactar::*hactar-rules-path* (merge-pathnames "hactar-test-rules-alias/" temp-dir)))
      (ensure-directories-exist hactar::*hactar-rules-path*)
      (unwind-protect
           (progn
             (with-open-file (s (merge-pathnames "rule-alias.lisp" hactar::*hactar-rules-path*)
                                :direction :output :if-does-not-exist :create)
               (format s ";; rule-alias"))

             (let ((output (with-output-to-string (*standard-output*)
                             (hactar::execute-command "/rules" nil))))
               (is-true (search "Available Rules:" output))
               (is-true (search "rule-alias" output))))
        (uiop:delete-directory-tree hactar::*hactar-rules-path* :validate t))))

  ;; Verify command examples are correctly registered in *commands*
  (let ((rules-info (gethash "/rules" hactar::*commands*))
        (list-info (gethash "/rules.list" hactar::*commands*))
        (show-info (gethash "/rules.show" hactar::*commands*))
        (add-info (gethash "/rules.add" hactar::*commands*))
        (install-info (gethash "/rules.install" hactar::*commands*)))
    (is (equal '("/rules") (fifth rules-info)))
    (is (equal '("/rules.list") (fifth list-info)))
    (is (equal '("/rules.show") (fifth show-info)))
    (is (equal '("/rules.add my-custom-rule") (fifth add-info)))
    (is (equal '("/rules.install user/repo" "/rules.install https://github.com/user/repo.git") (fifth install-info)))))

(test rules-slash-help-shows-examples
  "Test that /rules.show --help prints examples in slash command mode"
  (let ((output (with-output-to-string (*standard-output*)
                  (hactar::execute-command "/rules.show" '("--help")))))
    (is-true (search "Examples:" output))
    (is-true (search "/rules.show" output)))
  (let ((output (with-output-to-string (*standard-output*)
                  (hactar::execute-command "/rules.add" '("--help")))))
    (is-true (search "Examples:" output))
    (is-true (search "/rules.add my-custom-rule" output))))
