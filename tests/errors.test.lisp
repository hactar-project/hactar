;; errors handling tests
(in-package :hactar-tests)

(def-suite errors-tests
  :description "Tests for error handling")

(in-suite errors-tests)

(test format-error-for-preview-test
  "Test formatting an error for fzf preview."
  (let* ((err '((:title . "Test Error")
                (:code . "E001")
                (:stack . "lisp")
                (:slug . "lisp:E001")
                (:message . "Something broke")
                (:cause . "Bad syntax")
                (:solution . "Fix syntax")))
         (preview (hactar::format-error-for-preview err)))
    (is (search "Title: Test Error" preview))
    (is (search "Code: E001" preview))
    (is (search "Stack: lisp" preview))
    (is (search "Slug: lisp:E001" preview))
    (is (search "Message: Something broke" preview))
    (is (search "Cause: Bad syntax" preview))
    (is (search "Solution: Fix syntax" preview))))

(test deferror-macro-test
  "Test defining an error via deferror."
  (let ((hactar::*errors* nil))
    (eval '(hactar::deferror "My Error"
             :code "E123"
             :stack "test"
             :message "msg"
             :cause "cause"
             :solution "sol"
             :tags '("tag")))
    (is (= 1 (length hactar::*errors*)))
    (let ((err (first hactar::*errors*)))
      (is (string= "My Error" (cdr (assoc :title err))))
      (is (string= "E123" (cdr (assoc :code err))))
      (is (string= "test:E123" (cdr (assoc :slug err))))
      (is (equal '("tag") (cdr (assoc :tags err)))))))
