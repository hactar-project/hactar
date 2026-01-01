(in-package :hactar-tests)

(def-suite hactar-core-guide-tests
  :description "Tests for core guide functionality.")

(in-suite hactar-core-guide-tests)

(test active-guide-file-resolution-test
  "Test that active guide file is resolved correctly."
  (let* ((temp-dir (make-temp-dir))
         (guide-file (merge-pathnames ".hactar.guide.org" temp-dir)))
    (unwind-protect
        (progn
          (with-open-file (s guide-file :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format s "* Guide~%Content"))
          (let ((hactar::*hactar-guide-path* nil)
                (hactar::*active-guide-file* nil))
            (hactar::resolve-active-guide-file temp-dir)
            (is-true hactar::*active-guide-file*)))
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t)))))
