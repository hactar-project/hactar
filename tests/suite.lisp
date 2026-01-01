(in-package :hactar-tests)

;;; Test Database Configuration
(defvar *test-db-path* (or (uiop:getenv "HACTAR_TEST_DB_PATH") ":memory:") "Test database path.")
(def-suite hactar-tests
  :description "Tests for hactar")

(defun make-temp-dir ()
  "Create a temporary directory and return its native namestring."
  (let* ((base (uiop:temporary-directory))
         (dir (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "hactar-tests-~A/" (uuid:make-v4-uuid)) base))))
    (ensure-directories-exist dir)
    (uiop:native-namestring dir)))
