(in-package :hactar-tests)

(def-suite gen-helpers-tests
  :description "Tests for generation scaffold helpers")

(in-suite gen-helpers-tests)

(test refactor-scaffold-helpers
  "Test scaffold-create-file and scaffold-modify-file."
  (let ((hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory))))
    (let* ((rel-path "src/scaffold-test.txt")
           (full-path (merge-pathnames rel-path hactar::*repo-root*)))
      ;; Clean up if it somehow exists
      (ignore-errors (delete-file full-path))

      ;; 1. Test scaffold-create-file
      (let ((res (hactar::scaffold-create-file rel-path "Initial Content" :confirm nil)))
        (is-true res)
        (is-true (probe-file full-path))
        (is (string= "Initial Content" (uiop:read-file-string full-path)))

        ;; 2. Test scaffold-modify-file
        (let ((mod-res (hactar::scaffold-modify-file rel-path "Initial" "Modified")))
          (is-true mod-res)
          (is (string= "Modified Content" (uiop:read-file-string full-path))))

        ;; Clean up
        (ignore-errors (delete-file full-path))))))
