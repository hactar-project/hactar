(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test self-update-rolls-back-on-invalid-output
  (with-temp-project (root)
    (hactar-rebuild:write-hactar-file
     (hactar-rebuild:hactar-file-path root)
     '((defproject :name "x" :stack ("ts"))))
    (let ((before (uiop:read-file-string (hactar-rebuild:hactar-file-path root)))
          (hactar-rebuild:*mock-diffusion-output* "(defun nope ())"))
      (hactar-rebuild:run-self-update)
      (is (string= before (uiop:read-file-string
                           (hactar-rebuild:hactar-file-path root))))
      (is (hactar-rebuild:last-update-failed-p)))))

(test self-update-applies-valid-output
  (with-temp-project (root)
    (hactar-rebuild:write-hactar-file
     (hactar-rebuild:hactar-file-path root)
     '((defproject :name "x" :stack ("ts"))))
    (let ((hactar-rebuild:*mock-diffusion-output*
            "(defproject :name \"x\" :stack (\"ts\")) (deftool fresh () \"d\" 1)"))
      (hactar-rebuild:run-self-update)
      (is (not (hactar-rebuild:last-update-failed-p)))
      (hactar-rebuild:load-hactar-file (hactar-rebuild:hactar-file-path root))
      (is (gethash "fresh" hactar-rebuild::*tools*)))))
