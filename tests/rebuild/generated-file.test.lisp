(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test write-load-roundtrip
  (with-temp-project (root)
    (let ((path (hactar-rebuild:hactar-file-path root)))
      (hactar-rebuild:write-hactar-file
       path (engine-forms "(defproject :name \"x\" :stack (\"ts\")) (deftool test () \"d\" 1)"))
      (hactar-rebuild:load-hactar-file path)
      (is (gethash "test" hactar-rebuild::*tools*)))))

(test preserves-user-region
  (with-temp-project (root)
    (let ((path (hactar-rebuild:hactar-file-path root)))
      (hactar-rebuild:write-hactar-file path (engine-forms "(defproject :name \"x\" :stack (\"ts\"))"))
      (with-open-file (s path :direction :output :if-exists :append)
        (format s "~%;; user-owned marker line~%"))
      (hactar-rebuild:write-hactar-file
       path (engine-forms "(defproject :name \"y\" :stack (\"rails\"))"))
      (let ((content (uiop:read-file-string path)))
        (is (search "user-owned marker line" content))
        (is (search "rails" content))))))

(test read-forms-disables-read-eval
  (with-temp-project (root)
    (let ((path (merge-pathnames "danger.lisp" root)))
      (with-open-file (s path :direction :output)
        (write-string "(defproject :stack (#.(error \"boom\")))" s))
      ;; *read-eval* nil => #. must signal a reader error, not execute.
      (signals error (hactar-rebuild:read-forms path)))))
