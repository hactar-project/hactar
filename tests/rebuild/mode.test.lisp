(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test detects-react-router-stack
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"react-router\":\"7\"}}")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (declare (ignore mode))
      (is (member "react-router" stack :test #'string=)))))

(test detects-rails-stack
  (with-temp-project (root :files '(("Gemfile" . "gem 'rails'")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (declare (ignore mode))
      (is (member "rails" stack :test #'string=)))))

(test detects-common-lisp-stack
  (with-temp-project (root :files '(("demo.asd" . "(defsystem :demo)")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (declare (ignore mode))
      (is (member "common-lisp" stack :test #'string=)))))

(test unknown-repo-falls-back-to-generic
  (with-temp-project (root :files '(("notes.txt" . "hello")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (is (string= "generic" (getf mode :name)))
      (is (equal '("generic") stack)))))

(test react-router-harness-generates-worked-example
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"react-router\":\"7\"}}")))
    (let ((forms (hactar-rebuild:generate-harness root)))
      (hactar-rebuild:reset-registries)
      (dolist (f forms) (eval-in-user f))
      (is (gethash "dev" hactar-rebuild::*agents*))
      (is (gethash "test" hactar-rebuild::*tools*))
      (is (gethash "route.create" hactar-rebuild::*actions*))
      (is (gethash "react-router" hactar-rebuild::*skills*)))))
