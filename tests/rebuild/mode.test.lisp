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

(test detects-nextjs-stack
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"next\":\"14\"}}")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (is (string= "nextjs" (getf mode :name)))
      (is (member "nextjs" stack :test #'string=)))))

(test detects-python-stack
  (with-temp-project (root :files '(("pyproject.toml" . "[project]")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (is (string= "python" (getf mode :name)))
      (is (member "python" stack :test #'string=)))))

(test detects-django-via-manage-py
  (with-temp-project (root :files '(("manage.py" . "# django")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (is (string= "python" (getf mode :name))))))

(test detects-go-stack
  (with-temp-project (root :files '(("go.mod" . "module x")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (is (string= "go" (getf mode :name)))
      (is (member "go" stack :test #'string=)))))

(test detects-rust-stack
  (with-temp-project (root :files '(("Cargo.toml" . "[package]")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (is (string= "rust" (getf mode :name)))
      (is (member "rust" stack :test #'string=)))))

(test nextjs-beats-generic-and-has-harness
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"next\":\"14\"}}")))
    (let ((forms (hactar-rebuild:generate-harness root)))
      (hactar-rebuild:reset-registries)
      (dolist (f forms) (eval-in-user f))
      (is (gethash "dev" hactar-rebuild::*agents*))
      (is (gethash "page.create" hactar-rebuild::*actions*))
      (is (gethash "nextjs" hactar-rebuild::*skills*)))))

(test go-harness-resolves-test-command
  (with-temp-project (root :files '(("go.mod" . "module x")))
    (hactar-rebuild:generate-harness root)
    (is (string= "go test ./..."
                 (gethash :test hactar-rebuild::*stack-commands*)))))

(test detection-is-deterministic-priority-order
  ;; A project with both go.mod and a Cargo.toml: go (priority 25, name<) — but
  ;; both are 25, so name order decides deterministically (go before rust).
  (with-temp-project (root :files '(("go.mod" . "module x")
                                    ("Cargo.toml" . "[package]")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (declare (ignore stack))
      (is (string= "go" (getf mode :name))))))

(test extract-project-map-includes-scripts-and-docs
  (with-temp-project (root :files '(("package.json" .
       "{\"scripts\":{\"build\":\"x\",\"test\":\"vitest\"},\"dependencies\":{\"react-router\":\"7\"}}")
                                    ("README.md" . "# hi")))
    (multiple-value-bind (stack mode) (hactar-rebuild:detect-stack root)
      (setf hactar-rebuild:*stack* stack)
      (hactar-rebuild::install-stack-commands mode))
    (let ((map (hactar-rebuild:extract-project-map root hactar-rebuild:*stack*)))
      (is (member "build" (getf map :scripts) :test #'string=))
      (is (member "test" (getf map :scripts) :test #'string=))
      (is (member "README.md" (getf map :docs) :test #'string=))
      (is (string= "npx vitest run" (getf map :test-command))))))
