(in-package #:hactar-rebuild)

;;; ---------------------------------------------------------------------------
;;; Stack mode registry: each mode has a detector and a harness-forms builder.
;;; ---------------------------------------------------------------------------

(defun register-stack-mode (&key name detect forms stack-commands stack (priority 50))
  "Register a stack mode. DETECT is (root)->boolean, FORMS is (root)->list of
generated DSL forms, STACK-COMMANDS is a plist of :test/:lint/... command
templates, STACK is the normalized stack descriptor list, and PRIORITY orders
detection (lower is checked first; the generic fallback should be highest)."
  (setf (gethash (key-string name) *stack-modes*)
        (list :name (string-downcase (string name))
              :detect detect :forms forms :stack-commands stack-commands
              :stack stack :priority priority)))

(defun sorted-stack-modes ()
  "Return registered modes sorted by ascending :priority then name."
  (let ((modes '()))
    (maphash (lambda (k v) (declare (ignore k)) (push v modes)) *stack-modes*)
    (stable-sort (sort modes #'string< :key (lambda (m) (getf m :name)))
                 #'< :key (lambda (m) (or (getf m :priority) 50)))))

(defun file-exists-p (root name)
  (probe-file (merge-pathnames name (uiop:ensure-directory-pathname root))))

(defun file-contains-p (root name substring)
  (let ((f (file-exists-p root name)))
    (and f (search substring (uiop:read-file-string f)))))

(defun detect-stack (&optional (root (or *repo-root* (uiop:getcwd))))
  "Return (values stack-list mode-plist). Modes are tried in priority order;
the first match wins. The generic fallback matches everything and is tried last."
  (let ((generic nil))
    (dolist (mode (sorted-stack-modes))
      (cond
        ((string= (getf mode :name) "generic") (setf generic mode))
        ((funcall (getf mode :detect) root)
         (return-from detect-stack (values (mode-stack-list mode root) mode)))))
    (values (if generic (mode-stack-list generic root) '("unknown"))
            generic)))

(defun mode-stack-list (mode root)
  (declare (ignore root))
  (or (getf mode :stack) (list (getf mode :name))))

(defun install-stack-commands (mode)
  "Populate *stack-commands* from MODE's :stack-commands plist."
  (clrhash *stack-commands*)
  (loop for (k v) on (getf mode :stack-commands) by #'cddr
        do (setf (gethash k *stack-commands*) v)))

;;; ---------------------------------------------------------------------------
;;; Extraction: a compact project map
;;; ---------------------------------------------------------------------------

(defun read-file-string* (root name)
  (let ((f (file-exists-p root name)))
    (and f (uiop:read-file-string f))))

(defun extract-npm-scripts (root)
  "Naively extract npm script names from package.json's \"scripts\" object."
  (let ((json (read-file-string* root "package.json")))
    (when json
      (let ((start (search "\"scripts\"" json)))
        (when start
          (let* ((brace (position #\{ json :start start))
                 (close (and brace (position #\} json :start brace))))
            (when (and brace close)
              (let ((body (subseq json (1+ brace) close))
                    (names '()))
                (cl-ppcre:do-register-groups (name)
                    ("\"([A-Za-z0-9:_-]+)\"\\s*:" body)
                  (push name names))
                (nreverse names)))))))))

(defparameter *dependency-manifests*
  '("package.json" "Cargo.toml" "go.mod" "pyproject.toml" "requirements.txt"
    "Gemfile" "*.asd"))

(defun present-manifests (root)
  (remove nil (mapcar (lambda (n)
                        (cond ((string= n "*.asd")
                               (and (directory (merge-pathnames
                                                "*.asd"
                                                (uiop:ensure-directory-pathname root)))
                                    "*.asd"))
                              ((file-exists-p root n) n)))
                      *dependency-manifests*)))

(defun present-docs (root)
  (remove nil (mapcar (lambda (n) (and (file-exists-p root n) n))
                      '("README.md" "README.org" "README.rst" "docs/"
                        "CONTRIBUTING.md"))))

(defun extract-project-map (&optional (root (or *repo-root* (uiop:getcwd)))
                              (stack *stack*))
  "Build a compact project map plist: stack, manifests, scripts, docs, and the
resolved test command for the detected stack."
  (list :root (namestring (truename root))
        :stack stack
        :dependency-manifests (present-manifests root)
        :scripts (extract-npm-scripts root)
        :docs (present-docs root)
        :test-command (gethash :test *stack-commands*)))

;;; ---------------------------------------------------------------------------
;;; generate-harness: deterministic mode forms + defproject + project map
;;; ---------------------------------------------------------------------------

(defun stack-harness-forms (&optional (root (or *repo-root* (uiop:getcwd))))
  (multiple-value-bind (stack mode) (detect-stack root)
    (declare (ignore stack))
    (when mode
      (install-stack-commands mode)
      (funcall (getf mode :forms) root))))

(defun generate-harness (&optional (root (or *repo-root* (uiop:getcwd))))
  "Compose the generated forms for ROOT: defproject + harness forms."
  (multiple-value-bind (stack mode) (detect-stack root)
    (setf *stack* stack)
    (when mode (install-stack-commands mode))
    (append
     (list `(defproject :name ,(project-name root) :stack ,stack))
     (and mode (funcall (getf mode :forms) root)))))

(defun project-name (root)
  (let ((p (uiop:ensure-directory-pathname (truename root))))
    (car (last (pathname-directory p)))))

;;; ---------------------------------------------------------------------------
;;; Built-in stack modes
;;; ---------------------------------------------------------------------------

;; TypeScript + React Router
(defun react-router-p (root)
  (file-contains-p root "package.json" "react-router"))

(defun typescript-react-router-forms (root)
  (declare (ignore root))
  '((deftool test (&optional file)
      "Run unit tests for the current stack."
      (run-test-cmd file))
    (deftool lint ()
      "Lint the project."
      (run-lint-cmd))
    (deftool typecheck ()
      "Type-check the project."
      (run-typecheck-cmd))
    (deftool ast-edit (path &rest operations)
      "Edit TypeScript/JavaScript via AST."
      (apply #'run-ast-edit path operations))
    (deftool rr-routes ()
      "List the route tree."
      (shell-cmd "npx react-router routes" :sandbox "project"))
    (defskill react-router :doc "React Router project conventions."
      :docs ("react-router.org"))
    (defsandbox project :allow ("npx" "node" "git"))
    (defaction route.create
      :doc "Create a typed React Router route + loader/action."
      :prompt "Create a React Router route {{name}} under src/routes using a typed loader and action, then register it in src/routes.tsx. {{doc:react-router}}"
      :apply :create)
    (defaction api.create
      :doc "Create a typed API resource route."
      :prompt "Create an API resource route {{name}} with typed handlers for this {{stack}} project."
      :apply :create)
    (defagent dev
      :model "anthropic/claude-sonnet-4-6"
      :tools (test lint typecheck rr-routes ast-edit)
      :skills (react-router)
      :sandbox project
      :instructions "Build React Router features end-to-end. Use (test) to verify. Edit TypeScript through ast-edit when possible.")))

(register-stack-mode
 :name "typescript-react-router"
 :detect #'react-router-p
 :forms #'typescript-react-router-forms
 :stack '("typescript" "react" "react-router" "vite")
 :priority 20
 :stack-commands '(:test "npx vitest run" :lint "npx biome check ."
                   :typecheck "npx tsc --noEmit" :build "npm run build"))

;; Rails
(defun rails-p (root)
  (or (file-contains-p root "Gemfile" "rails")
      (file-exists-p root "config/application.rb")))

(defun rails-forms (root)
  (declare (ignore root))
  '((deftool test (&optional file)
      "Run RSpec tests."
      (run-test-cmd file))
    (deftool lint ()
      "Run Rubocop."
      (run-lint-cmd))
    (deftool rails-routes ()
      "List Rails routes."
      (shell-cmd "bin/rails routes" :sandbox "project"))
    (deftool rails-db-migrate ()
      "Run pending database migrations."
      (shell-cmd "bin/rails db:migrate" :sandbox "project"))
    (defskill rails :doc "Rails project conventions.")
    (defsandbox project :allow ("bin/rails" "bundle" "rspec" "git"))
    (defaction migration.create
      :doc "Create a database migration."
      :prompt "Create a Rails migration named {{name}}." :apply :create)
    (defaction model.create
      :doc "Create a model." :prompt "Create a Rails model {{name}}."
      :apply :create)
    (defaction controller.create
      :doc "Create a controller." :prompt "Create a Rails controller {{name}}."
      :apply :create)
    (defagent dev
      :model "anthropic/claude-sonnet-4-6"
      :tools (test lint rails-routes rails-db-migrate)
      :skills (rails) :sandbox project
      :instructions "Build Rails features end-to-end. Use (test) to verify.")))

(register-stack-mode
 :name "rails"
 :detect #'rails-p
 :forms #'rails-forms
 :stack '("ruby" "rails")
 :priority 20
 :stack-commands '(:test "bundle exec rspec" :lint "bundle exec rubocop"))

;; Common Lisp (self-hosting)
(defun common-lisp-p (root)
  (some (lambda (n) (file-exists-p root n))
        (directory (merge-pathnames "*.asd"
                                    (uiop:ensure-directory-pathname root)))))

(defun common-lisp-forms (root)
  (declare (ignore root))
  '((deftool test (&optional file)
      "Run the Lisp test suite via the Makefile."
      (declare (ignore file))
      (run-test-cmd))
    (deftool build ()
      "Build the Lisp system via the Makefile."
      (run-build-cmd))
    (deftool ast-edit (path &rest operations)
      "Structural sexp edits (Hactar eats its own food)."
      (apply #'run-ast-edit path operations))
    (defskill common-lisp :doc "Common Lisp / ASDF conventions.")
    (defsandbox project :allow ("make" "sbcl" "git"))
    (defaction system.create
      :doc "Create a new ASDF component." :prompt "Add an ASDF component {{name}}."
      :apply :create)
    (defagent dev
      :model "anthropic/claude-sonnet-4-6"
      :tools (test build ast-edit) :skills (common-lisp) :sandbox project
      :instructions "Build Lisp features end-to-end. Use (test) to verify via make test.")))

(register-stack-mode
 :name "common-lisp"
 :detect #'common-lisp-p
 :forms #'common-lisp-forms
 :stack '("common-lisp" "asdf")
 :priority 30
 :stack-commands '(:test "make test" :build "make build"))

;; Next.js (checked before React Router so "next" wins for Next projects)
(defun nextjs-p (root)
  (or (file-contains-p root "package.json" "\"next\"")
      (file-exists-p root "next.config.js")
      (file-exists-p root "next.config.mjs")
      (file-exists-p root "next.config.ts")))

(defun nextjs-forms (root)
  (declare (ignore root))
  '((deftool test (&optional file)
      "Run unit tests for the current stack."
      (run-test-cmd file))
    (deftool lint ()
      "Lint the project."
      (run-lint-cmd))
    (deftool typecheck ()
      "Type-check the project."
      (run-typecheck-cmd))
    (deftool ast-edit (path &rest operations)
      "Edit TypeScript/JavaScript via AST (fern backend)."
      (apply #'run-ast-edit path operations))
    (defskill nextjs :doc "Next.js App Router conventions." :docs ("nextjs.org"))
    (defsandbox project :allow ("npx" "node" "pnpm" "yarn" "git"))
    (defaction page.create
      :doc "Create a Next.js App Router page."
      :prompt "Create a Next.js App Router page {{name}} under app/, with a typed server component. {{doc:nextjs}}"
      :apply :create)
    (defaction route.create
      :doc "Create a Next.js route handler."
      :prompt "Create a Next.js route handler {{name}} under app/api using typed Request/Response."
      :apply :create)
    (defagent dev
      :model "anthropic/claude-sonnet-4-6"
      :tools (test lint typecheck ast-edit)
      :skills (nextjs) :sandbox project
      :instructions "Build Next.js features end-to-end. Use (test) to verify. Edit TypeScript through ast-edit when possible.")))

(register-stack-mode
 :name "nextjs"
 :detect #'nextjs-p
 :forms #'nextjs-forms
 :stack '("typescript" "react" "nextjs")
 :priority 15
 :stack-commands '(:test "npx vitest run" :lint "npx next lint"
                   :typecheck "npx tsc --noEmit" :build "npx next build"))

;; Python (Django / Flask / FastAPI)
(defun python-p (root)
  (or (file-exists-p root "pyproject.toml")
      (file-exists-p root "requirements.txt")
      (file-exists-p root "setup.py")
      (file-exists-p root "manage.py")))

(defun python-framework (root)
  "Best-effort framework detection for prompt/skill context."
  (cond
    ((file-exists-p root "manage.py") "django")
    ((or (file-contains-p root "pyproject.toml" "fastapi")
         (file-contains-p root "requirements.txt" "fastapi")) "fastapi")
    ((or (file-contains-p root "pyproject.toml" "flask")
         (file-contains-p root "requirements.txt" "flask")) "flask")
    (t "python")))

(defun python-forms (root)
  (declare (ignore root))
  '((deftool test (&optional file)
      "Run the test suite (pytest)."
      (run-test-cmd file))
    (deftool lint ()
      "Lint with ruff."
      (run-lint-cmd))
    (deftool typecheck ()
      "Type-check with pyright."
      (run-typecheck-cmd))
    (defskill python :doc "Python project conventions (pytest, ruff, typing).")
    (defsandbox project :allow ("python" "python3" "pytest" "ruff" "pyright"
                                "pip" "uv" "git"))
    (defaction module.create
      :doc "Create a Python module."
      :prompt "Create a Python module {{name}} with typed functions and a pytest test. {{doc:python}}"
      :apply :create)
    (defagent dev
      :model "anthropic/claude-sonnet-4-6"
      :tools (test lint typecheck) :skills (python) :sandbox project
      :instructions "Build Python features end-to-end. Use (test) to verify. Keep functions typed.")))

(register-stack-mode
 :name "python"
 :detect #'python-p
 :forms #'python-forms
 :stack '("python")
 :priority 25
 :stack-commands '(:test "pytest" :lint "ruff check ." :typecheck "pyright"))

;; Go
(defun go-p (root)
  (file-exists-p root "go.mod"))

(defun go-forms (root)
  (declare (ignore root))
  '((deftool test (&optional file)
      "Run Go tests."
      (declare (ignore file))
      (run-test-cmd))
    (deftool lint ()
      "Vet the project."
      (run-lint-cmd))
    (deftool build ()
      "Build the project."
      (run-build-cmd))
    (defskill go :doc "Go project conventions (go test, go vet, gofmt).")
    (defsandbox project :allow ("go" "gofmt" "golangci-lint" "git"))
    (defaction package.create
      :doc "Create a Go package."
      :prompt "Create a Go package {{name}} with an idiomatic exported API and a _test.go file. {{doc:go}}"
      :apply :create)
    (defagent dev
      :model "anthropic/claude-sonnet-4-6"
      :tools (test lint build) :skills (go) :sandbox project
      :instructions "Build Go features end-to-end. Use (test) to verify. Run (lint) before finishing.")))

(register-stack-mode
 :name "go"
 :detect #'go-p
 :forms #'go-forms
 :stack '("go")
 :priority 25
 :stack-commands '(:test "go test ./..." :lint "go vet ./..." :build "go build ./..."))

;; Rust
(defun rust-p (root)
  (file-exists-p root "Cargo.toml"))

(defun rust-forms (root)
  (declare (ignore root))
  '((deftool test (&optional file)
      "Run cargo tests."
      (declare (ignore file))
      (run-test-cmd))
    (deftool lint ()
      "Run clippy."
      (run-lint-cmd))
    (deftool typecheck ()
      "Type-check with cargo check."
      (run-typecheck-cmd))
    (deftool build ()
      "Build with cargo."
      (run-build-cmd))
    (defskill rust :doc "Rust project conventions (cargo test, clippy).")
    (defsandbox project :allow ("cargo" "rustc" "rustfmt" "git"))
    (defaction module.create
      :doc "Create a Rust module."
      :prompt "Create a Rust module {{name}} with idiomatic types and unit tests. {{doc:rust}}"
      :apply :create)
    (defagent dev
      :model "anthropic/claude-sonnet-4-6"
      :tools (test lint typecheck build) :skills (rust) :sandbox project
      :instructions "Build Rust features end-to-end. Use (test) to verify. Run (lint) before finishing.")))

(register-stack-mode
 :name "rust"
 :detect #'rust-p
 :forms #'rust-forms
 :stack '("rust")
 :priority 25
 :stack-commands '(:test "cargo test" :lint "cargo clippy"
                   :typecheck "cargo check" :build "cargo build"))

;; Generic fallback
(defun generic-forms (root)
  (declare (ignore root))
  '((deftool test (&optional file)
      "Best-effort test entry point."
      (declare (ignore file))
      (run-test-cmd))
    (defsandbox project :allow ("git" "make"))
    (defagent dev
      :model "anthropic/claude-sonnet-4-6"
      :tools (test) :sandbox project
      :instructions "Inspect the repository first. Prefer compound tools; avoid destructive writes. Ask the user to refine .hactar.lisp.")))

(register-stack-mode
 :name "generic"
 :detect (constantly t)
 :forms #'generic-forms
 :stack '("generic")
 :priority 1000
 :stack-commands '(:test "make test"))
