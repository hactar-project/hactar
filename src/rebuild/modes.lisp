(in-package #:hactar-rebuild)

;;; ---------------------------------------------------------------------------
;;; Stack mode registry: each mode has a detector and a harness-forms builder.
;;; ---------------------------------------------------------------------------

(defun register-stack-mode (&key name detect forms stack-commands)
  "Register a stack mode. DETECT is (root)->boolean, FORMS is (root)->list of
generated DSL forms, STACK-COMMANDS is a plist of :test/:lint/... command
templates."
  (setf (gethash (key-string name) *stack-modes*)
        (list :name (string-downcase (string name))
              :detect detect :forms forms :stack-commands stack-commands)))

(defun file-exists-p (root name)
  (probe-file (merge-pathnames name (uiop:ensure-directory-pathname root))))

(defun file-contains-p (root name substring)
  (let ((f (file-exists-p root name)))
    (and f (search substring (uiop:read-file-string f)))))

(defun detect-stack (&optional (root (or *repo-root* (uiop:getcwd))))
  "Return (values stack-list mode-plist). The first matching mode wins;
generic fallback matches everything."
  (let ((generic nil))
    (loop for mode being the hash-values of *stack-modes*
          do (cond
               ((string= (getf mode :name) "generic") (setf generic mode))
               ((funcall (getf mode :detect) root)
                (return-from detect-stack
                  (values (mode-stack-list mode root) mode)))))
    (values (if generic (mode-stack-list generic root) '("unknown"))
            generic)))

(defun mode-stack-list (mode root)
  (declare (ignore root))
  (let ((name (getf mode :name)))
    (cond
      ((string= name "typescript-react-router")
       (list "typescript" "react" "react-router" "vite"))
      ((string= name "rails") (list "ruby" "rails"))
      ((string= name "common-lisp") (list "common-lisp" "asdf"))
      (t (list name)))))

(defun install-stack-commands (mode)
  "Populate *stack-commands* from MODE's :stack-commands plist."
  (clrhash *stack-commands*)
  (loop for (k v) on (getf mode :stack-commands) by #'cddr
        do (setf (gethash k *stack-commands*) v)))

;;; ---------------------------------------------------------------------------
;;; Extraction: a compact project map
;;; ---------------------------------------------------------------------------

(defun extract-project-map (&optional (root (or *repo-root* (uiop:getcwd)))
                              (stack *stack*))
  "Build a compact project map plist."
  (list :root (namestring (truename root))
        :stack stack
        :files-of-interest
        (remove nil
                (mapcar (lambda (n) (and (file-exists-p root n) n))
                        '("package.json" "Cargo.toml" "go.mod" "pyproject.toml"
                          "Gemfile" "Makefile" "README.md")))))

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
 :stack-commands '(:test "make test" :build "make build"))

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
 :stack-commands '(:test "make test"))
