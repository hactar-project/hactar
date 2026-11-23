(in-package :hactar-user)

(defmode python
  :priority 25
  :stack ("python")
  :detect (:any (:file-exists "pyproject.toml")
                (:file-exists "requirements.txt")
                (:file-exists "setup.py")
                (:file-exists "manage.py"))
  :stack-commands (:test "pytest" :lint "ruff check ." :typecheck "pyright")
  :forms ((deftool test (&optional file)
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

(defmode go
  :priority 25
  :stack ("go")
  :detect (:file-exists "go.mod")
  :stack-commands (:test "go test ./..." :lint "go vet ./..." :build "go build ./...")
  :forms ((deftool test (&optional file)
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

(defmode rust
  :priority 25
  :stack ("rust")
  :detect (:file-exists "Cargo.toml")
  :stack-commands (:test "cargo test" :lint "cargo clippy"
                   :typecheck "cargo check" :build "cargo build")
  :forms ((deftool test (&optional file)
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
