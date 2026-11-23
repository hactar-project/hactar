(in-package :hactar-user)

(generated :updated-at "<2026-06-30 Tue 21:48>"

  (defproject :name "hactar" :stack ("common-lisp" "asdf"))
  (deftool test
      (&optional file)
    "Run the Lisp test suite via the Makefile."
    (declare (ignore file))
    (run-test-cmd))
  (deftool build
      nil
    "Build the Lisp system via the Makefile."
    (run-build-cmd))
  (deftool ast-edit
      (path &rest operations)
    "Structural sexp edits (Hactar eats its own food)."
    (apply #'run-ast-edit path operations))
  (defskill common-lisp :doc "Common Lisp / ASDF conventions.")
  (defsandbox project :allow ("make" "sbcl" "git"))
  (defaction system.create :doc "Create a new ASDF component." :prompt
             "Add an ASDF component {{name}}." :apply :create)
  (defagent dev :model "anthropic/claude-sonnet-4-6" :tools (test build ast-edit) :skills
            (common-lisp) :sandbox project :instructions
            "Build Lisp features end-to-end. Use (test) to verify via make test."))

;; --- USER REGION (preserved across updates) ---

