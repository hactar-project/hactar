(in-package :hactar-tests)

(def-suite cli-tests
  :description "Tests for the unified defflag/defroute CLI system.")

(in-suite cli-tests)

(test register-hactar-flags-test
  "REGISTER-HACTAR-FLAGS populates *FLAGS* with known flags."
  (hactar::register-hactar-flags)
  (is-true (gethash "--model" hactar::*flags*))
  (is-true (gethash "-m" hactar::*flags*))
  (is-true (gethash "--tui" hactar::*flags*))
  (is-true (gethash "--acp" hactar::*flags*))
  (is-true (gethash "--help" hactar::*flags*)))

(test cli-opts-population-test
  "Parsing CLI args populates *CLI-OPTS* via the flag handlers."
  (hactar::register-hactar-flags)
  (clrhash hactar::*cli-opts*)
  (hactar::parse-cli-input '("--model" "test/model" "--tui" "extra"))
  (is (string= (hactar::cli-opt :model) "test/model"))
  (is-true (hactar::cli-opt :tui)))

(test cli-positional-preserved-test
  "Positional arguments survive flag parsing."
  (hactar::register-hactar-flags)
  (clrhash hactar::*cli-opts*)
  (let ((rest (hactar::parse-cli-input '("--model" "m" "hactar.init" "--name" "x"))))
    (is (equal rest '("hactar.init")))
    (is (string= (hactar::cli-opt :model) "m"))
    (is (string= (hactar::cli-opt :name) "x"))))

(test cli-opt-setf-test
  "Test (setf cli-opt) updates *cli-opts*."
  (clrhash hactar::*cli-opts*)
  (setf (hactar::cli-opt :my-custom-opt) "my-value")
  (is (string= (hactar::cli-opt :my-custom-opt) "my-value")))
