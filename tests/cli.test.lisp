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
  (let ((rest (hactar::parse-cli-input '("--model" "m" "hactar.init" "--project-name" "x"))))
    (is (equal rest '("hactar.init")))
    (is (string= (hactar::cli-opt :model) "m"))
    (is (string= (hactar::cli-opt :name) "x"))))

(test cli-opt-setf-test
  "Test (setf cli-opt) updates *cli-opts*."
  (clrhash hactar::*cli-opts*)
  (setf (hactar::cli-opt :my-custom-opt) "my-value")
  (is (string= (hactar::cli-opt :my-custom-opt) "my-value")))

(test cli-query-subcommand-test
  "Test that the query subcommand is registered."
  (is-true (gethash "query" hactar::*sub-commands*)))

(test cli-execute-subcommand-test
  "Test that the execute subcommand is registered."
  (is-true (gethash "execute" hactar::*sub-commands*)))

(test cli-copilotapi-subcommand-test
  "Test that the copilotapi subcommand is registered."
  (is-true (gethash "copilotapi" hactar::*sub-commands*)))

(test cli-copilotapi-help-test
  "Test that copilotapi help prints help instructions."
  (let ((output (with-output-to-string (*standard-output*)
                  (funcall (first (gethash "copilotapi" hactar::*sub-commands*)) '("help")))))
    (is-true (search "Usage: hactar copilotapi" output))
    (is-true (search "help       - Display this help message" output))))

(test cli-syntax-recording-test
  "Test that :syntax option is recorded correctly."
  (let ((hactar::*sub-commands* (make-hash-table :test 'equal)))
    (eval `(hactar::define-sub-command test-syntax-cmd (args)
             "A test command for syntax"
             (declare (ignore args))
             :syntax "hactar test-syntax-cmd <arg1>"))
    (let ((info (gethash "test-syntax-cmd" hactar::*sub-commands*)))
      (is (string= (sixth info) "hactar test-syntax-cmd <arg1>")))))

(test cli-man-page-generation-test
  "Test that emit-man-page outputs valid troff markup."
  (let ((output (with-output-to-string (s)
                  (hactar::emit-man-page :stream s))))
    (is-true (search ".TH HACTAR 1" output))
    (is-true (search ".SH NAME" output))
    (is-true (search ".SH SYNOPSIS" output))
    (is-true (search ".SH DESCRIPTION" output))
    (is-true (search ".SH OPTIONS" output))
    (is-true (search ".SH SUBCOMMANDS" output))))

(test cli-man-install-test
  "Test hactar.man.install writes output to writable dir."
  (uiop:with-temporary-file (:pathname temp-file :keep nil)
    (let* ((temp-dir (uiop:ensure-directory-pathname
                      (make-pathname :name nil :type nil :defaults temp-file)))
           (hactar::*sub-commands* (make-hash-table :test 'equal)))
      ;; We can test the helper directly
      (is-true (hactar::directory-writable-p temp-dir))
      ;; Test writing to a nested path in the temp directory
      (let ((dest (merge-pathnames "hactar-man-test" temp-dir)))
        (with-open-file (out dest :direction :output :if-exists :supersede :if-does-not-exist :create)
          (hactar::emit-man-page :stream out))
        (is-true (uiop:file-exists-p dest))
        (uiop:delete-file-if-exists dest)))))

