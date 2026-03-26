(in-package :hactar-tests)

;;; --- Test Runner ---

(defun %ensure-fresh-test-db! (db-path)
  "Ensure DB-PATH is fresh before running tests. For file DBs, delete the file."
  (unless (string= db-path ":memory:")
    (when (probe-file db-path)
      (format t "~&Deleting existing test database ~A to ensure fresh schema...~%" db-path)
      (delete-file db-path))))

(defun %ensure-vec-extension! (conn)
  "Ensure the vec extension is loaded for CONN."
  (handler-case (sqlite:execute-non-query conn "SELECT vec_version()")
    (error (e)
      (declare (ignore e))
      (format t "~&vec extension not found for test DB, attempting to load...~%")
      (let ((vec-path (or (uiop:getenv "SQLITE_VEC_PATH") "vec0")))
        (hactar::load-extension conn vec-path)))))

(defun %clear-test-tables! (conn)
  "Clear tables used by tests."
  (format t "~&Clearing tables in test database...~%")
  (handler-case
      (progn
        (execute-non-query conn "DELETE FROM documents")
        (execute-non-query conn "DELETE FROM vec_documents")
        (execute-non-query conn "DELETE FROM errors")
        (execute-non-query conn "DELETE FROM vec_errors"))
    (error (e)
      (format *error-output* "~&Error clearing tables: ~A~%" e)
      (error "Failed to clear tables. Aborting tests.")))
  (format t "~&Tables cleared.~%"))

(defun %run-suite (suite)
  "Run a FiveAM SUITE, printing a short banner and returning the boolean result."
  (format t "~&Starting ~A test execution...~%" suite)
  (let ((result (explain! (run suite))))
    (format t "~&--- ~A Tests Complete ---~%" suite)
    result))

(defun %colorize-test-results (suites results)
  "Pretty-print and colorize a test summary for SUITES and their boolean RESULTS.

Returns T iff all results are true."
  (let* ((total (length suites))
         (passed (count-if #'identity results))
         (failed (- total passed))
         (all-passed (zerop failed)))
    (format t "~&~%--- Test Summary ---~%")
    (loop for suite in suites
          for ok in results
          do (format t "~A ~A~%"
                     (hactar::colorize (if ok "PASS" "FAIL")
                                      (if ok :green :red))
                     suite))
    (format t "~&--------------------~%")
    (format t "~A~%"
            (hactar::colorize
             (format nil "Passed ~D/~D, Failed ~D" passed total failed)
             (if all-passed :green :red)))
    all-passed))

;;; Function to run all tests
(defun run-tests ()
  "Run all FiveAM test suites for Hactar.
Sets up the test database, runs migrations, clears tables, then runs all suites.
Returns true iff all suites pass."
  (let ((hactar::*db-path* *test-db-path*))
    (format t "~&--- Running Hactar Tests ---~%")
    (format t "~&Using test database: ~A~%" hactar::*db-path*)

    (%ensure-fresh-test-db! hactar::*db-path*)

    (with-open-database (conn hactar::*db-path*)
      (%ensure-vec-extension! conn)

      (unless (run-migrations :test t :connection conn)
        (error "Failed to run migrations on test database. Aborting tests."))

      (let ((hactar::*connection* conn))
        (%clear-test-tables! conn)

        ;; Keep this list in sync with the suites defined across tests/*.test.lisp
        ;; and with hactar-tests.asd components.
        (let* ((suites '(hactar-tests
                         errors-tests
                         org-mode-parser-tests
                         org-mode-tests
			 markdown-tests
                         utils-tests
                         router-tests
                         web-command-tests
                         copilot-tests
                         hactar-core-guide-tests
                         agent-tests
                         ai-comment-tests
                         import-tests
                         npm-tests
                         ctags-tests
                         stripe-mode-tests
                         generator-tests
                         bot-tests
			 feature-tests
                         code-value-tests
			 context-tests
                         preset-tests
                         session-tests
			 processors-tests
			 permissions-tests
                         hyperfractal-tests
                         ruhe-tests
                         tools-tests
                         lisp-rpc-tests
			 acp-tests
			 mcp-tests
                         tui-tests
			 to-org-tests
			 rules-tests
			 skills-tests
                         ;; monolith-tests
                         ;; litmode-tests
			 ))
               (results (mapcar #'%run-suite suites)))
          (%colorize-test-results suites results))))))
