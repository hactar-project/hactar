(in-package :hactar-tests)

;;* helpers
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

(defun run-tests ()
  "Run all FiveAM test suites for Hactar.
Returns true iff all suites pass."
  (format t "~&--- Running Hactar Tests ---~%")
  ;; Keep this list in sync with the suites defined across tests/*.test.lisp
  ;; and with hactar-tests.asd components.
  (let* ((suites '(hactar-tests
                   errors-tests
                   org-mode-parser-tests
                   org-mode-tests
                   markdown-tests
                   utils-tests
                   fzf-tests
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
                   code-value-tests
                   context-tests
                   docs-tests
                   preset-tests
                   session-tests
                   processors-tests
                   permissions-tests
                   hyperfractal-tests
                   ruhe-tests
                   tools-tests
                   lisp-rpc-tests
                   lisp-mode-tests
                   acp-tests
                   mcp-tests
                   tui-tests
                   llm-utils-tests
                   to-org-tests
                   rules-tests
                   skills-tests
                   mold-tests
                   proxy-tests
                   compiler-tests
                   json-target-tests
                   toml-target-tests
                   css-target-tests
                   checker-tests
                   javascript-target-tests
                   typescript-target-tests
                   worker-target-tests
                   redwood-target-tests
                   lsp-tests
                   convert-tests
                   wiki-tests
                   hypertext-tests
                   cli-tests
                   format-tests
                   spec-tests
                   interface-tests
                   hydra-tests
                   prompts-tests
                   persist-tests
		   history-tests
                   mode-tests
                   gen-helpers-tests
                   monolith-tests
                   litmode-tests))
         (results (mapcar #'%run-suite suites)))
    (%colorize-test-results suites results)))
