(in-package #:hactar-rebuild-tests)

(def-suite rebuild-suite
  :description "Greenfield Hactar rebuild engine tests (network-free).")

(defun run-tests ()
  "Run the rebuild suite and return T on success, NIL on failure."
  (let ((results (run 'rebuild-suite)))
    (explain! results)
    (null (remove-if (lambda (r) (typep r 'fiveam::test-passed)) results))
    (every (lambda (r) (typep r 'fiveam::test-passed)) results)))
