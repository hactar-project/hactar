(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test rejects-out-of-surface-forms
  (multiple-value-bind (ok errs)
      (hactar-rebuild:validate-forms
       '((defproject :stack ("ts")) (defun sneaky () 42)))
    (is (null ok))
    (is (find-if (lambda (e) (search "surface" e)) errs))))

(test accepts-surface-forms
  (is (hactar-rebuild:validate-forms
       '((defproject :stack ("ts")) (deftool t1 () "doc" 1)))))

(test validates-inside-generated-region
  (multiple-value-bind (ok errs)
      (hactar-rebuild:validate-forms
       '((generated :updated-at "x" (defproject :stack ("ts")) (defun bad () 1))))
    (is (null ok))
    (is (find-if (lambda (e) (search "surface" e)) errs))))

(test deftool-registers-and-runs
  (hactar-rebuild:reset-registries)
  (eval-in-user "(deftool echo (a) \"echo it\" a)")
  (let ((r (hactar-rebuild:run-tool "echo" "hi")))
    (is (eq :ok (hactar-rebuild:result-status r)))
    (is (string= "\"hi\"" (hactar-rebuild:result-message r)))))

(test unknown-tool-errors
  (hactar-rebuild:reset-registries)
  (is (eq :error (hactar-rebuild:result-status
                  (hactar-rebuild:run-tool "nope")))))

(test reload-idempotency
  (hactar-rebuild:reset-registries)
  (dotimes (i 2) (eval-in-user "(deftool onlyone () \"d\" 1)"))
  (is (= 1 (hactar-rebuild:registry-count :tools))))

(test prompt-rendering-expands-vars-and-stack
  (hactar-rebuild:reset-registries)
  (let ((hactar-rebuild:*stack* '("typescript" "react")))
    (is (string= "Route login for typescript react"
                 (hactar-rebuild:render-prompt
                  "Route {{name}} for {{stack}}" '(:name "login"))))))
