(in-package :hactar-tests)

(def-suite router-tests
  :description "Tests for the generic regex-based router.")

(in-suite router-tests)

(test defroute-basic-test
  "Test basic route definition and matching."
  ;; Clear routes before this test
  (clrhash hactar::*routes*)

  (eval '(hactar::defroute "^test:(\\w+)$" (value)
           :params (:value)
           :priority 10
           (format nil "Matched: ~A" value)))

  (let ((result (hactar::execute-route "test:hello")))
    (is (string= result "Matched: hello")))

  (let ((result (hactar::execute-route "test:world")))
    (is (string= result "Matched: world")))

  ;; No match
  (let* ((hactar::*route-fallback-fn* nil)
         (result (hactar::execute-route "other:test")))
    (is (null result))))

(test defroute-npm-pattern-test
  "Test NPM-style package pattern matching."
  ;; Clear routes before this test
  (clrhash hactar::*routes*)

  (eval '(hactar::defroute "npm:([^@]+)@(.+)\\.(.+)$" (package version extension)
           :params (:package :version :extension)
           :priority 10
           (format nil "Package: ~A, Version: ~A, Extension: ~A"
                   package version extension)))

  (let ((result (hactar::execute-route "npm:react@18.2.0.tgz")))
    (is (string= result "Package: react, Version: 18.2.0, Extension: tgz")))

  (let ((result (hactar::execute-route "npm:lodash@4.17.21.tar.gz")))
    (is (string= result "Package: lodash, Version: 4.17.21.tar, Extension: gz"))))

(test defroute-priority-test
  "Test that routes are matched by priority."
  ;; Clear routes before this test
  (clrhash hactar::*routes*)

  (eval '(hactar::defroute "^test:(.+)$" (value)
           :params (:value)
           :priority 5
           (format nil "Low priority: ~A" value)))

  (eval '(hactar::defroute "^test:(\\w+)$" (value)
           :params (:value)
           :priority 15
           (format nil "High priority: ~A" value)))

  ;; Should match high priority route first
  (let ((result (hactar::execute-route "test:hello")))
    (is (string= result "High priority: hello")))

  ;; Should match low priority when high priority doesn't match
  (let ((result (hactar::execute-route "test:hello-world")))
    (is (string= result "Low priority: hello-world"))))

(test defroute-github-url-test
  "Test GitHub URL pattern matching."
  ;; Clear routes before this test
  (clrhash hactar::*routes*)

  (eval '(hactar::defroute "github\\.com/([^/]+)/([^/]+)(?:/(.+))?" (user repo path)
           :params (:user :repo :path)
           :priority 10
           (if path
               (format nil "User: ~A, Repo: ~A, Path: ~A" user repo path)
               (format nil "User: ~A, Repo: ~A" user repo))))

  (let ((result (hactar::execute-route "github.com/user/repo")))
    (is (string= result "User: user, Repo: repo")))

  (let ((result (hactar::execute-route "github.com/user/repo/src/main.lisp")))
    (is (string= result "User: user, Repo: repo, Path: src/main.lisp"))))

(test defflag-value-test
  "DEFFLAG with a value argument stores the captured value."
  (clrhash hactar::*flags*)
  (let ((captured nil))
    (declare (ignore captured))
    (eval `(hactar::defflag :model-test ("--model" "-m") (val)
             :description "Test model flag"
             (setf ,'captured val))))
  (let* ((captured nil)
         (hactar::*flags* hactar::*flags*))
    (hactar::register-flag :ct '("--ct") '("-c") t "test"
                           (lambda (v) (setf captured v)))
    (let ((rest (hactar::parse-cli-input '("--ct" "hello" "arg1"))))
      (is (string= captured "hello"))
      (is (equal rest '("arg1"))))
    (setf captured nil)
    (let ((rest (hactar::parse-cli-input '("-c" "short" "x"))))
      (is (string= captured "short"))
      (is (equal rest '("x"))))
    (setf captured nil)
    (let ((rest (hactar::parse-cli-input '("--ct=eq" "y"))))
      (is (string= captured "eq"))
      (is (equal rest '("y"))))))

(test defflag-boolean-test
  "Boolean DEFFLAG triggers handler without consuming next arg."
  (clrhash hactar::*flags*)
  (let ((called 0))
    (hactar::register-flag :bool '("--bool") nil nil "test bool"
                           (lambda () (incf called)))
    (let ((rest (hactar::parse-cli-input '("--bool" "free" "arg"))))
      (is (= called 1))
      (is (equal rest '("free" "arg"))))))

(test defflag-unknown-passthrough-test
  "Unknown flags are returned as positional args (no error)."
  (clrhash hactar::*flags*)
  (let ((rest (hactar::parse-cli-input '("--unknown" "value"))))
    (is (equal rest '("--unknown" "value")))))

(test execute-route-llm-fallback-test
  "When no route matches, *route-fallback-fn* is invoked."
  (clrhash hactar::*routes*)
  (let* ((called-with nil)
         (hactar::*route-fallback-fn* (lambda (input) (setf called-with input) :fallback-result)))
    (let ((result (hactar::execute-route "unknown/path")))
      (is (eq result :fallback-result))
      (is (string= called-with "unknown/path")))))

(test unregister-route-test
  "Test removing a route."
  (let ((hactar::*route-reinit-hooks* nil))
    (clrhash hactar::*routes*)
    (let ((route-name (eval '(hactar::defroute "^del:(.+)$" (v) :params (:v) (format nil "~A" v)))))
      (is-true (hactar::execute-route "del:x"))
      (hactar::unregister-route route-name)
      (let ((hactar::*route-fallback-fn* nil))
        (is (null (hactar::execute-route "del:x")))))))

(test unregister-flag-test
  "Test removing a flag."
  (clrhash hactar::*flags*)
  (eval '(hactar::defflag :my-flag ("--my-flag") ()))
  (is-true (gethash "--my-flag" hactar::*flags*))
  (hactar::unregister-flag :my-flag)
  (is (null (gethash "--my-flag" hactar::*flags*))))

(test list-registered-flags-test
  "Test listing registered flags without duplicates."
  (clrhash hactar::*flags*)
  (hactar::register-flag :f1 '("--f1") '("-1") nil "test" (lambda ()))
  (let ((flags (hactar::list-registered-flags)))
    (is (= 1 (length flags)))
    (is (eq :f1 (hactar::flag-name (first flags))))))

(test defflag-initializer-test
  "defflag registers an initializer function which is triggered by run-flag-initializers."
  (clrhash hactar::*flags*)
  (let ((flag (hactar::register-flag :init-test '("--init-test") nil nil "test" (lambda ()) nil nil :error
                                     (lambda () (setf (hactar::cli-opt :init-tested) "yay")))))
    (is (not (null flag)))
    (is (not (null (hactar::flag-initializer flag))))
    (hactar::run-flag-initializers)
    (is (string= "yay" (hactar::cli-opt :init-tested)))))

(test subcommand-flag-warning-suppression-test
  "Valid subcommand flags do not trigger unknown flag warnings, but invalid ones do."
  (let ((hactar::*sub-commands* (make-hash-table :test 'equal)))
    ;; Register a mock subcommand with options
    (setf (gethash "mock-sub" hactar::*sub-commands*)
          (list 'mock-fn
                "Mock doc"
                '((:long "path" :short "p" :description "Path")
                  (:long "starter" :short "s" :description "Starter"))
                nil))
    ;; Test valid subcommand flags
    (let ((out (with-output-to-string (*standard-output*)
                 (hactar::parse-cli-input '("mock-sub" "--path" "dir" "-s" "react")))))
      (is (string= out "")))
    ;; Test invalid subcommand flags
    (let ((out (with-output-to-string (*standard-output*)
                 (hactar::parse-cli-input '("mock-sub" "--invalid-flag")))))
      (is (not (null (search "Unknown flag: --invalid-flag" out)))))))
