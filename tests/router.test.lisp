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
  (let ((result (hactar::execute-route "other:test")))
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
