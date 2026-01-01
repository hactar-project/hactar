(in-package :hactar-tests)

(def-suite npm-tests
  :description "Tests for NPM package operations and web commands.")

(in-suite npm-tests)

(test npm-command-registration-test
  "Test that the npm command is registered correctly."
  (let ((web-cmd (gethash "npm" hactar::*web-commands*)))
    (is-true web-cmd)
    (is (string= (hactar::web-command-name web-cmd) "npm"))
    (is (> (length (hactar::web-command-routes web-cmd)) 0))
    (is-true (hactar::web-command-default-route web-cmd))))

(test npm-search-route-test
  "Test npm search route with mocked results."
  (let ((mock-results (vector
                       `((:package . ((:name . "test-package")
                                     (:version . "1.0.0")
                                     (:description . "A test package")
                                     (:links . ((:npm . "https://npmjs.com/test-package"))))))
                       `((:package . ((:name . "another-package")
                                     (:version . "2.0.0")
                                     (:description . "Another test package")
                                     (:links . ((:npm . "https://npmjs.com/another-package")))))))))
    (with-dynamic-stubs ((hactar::search-npm-packages (lambda (query &key limit)
                                                        (declare (ignore query limit))
                                                        mock-results)))
      (let* ((output-stream (make-string-output-stream))
             (*standard-output* output-stream))
        (hactar::execute-web-command "npm" '("search" "test"))
        (let ((output (get-output-stream-string output-stream)))
          (is-true (search "test-package" output))
          (is-true (search "another-package" output))
          (is-true (search "1.0.0" output)))))))

(test npm-docs-route-test
  "Test npm docs route with mocked fetch."
  (with-dynamic-stubs ((hactar::get-npm-docs (lambda (package version)
                                               (declare (ignore version))
                                               (if (string= package "react")
                                                   "# React Documentation\n\nReact is a library..."
                                                   nil))))
    (let* ((output-stream (make-string-output-stream))
           (*standard-output* output-stream))
      (hactar::execute-web-command "npm" '("docs" "react@19.0.0"))
      (let ((output (get-output-stream-string output-stream)))
        (is-true (search "React Documentation" output))))))

(test npm-meta-route-test
  "Test npm meta route with mocked metadata."
  (with-dynamic-stubs ((hactar::get-npm-meta (lambda (package version)
                                              (declare (ignore version))
                                              (if (string= package "lodash")
                                                  "{\"name\":\"lodash\",\"version\":\"4.17.21\"}"
                                                  nil))))
    (let* ((output-stream (make-string-output-stream))
           (*standard-output* output-stream))
      (hactar::execute-web-command "npm" '("meta" "lodash@4.17.21"))
      (let ((output (get-output-stream-string output-stream)))
        (is-true (search "lodash" output))
        (is-true (search "4.17.21" output))))))

(test defdocsource-macro-test
  "Test that defdocsource creates custom documentation routes."
  ;; The defdoc for vue@3.^ is defined in npm.lisp
  ;; Test that it creates a route that matches
  (multiple-value-bind (route params)
      (hactar::match-route "npm:vue@3.2.0")
    (is-true route)
    (is-true params)))

(test format-npm-search-results-test
  "Test formatting of NPM search results."
  (let* ((results (vector
                   `((:package . ((:name . "express")
                                 (:version . "4.18.0")
                                 (:description . "Fast, unopinionated web framework")
                                 (:links . ((:npm . "https://npmjs.com/express"))))))
                   `((:package . ((:name . "koa")
                                 (:version . "2.14.0")
                                 (:description . "Expressive middleware framework")
                                 (:links . ((:npm . "https://npmjs.com/koa"))))))))
         (formatted (hactar::format-npm-search-results results)))
    (is-true (search "express@4.18.0" formatted))
    (is-true (search "koa@2.14.0" formatted))
    (is-true (search "Fast, unopinionated" formatted))
    (is-true (search "Expressive middleware" formatted))))
