(in-package :hactar-tests)
(def-suite import-tests
	   :description "Tests for the generic import system.")

(in-suite import-tests)

(test defsource-macro-test
  "Test that defsource correctly creates and registers an import source."
  (eval '(hactar::defsource test-import-source
           :pattern "^test:(.+)$"
           :params (value)
           :priority 10
           (lambda (value)
             (values (format nil "Content: ~A" value)
                     (format nil "Title: ~A" value)))))

  (let ((source (gethash 'test-import-source hactar::*import-sources*)))
    (is-true source)
    (is (eq 'test-import-source (hactar::import-source-name source)))
    (is (equal '(value) (hactar::import-source-param-names source)))
    (is (= 10 (hactar::import-source-priority source)))
    (is-true (hactar::import-source-handler source))))

(test match-import-source-test
  "Test matching URIs against registered import sources."
  (eval '(hactar::defsource match-test-source
           :pattern "^match:([^:]+):(.+)$"
           :params (type value)
           :priority 10
           (lambda (type value)
             (values (format nil "~A:~A" type value) "test"))))

  (multiple-value-bind (source params)
      (hactar::match-import-source "match:foo:bar")
    (is-true source)
    (is (string= (cdr (assoc 'type params)) "foo"))
    (is (string= (cdr (assoc 'value params)) "bar")))

  ;; No match
  (multiple-value-bind (source params)
      (hactar::match-import-source "nomatch:test")
    (is (null source))
    (is (null params))))

(test import-source-priority-test
  "Test that import sources are matched by priority."
  (eval '(hactar::defsource low-priority-source
           :pattern "^prio:(.+)$"
           :params (value)
           :priority 5
           (lambda (value) (values "low" "low"))))

  (eval '(hactar::defsource high-priority-source
           :pattern "^prio:(.+)$"
           :params (value)
           :priority 15
           (lambda (value) (values "high" "high"))))

  (multiple-value-bind (source params)
      (hactar::match-import-source "prio:test")
    (is-true source)
    (is (eq 'high-priority-source (hactar::import-source-name source)))))

(test npm-source-registration-test
  "Test that npm-source is registered correctly."
  (let ((source (gethash 'npm-source hactar::*import-sources*)))
    (is-true source)
    (is (string= (hactar::import-source-pattern source) "^npm:([^@]+)@(.+)$"))
    (is (equal (hactar::import-source-param-names source) '(package-name version)))))

(test file-source-registration-test
  "Test that file-source is registered correctly."
  (let ((source (gethash 'file-source hactar::*import-sources*)))
    (is-true source)
    (is (string= (hactar::import-source-pattern source) "^file:(.+)$"))
    (is (equal (hactar::import-source-param-names source) '(filepath)))))

(test http-source-registration-test
  "Test that http-source is registered correctly."
  (let ((source (gethash 'http-source hactar::*import-sources*)))
    (is-true source)
    (is-true (cl-ppcre:scan (hactar::import-source-pattern source) "http://example.com"))
    (is-true (cl-ppcre:scan (hactar::import-source-pattern source) "https://example.com"))))

(test github-repo-source-registration-test
  "Test that github-repo-source is registered correctly."
  (let ((source (gethash 'github-repo-source hactar::*import-sources*)))
    (is-true source)
    (is-true (cl-ppcre:scan (hactar::import-source-pattern source) "https://github.com/user/repo"))
    (is (equal (hactar::import-source-param-names source) '(user repo)))))

(test execute-import-npm-test
  "Test executing an npm import with mocked get-npm-docs."
  (with-dynamic-stubs ((hactar::get-npm-docs (lambda (package version)
                                               (declare (ignore version))
                                               (if (string= package "react")
                                                   "# React Documentation"
                                                   nil)))
                       (hactar::generate-doc-metadata (lambda (content title uri)
                                                       (declare (ignore content title uri))
                                                       (values '("react" "javascript") "React library")))
                       (hactar::docs-create (lambda (&key source title content tags covers meta)
                                             (declare (ignore source title content covers meta))
                                             (list (gensym "DOC-")))))
    (let* ((output-stream (make-string-output-stream))
           (*standard-output* output-stream))
      (let ((result (hactar::execute-import "npm:react@19.2")))
        (is-true result)
        (let ((output (get-output-stream-string output-stream)))
          (is-true (search "Successfully imported" output))
          (is-true (search "npm:react@19.2" output)))))))

(test execute-import-file-test
  "Test executing a file import."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "Test documentation content"))

    (with-dynamic-stubs ((hactar::generate-doc-metadata (lambda (content title uri)
                                                          (declare (ignore content title uri))
                                                          (values '("test" "doc") "Test doc")))
                         (hactar::docs-create (lambda (&key source title content tags covers meta)
                                               (declare (ignore source title content covers meta))
                                               (list (gensym "DOC-")))))
      (let* ((uri (format nil "file:~A" (uiop:native-namestring p)))
             (output-stream (make-string-output-stream))
             (*standard-output* output-stream)
             (hactar::*repo-root* (uiop:pathname-directory-pathname p)))
        (let ((result (hactar::execute-import uri)))
          (is-true result))))))

(test execute-import-no-match-test
  "Test that execute-import handles unknown URI patterns gracefully."
  (let* ((output-stream (make-string-output-stream))
         (*standard-output* output-stream))
    (let ((result (hactar::execute-import "unknown:scheme:value")))
      (is (null result))
      (let ((output (get-output-stream-string output-stream)))
        (is-true (search "No import source matches" output))))))
