(in-package :hactar-tests)
(def-suite starters-tests
  :description "Tests for the starters subsystem.")

(in-suite starters-tests)

(test defstarter-macro-registration
  "Test that defstarter registers variables correctly with metadata."
  (eval '(hactar::defstarter "react-test"
          :source "This is a test react template"
          :stack '("react" "js")
          :language "js"
          :version "1.2.3"
          :uri "starters:react-test"))
  
  (multiple-value-bind (content mime doc) (hactar::resolve-hypertext-uri "starters:react-test")
    (declare (ignore mime))
    (is (string= content "This is a test react template"))
    (is-true doc)
    (let ((meta (getf doc :metadata)))
      (is (equal (cdr (assoc :stack meta)) '("react" "js")))
      (is (string= (cdr (assoc :language meta)) "js"))
      (is (string= (cdr (assoc :version meta)) "1.2.3")))))

(test defstart-alias-test
  "Test that defstart macro is an alias to defstarter."
  (eval '(hactar::defstart "sinatra-test"
          :source "This is a sinatra template"
          :stack '("ruby" "sinatra")
          :language "ruby"
          :version "0.1.0"
          :uri "starters:sinatra-test"))
  (multiple-value-bind (content mime doc) (hactar::resolve-hypertext-uri "starters:sinatra-test")
    (declare (ignore mime))
    (is (string= content "This is a sinatra template"))
    (is-true doc)))

(test create-project-uri-lookup-test
  "Test that create-project resolves URI-based starters correctly."
  (eval '(hactar::defstarter "uri-lookup-test"
          :source "my-special-content"
          :uri "starters:uri-lookup-test"))
  (let ((hactar::*current-model* (hactar::make-model-config :name "mock-model" :provider "mock-provider" :model-name "mock-name" :max-input-tokens 32000 :max-output-tokens 4096 :input-cost-per-token 0.0 :output-cost-per-token 0.0))
        (called-llm nil))
    (with-dynamic-stubs ((hactar::get-llm-response (lambda (prompt &key custom-system-prompt add-to-history)
                                                     (declare (ignore prompt add-to-history))
                                                     (setf called-llm t)
                                                     (is (search "my-special-content" custom-system-prompt))
                                                     "Response")))
      (hactar::create-project "starters:uri-lookup-test" "my prompt")
      (is-true called-llm))))

(test starters-import-test
  "Test starters.import command and starter->lisp."
  (uiop:with-temporary-file (:pathname p :keep t)
    (let* ((temp-dir (uiop:pathname-directory-pathname p))
           (hactar::*starters-import-path* temp-dir))
      ;; Mock fetch-import-content
      (with-dynamic-stubs ((hactar::fetch-import-content (lambda (uri)
                                                           (declare (ignore uri))
                                                           (values "starter content" "imported starter" nil))))
        (hactar::execute-command "/starters.import" '("file:mock-uri" "-stack=react,js" "-language=javascript" "-version=1.0"))
        (let ((out-file (uiop:native-namestring
                         (merge-pathnames "file_mock-uri.lisp" temp-dir))))
          (is-true (probe-file out-file))
          (let ((content (uiop:read-file-string out-file)))
            (is (search "defstarter" content))
            (is (search "starter content" content))
            (is (search ":stack '(\"react\" \"js\")" content))
            (is (search ":language \"javascript\"" content))))))))
