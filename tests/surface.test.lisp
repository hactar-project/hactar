(in-package :hactar-tests)
(in-suite hactar-tests)

(test validate-surface-basic
  "Test public surface validator with various valid and invalid forms."
  ;; 1. defproject
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:defproject "my-app" :stack (redwood react) :root *repo-root* :modes (dev)))
    (is-true valid-p)
    (is (null errors)))
  
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:defproject 123 :stack (react)))
    (is-false valid-p)
    (is-true (member "defproject name must be a symbol or a string." errors :test #'string=)))
  
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:defproject "my-app" :invalid-key 42))
    (is-false valid-p)
    (is-true (member "Invalid keyword parameter :INVALID-KEY for DEFPROJECT." errors :test #'string=)))

  ;; 2. defaction
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:defaction route.create :doc "Create a route" :prompt "Create it" :apply :create))
    (is-true valid-p)
    (is (null errors)))
  
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:defaction route.create :doc "Create" :unknown-key 42))
    (is-false valid-p)
    (is-true (member "Invalid keyword parameter :UNKNOWN-KEY for DEFACTION." errors :test #'string=)))

  ;; 3. deftool
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:deftool test :doc "Run tests" :run "npm test" :permissions :auto))
    (is-true valid-p)
    (is (null errors)))

  ;; 4. with-context
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:with-context (:files ("src/main.lisp") :project my-app)
                                         (print "hello")))
    (is-true valid-p)
    (is (null errors)))
  
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:with-context (:invalid-key 123) (print "hello")))
    (is-false valid-p)
    (is-true (member "Invalid keyword parameter :INVALID-KEY for WITH-CONTEXT." errors :test #'string=)))

  ;; 5. with-generated & with-diffusion
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:with-generated (:id :routes :owner :diffusion)
                                         (hactar:defroutes)))
    (is-true valid-p)
    (is (null errors)))
  
  (multiple-value-bind (valid-p errors)
      (hactar:validate-public-surface '(hactar:with-diffusion (:id :actions :model "fast")
                                         (print "diff")))
    (is-true valid-p)
    (is (null errors))))

(test result-rendering-test
  "Test result structure and render-result generic functions."
  (let ((res (hactar:make-result :status :ok
                                 :message "Operation completed successfully."
                                 :data '((:success . t) (:id . 123))
                                 :artifacts '("file:///path/to/file.lisp"))))
    ;; Test fields
    (is (eq :ok (hactar:result-status res)))
    (is (string= "Operation completed successfully." (hactar:result-message res)))
    (is (equal '((:success . t) (:id . 123)) (hactar:result-data res)))
    (is (equal '("file:///path/to/file.lisp") (hactar:result-artifacts res)))

    ;; Test Text Renderer
    (let ((text-out (hactar:render-result res :text)))
      (is-true (search "Operation completed successfully." text-out))
      (is-true (search "file:///path/to/file.lisp" text-out)))

    ;; Test JSON Renderer
    (let ((json-out (hactar:render-result res :json)))
      (is-true (search "\"status\"" json-out))
      (is-true (search "\"ok\"" json-out))
      (is-true (search "\"message\"" json-out))
      (is-true (search "Operation completed successfully." json-out))
      (is-true (search "file:///path/to/file.lisp" json-out)))

    ;; Test Org Renderer
    (let ((org-out (hactar:render-result res :org)))
      (is-true (search "* Result: OK" org-out))
      (is-true (search "Operation completed successfully." org-out))
      (is-true (search "- file:///path/to/file.lisp" org-out)))

    ;; Test Markdown Renderer
    (let ((md-out (hactar:render-result res :markdown)))
      (is-true (search "# Result: OK" md-out))
      (is-true (search "Operation completed successfully." md-out))
      (is-true (search "- file:///path/to/file.lisp" md-out)))))
