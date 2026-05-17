(in-package :hactar-tests)

(def-suite wiki-tests
  :description "Tests for the wiki knowledge graph features")

(in-suite wiki-tests)

(test parse-wiki-uri-basic-test
  "URIs of the form 'namespace.docs/path.resource' parse correctly."
  (multiple-value-bind (ns path resource)
      (hactar::parse-wiki-uri "redwood.docs/auth/login.types")
    (is (string= ns "redwood.docs"))
    (is (string= path "auth/login"))
    (is (string= resource "types"))))

(test parse-wiki-uri-no-resource-test
  "URIs without a trailing .resource parse with resource = nil."
  (multiple-value-bind (ns path resource)
      (hactar::parse-wiki-uri "redwood.docs/auth")
    (is (string= ns "redwood.docs"))
    (is (string= path "auth"))
    (is (null resource))))

(test parse-wiki-uri-strip-prefix-test
  "Leading 'wiki:' is stripped during parsing."
  (multiple-value-bind (ns path resource)
      (hactar::parse-wiki-uri "wiki:redwood.docs/auth.types")
    (is (string= ns "redwood.docs"))
    (is (string= path "auth"))
    (is (string= resource "types"))))

(test parse-wiki-uri-no-namespace-test
  "URIs without a namespace return nil for namespace."
  (multiple-value-bind (ns path resource)
      (hactar::parse-wiki-uri "auth.types")
    (is (null ns))
    (is (string= path "auth"))
    (is (string= resource "types"))))

(test register-and-find-wiki-node-test
  "Registered nodes can be looked up by namespace+path."
  (clrhash hactar::*wiki-registry*)
  (hactar::register-wiki-node "test.docs" "auth/login"
                              :type :endpoint
                              :properties (list :method "POST"
                                                :types "interface LoginArgs {}"))
  (let ((node (hactar::find-wiki-node "test.docs" "auth/login")))
    (is-true node)
    (is (eq (hactar::wiki-node-type node) :endpoint))
    (is (hactar::has-resource-p node "types"))
    (is (string= (hactar::extract-resource node "types") "interface LoginArgs {}"))))

(test stack-inference-test
  "When namespace is omitted, lookup tries '<stack>.docs' namespaces."
  (clrhash hactar::*wiki-registry*)
  (hactar::register-wiki-node "redwood.docs" "auth/login"
                              :type :endpoint
                              :properties (list :method "POST"))
  (let ((hactar::*stack* (list "redwood")))
    (let ((node (hactar::find-wiki-node nil "auth/login")))
      (is-true node)
      (is (string= (hactar::wiki-node-namespace node) "redwood.docs")))))

(test defendpoint-macro-test
  "defendpoint registers a node under the correct key."
  (clrhash hactar::*wiki-registry*)
  (eval '(hactar::defendpoint redwood.docs/auth/logout
                              :method "POST"
                              :content "Logs out the user."))
  (let ((node (hactar::find-wiki-node "redwood.docs" "auth/logout")))
    (is-true node)
    (is (string= (hactar::extract-resource node "method") "POST"))))

(test dispatch-wiki-query-exact-match-test
  "Exact resource matches are returned directly without LLM synthesis."
  (clrhash hactar::*wiki-registry*)
  (hactar::register-wiki-node "test.docs" "users"
                              :type :endpoint
                              :properties (list :curl "curl https://api.example.com/users"))
  (let ((result (hactar::dispatch-wiki-query "test.docs/users.curl")))
    (is (string= result "curl https://api.example.com/users"))))

(test wiki-route-installed-test
  "The wiki catch-all routes are present in *routes*."
  (clrhash hactar::*routes*)
  (clrhash hactar::*wiki-registry*)
  (hactar::%install-wiki-routes)
  (let ((patterns '()))
    (maphash (lambda (k v) (declare (ignore k))
               (push (hactar::route-pattern v) patterns))
             hactar::*routes*)
    (is (member "^wiki:(.+)$" patterns :test #'string=))
    (is (member "^([a-z0-9_-]+\\.docs)/(.+)$" patterns :test #'string=))))

(test defcomponent-macro-test
  "defcomponent registers a node with type :component."
  (clrhash hactar::*wiki-registry*)
  (eval '(hactar::defcomponent ui.docs/button
                              :props "ButtonProps"
                              :content "A standard button."))
  (let ((node (hactar::find-wiki-node "ui.docs" "button")))
    (is-true node)
    (is (eq (hactar::wiki-node-type node) :component))
    (is (string= (hactar::extract-resource node "props") "ButtonProps"))))

(test defvariable-macro-test
  "defvariable registers a node with type :variable."
  (clrhash hactar::*wiki-registry*)
  (eval '(hactar::defvariable config.docs/api-url
                              :default "http://localhost:3000"))
  (let ((node (hactar::find-wiki-node "config.docs" "api-url")))
    (is-true node)
    (is (eq (hactar::wiki-node-type node) :variable))
    (is (string= (hactar::extract-resource node "default") "http://localhost:3000"))))

(test defdocset-macro-test
  "defdocset registers a docset in *wiki-docsets*."
  (clrhash hactar::*wiki-docsets*)
  (eval '(hactar::defdocset my.docs :version "1.0" :description "My docs"))
  (let ((ds (gethash "my.docs" hactar::*wiki-docsets*)))
    (is-true ds)
    (is (string= (hactar::wiki-docset-version ds) "1.0"))
    (is (string= (hactar::wiki-docset-description ds) "My docs"))))

(test wiki-import-test
  "wiki-import runs the specified importer."
  (clrhash hactar::*wiki-importers*)
  (hactar::defimporter dummy-importer (x)
    (list "dummy.docs" x))
  (let ((res (hactar::wiki-import 'dummy-importer "test-path")))
    (is (equal res '("dummy.docs" "test-path")))))

(test wiki-list-test
  "wiki-list runs without error."
  (clrhash hactar::*wiki-registry*)
  (hactar::register-wiki-node "test.docs" "a" :type :endpoint)
  (hactar::register-wiki-node "test.docs" "b" :type :endpoint)
  (let ((out (with-output-to-string (*standard-output*)
               (hactar::wiki-list "test.docs"))))
    (is-true (search "test.docs/a" out))
    (is-true (search "test.docs/b" out))))

(test wiki-show-test
  "wiki-show runs without error."
  (clrhash hactar::*wiki-registry*)
  (hactar::register-wiki-node "show.docs" "node" :type :endpoint :properties '(:foo "bar"))
  (let ((out (with-output-to-string (*standard-output*)
               (hactar::wiki-show "show.docs/node"))))
    (is-true (search "show.docs/node" out))
    (is-true (search "foo: bar" out))))
