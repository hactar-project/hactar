;; mold tests
(in-package :hactar-tests)

(def-suite mold-tests
  :description "Tests for the Mold system")

(in-suite mold-tests)

;;* Helper macro to isolate mold state

(defmacro with-clean-mold-env (&body body)
  "Run BODY with fresh mold registries and related state."
  `(let ((hactar::*molds* (make-hash-table :test 'equal))
         (hactar::*active-mold* nil)
         (hactar::*active-rules* (make-hash-table :test 'equal))
         (hactar::*entity-registry* (make-hash-table :test 'equal))
         (hactar::*entity-instances* (make-hash-table :test 'equal))
         (hactar::*entity-implementations* (make-hash-table :test 'equal))
         (hactar::*behavior-registry* (make-hash-table :test 'equal))
         (hactar::*repo-root* (uiop:temporary-directory))
         (hactar::*hactar-data-path* (uiop:temporary-directory))
         (hactar::*molds-path* (merge-pathnames "molds/" (uiop:temporary-directory)))
         (hactar::*name* "test-project")
         (hactar::*silent* t)
         (hactar::*commands* (make-hash-table :test 'equal))
         (hactar::*acp-commands* (make-hash-table :test 'equal)))
     ,@body))

;;* Mold Struct Tests

(test mold-definition-struct-creation
  "Test that mold-definition structs are created correctly."
  (let ((mold (hactar::make-mold-definition
               :name "test-mold"
               :description "A test mold"
               :llms '(:all)
               :entities '()
               :interfaces '()
               :rules '()
               :source "lisp"
               :version "1.0")))
    (is (string= "test-mold" (hactar::mold-definition-name mold)))
    (is (string= "A test mold" (hactar::mold-definition-description mold)))
    (is (equal '(:all) (hactar::mold-definition-llms mold)))
    (is (null (hactar::mold-definition-entities mold)))
    (is (null (hactar::mold-definition-interfaces mold)))
    (is (null (hactar::mold-definition-rules mold)))
    (is (string= "lisp" (hactar::mold-definition-source mold)))
    (is (string= "1.0" (hactar::mold-definition-version mold)))))

(test mold-entity-struct-creation
  "Test that mold-entity structs are created correctly."
  (let ((entity (hactar::make-mold-entity
                 :name 'route
                 :description "A route entity"
                 :default-tags '(:api :router)
                 :required-behaviors '(:error-handling)
                 :example "src/worker.tsx"
                 :rules '("Use defineApp" "Return Response objects")
                 :schema '((:path :type string)))))
    (is (eq 'route (hactar::mold-entity-name entity)))
    (is (string= "A route entity" (hactar::mold-entity-description entity)))
    (is (equal '(:api :router) (hactar::mold-entity-default-tags entity)))
    (is (equal '(:error-handling) (hactar::mold-entity-required-behaviors entity)))
    (is (string= "src/worker.tsx" (hactar::mold-entity-example entity)))
    (is (= 2 (length (hactar::mold-entity-rules entity))))
    (is (= 1 (length (hactar::mold-entity-schema entity))))))

(test mold-interface-struct-creation
  "Test that mold-interface structs are created correctly."
  (let ((iface (hactar::make-mold-interface
                :name :landing-page
                :entity 'route
                :description "A landing page"
                :rules '("Use tailwind" "Add hero section"))))
    (is (eq :landing-page (hactar::mold-interface-name iface)))
    (is (eq 'route (hactar::mold-interface-entity iface)))
    (is (string= "A landing page" (hactar::mold-interface-description iface)))
    (is (= 2 (length (hactar::mold-interface-rules iface))))))

(test mold-rule-struct-creation
  "Test that mold-rule structs are created correctly."
  (let ((rule (hactar::make-mold-rule
               :text "Use modern standards"
               :scope :generic
               :target nil)))
    (is (string= "Use modern standards" (hactar::mold-rule-text rule)))
    (is (eq :generic (hactar::mold-rule-scope rule)))
    (is (null (hactar::mold-rule-target rule)))))

;;* Rule Parsing Tests

(test parse-mold-rule-string
  "Test parsing a string rule spec into a generic mold-rule."
  (let ((rule (hactar::%parse-mold-rule "Use modern coding standards")))
    (is-true rule)
    (is (string= "Use modern coding standards" (hactar::mold-rule-text rule)))
    (is (eq :generic (hactar::mold-rule-scope rule)))
    (is (null (hactar::mold-rule-target rule)))))

(test parse-mold-rule-entity-scoped
  "Test parsing an entity-scoped rule spec."
  (let ((rule (hactar::%parse-mold-rule '(:entity route "Always validate input"))))
    (is-true rule)
    (is (string= "Always validate input" (hactar::mold-rule-text rule)))
    (is (eq :entity (hactar::mold-rule-scope rule)))
    (is (eq 'route (hactar::mold-rule-target rule)))))

(test parse-mold-rule-interface-scoped
  "Test parsing an interface-scoped rule spec."
  (let ((rule (hactar::%parse-mold-rule '(:interface :landing-page "Include a CTA"))))
    (is-true rule)
    (is (string= "Include a CTA" (hactar::mold-rule-text rule)))
    (is (eq :interface (hactar::mold-rule-scope rule)))
    (is (eq :landing-page (hactar::mold-rule-target rule)))))

(test parse-mold-rule-invalid
  "Test parsing an invalid rule spec returns nil."
  (let ((rule (hactar::%parse-mold-rule 42)))
    (is (null rule))))

(test parse-mold-rule-short-list
  "Test parsing a list with fewer than 3 elements returns nil."
  (let ((rule (hactar::%parse-mold-rule '(:entity))))
    (is (null rule))))

;;* Entity Parsing Tests

(test parse-mold-entity-full
  "Test parsing a full entity specification."
  (let ((entity (hactar::%parse-mold-entity
                 '(route
                   :default-tags (:api :router)
                   :required-behaviors (:error-handling :middleware)
                   :desc "A route"
                   :example "src/worker.tsx"
                   :rules ("Use defineApp" "Return Response objects")
                   :schema ((:path :type string))))))
    (is (eq 'route (hactar::mold-entity-name entity)))
    (is (string= "A route" (hactar::mold-entity-description entity)))
    (is (equal '(:api :router) (hactar::mold-entity-default-tags entity)))
    (is (equal '(:error-handling :middleware) (hactar::mold-entity-required-behaviors entity)))
    (is (string= "src/worker.tsx" (hactar::mold-entity-example entity)))
    (is (= 2 (length (hactar::mold-entity-rules entity))))
    (is (= 1 (length (hactar::mold-entity-schema entity))))))

(test parse-mold-entity-minimal
  "Test parsing an entity spec with only a name."
  (let ((entity (hactar::%parse-mold-entity '(migration))))
    (is (eq 'migration (hactar::mold-entity-name entity)))
    (is (null (hactar::mold-entity-description entity)))
    (is (null (hactar::mold-entity-default-tags entity)))
    (is (null (hactar::mold-entity-required-behaviors entity)))
    (is (null (hactar::mold-entity-example entity)))
    (is (null (hactar::mold-entity-rules entity)))))

;;* Interface Parsing Tests

(test parse-mold-interface-plist-form
  "Test parsing an interface in plist form with keyword name."
  (let ((iface (hactar::%parse-mold-interface
                '(route :landing-page :desc "A landing page" :rules ("Use tailwind")))))
    (is-true iface)
    (is (eq :landing-page (hactar::mold-interface-name iface)))
    (is (eq 'route (hactar::mold-interface-entity iface)))
    (is (string= "A landing page" (hactar::mold-interface-description iface)))
    (is (= 1 (length (hactar::mold-interface-rules iface))))))

(test parse-mold-interface-simple-list-form
  "Test parsing an interface in simple list form."
  (let ((iface (hactar::%parse-mold-interface
                '(routes ("Login route" "Logout route")))))
    (is-true iface)
    (is (eq 'routes (hactar::mold-interface-name iface)))
    (is (eq 'routes (hactar::mold-interface-entity iface)))
    (is (null (hactar::mold-interface-description iface)))
    (is (= 2 (length (hactar::mold-interface-rules iface))))))

(test parse-mold-interface-invalid
  "Test parsing an invalid interface spec returns nil."
  (let ((iface (hactar::%parse-mold-interface '(42))))
    (is (null iface))))

;;* defmold Macro Tests

(test defmold-basic
  "Test that defmold registers a mold in the registry."
  (with-clean-mold-env
    (eval '(hactar::defmold test-basic
             "A basic test mold"
             :entities
             ((route
               :desc "A route"
               :rules ("Rule 1")))
             :interfaces
             ((route :landing-page
               :desc "Landing"
               :rules ("Use tailwind")))
             :rules
             ("Generic rule")))
    (is (= 1 (hash-table-count hactar::*molds*)))
    (let ((mold (gethash "test-basic" hactar::*molds*)))
      (is-true mold)
      (is (string= "test-basic" (hactar::mold-definition-name mold)))
      (is (string= "A basic test mold" (hactar::mold-definition-description mold)))
      (is (= 1 (length (hactar::mold-definition-entities mold))))
      (is (= 1 (length (hactar::mold-definition-interfaces mold))))
      (is (= 1 (length (hactar::mold-definition-rules mold))))
      (is (string= "lisp" (hactar::mold-definition-source mold))))))

(test defmold-with-llms
  "Test that defmold correctly stores LLM specifications."
  (with-clean-mold-env
    (eval '(hactar::defmold test-llms
             "Mold with specific LLMs"
             :llms (:claude :gpt)
             :entities ()
             :interfaces ()
             :rules ()))
    (let ((mold (gethash "test-llms" hactar::*molds*)))
      (is-true mold)
      (is (equal '(:claude :gpt) (hactar::mold-definition-llms mold))))))

(test defmold-multiple-entities
  "Test defmold with multiple entities."
  (with-clean-mold-env
    (eval '(hactar::defmold test-multi
             "Multi-entity mold"
             :entities
             ((route
               :default-tags (:api)
               :desc "A route"
               :rules ("Return JSON"))
              (migration
               :default-tags (:sqlite)
               :desc "A migration"
               :rules ("Implement up and down")))
             :interfaces ()
             :rules ()))
    (let ((mold (gethash "test-multi" hactar::*molds*)))
      (is (= 2 (length (hactar::mold-definition-entities mold))))
      (let ((route-entity (first (hactar::mold-definition-entities mold)))
            (migration-entity (second (hactar::mold-definition-entities mold))))
        (is (eq 'route (hactar::mold-entity-name route-entity)))
        (is (eq 'migration (hactar::mold-entity-name migration-entity)))))))

(test defmold-scoped-rules
  "Test defmold with scoped rules."
  (with-clean-mold-env
    (eval '(hactar::defmold test-scoped
             "Scoped rules mold"
             :entities
             ((route :desc "A route"))
             :interfaces ()
             :rules
             ("Generic rule"
              (:entity route "Entity-scoped rule")
              (:interface :landing-page "Interface-scoped rule"))))
    (let* ((mold (gethash "test-scoped" hactar::*molds*))
           (rules (hactar::mold-definition-rules mold)))
      (is (= 3 (length rules)))
      (is (eq :generic (hactar::mold-rule-scope (first rules))))
      (is (eq :entity (hactar::mold-rule-scope (second rules))))
      (is (eq :interface (hactar::mold-rule-scope (third rules)))))))

(test defmold-returns-name-symbol
  "Test that defmold returns the name symbol."
  (with-clean-mold-env
    (let ((result (eval '(hactar::defmold test-return
                           "Return test"
                           :entities ()
                           :rules ()))))
      (is (eq 'test-return result)))))

;;* Mold Activation Tests

(test mold-use-activates-mold
  "Test that mold-use sets *active-mold*."
  (with-clean-mold-env
    (eval '(hactar::defmold activation-test
             "Activation test"
             :entities ((route :desc "A route" :rules ("Rule 1")))
             :rules ("Generic rule")))
    (hactar::mold-use "activation-test")
    (is-true hactar::*active-mold*)
    (is (string= "activation-test" (hactar::mold-definition-name hactar::*active-mold*)))))

(test mold-use-not-found
  "Test that mold-use returns nil for unknown mold."
  (with-clean-mold-env
    (is (null (hactar::mold-use "nonexistent")))))

(test mold-activate-rules-generic
  "Test that generic rules are activated into *active-rules*."
  (with-clean-mold-env
    (eval '(hactar::defmold rules-test
             "Rules test"
             :entities ()
             :rules ("Use modern standards")))
    (hactar::mold-use "rules-test")
    (let ((found nil))
      (maphash (lambda (k v)
                 (when (and (search "mold/rules-test/rule/" k)
                            (string= v "Use modern standards"))
                   (setf found t)))
               hactar::*active-rules*)
      (is-true found))))

(test mold-activate-rules-entity
  "Test that entity-level rules are activated."
  (with-clean-mold-env
    (eval '(hactar::defmold entity-rules-test
             "Entity rules test"
             :entities ((route :desc "A route" :rules ("Validate input")))
             :rules ()))
    (hactar::mold-use "entity-rules-test")
    (let ((found nil))
      (maphash (lambda (k v)
                 (when (and (search "mold/entity-rules-test/entity/" k)
                            (search "ROUTE" (string-upcase k))
                            (string= v "Validate input"))
                   (setf found t)))
               hactar::*active-rules*)
      (is-true found))))

(test mold-activate-rules-interface
  "Test that interface-level rules are activated."
  (with-clean-mold-env
    (eval '(hactar::defmold iface-rules-test
             "Interface rules test"
             :entities ()
             :interfaces ((route :landing-page :desc "Landing" :rules ("Use tailwind")))
             :rules ()))
    (hactar::mold-use "iface-rules-test")
    (let ((found nil))
      (maphash (lambda (k v)
                 (when (and (search "mold/iface-rules-test/interface/" k)
                            (string= v "Use tailwind"))
                   (setf found t)))
               hactar::*active-rules*)
      (is-true found))))

(test mold-deactivate-rules
  "Test that deactivating a mold removes its rules from *active-rules*."
  (with-clean-mold-env
    (eval '(hactar::defmold deactivation-test
             "Deactivation test"
             :entities ((route :desc "A route" :rules ("Rule A")))
             :rules ("Generic rule B")))
    (let ((mold (gethash "deactivation-test" hactar::*molds*)))
      (hactar::mold-activate-rules mold)
      ;; Verify rules exist
      (let ((count 0))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (when (search "mold/deactivation-test/" k)
                     (incf count)))
                 hactar::*active-rules*)
        (is (> count 0)))
      ;; Now deactivate
      (hactar::mold-deactivate-rules mold)
      (let ((count 0))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (when (search "mold/deactivation-test/" k)
                     (incf count)))
                 hactar::*active-rules*)
        (is (= 0 count))))))

(test mold-use-deactivates-previous
  "Test that activating a new mold deactivates the previous one."
  (with-clean-mold-env
    (eval '(hactar::defmold mold-a
             "Mold A"
             :entities ()
             :rules ("Rule from A")))
    (eval '(hactar::defmold mold-b
             "Mold B"
             :entities ()
             :rules ("Rule from B")))
    (hactar::mold-use "mold-a")
    (is (string= "mold-a" (hactar::mold-definition-name hactar::*active-mold*)))
    ;; Verify mold-a rules are active
    (let ((found-a nil))
      (maphash (lambda (k v)
                 (when (and (search "mold/mold-a/" k)
                            (string= v "Rule from A"))
                   (setf found-a t)))
               hactar::*active-rules*)
      (is-true found-a))
    ;; Activate mold-b
    (hactar::mold-use "mold-b")
    (is (string= "mold-b" (hactar::mold-definition-name hactar::*active-mold*)))
    ;; mold-a rules should be gone
    (let ((found-a nil))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (when (search "mold/mold-a/" k)
                   (setf found-a t)))
               hactar::*active-rules*)
      (is (null found-a)))
    ;; mold-b rules should be present
    (let ((found-b nil))
      (maphash (lambda (k v)
                 (when (and (search "mold/mold-b/" k)
                            (string= v "Rule from B"))
                   (setf found-b t)))
               hactar::*active-rules*)
      (is-true found-b))))

;;* Mold Activate Entities Tests

(test mold-activate-entities-registers
  "Test that mold activation registers entity types in *entity-registry*."
  (with-clean-mold-env
    (eval '(hactar::defmold entity-reg-test
             "Entity registration test"
             :entities
             ((widget :desc "A widget" :default-tags (:ui))
              (gadget :desc "A gadget"))
             :rules ()))
    (hactar::mold-use "entity-reg-test")
    (is-true (gethash "widget" hactar::*entity-registry*))
    (is-true (gethash "gadget" hactar::*entity-registry*))
    (let ((widget-def (gethash "widget" hactar::*entity-registry*)))
      (is (string= "A widget" (hactar::entity-definition-description widget-def))))))

(test mold-activate-entities-no-overwrite
  "Test that mold activation doesn't overwrite existing entity type definitions."
  (with-clean-mold-env
    ;; Pre-register an entity
    (setf (gethash "widget" hactar::*entity-registry*)
          (hactar::make-entity-definition :name 'widget :description "Pre-existing widget"))
    (eval '(hactar::defmold no-overwrite-test
             "No overwrite test"
             :entities ((widget :desc "Mold widget"))
             :rules ()))
    (hactar::mold-use "no-overwrite-test")
    ;; Should keep pre-existing definition
    (let ((widget-def (gethash "widget" hactar::*entity-registry*)))
      (is (string= "Pre-existing widget" (hactar::entity-definition-description widget-def))))))

;;* Mold Display Tests

(test mold-show-no-active-mold
  "Test mold-show when no mold is active."
  (with-clean-mold-env
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::mold-show))))
      (is (search "No active mold" output)))))

(test mold-show-with-active-mold
  "Test mold-show displays mold details."
  (with-clean-mold-env
    (eval '(hactar::defmold show-test
             "Show test mold"
             :entities ((route :desc "A route" :rules ("Rule 1")))
             :interfaces ((route :landing :desc "Landing" :rules ("Tailwind")))
             :rules ("Generic rule")))
    (hactar::mold-use "show-test")
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::mold-show))))
      (is (search "show-test" output))
      (is (search "Show test mold" output))
      (is (search "ROUTE" output))
      (is (search "Rule 1" output))
      (is (search "Generic rule" output)))))

(test mold-show-specific-mold
  "Test mold-show with a specific mold argument."
  (with-clean-mold-env
    (eval '(hactar::defmold specific-show
             "Specific show test"
             :entities ()
             :rules ()))
    (let ((mold (gethash "specific-show" hactar::*molds*)))
      (let ((output (with-output-to-string (*standard-output*)
                      (hactar::mold-show mold))))
        (is (search "specific-show" output))
        (is (search "Specific show test" output))))))

;;* Mold List Tests

(test mold-list-empty
  "Test mold-list with no molds loaded."
  (with-clean-mold-env
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::mold-list))))
      (is (search "No molds loaded" output)))))

(test mold-list-with-molds
  "Test mold-list with loaded molds."
  (with-clean-mold-env
    (eval '(hactar::defmold list-test-a "Mold A" :entities () :rules ()))
    (eval '(hactar::defmold list-test-b "Mold B" :entities () :rules ()))
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::mold-list))))
      (is (search "list-test-a" output))
      (is (search "list-test-b" output)))))

(test mold-list-shows-active
  "Test mold-list shows [ACTIVE] for the active mold."
  (with-clean-mold-env
    (eval '(hactar::defmold active-list-test "Active mold" :entities () :rules ()))
    (hactar::mold-use "active-list-test")
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::mold-list))))
      (is (search "ACTIVE" output)))))

;;* Mold Validation Tests

(test mold-validate-no-active-mold
  "Test validation with no active mold."
  (with-clean-mold-env
    (multiple-value-bind (ok issues)
        (hactar::mold-validate)
      (is (null ok))
      (is (= 1 (length issues))))))

(test mold-validate-missing-behaviors
  "Test validation detects missing required behaviors."
  (with-clean-mold-env
    (eval '(hactar::defmold validate-test
             "Validation test"
             :entities
             ((route
               :required-behaviors (:nonexistent-behavior)
               :desc "A route"))
             :rules ()))
    (hactar::mold-use "validate-test")
    (multiple-value-bind (ok issues)
        (hactar::mold-validate)
      (is (null ok))
      (is (> (length issues) 0))
      (is (some (lambda (issue) (search "nonexistent-behavior" issue)) issues)))))

(test mold-validate-passes-clean
  "Test validation passes when all requirements are met."
  (with-clean-mold-env
    (eval '(hactar::defmold clean-validate
             "Clean validation"
             :entities ()
             :rules ()))
    (hactar::mold-use "clean-validate")
    (multiple-value-bind (ok issues)
        (hactar::mold-validate)
      (is-true ok)
      (is (null issues)))))

;;* Mold Export Tests

(test mold-export-json
  "Test exporting a mold as JSON."
  (with-clean-mold-env
    (eval '(hactar::defmold export-json-test
             "Export JSON test"
             :entities ((route :desc "A route" :rules ("Rule 1")))
             :interfaces ((route :landing :desc "Landing" :rules ("Tailwind")))
             :rules ("Generic rule")))
    (hactar::mold-use "export-json-test")
    (let ((json (hactar::mold-export "json")))
      (is (stringp json))
      (is (search "export-json-test" json))
      (is (search "Export JSON test" json))
      (is (search "ROUTE" json)))))

(test mold-export-org
  "Test exporting a mold as org-mode."
  (with-clean-mold-env
    (eval '(hactar::defmold export-org-test
             "Export org test"
             :entities ((route :desc "A route"))
             :rules ("Generic rule")))
    (hactar::mold-use "export-org-test")
    (let ((org (hactar::mold-export "org")))
      (is (stringp org))
      (is (search "export-org-test" org))
      (is (search "Export org test" org))
      (is (search "* Entities" org))
      (is (search "* Rules" org)))))

(test mold-export-xml
  "Test exporting a mold as XML."
  (with-clean-mold-env
    (eval '(hactar::defmold export-xml-test
             "Export XML test"
             :entities ((route :desc "A route"))
             :rules ("Generic rule")))
    (hactar::mold-use "export-xml-test")
    (let ((xml (hactar::mold-export "xml")))
      (is (stringp xml))
      (is (search "<mold" xml))
      (is (search "export-xml-test" xml))
      (is (search "<entity" xml))
      (is (search "<rule" xml)))))

(test mold-export-no-active-mold
  "Test exporting when no mold is active."
  (with-clean-mold-env
    (is (null (hactar::mold-export "json")))))

(test mold-export-unsupported-format
  "Test exporting with unsupported format."
  (with-clean-mold-env
    (eval '(hactar::defmold unsupported-fmt
             "Format test"
             :entities ()
             :rules ()))
    (hactar::mold-use "unsupported-fmt")
    (is (null (hactar::mold-export "pdf")))))

;;* Mold Export JSON Detail Tests

(test mold-export-json-structure
  "Test that JSON export has the correct structure."
  (with-clean-mold-env
    (eval '(hactar::defmold json-struct-test
             "JSON structure test"
             :llms (:claude :gpt)
             :entities
             ((route :desc "A route" :default-tags (:api) :rules ("R1")))
             :interfaces
             ((route :dash :desc "Dashboard" :rules ("Use charts")))
             :rules
             ("Generic rule"
              (:entity route "Entity rule"))))
    (let* ((mold (gethash "json-struct-test" hactar::*molds*))
           (json-str (hactar::mold-export-as-json mold))
           (parsed (cl-json:decode-json-from-string json-str)))
      (is (string= "json-struct-test" (cdr (assoc :name parsed))))
      (is (string= "JSON structure test" (cdr (assoc :description parsed))))
      ;; Check entities array
      (let ((entities (cdr (assoc :entities parsed))))
        (is (= 1 (length entities)))
        (let ((entity (first entities)))
          (is (search "ROUTE" (cdr (assoc :name entity))))))
      ;; Check interfaces array
      (let ((interfaces (cdr (assoc :interfaces parsed))))
        (is (= 1 (length interfaces))))
      ;; Check rules array
      (let ((rules (cdr (assoc :rules parsed))))
        (is (= 2 (length rules)))))))

;;* Mold Export Org Detail Tests

(test mold-export-org-sections
  "Test that org export contains all expected sections."
  (with-clean-mold-env
    (eval '(hactar::defmold org-sections-test
             "Org sections test"
             :entities
             ((route :desc "A route" :default-tags (:api) :example "src/app.ts"
               :required-behaviors (:auth) :rules ("R1")))
             :interfaces
             ((route :landing :desc "Landing" :rules ("Tailwind")))
             :rules
             ("Generic"
              (:entity route "Entity rule")
              (:interface :landing "Interface rule"))))
    (let* ((mold (gethash "org-sections-test" hactar::*molds*))
           (org (hactar::mold-export-as-org mold)))
      (is (search "#+TITLE:" org))
      (is (search "* Metadata" org))
      (is (search "* Entities" org))
      (is (search "* Interfaces" org))
      (is (search "* Rules" org))
      (is (search ":DEFAULT_TAGS:" org))
      (is (search ":REQUIRED_BEHAVIORS:" org))
      (is (search ":EXAMPLE:" org)))))

;;* Mold Context String Tests

(test mold-context-string-no-mold
  "Test context string when no mold is active."
  (with-clean-mold-env
    (is (string= "" (hactar::mold-context-string)))))

(test mold-context-string-with-mold
  "Test context string with an active mold."
  (with-clean-mold-env
    (eval '(hactar::defmold context-test
             "Context test mold"
             :entities
             ((route :desc "A route" :default-tags (:api) :example "src/app.ts"
               :rules ("Return JSON")))
             :interfaces
             ((route :landing :desc "Landing" :rules ("Use tailwind")))
             :rules
             ("Use modern standards"
              (:entity route "Always validate"))))
    (hactar::mold-use "context-test")
    (let ((ctx (hactar::mold-context-string)))
      (is (search "Active Mold: context-test" ctx))
      (is (search "Context test mold" ctx))
      (is (search "### Entities" ctx))
      (is (search "ROUTE" ctx))
      (is (search "Return JSON" ctx))
      (is (search "### Interfaces" ctx))
      (is (search "### Mold Rules" ctx))
      (is (search "Use modern standards" ctx)))))

(test mold-context-string-entity-details
  "Test context string includes entity details."
  (with-clean-mold-env
    (eval '(hactar::defmold ctx-details-test
             "Details test"
             :entities
             ((route :desc "A route" :default-tags (:api :crud)
               :example "src/worker.tsx"))
             :rules ()))
    (hactar::mold-use "ctx-details-test")
    (let ((ctx (hactar::mold-context-string)))
      (is (search "Tags:" ctx))
      (is (search "Example:" ctx))
      (is (search "src/worker.tsx" ctx)))))

;;* Mold Init Tests

(test mold-init-creates-file
  "Test that mold-init creates a scaffold file."
  (with-clean-mold-env
    (let* ((mold-file (merge-pathnames "test-init.mold.lisp" hactar::*repo-root*)))
      (unwind-protect
           (progn
             (hactar::mold-init "test-init")
             (is (probe-file mold-file))
             (let ((content (uiop:read-file-string mold-file)))
               (is (search "defmold" content))
               (is (search "test-init" content))))
        (ignore-errors (delete-file mold-file))))))

(test mold-init-does-not-overwrite
  "Test that mold-init does not overwrite an existing file."
  (with-clean-mold-env
    (let* ((mold-file (merge-pathnames "test-no-overwrite.mold.lisp" hactar::*repo-root*)))
      (unwind-protect
           (progn
             (with-open-file (s mold-file :direction :output :if-exists :supersede
                                          :if-does-not-exist :create)
               (write-string "existing content" s))
             (hactar::mold-init "test-no-overwrite")
             ;; File should still have original content
             (is (string= "existing content" (uiop:read-file-string mold-file))))
        (ignore-errors (delete-file mold-file))))))

;;* Mold Install Tests

(test mold-install-from-lisp-file
  "Test installing a mold from a .lisp file."
  (with-clean-mold-env
    (let* ((mold-file (merge-pathnames "install-test.lisp" hactar::*repo-root*)))
      (unwind-protect
           (progn
             (with-open-file (s mold-file :direction :output :if-exists :supersede
                                          :if-does-not-exist :create)
               (format s "(in-package :hactar)~%(defmold install-from-file \"Installed mold\" :entities () :rules ())"))
             (is-true (hactar::mold-install (namestring mold-file)))
             (is-true (gethash "install-from-file" hactar::*molds*)))
        (ignore-errors (delete-file mold-file))))))

(test mold-install-file-not-found
  "Test installing a mold from a nonexistent file."
  (with-clean-mold-env
    (is (null (hactar::mold-install "/tmp/nonexistent-mold-12345.lisp")))))

;;* Mold Pour Prompt Tests

(test mold-pour-entity-prompt-generation
  "Test that pour prompt includes entity rules and context."
  (with-clean-mold-env
    (eval '(hactar::defmold pour-test
             "Pour test"
             :entities
             ((route :desc "An API route"
               :default-tags (:api)
               :rules ("Use defineApp" "Return Response")))
             :interfaces
             ((route :dashboard :desc "Dashboard page"
               :rules ("Use charts")))
             :rules
             ("Use modern standards"
              (:entity route "Always validate input"))))
    (let* ((mold (gethash "pour-test" hactar::*molds*))
           (entity (first (hactar::mold-definition-entities mold)))
           (prompt (hactar::%mold-pour-entity-prompt mold entity)))
      (is (search "route" (string-downcase prompt)))
      (is (search "An API route" prompt))
      (is (search "Use defineApp" prompt))
      (is (search "Return Response" prompt))
      (is (search "Always validate input" prompt))
      (is (search "Use modern standards" prompt))
      (is (search "Dashboard page" prompt)))))

(test mold-pour-no-active-mold
  "Test that mold-pour returns nil when no mold is active."
  (with-clean-mold-env
    (is (null (hactar::mold-pour "route")))))

(test mold-pour-entity-not-found
  "Test that mold-pour returns nil for unknown entity."
  (with-clean-mold-env
    (eval '(hactar::defmold pour-notfound
             "Pour not found"
             :entities ((route :desc "A route"))
             :rules ()))
    (hactar::mold-use "pour-notfound")
    (is (null (hactar::mold-pour "nonexistent")))))

;;* Org-Mode Mold Parsing Tests

(test org-extract-list-items
  "Test extracting list items from org content."
  (let ((items (hactar::%org-extract-list-items
                (format nil "Some text~%- Item 1~%- Item 2~%More text~%- Item 3~%"))))
    (is (= 3 (length items)))
    (is (string= "Item 1" (first items)))
    (is (string= "Item 2" (second items)))
    (is (string= "Item 3" (third items)))))

(test org-extract-list-items-empty
  "Test extracting list items from content with none."
  (let ((items (hactar::%org-extract-list-items "No list items here.")))
    (is (null items))))

(test org-parse-scoped-rule-generic
  "Test parsing a generic rule (no scope prefix)."
  (let ((rule (hactar::%org-parse-scoped-rule "Use modern standards")))
    (is (string= "Use modern standards" (hactar::mold-rule-text rule)))
    (is (eq :generic (hactar::mold-rule-scope rule)))))

(test org-parse-scoped-rule-entity
  "Test parsing an entity-scoped rule."
  (let ((rule (hactar::%org-parse-scoped-rule "[entity:route] Always validate input")))
    (is (string= "Always validate input" (hactar::mold-rule-text rule)))
    (is (eq :entity (hactar::mold-rule-scope rule)))
    (is (string= "route" (hactar::mold-rule-target rule)))))

(test org-parse-scoped-rule-interface
  "Test parsing an interface-scoped rule."
  (let ((rule (hactar::%org-parse-scoped-rule "[interface:landing-page] Include CTA")))
    (is (string= "Include CTA" (hactar::mold-rule-text rule)))
    (is (eq :interface (hactar::mold-rule-scope rule)))
    (is (string= "landing-page" (hactar::mold-rule-target rule)))))

(test org-parse-entity-subsections
  "Test parsing entity subsections from org content."
  (let* ((content (format nil "** Route~%:PROPERTIES:~%:DEFAULT_TAGS: API, ROUTER~%:DESCRIPTION: An API route~%:EXAMPLE: src/app.ts~%:END:~%*** Rules~%- Use defineApp~%- Return Response~%"))
         (entities (hactar::%org-parse-entity-subsections content)))
    (is (= 1 (length entities)))
    (let ((entity (first entities)))
      (is-true (hactar::mold-entity-name entity))
      (is (= 2 (length (hactar::mold-entity-rules entity))))
      (is (string= "Use defineApp" (first (hactar::mold-entity-rules entity))))
      (is (string= "Return Response" (second (hactar::mold-entity-rules entity)))))))

(test org-parse-entity-subsections-multiple
  "Test parsing multiple entity subsections."
  (let* ((content (format nil "** Route~%A route entity~%*** Rules~%- Rule 1~%** Migration~%A migration entity~%*** Rules~%- Rule 2~%"))
         (entities (hactar::%org-parse-entity-subsections content)))
    (is (= 2 (length entities)))))

(test org-parse-interface-subsections
  "Test parsing interface subsections from org content."
  (let* ((content (format nil "** Landing~%:PROPERTIES:~%:ENTITY: route~%:DESCRIPTION: A landing page~%:END:~%*** Rules~%- Use tailwind~%"))
         (interfaces (hactar::%org-parse-interface-subsections content)))
    (is (= 1 (length interfaces)))
    (let ((iface (first interfaces)))
      (is-true (hactar::mold-interface-name iface))
      (is (string= "route" (hactar::mold-interface-entity iface)))
      (is (= 1 (length (hactar::mold-interface-rules iface)))))))

;;* Full Org File Parsing Test

(test mold-parse-org-file-full
  "Test parsing a complete org-mode mold file."
  (with-clean-mold-env
    (let ((org-file (merge-pathnames "test-parse.org" hactar::*repo-root*)))
      (unwind-protect
           (progn
             (with-open-file (s org-file :direction :output :if-exists :supersede
                                         :if-does-not-exist :create)
               (format s "#+TITLE: Mold: test-org-mold~%")
               (format s "#+DESCRIPTION: An org-mode mold~%")
               (format s "~%* Metadata~%")
               (format s ":PROPERTIES:~%")
               (format s ":NAME: test-org-mold~%")
               (format s ":LLMS: CLAUDE, GPT~%")
               (format s ":END:~%")
               (format s "~%* Entities~%")
               (format s "** Route~%")
               (format s ":PROPERTIES:~%")
               (format s ":DEFAULT_TAGS: API~%")
               (format s ":DESCRIPTION: An API route~%")
               (format s ":END:~%")
               (format s "*** Rules~%")
               (format s "- Return JSON~%")
               (format s "~%* Interfaces~%")
               (format s "** Dashboard~%")
               (format s ":PROPERTIES:~%")
               (format s ":ENTITY: route~%")
               (format s ":DESCRIPTION: A dashboard~%")
               (format s ":END:~%")
               (format s "*** Rules~%")
               (format s "- Use charts~%")
               (format s "~%* Rules~%")
               (format s "- Use modern standards~%")
               (format s "- [entity:route] Always validate~%")
               (format s "- [interface:dashboard] Include nav~%"))
             (let ((mold (hactar::mold-parse-org-file org-file)))
               (is-true mold)
               (is (string= "test-org-mold" (hactar::mold-definition-name mold)))
               ;; Check entities
               (is (>= (length (hactar::mold-definition-entities mold)) 1))
               ;; Check interfaces
               (is (>= (length (hactar::mold-definition-interfaces mold)) 1))
               ;; Check rules - should have generic, entity-scoped, and interface-scoped
               (let ((rules (hactar::mold-definition-rules mold)))
                 (is (>= (length rules) 3))
                 (is (some (lambda (r) (eq :generic (hactar::mold-rule-scope r))) rules))
                 (is (some (lambda (r) (eq :entity (hactar::mold-rule-scope r))) rules))
                 (is (some (lambda (r) (eq :interface (hactar::mold-rule-scope r))) rules)))))
        (ignore-errors (delete-file org-file))))))

;;* Mold Install from Org File

(test mold-install-from-org-file
  "Test installing a mold from an org file."
  (with-clean-mold-env
    (ensure-directories-exist hactar::*molds-path*)
    (let ((org-file (merge-pathnames "install-test.org" hactar::*repo-root*)))
      (unwind-protect
           (progn
             (with-open-file (s org-file :direction :output :if-exists :supersede
                                         :if-does-not-exist :create)
               (format s "#+TITLE: Mold: org-install-test~%")
               (format s "* Metadata~%")
               (format s ":PROPERTIES:~%")
               (format s ":NAME: org-install-test~%")
               (format s ":END:~%")
               (format s "* Entities~%")
               (format s "* Rules~%")
               (format s "- A rule~%"))
             (is-true (hactar::mold-install (namestring org-file)))
             (is-true (gethash "org-install-test" hactar::*molds*)))
        (ignore-errors (delete-file org-file))
        ;; Clean up copied file in molds-path
        (ignore-errors (delete-file (merge-pathnames "org-install-test.org" hactar::*molds-path*)))))))

;;* Mold Load Installed Tests

(test mold-load-installed-lisp
  "Test loading installed molds from lisp files in *molds-path*."
  (with-clean-mold-env
    (ensure-directories-exist hactar::*molds-path*)
    (let ((mold-file (merge-pathnames "loaded-mold.lisp" hactar::*molds-path*)))
      (unwind-protect
           (progn
             (with-open-file (s mold-file :direction :output :if-exists :supersede
                                          :if-does-not-exist :create)
               (format s "(in-package :hactar)~%(defmold loaded-mold \"Loaded on startup\" :entities () :rules ())"))
             (hactar::mold-load-installed)
             (is-true (gethash "loaded-mold" hactar::*molds*)))
        (ignore-errors (delete-file mold-file))))))

;;* Edge Case Tests

(test defmold-empty-mold
  "Test defining a mold with no entities, interfaces, or rules."
  (with-clean-mold-env
    (eval '(hactar::defmold empty-mold "Empty" :entities () :interfaces () :rules ()))
    (let ((mold (gethash "empty-mold" hactar::*molds*)))
      (is-true mold)
      (is (null (hactar::mold-definition-entities mold)))
      (is (null (hactar::mold-definition-interfaces mold)))
      (is (null (hactar::mold-definition-rules mold))))))

(test defmold-name-case-normalization
  "Test that mold names are normalized to lowercase."
  (with-clean-mold-env
    (eval '(hactar::defmold MyMixedCase "Mixed case" :entities () :rules ()))
    (is-true (gethash "mymixedcase" hactar::*molds*))
    (is (null (gethash "MyMixedCase" hactar::*molds*)))))

(test mold-use-case-insensitive
  "Test that mold-use works case-insensitively."
  (with-clean-mold-env
    (eval '(hactar::defmold case-test "Case test" :entities () :rules ()))
    (is-true (hactar::mold-use "Case-Test"))
    (is-true hactar::*active-mold*)))

;;* Mold Export JSON Roundtrip Integrity

(test mold-export-json-entities-have-fields
  "Test that exported JSON entities contain all expected fields."
  (with-clean-mold-env
    (eval '(hactar::defmold json-fields-test
             "JSON fields test"
             :entities
             ((route :desc "A route" :default-tags (:api) :required-behaviors (:auth)
               :example "src/app.ts" :rules ("R1" "R2")))
             :rules ()))
    (let* ((mold (gethash "json-fields-test" hactar::*molds*))
           (json-str (hactar::mold-export-as-json mold))
           (parsed (cl-json:decode-json-from-string json-str))
           (entities (cdr (assoc :entities parsed)))
           (entity (first entities)))
      (is-true (assoc :name entity))
      (is-true (assoc :description entity))
      (is-true (assoc :default-tags entity))
      (is-true (assoc :required-behaviors entity))
      (is-true (assoc :example entity))
      (is-true (assoc :rules entity))
      (is (= 2 (length (cdr (assoc :rules entity))))))))

;;* Mold Validate Entity Without Implementation

(test mold-validate-missing-entity-implementation
  "Test that validation detects entities with no implementations."
  (with-clean-mold-env
    (eval '(hactar::defmold validate-entity-test
             "Validate entity test"
             :entities ((unique-entity-xyz :desc "Entity with no impl"))
             :rules ()))
    (hactar::mold-use "validate-entity-test")
    (multiple-value-bind (ok issues)
        (hactar::mold-validate)
      (is (null ok))
      (is (some (lambda (issue) (search "unique-entity-xyz" issue)) issues)))))
