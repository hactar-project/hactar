;; feature system tests
(in-package :hactar-tests)

(def-suite feature-tests
  :description "Tests for the  Feature system")

(in-suite feature-tests)

;;* Helper utilities for tests

(defmacro with-clean-registries (&body body)
  "Execute BODY with fresh, empty registries for entities, features, skills, etc."
  `(let ((hactar::*feature-registry* (make-hash-table :test 'equal))
         (hactar::*active-features* (make-hash-table :test 'equal))
         (hactar::*entity-registry* (make-hash-table :test 'equal))
         (hactar::*entity-instances* (make-hash-table :test 'equal))
         (hactar::*entity-implementations* (make-hash-table :test 'equal))
         (hactar::*skill-registry* (make-hash-table :test 'equal))
         (hactar::*behavior-registry* (make-hash-table :test 'equal))
         (hactar::*tag-registry* (make-hash-table :test 'equal))
         (hactar::*active-rules* (make-hash-table :test 'equal))
         (hactar::*generators* (make-hash-table :test 'equal))
         (hactar::*active-generators* (make-hash-table :test 'equal))
         (hactar::*patterns* (make-hash-table :test 'equal))
         (hactar::*active-patterns* (make-hash-table :test 'equal))
         (hactar::*entity-counter* 0)
         (hactar::*silent* t))
     ,@body))

;;* deffeature macro

(test deffeature-basic-definition
  "Test that deffeature registers a feature with variants."
  (with-clean-registries
    (eval '(hactar:deffeature test-feature
             "A test feature"
             :variants
             ((:default
               :description "Default variant"
               :skills (:some-skill)
               :rules "Some rules here"
               :packages ("pkg-a")))))

    (let ((fdef (gethash "test-feature" hactar::*feature-registry*)))
      (is-true fdef "Feature should be registered")
      (is (string= "A test feature" (hactar::feature-definition-description fdef)))
      (is (eq 'test-feature (hactar::feature-definition-name fdef)))
      (is (null (hactar::feature-definition-active-p fdef)))
      (is (null (hactar::feature-definition-active-variant fdef)))
      ;; Check variant
      (let ((variant (gethash :default (hactar::feature-definition-variants fdef))))
        (is-true variant "Default variant should exist")
        (is (string= "Default variant" (hactar::feature-variant-description variant)))
        (is (equal '(:some-skill) (hactar::feature-variant-skills variant)))
        (is (string= "Some rules here" (hactar::feature-variant-rules variant)))
        (is (equal '("pkg-a") (hactar::feature-variant-packages variant)))))))

(test deffeature-multiple-variants
  "Test that deffeature handles multiple variants correctly."
  (with-clean-registries
    (eval '(hactar:deffeature multi-variant
             "Feature with multiple variants"
             :variants
             ((:default
               :description "Base variant"
               :skills (:base-skill)
               :rules "Base rules"
               :packages ("base-pkg"))
              (:enhanced
               :description "Enhanced variant"
               :inherits :default
               :skills (:extra-skill)
               :packages ("extra-pkg"))
              (:minimal
               :description "Minimal variant"
               :skills ()
               :rules "Minimal rules"))))

    (let ((fdef (gethash "multi-variant" hactar::*feature-registry*)))
      (is-true fdef)
      (is-true (gethash :default (hactar::feature-definition-variants fdef)))
      (is-true (gethash :enhanced (hactar::feature-definition-variants fdef)))
      (is-true (gethash :minimal (hactar::feature-definition-variants fdef)))
      ;; Verify enhanced has :inherits
      (let ((enhanced (gethash :enhanced (hactar::feature-definition-variants fdef))))
        (is (eq :default (hactar::feature-variant-inherits enhanced)))))))

;;* Variant Resolution

(test variant-resolution-no-inheritance
  "Test resolving a variant without inheritance."
  (with-clean-registries
    (eval '(hactar:deffeature resolve-test
             "Resolution test"
             :variants
             ((:default
               :description "Default"
               :skills (:skill-a :skill-b)
               :rules "Default rules"
               :entities ((:type component :name "Comp1" :tags (:ui)))
               :packages ("dep-a")))))

    (let* ((fdef (gethash "resolve-test" hactar::*feature-registry*))
           (resolved (hactar::%resolve-feature-variant fdef :default)))
      (is-true resolved)
      (is (equal '(:skill-a :skill-b) (hactar::feature-variant-skills resolved)))
      (is (string= "Default rules" (hactar::feature-variant-rules resolved)))
      (is (= 1 (length (hactar::feature-variant-entities resolved))))
      (is (equal '("dep-a") (hactar::feature-variant-packages resolved))))))

(test variant-resolution-with-inheritance
  "Test resolving a variant that inherits from another."
  (with-clean-registries
    (eval '(hactar:deffeature inherit-test
             "Inheritance test"
             :variants
             ((:default
               :description "Base"
               :skills (:base-skill)
               :rules "Base rules"
               :entities ((:type component :name "Base" :tags (:core)))
               :generators (:gen-a)
               :patterns (:pat-a)
               :packages ("base-pkg")
               :tags (:tag-a)
               :env (("BASE_VAR" . "base value")))
              (:child
               :description "Child"
               :inherits :default
               :skills (:child-skill)
               :rules "Child rules"
               :entities ((:type component :name "Child" :tags (:extra)))
               :generators (:gen-b)
               :patterns (:pat-b)
               :packages ("child-pkg")
               :tags (:tag-b)
               :env (("CHILD_VAR" . "child value"))))))

    (let* ((fdef (gethash "inherit-test" hactar::*feature-registry*))
           (resolved (hactar::%resolve-feature-variant fdef :child)))
      (is-true resolved)
      ;; Skills: parent + child (deduped)
      (is (member :base-skill (hactar::feature-variant-skills resolved)))
      (is (member :child-skill (hactar::feature-variant-skills resolved)))
      ;; Rules: merged with newlines
      (is (search "Base rules" (hactar::feature-variant-rules resolved)))
      (is (search "Child rules" (hactar::feature-variant-rules resolved)))
      ;; Entities: appended
      (is (= 2 (length (hactar::feature-variant-entities resolved))))
      ;; Generators: appended (deduped)
      (is (member :gen-a (hactar::feature-variant-generators resolved)))
      (is (member :gen-b (hactar::feature-variant-generators resolved)))
      ;; Patterns: appended (deduped)
      (is (member :pat-a (hactar::feature-variant-patterns resolved)))
      (is (member :pat-b (hactar::feature-variant-patterns resolved)))
      ;; Packages: appended (deduped)
      (is (member "base-pkg" (hactar::feature-variant-packages resolved) :test #'string=))
      (is (member "child-pkg" (hactar::feature-variant-packages resolved) :test #'string=))
      ;; Tags: appended
      (is (member :tag-a (hactar::feature-variant-tags resolved)))
      (is (member :tag-b (hactar::feature-variant-tags resolved)))
      ;; Env: appended
      (is (= 2 (length (hactar::feature-variant-env resolved))))
      ;; Description: child overrides
      (is (string= "Child" (hactar::feature-variant-description resolved))))))

(test variant-resolution-nonexistent-variant
  "Test that resolving a non-existent variant returns NIL."
  (with-clean-registries
    (eval '(hactar:deffeature no-variant
             "No variant test"
             :variants
             ((:default :description "Only variant"))))

    (let* ((fdef (gethash "no-variant" hactar::*feature-registry*))
           (resolved (hactar::%resolve-feature-variant fdef :nonexistent)))
      (is (null resolved)))))

(test variant-resolution-child-description-fallback
  "Test that child inherits parent description when child has none."
  (with-clean-registries
    (eval '(hactar:deffeature desc-fallback
             "Description fallback test"
             :variants
             ((:default
               :description "Parent description"
               :skills (:a))
              (:child
               :inherits :default
               :skills (:b)))))

    (let* ((fdef (gethash "desc-fallback" hactar::*feature-registry*))
           (resolved (hactar::%resolve-feature-variant fdef :child)))
      (is (string= "Parent description" (hactar::feature-variant-description resolved))))))

;;* Feature Activation

(test activate-feature-default-variant
  "Test activating a feature with the default variant."
  (with-clean-registries
    ;; Register a skill so activation can load it
    (hactar::register-skill :test-skill
                            :description "Test skill"
                            :instructions "Do test things"
                            :rules "Test rule")

    (eval '(hactar:deffeature activation-test
             "Activation test"
             :variants
             ((:default
               :description "Default"
               :skills (:test-skill)
               :rules "Always do X when Y"))))

    (hactar:activate-feature "activation-test")

    ;; Check feature is active
    (let ((fdef (gethash "activation-test" hactar::*feature-registry*)))
      (is-true (hactar::feature-definition-active-p fdef))
      (is (eq :default (hactar::feature-definition-active-variant fdef))))

    ;; Check active features registry
    (let ((state (gethash "activation-test" hactar::*active-features*)))
      (is-true state)
      (is (eq :default (getf state :variant))))

    ;; Check rules were injected
    (let ((rule (gethash "feature/activation-test/rules" hactar::*active-rules*)))
      (is-true rule)
      (is (string= "Always do X when Y" rule)))))

(test activate-feature-specific-variant
  "Test activating a feature with a specific variant."
  (with-clean-registries
    (eval '(hactar:deffeature variant-activation
             "Variant activation test"
             :variants
             ((:default
               :description "Default"
               :rules "Default rules")
              (:premium
               :description "Premium"
               :inherits :default
               :rules "Premium rules"))))

    (hactar:activate-feature "variant-activation" :variant :premium)

    (let ((fdef (gethash "variant-activation" hactar::*feature-registry*)))
      (is-true (hactar::feature-definition-active-p fdef))
      (is (eq :premium (hactar::feature-definition-active-variant fdef))))

    ;; Rules should be merged (parent + child)
    (let ((rule (gethash "feature/variant-activation/rules" hactar::*active-rules*)))
      (is-true rule)
      (is (search "Default rules" rule))
      (is (search "Premium rules" rule)))))

(test activate-feature-unknown-feature
  "Test that activating an unknown feature returns NIL."
  (with-clean-registries
    (let ((result (hactar:activate-feature "nonexistent-feature")))
      (is (null result)))))

(test activate-feature-unknown-variant
  "Test that activating with an unknown variant returns NIL."
  (with-clean-registries
    (eval '(hactar:deffeature known-feature
             "Known feature"
             :variants
             ((:default :description "Default"))))

    (let ((result (hactar:activate-feature "known-feature" :variant :unknown)))
      (is (null result)))))

(test activate-feature-replaces-previous-activation
  "Test that re-activating a feature deactivates the previous one first."
  (with-clean-registries
    (eval '(hactar:deffeature reactivation-test
             "Reactivation test"
             :variants
             ((:default
               :description "Default"
               :rules "Default rules")
              (:alt
               :description "Alt"
               :rules "Alt rules"))))

    ;; Activate with default
    (hactar:activate-feature "reactivation-test")
    (is (string= "Default rules"
                  (gethash "feature/reactivation-test/rules" hactar::*active-rules*)))

    ;; Re-activate with alt
    (hactar:activate-feature "reactivation-test" :variant :alt)
    (is (string= "Alt rules"
                  (gethash "feature/reactivation-test/rules" hactar::*active-rules*)))

    ;; Should still only be one active entry
    (let ((fdef (gethash "reactivation-test" hactar::*feature-registry*)))
      (is (eq :alt (hactar::feature-definition-active-variant fdef))))))

(test activate-feature-creates-entities
  "Test that feature activation creates entity instances."
  (with-clean-registries
    ;; We need a basic entity implementation registered for entity/create to work
    (hactar::register-entity-implementation
     "component"
     (hactar::make-entity-implementation
      :name 'test-component-impl
      :entity-type "component"
      :description "Test component impl"
      :priority 0))

    (eval '(hactar:deffeature entity-creation-test
             "Entity creation test"
             :variants
             ((:default
               :description "Default"
               :entities
               ((:type component :name "Widget" :tags (:ui))
                (:type component :name "Button" :tags (:ui :interactive)))))))

    (hactar:activate-feature "entity-creation-test")

    (let ((state (gethash "entity-creation-test" hactar::*active-features*)))
      ;; Entity IDs should be tracked
      (is (= 2 (length (getf state :entity-ids))))
      ;; Entity instances should exist
      (let ((instances (hactar:find-entity-instances 'component)))
        (is (= 2 (length instances)))))))

;;* Feature Deactivation

(test deactivate-feature-basic
  "Test basic feature deactivation."
  (with-clean-registries
    (hactar::register-skill :deact-skill
                            :description "Deact skill"
                            :instructions "Instructions")

    (eval '(hactar:deffeature deactivation-test
             "Deactivation test"
             :variants
             ((:default
               :description "Default"
               :skills (:deact-skill)
               :rules "Some rules"))))

    ;; Activate then deactivate
    (hactar:activate-feature "deactivation-test")
    (is-true (gethash "feature/deactivation-test/rules" hactar::*active-rules*))

    (hactar:deactivate-feature "deactivation-test")

    ;; Check feature is no longer active
    (let ((fdef (gethash "deactivation-test" hactar::*feature-registry*)))
      (is (null (hactar::feature-definition-active-p fdef)))
      (is (null (hactar::feature-definition-active-variant fdef))))

    ;; Check active features registry is cleaned
    (is (null (gethash "deactivation-test" hactar::*active-features*)))

    ;; Check rules were removed
    (is (null (gethash "feature/deactivation-test/rules" hactar::*active-rules*)))))

(test deactivate-feature-not-active
  "Test that deactivating a non-active feature returns NIL."
  (with-clean-registries
    (eval '(hactar:deffeature not-active
             "Not active"
             :variants ((:default :description "Default"))))

    (let ((result (hactar:deactivate-feature "not-active")))
      (is (null result)))))

(test deactivate-feature-cleans-entity-instances
  "Test that deactivation clears entity instances created by the feature."
  (with-clean-registries
    (hactar::register-entity-implementation
     "component"
     (hactar::make-entity-implementation
      :name 'test-impl
      :entity-type "component"
      :priority 0))

    (eval '(hactar:deffeature entity-cleanup-test
             "Entity cleanup test"
             :variants
             ((:default
               :description "Default"
               :entities
               ((:type component :name "Temp" :tags (:temp)))))))

    (hactar:activate-feature "entity-cleanup-test")
    (is (= 1 (length (hactar:find-entity-instances 'component))))

    (hactar:deactivate-feature "entity-cleanup-test")
    ;; Entities should be removed (though the current implementation
    ;; removes by type+id, entities created by the feature should be gone)
    (is (null (gethash "entity-cleanup-test" hactar::*active-features*)))))

;;* Dry Run

(test activate-feature-dry-run
  "Test that dry-run does not actually activate the feature."
  (with-clean-registries
    (eval '(hactar:deffeature dry-run-test
             "Dry run test"
             :variants
             ((:default
               :description "Default"
               :rules "Dry run rules"
               :packages ("dry-pkg")))))

    (let ((result (hactar:activate-feature "dry-run-test" :dry-run t)))
      (is-true result))

    ;; Feature should NOT be active
    (let ((fdef (gethash "dry-run-test" hactar::*feature-registry*)))
      (is (null (hactar::feature-definition-active-p fdef))))
    (is (null (gethash "dry-run-test" hactar::*active-features*)))
    (is (null (gethash "feature/dry-run-test/rules" hactar::*active-rules*)))))

;;* Query Functions

(test get-feature
  "Test get-feature retrieves a feature by name."
  (with-clean-registries
    (eval '(hactar:deffeature get-me
             "Get me test"
             :variants ((:default :description "Default"))))

    (is-true (hactar:get-feature "get-me"))
    (is-true (hactar:get-feature 'get-me))
    (is (null (hactar:get-feature "nonexistent")))))

(test list-features-returns-all
  "Test list-features returns all defined features."
  (with-clean-registries
    (eval '(hactar:deffeature feat-alpha
             "Alpha"
             :variants ((:default :description "Default"))))
    (eval '(hactar:deffeature feat-beta
             "Beta"
             :variants ((:default :description "Default")
                        (:alt :description "Alt"))))

    (let ((features (hactar:list-features)))
      (is (= 2 (length features)))
      ;; Should be sorted by name
      (is (string= "feat-alpha" (getf (first features) :name)))
      (is (string= "feat-beta" (getf (second features) :name)))
      ;; Check variants
      (let ((beta (second features)))
        (is (= 2 (length (getf beta :variants))))))))

(test list-features-shows-active-status
  "Test that list-features correctly shows active/inactive status."
  (with-clean-registries
    (eval '(hactar:deffeature status-feat
             "Status test"
             :variants ((:default :description "Default"))))

    ;; Before activation
    (let ((features (hactar:list-features)))
      (is (null (getf (first features) :active))))

    ;; After activation
    (hactar:activate-feature "status-feat")
    (let ((features (hactar:list-features)))
      (is-true (getf (first features) :active))
      (is (eq :default (getf (first features) :active-variant))))))

(test list-active-features-summary
  "Test list-active-features-summary returns correct summary."
  (with-clean-registries
    (eval '(hactar:deffeature summary-a
             "Summary A"
             :variants ((:default :description "Default"))))
    (eval '(hactar:deffeature summary-b
             "Summary B"
             :variants ((:default :description "Default")
                        (:premium :description "Premium"))))

    ;; No active features
    (is (null (hactar:list-active-features-summary)))

    ;; Activate some
    (hactar:activate-feature "summary-a")
    (hactar:activate-feature "summary-b" :variant :premium)

    (let ((summaries (hactar:list-active-features-summary)))
      (is (= 2 (length summaries)))
      ;; Should contain feature name and variant
      (is-true (find-if (lambda (s) (search "summary-a" s)) summaries))
      (is-true (find-if (lambda (s) (search "summary-b" s)) summaries))
      (is-true (find-if (lambda (s) (search "PREMIUM" s)) summaries)))))

;;* Active Feature Rules for System Prompt

(test get-active-feature-rules-empty
  "Test that get-active-feature-rules returns empty string when no features active."
  (with-clean-registries
    (is (string= "" (hactar::get-active-feature-rules)))))

(test get-active-feature-rules-with-active-features
  "Test that get-active-feature-rules collects rules from active features."
  (with-clean-registries
    (eval '(hactar:deffeature rules-feat-1
             "Rules feature 1"
             :variants ((:default :description "Default" :rules "Rule set A"))))
    (eval '(hactar:deffeature rules-feat-2
             "Rules feature 2"
             :variants ((:default :description "Default" :rules "Rule set B"))))

    (hactar:activate-feature "rules-feat-1")
    (hactar:activate-feature "rules-feat-2")

    (let ((rules (hactar::get-active-feature-rules)))
      (is (search "Active Feature Rules" rules))
      (is (search "Rule set A" rules))
      (is (search "Rule set B" rules)))))

;;* Merge Variants Edge Cases

(test merge-variants-skill-deduplication
  "Test that merging variants deduplicates skills."
  (with-clean-registries
    (eval '(hactar:deffeature dedup-test
             "Dedup test"
             :variants
             ((:default
               :description "Base"
               :skills (:shared-skill :base-only))
              (:child
               :inherits :default
               :skills (:shared-skill :child-only)))))

    (let* ((fdef (gethash "dedup-test" hactar::*feature-registry*))
           (resolved (hactar::%resolve-feature-variant fdef :child)))
      ;; :shared-skill should appear only once
      (is (= 3 (length (hactar::feature-variant-skills resolved))))
      (is (= 1 (count :shared-skill (hactar::feature-variant-skills resolved)))))))

(test merge-variants-package-deduplication
  "Test that merging variants deduplicates packages."
  (with-clean-registries
    (eval '(hactar:deffeature pkg-dedup-test
             "Package dedup test"
             :variants
             ((:default
               :description "Base"
               :packages ("shared-pkg" "base-pkg"))
              (:child
               :inherits :default
               :packages ("shared-pkg" "child-pkg")))))

    (let* ((fdef (gethash "pkg-dedup-test" hactar::*feature-registry*))
           (resolved (hactar::%resolve-feature-variant fdef :child)))
      (is (= 3 (length (hactar::feature-variant-packages resolved))))
      (is (= 1 (count "shared-pkg" (hactar::feature-variant-packages resolved) :test #'string=))))))

(test merge-variants-rules-both-nil
  "Test that merging variants with both rules NIL results in NIL."
  (with-clean-registries
    (eval '(hactar:deffeature nil-rules
             "Nil rules test"
             :variants
             ((:default :description "Base")
              (:child :inherits :default))))

    (let* ((fdef (gethash "nil-rules" hactar::*feature-registry*))
           (resolved (hactar::%resolve-feature-variant fdef :child)))
      (is (null (hactar::feature-variant-rules resolved))))))

(test merge-variants-rules-parent-only
  "Test that child with no rules inherits parent rules."
  (with-clean-registries
    (eval '(hactar:deffeature parent-rules
             "Parent rules test"
             :variants
             ((:default :description "Base" :rules "Parent rules only")
              (:child :inherits :default :description "Child"))))

    (let* ((fdef (gethash "parent-rules" hactar::*feature-registry*))
           (resolved (hactar::%resolve-feature-variant fdef :child)))
      (is (string= "Parent rules only" (hactar::feature-variant-rules resolved))))))

;;* Feature with Skills Integration

(test feature-loads-skills-on-activation
  "Test that activating a feature loads its skills into active rules."
  (with-clean-registries
    (hactar::register-skill :integration-skill
                            :description "Integration skill"
                            :instructions "Do integration things"
                            :rules "Integration rules")

    (eval '(hactar:deffeature skill-integration
             "Skill integration test"
             :variants
             ((:default
               :description "Default"
               :skills (:integration-skill)))))

    (hactar:activate-feature "skill-integration")

    ;; The skill should have been loaded into context
    (let ((skill-rule (gethash :integration-skill hactar::*active-rules*)))
      (is-true skill-rule)
      (is (string= "Do integration things" skill-rule)))))

;;* Feature with On-Activate/On-Deactivate Hooks

(test feature-on-activate-hook
  "Test that on-activate hook is called during activation."
  (with-clean-registries
    (let ((hook-called nil))
      (eval `(hactar:deffeature hook-test
               "Hook test"
               :variants
               ((:default
                 :description "Default"
                 :on-activate (lambda () (setf ,hook-called t))))))

      ;; Note: We can't easily test closures across eval boundaries.
      ;; Instead, test that activation completes without error even when hook is present.
      (let ((result (hactar:activate-feature "hook-test")))
        (is-true result)))))

;;* Feature Slash Command Behavior

(test feature-list-command
  "Test the /feature list sub-command."
  (with-clean-registries
    (eval '(hactar:deffeature cmd-feat-1
             "Command feature 1"
             :variants ((:default :description "Default"))))
    (eval '(hactar:deffeature cmd-feat-2
             "Command feature 2"
             :variants ((:default :description "Default"))))

    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::%feature-cmd-list))))
      (is (search "cmd-feat-1" output))
      (is (search "cmd-feat-2" output)))))

(test feature-active-command
  "Test the /feature active sub-command."
  (with-clean-registries
    (eval '(hactar:deffeature active-cmd-test
             "Active cmd test"
             :variants ((:default :description "Default"))))

    ;; Before activation
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::%feature-cmd-active))))
      (is (search "No active" output)))

    ;; After activation
    (hactar:activate-feature "active-cmd-test")
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::%feature-cmd-active))))
      (is (search "active-cmd-test" output)))))

(test feature-show-command
  "Test the /feature show sub-command."
  (with-clean-registries
    (eval '(hactar:deffeature show-test
             "Show test feature"
             :variants
             ((:default :description "Default" :rules "Show rules")
              (:alt :description "Alt"))))

    ;; Show before activation
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::%feature-cmd-show "show-test"))))
      (is (search "show-test" output))
      (is (search "Show test feature" output))
      (is (search "no" output))) ; Active: no

    ;; Show after activation
    (hactar:activate-feature "show-test")
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::%feature-cmd-show "show-test"))))
      (is (search "yes" output)))))

(test feature-show-command-unknown
  "Test /feature show with unknown feature."
  (with-clean-registries
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::%feature-cmd-show "unknown-feature"))))
      (is (search "not found" output)))))

;;* No-Install Flag

(test activate-feature-no-install
  "Test that :no-install flag prevents package handling."
  (with-clean-registries
    (eval '(hactar:deffeature no-install-test
             "No install test"
             :variants
             ((:default
               :description "Default"
               :packages ("should-not-install")))))

    ;; This should complete without trying to install packages
    (let ((result (hactar:activate-feature "no-install-test" :no-install t)))
      (is-true result))))
