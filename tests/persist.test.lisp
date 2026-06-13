(in-package :hactar-tests)

(def-suite persist-tests
  :description "Tests for the persistence system")

(in-suite persist-tests)

(test refactor-persist-capture-and-apply
  "Test capture-state, apply-state and defsession."
  (let ((hactar::*name* "orig-name")
        (hactar::*author* "orig-author")
        (hactar::*language* "orig-lang")
        (hactar::*stack* '("orig-stack"))
        (hactar::*files* '())
        (hactar::*images* '())
        (hactar::*docs-context* '())
        (hactar::*errors-context* '())
        (hactar::*chat-history* '())
        (hactar::*active-presets* '())
        (hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*git-autocommit* nil)
        (hactar::*tool-use-enabled* nil))

    (setf (gethash "rule1" hactar::*active-rules*) "rule-text-1")

    (let ((state (hactar::capture-state)))
      (is (equal "orig-name" (getf state :name)))
      (is (equal "orig-author" (getf state :author)))
      (is (equal "orig-lang" (getf state :language)))
      (is (equal '("orig-stack") (getf state :stack)))
      (is (equal '(("rule1" . "rule-text-1")) (getf state :active-rules)))

      ;; Mutate original values
      (setf hactar::*name* "new-name"
            hactar::*author* "new-author"
            hactar::*language* "new-lang"
            hactar::*stack* '("new-stack"))
      (clrhash hactar::*active-rules*)

      ;; Restore via apply-state
      (hactar::apply-state state :merge nil)
      (is (equal "orig-name" hactar::*name*))
      (is (equal "orig-author" hactar::*author*))
      (is (equal "orig-lang" hactar::*language*))
      (is (equal '("orig-stack") hactar::*stack*))
      (is (equal "rule-text-1" (gethash "rule1" hactar::*active-rules*)))

      ;; Test merge
      (let ((merge-state '(:name "merged-name" :author "merged-author")))
        (hactar::apply-state merge-state :merge t)
        (is (equal "merged-name" hactar::*name*))
        (is (equal "merged-author" hactar::*author*))
        (is (equal "orig-lang" hactar::*language*))))))
