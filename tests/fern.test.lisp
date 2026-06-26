(in-package :hactar-tests)

(def-suite fern-tests
  :description "Tests for the Fern PEG parser and query engine.")

(in-suite fern-tests)

(test grammar-definition-test
  "Verify custom grammars can be defined, inherited, and rules retrieved."
  (hactar:defgrammar :test-parent
    :name "Test Parent"
    :file-extensions ("t1")
    :rules
    ((root (choice identifier number))
     (identifier (seq #\a))
     (number (seq #\1))))
  
  (hactar:defgrammar :test-child
    :name "Test Child"
    :file-extensions ("t2")
    :inherits :test-parent
    :rules
    ((root (choice identifier number float))
     (float (seq #\1 #\. #\0))))

  (let ((parent-rules (hactar::get-grammar-rules :test-parent))
        (child-rules (hactar::get-grammar-rules :test-child)))
    (is-true (assoc 'root parent-rules))
    (is-true (assoc 'identifier parent-rules))
    (is-true (assoc 'number parent-rules))
    (is-false (assoc 'float parent-rules))
    
    (is-true (assoc 'root child-rules))
    (is-true (assoc 'float child-rules))
    ;; Inherited rules should be present
    (is-true (assoc 'identifier child-rules))
    (is-true (assoc 'number child-rules))))

(test basic-parsing-test
  "Verify parsing simple typescript-like input works correctly."
  (let* ((source "class A { }")
         (tree (hactar:fern-parse :typescript source)))
    (is-true (hactar::fern-tree-p tree))
    (let ((root (hactar::fern-tree-root-node tree)))
      (is (eq 'hactar::program (hactar:fern-node-type root)))
      (is (= 0 (hactar::fern-node-start-byte root)))
      (is (= (length source) (hactar::fern-node-end-byte root)))
      (is-true (hactar:fern-node-children root))
      ;; The first child is a statement wrapping the class-declaration
      (let ((stmt (first (hactar:fern-node-children root))))
        (is (eq 'hactar::statement (hactar:fern-node-type stmt)))
        (is (string= "class A { }" (hactar:fern-node-text stmt)))))))

(test fern-node-structure-test
  "Verify fern-node struct accessors work correctly."
  (let ((node (hactar::make-fern-node
               :type :test
               :start-byte 0
               :end-byte 5
               :start-point (cons 1 1)
               :end-point (cons 1 6)
               :children nil
               :parent nil
               :named-p t
               :text "hello")))
    (is (eq :test (hactar:fern-node-type node)))
    (is (= 0 (hactar::fern-node-start-byte node)))
    (is (= 5 (hactar::fern-node-end-byte node)))
    (is (equal (cons 1 1) (hactar::fern-node-start-point node)))
    (is (equal (cons 1 6) (hactar::fern-node-end-point node)))
    (is (null (hactar:fern-node-children node)))
    (is (null (hactar:fern-node-parent node)))
    (is-true (hactar::fern-node-named-p node))
    (is (string= "hello" (hactar:fern-node-text node)))
    (is-true (hactar:fern-node-p node))))

(test query-engine-test
  "Verify querying node patterns and variables works."
  ;; Use the built-in :typescript grammar to avoid cross-package symbol issues.
  ;; "class B { }" parses to: program -> statement -> (class B { })
  ;; The statement has a named child :name with text "B" and :body with text "{ }".
  (let* ((source "class B { }")
         (tree (hactar:fern-parse :typescript source)))
    ;; Simple symbol query: find all 'statement' nodes in the tree
    (let ((nodes (hactar::fern-query-nodes tree 'hactar::statement)))
      (is (= 1 (length nodes)))
      (is (eq 'hactar::statement (hactar:fern-node-type (first nodes)))))

    ;; Variable capture query: match a :name node and capture its child
    (let* ((var-sym (intern "@CAPTURED" :hactar-tests))
           (query-var (list :name var-sym))
           (results (hactar:fern-query tree query-var)))
      (is (plusp (length results))
          ":name nodes with captured children should be found")
      (when (plusp (length results))
        (let* ((bindings (first results))
               (binding (assoc var-sym bindings)))
          (is-true binding "The capture variable should be bound")
          (when binding
            (is-true (hactar:fern-node-p (cdr binding))
                     "The captured value should be a fern-node")))))))

(test query-nodes-empty-test
  "fern-query-nodes returns empty list when pattern does not match."
  (let* ((source "class A { }")
         (tree (hactar:fern-parse :typescript source)))
    (let ((nodes (hactar::fern-query-nodes tree 'hactar::import-statement)))
      (is (= 0 (length nodes))))))

(test compute-row-col-test
  "Verify row/col computation from byte index."
  (is (equal (cons 1 1) (hactar::compute-row-col "hello" 0)))
  (is (equal (cons 1 6) (hactar::compute-row-col "hello" 5)))
  ;; Multi-line
  (let ((src (format nil "ab~%cd")))
    (is (equal (cons 1 1) (hactar::compute-row-col src 0)))
    (is (equal (cons 1 3) (hactar::compute-row-col src 2)))
    ;; After the newline
    (is (equal (cons 2 1) (hactar::compute-row-col src 3)))
    (is (equal (cons 2 2) (hactar::compute-row-col src 4)))))

(test skip-whitespace-test
  "Verify skip-whitespace skips spaces, tabs, and newlines."
  (is (= 0 (hactar::skip-whitespace "hello" 0)))
  (is (= 3 (hactar::skip-whitespace "   hello" 0)))
  (is (= 2 (hactar::skip-whitespace (format nil "~%~%x") 0)))
  (is (= 5 (hactar::skip-whitespace "hello" 5))))

(test incremental-parsing-test
  "Verify fern-edit shifts memo keys and incrementally reparses."
  (let* ((source "class A { }")
         (tree (hactar:fern-parse :typescript source))
         (new-source "class Foobar { }")
         (edit (hactar::make-fern-edit
                :start-byte 6
                :old-end-byte 7
                :new-end-byte 12
                :start-point (cons 1 7)
                :old-end-point (cons 1 8)
                :new-end-point (cons 1 13))))
    ;; Update tree source first as required by fern-edit
    (setf (hactar::fern-tree-source tree) new-source)
    (let ((new-tree (hactar:fern-edit tree edit)))
      (is-true (hactar::fern-tree-p new-tree))
      (let ((root (hactar::fern-tree-root-node new-tree)))
        (is (eq 'hactar::program (hactar:fern-node-type root)))
        (is (string= "class Foobar { }" (hactar:fern-node-text root)))))))
