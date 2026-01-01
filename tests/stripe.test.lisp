(in-package :hactar-tests)

(def-suite stripe-mode-tests
  :description "Tests for stripe-mode functionality.")

(in-suite stripe-mode-tests)

(test stripe-normalize-filename-test
  "Test converting snake_case filenames to Title Case."
  (is (string= (hactar::stripe--normalize-filename "foo_bar.md") "Foo Bar"))
  (is (string= (hactar::stripe--normalize-filename "create_account_token.md") "Create Account Token"))
  (is (string= (hactar::stripe--normalize-filename "simple") "Simple"))
  (is (string= (hactar::stripe--normalize-filename "a/b/c/nested_file.md") "Nested File")))

(test stripe-extract-blocks-test
  "Test extracting code blocks."
  (let ((content "intro
```curl
curl https://api.stripe.com/v1/charges
```
mid text
```ruby
Stripe::Charge.create
```
end text"))
    (let ((curl-blocks (hactar::stripe--extract-blocks content "curl")))
      (is (= 1 (length curl-blocks)))
      (is (string= (first curl-blocks) "curl https://api.stripe.com/v1/charges")))
    
    (let ((ruby-blocks (hactar::stripe--extract-blocks content "ruby")))
      (is (= 1 (length ruby-blocks)))
      (is (string= (first ruby-blocks) "Stripe::Charge.create")))

    (let ((py-blocks (hactar::stripe--extract-blocks content "python")))
      (is (null py-blocks)))))

(test stripe-extract-blocks-multiple-same-lang
  "Test extracting multiple blocks of the same language."
  (let ((content "
```curl
block 1
```
text
```curl
block 2
```"))
    (let ((blocks (hactar::stripe--extract-blocks content "curl")))
      (is (= 2 (length blocks)))
      (is (string= (first blocks) "block 1"))
      (is (string= (second blocks) "block 2")))))

(test stripe-tree-operations-test
  "Test tree building operations."
  (let ((root (hactar::make-stripe-doc-node :name "Root" :children (make-hash-table :test 'equal))))
    ;; Test get-or-create-child
    (let ((child (hactar::stripe--get-or-create-child root "Child")))
      (is (typep child 'hactar::stripe-doc-node))
      (is (string= (hactar::stripe-doc-node-name child) "Child")))
    
    ;; Test add-file-to-tree
    (hactar::stripe--add-file-to-tree root "core_resources/charges/create_charge.md" '("code1"))
    
    ;; Verify structure: Root -> Core Resources -> Charges -> Create Charge
    (let* ((core (gethash "Core Resources" (hactar::stripe-doc-node-children root)))
           (charges (when core (gethash "Charges" (hactar::stripe-doc-node-children core))))
           (leaf (when charges (gethash "Create Charge" (hactar::stripe-doc-node-children charges)))))
      (is-true core)
      (is-true charges)
      (is-true leaf)
      (is (equal (hactar::stripe-doc-node-content leaf) '("code1"))))))

(test stripe-print-tree-test
  "Test printing the tree structure."
  (let ((root (hactar::make-stripe-doc-node :name "Stripe" :children (make-hash-table :test 'equal))))
    (hactar::stripe--add-file-to-tree root "api/test.md" '("code"))
    
    (let ((output (with-output-to-string (*standard-output*)
                    (hactar::stripe--print-tree root 1 "curl"))))
      (is (search "* Stripe" output))
      (is (search "** Api" output))
      (is (search "*** Test" output))
      (is (search "#+begin_src curl" output))
      (is (search "code" output))
      (is (search "#+end_src" output)))))
