(in-package :hactar-tests)

(def-suite docs-tests
  :description "Tests for documentation registry, guide registry, and documentation commands.")

(in-suite docs-tests)

;;* Helpers
(defun run-command-in-test (name args)
  (let ((cmd-info (gethash name hactar::*commands*)))
    (if cmd-info
        (funcall (first cmd-info) args)
        (error "Command ~A not found" name))))

;;* add-doc-to-context and remove-doc-from-context hooks & side effects

(test add-doc-to-context-hook-test
  "add-doc-to-context adds a doc and fires the added hook."
  (let ((hactar::*docs-context* nil)
        (hactar::*exposed-context-file* nil)
        (hactar::*silent* t)
        (fired nil)
        (hactar::*context-doc-added-hook*
          (make-instance 'hactar::hook-context-doc-added)))
    (nhooks:add-hook hactar::*context-doc-added-hook*
                     (make-instance 'nhooks:handler
                                    :fn (lambda (d) (setf fired d))
                                    :name 'test-doc-added-hook))
    (let ((doc '((:id . "my-doc-1") (:title . "Test Doc") (:content . "Test Content"))))
      (hactar::add-doc-to-context doc)
      (is (member doc hactar::*docs-context* :test #'equal))
      (is (equal doc fired)))))

(test remove-doc-from-context-hook-test
  "remove-doc-from-context removes a doc and fires the dropped hook."
  (let* ((doc '((:id . "my-doc-1") (:title . "Test Doc") (:content . "Test Content")))
         (hactar::*docs-context* (list doc))
         (hactar::*exposed-context-file* nil)
         (hactar::*silent* t)
         (fired nil)
         (hactar::*context-doc-dropped-hook*
          (make-instance 'hactar::hook-context-doc-dropped)))
    (nhooks:add-hook hactar::*context-doc-dropped-hook*
                     (make-instance 'nhooks:handler
                                    :fn (lambda (id) (setf fired id))
                                    :name 'test-doc-dropped-hook))
    (hactar::remove-doc-from-context "my-doc-1")
    (is (null hactar::*docs-context*))
    (is (string= "my-doc-1" fired))))

(test add-doc-to-context-expose-test
  "add-doc-to-context calls context-expose-upsert-docs-section if *exposed-context-file* is active."
  (let ((hactar::*docs-context* nil)
        (hactar::*exposed-context-file* "/tmp/fake-exposed-file.org")
        (hactar::*silent* t)
        (called-expose nil))
    (mockingbird:with-dynamic-stubs ((hactar::context-expose-upsert-docs-section
                                      (lambda () (setf called-expose t))))
      (hactar::add-doc-to-context '((:id . "my-doc-2") (:title . "Exposed Doc")))
      (is-true called-expose))))

(test remove-doc-from-context-expose-test
  "remove-doc-from-context calls context-expose-upsert-docs-section if *exposed-context-file* is active."
  (let ((hactar::*docs-context* '(((:id . "my-doc-2") (:title . "Exposed Doc"))))
        (hactar::*exposed-context-file* "/tmp/fake-exposed-file.org")
        (hactar::*silent* t)
        (called-expose nil))
    (mockingbird:with-dynamic-stubs ((hactar::context-expose-upsert-docs-section
                                      (lambda () (setf called-expose t))))
      (hactar::remove-doc-from-context "my-doc-2")
      (is-true called-expose))))

;;* add-doc

(test add-doc-basic-test
  "Test add-doc registers documentation in global lists and dispatcher registry."
  (let ((hactar::*docs* nil)
        (hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::add-doc "My First Doc" "This is documentation content" :id "doc-id-123")
    ;; Check global *docs* list
    (is (= 1 (length hactar::*docs*)))
    (let ((doc-alist (first hactar::*docs*)))
      (is (string= "doc-id-123" (cdr (assoc :id doc-alist))))
      (is (string= "My First Doc" (cdr (assoc :title doc-alist))))
      (is (string= "This is documentation content" (cdr (assoc :content doc-alist))))
      (is (string= "docs:my-first-doc" (cdr (assoc :uri doc-alist)))))

    ;; Check dispatcher registry
    (let ((docs (hactar::get-all-registered-docs)))
      (is (= 1 (length docs)))
      (is (string= "doc-id-123" (cdr (assoc :id (first docs))))))))

(test add-doc-raw-content-test
  "Test add-doc stripping of raw: prefix."
  (let ((hactar::*docs* nil)
        (hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::add-doc "Raw Doc" "raw:This is raw content" :id "doc-id-raw")
    (let ((doc-alist (first hactar::*docs*)))
      (is (string= "This is raw content" (cdr (assoc :content doc-alist)))))))

(test add-doc-custom-fields-test
  "Test add-doc with custom fields."
  (let ((hactar::*docs* nil)
        (hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::add-doc "Custom Doc" "Custom Content"
                     :id "custom-1"
                     :uri "custom:uri"
                     :tags '("tag1" "tag2")
                     :covers '("cover1")
                     :related '((:target "other-doc" :type :related)))
    (let ((doc-alist (first hactar::*docs*)))
      (is (string= "custom:uri" (cdr (assoc :uri doc-alist))))
      (is (equal '("tag1" "tag2") (cdr (assoc :tags doc-alist))))
      (is (equal '("cover1") (cdr (assoc :covers doc-alist))))
      (let* ((metadata (cdr (assoc :meta doc-alist)))
             (id-meta (cdr (assoc :id metadata)))
             (related-meta (cdr (assoc :related metadata))))
        (is (string= "custom-1" id-meta))
        (is (equal '((:target "other-doc" :type :related)) related-meta))))))

(test add-doc-symbol-uri-test
  "Test add-doc URI generation for symbols."
  (let ((hactar::*docs* nil)
        (hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::add-doc "My Function" "Some doc"
                     :meta '((:symbol . "my-func"))
                     :id "sym-1")
    (let ((doc-alist (first hactar::*docs*)))
      (is (string= "apidoc:my-func" (cdr (assoc :uri doc-alist)))))))

(test get-all-registered-docs-includes-apidoc-test
  "Test get-all-registered-docs returns both docs and apidoc entries."
  (let ((hactar::*docs* nil)
        (hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::add-doc "Regular Doc" "Regular content" :id "doc-1")
    (hactar::add-doc "API Doc" "API content"
                     :meta '((:symbol . "api-doc"))
                     :id "doc-2")
    (let ((docs (hactar::get-all-registered-docs)))
      (is (= 2 (length docs)))
      (is-true (find "docs:regular-doc" docs :key (lambda (d) (cdr (assoc :uri d))) :test #'string=))
      (is-true (find "apidoc:api-doc" docs :key (lambda (d) (cdr (assoc :uri d))) :test #'string=)))))

;;* defdocforsymbol macro

(test defdocforsymbol-macro-test
  "Test defdocforsymbol macro expands and registers properly."
  (let ((hactar::*docs* nil)
        (hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (eval '(hactar::defdocforsymbol sample-function "Docs for sample-function"
            :type :function
            :version "1.0.0"
            :related 'other-function
            :covers '("extra-cover")
            :tags '("extra-tag")))
    (let* ((dispatcher (hactar::get-or-create-dispatcher "apidoc"))
           (registry (hactar::protocol-dispatcher-registry dispatcher))
           (doc-plist (gethash "sample-function" registry))
           (doc-alist (hactar::hypertext-to-alist doc-plist)))
      (is-true doc-plist)
      (is (string= "apidoc:sample-function" (cdr (assoc :uri doc-alist))))
      (is (string= "sample-function (FUNCTION)" (cdr (assoc :title doc-alist))))
      (is (equal '("sample-function" "extra-cover") (cdr (assoc :covers doc-alist))))
      (is (equal '("symbol" "function" "extra-tag") (cdr (assoc :tags doc-alist))))
      (let* ((metadata (cdr (assoc :meta doc-alist)))
             (symbol (cdr (assoc :symbol metadata)))
             (type (cdr (assoc :type metadata)))
             (version (cdr (assoc :version metadata)))
             (related (cdr (assoc :related metadata))))
        (is (string= "sample-function" symbol))
        (is (eq :function type))
        (is (string= "1.0.0" version))
        (is (equal '((:target "other-function" :type :related)) related))))))

;;* defdoc macro

(test defdoc-macro-test
  "Test defdoc macro registers documentation."
  (let ((hactar::*docs* nil)
        (hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (eval '(hactar::defdoc "My Feature Doc"
            :source "Feature content"
            :covers '("my-feature")))
    (eval '(hactar::defdoc "My Versioned Doc"
            :source "Versioned content"
            :version "1.1"))
    (let ((docs (hactar::get-all-registered-docs)))
      (is (= 2 (length docs)))
      (let ((doc1 (find "My Feature Doc" docs :key (lambda (d) (cdr (assoc :title d))) :test #'string=))
            (doc2 (find "My Versioned Doc" docs :key (lambda (d) (cdr (assoc :title d))) :test #'string=)))
        (is-true doc1)
        (is-true doc2)
        (is (equal '("my-feature") (cdr (assoc :covers doc1))))
        (is (equal '("1.1") (cdr (assoc :covers doc2))))))))

(test defdoc-macro-error-test
  "Test defdoc macro errors when both :version and :covers are missing."
  (signals error
    (macroexpand-1 '(hactar::defdoc "Invalid Doc" :source "No version or covers"))))

;;* Guide registry functions

(test register-guide-basic-test
  "Test registering a guide directly and through defguide macro."
  (let ((hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    ;; 1. Directly
    (hactar::register-guide "basic-guide"
                            :title "Basic Guide"
                            :content "Basic content"
                            :tags '("guide" "test")
                            :covers '("basic")
                            :sections '((:title "Sec 1" :content "Sec 1 content")))
    (let ((guide (hactar::get-guide "basic-guide")))
      (is-true guide)
      (is (string= "Basic Guide" (getf guide :title)))
      (is (string= "Basic content" (getf guide :content)))
      (is (equal '("guide" "test") (getf guide :tags)))
      (is (equal '("basic") (getf guide :covers)))
      (is (equal '((:title "Sec 1" :content "Sec 1 content")) (getf guide :sections))))

    ;; 2. Using defguide macro
    (eval '(hactar::defguide macro-guide
            :title "Macro Guide"
            :content "Macro content"
            :tags '("macro")
            :covers '("macro-cov")
            :sections '((:title "Sec A" :content "Sec A content"))))
    (let ((guide (hactar::get-guide "macro-guide")))
      (is-true guide)
      (is (string= "Macro Guide" (getf guide :title)))
      (is (string= "Macro content" (getf guide :content)))
      (is (equal '("macro") (getf guide :tags)))
      (is (equal '("macro-cov") (getf guide :covers)))
      (is (equal '((:title "Sec A" :content "Sec A content")) (getf guide :sections))))))

(test guide-activate-test
  "Test guide-activate adds the guide to the documentation context."
  (let ((hactar::*protocol-dispatchers* (make-hash-table :test 'equal))
        (hactar::*docs-context* nil)
        (hactar::*silent* t))
    (hactar::register-guide "active-guide"
                            :title "Active Guide"
                            :content "Some content")
    (let ((result (hactar::guide-activate "active-guide")))
      (is-true result)
      (is (= 1 (length hactar::*docs-context*)))
      (let ((doc (first hactar::*docs-context*)))
        (is (string= "Active Guide" (cdr (assoc :title doc))))
        (is (string= "Some content" (cdr (assoc :content doc))))))))

(test guide-add-section-test
  "Test guide-add-section appends sections correctly."
  (let ((hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::register-guide "section-guide"
                            :title "Section Guide"
                            :content "Initial content"
                            :sections '((:title "Sec 1" :content "Content 1")))
    (let ((updated-guide (hactar::guide-add-section "section-guide" "Sec 2" "Content 2")))
      (is-true updated-guide)
      (let ((sections (getf updated-guide :sections)))
        (is (= 2 (length sections)))
        (is (equal '(:title "Sec 2" :content "Content 2") (second sections))))

      ;; Verify it was persisted in the registry
      (let ((retrieved (hactar::get-guide "section-guide")))
        (is (= 2 (length (getf retrieved :sections))))))))

;;* Parser helper

(test lookup-parse-symbols-test
  "Test parsing symbol lookup args."
  (let ((result1 (hactar::%lookup-parse-symbols '("sym1,sym2")))
        (result2 (hactar::%lookup-parse-symbols '("sym1, sym2" "--format")))
        (result3 (hactar::%lookup-parse-symbols '("sym1" "--format=json"))))
    (is (equal '("sym1" "sym2") result1))
    (is (equal '("sym1" "sym2") result2))
    (is (equal '("sym1") result3))))

;;* Slash Commands

(test docs-context-command-test
  "Test the /docs-context command output."
  (let ((hactar::*docs-context* '(((:title . "API Guide") (:id . "api-123") (:source . "docs:api")))))
    (let ((output (with-output-to-string (*standard-output*)
                    (run-command-in-test "/docs-context" '()))))
      (is (search "Documentation in context:" output))
      (is (search "API Guide" output))
      (is (search "api-123" output))
      (is (search "docs:api" output)))))

(test docs-context-command-empty-test
  "Test the /docs-context command output when empty."
  (let ((hactar::*docs-context* nil)
        (output (with-output-to-string (*standard-output*)
                  (run-command-in-test "/docs-context" '()))))
    (is (search "No documentation in context." output))))

(test docs-add-to-context-command-test
  "Test /docs-add-to-context command adds by URI or Source."
  (let* ((doc '((:id . "doc-1") (:uri . "docs:my-doc") (:source . "my-source") (:title . "My Doc")))
         (hactar::*docs* (list doc))
         (hactar::*docs-context* nil)
         (hactar::*exposed-context-file* nil)
         (hactar::*silent* t))
    ;; Add by URI
    (run-command-in-test "/docs-add-to-context" '("docs:my-doc"))
    (is (= 1 (length hactar::*docs-context*)))
    (is (string= "doc-1" (cdr (assoc :id (first hactar::*docs-context*)))))

    ;; Clear and add by Source
    (setf hactar::*docs-context* nil)
    (run-command-in-test "/docs-add-to-context" '("my-source"))
    (is (= 1 (length hactar::*docs-context*)))
    (is (string= "doc-1" (cdr (assoc :id (first hactar::*docs-context*)))))

    ;; Try adding non-existent
    (let ((output (with-output-to-string (*standard-output*)
                    (run-command-in-test "/docs-add-to-context" '("non-existent")))))
      (is (search "No document found matching URI/Source:" output)))))

(test docs-command-test
  "Test the /docs command with mocked fzf selection."
  (let* ((doc '((:id . "doc-1") (:uri . "docs:my-doc") (:source . "my-source") (:title . "My Doc")))
         (hactar::*protocol-dispatchers* (make-hash-table :test 'equal))
         (hactar::*docs-context* nil)
         (hactar::*exposed-context-file* nil)
         (hactar::*silent* nil))
    (hactar::add-doc "My Doc" "content" :uri "docs:my-doc" :id "doc-1")
    (mockingbird:with-dynamic-stubs ((hactar::fuzzy-select-doc
                                      (lambda (results)
                                        (declare (ignore results))
                                        doc)))
      (let ((output (with-output-to-string (*standard-output*)
                      (run-command-in-test "/docs" '()))))
        (is (= 1 (length hactar::*docs-context*)))
        (is (search "Added doc to context: My Doc" output))))))

(test docs-command-includes-apidoc-test
  "Test /docs includes apidoc entries so selected API docs can be added to context."
  (let* ((doc '((:id . "sym-1") (:uri . "apidoc:sample-function") (:title . "sample-function (FUNCTION)")))
         (hactar::*protocol-dispatchers* (make-hash-table :test 'equal))
         (hactar::*docs-context* nil)
         (hactar::*exposed-context-file* nil)
         (hactar::*silent* nil))
    (eval '(hactar::defdocforsymbol sample-function "Docs for sample-function"))
    (mockingbird:with-dynamic-stubs ((hactar::fuzzy-select-doc
                                      (lambda (results)
                                        (is (= 1 (length results)))
                                        (is (string= "apidoc:sample-function"
                                                     (cdr (assoc :uri (first results)))))
                                        doc)))
      (let ((output (with-output-to-string (*standard-output*)
                      (run-command-in-test "/docs" '()))))
        (is (= 1 (length hactar::*docs-context*)))
        (is (string= "sym-1" (cdr (assoc :id (first hactar::*docs-context*)))))
        (is (search "Added doc to context: sample-function (FUNCTION)" output))))))

(test fuzzy-select-doc-returns-doc-from-preview-selection-test
  "Test fuzzy-select-doc returns the embedded doc when preview selection returns an alist."
  (let* ((doc '((:id . "doc-1") (:uri . "docs:my-doc") (:title . "My Doc")))
         (selected-item `((:item . "My Doc")
                          (:preview . "preview")
                          (:doc . ,doc))))
    (mockingbird:with-dynamic-stubs ((hactar::fuzzy-select
                                      (lambda (items)
                                        (is (= 1 (length items)))
                                        selected-item)))
      (is (equal doc (hactar::fuzzy-select-doc (list doc)))))))

(test guide-command-test
  "Test /guide command prints guide content or usage."
  (let ((hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::register-guide "test-guide" :title "Test Guide" :content "Guide content")

    ;; 1. Valid guide
    (let ((output (with-output-to-string (*standard-output*)
                    (run-command-in-test "/guide" '("test-guide")))))
      (is (search "--- Guide: Test Guide ---" output))
      (is (search "Guide content" output))
      (is (search "--- End of Guide ---" output)))

    ;; 2. Missing name (usage)
    (let ((output (with-output-to-string (*standard-output*)
                    (run-command-in-test "/guide" '()))))
      (is (search "Usage: /guide <name>" output)))

    ;; 3. Not found
    (let ((output (with-output-to-string (*standard-output*)
                    (run-command-in-test "/guide" '("non-existent-guide")))))
      (is (search "Guide 'non-existent-guide' not found." output)))))

(test guides-command-test
  "Test the /guides command with mocked fzf selection."
  (let* ((guide-alist '((:id . "guide-1") (:title . "My Guide")))
         (hactar::*protocol-dispatchers* (make-hash-table :test 'equal))
         (hactar::*docs-context* nil)
         (hactar::*exposed-context-file* nil)
         (hactar::*silent* t))
    (hactar::register-guide "my-guide" :title "My Guide")
    (mockingbird:with-dynamic-stubs ((hactar::fuzzy-select-doc
                                      (lambda (results)
                                        (declare (ignore results))
                                        guide-alist)))
      (run-command-in-test "/guides" '())
      (is (= 1 (length hactar::*docs-context*)))
      (is (string= "guide-1" (cdr (assoc :id (first hactar::*docs-context*))))))))

(test lookup-command-test
  "Test /lookup command fuzzy matches symbols and adds selected doc."
  (let* ((doc '((:id . "doc-1") (:title . "Test Function") (:covers . ("test-fun"))))
         (hactar::*protocol-dispatchers* (make-hash-table :test 'equal))
         (hactar::*docs-context* nil)
         (hactar::*exposed-context-file* nil)
         (hactar::*silent* t))
    (hactar::add-doc "Test Function" "content" :covers '("test-fun"))

    (mockingbird:with-dynamic-stubs ((hactar::fuzzy-select-doc
                                      (lambda (results)
                                        (declare (ignore results))
                                        doc)))
      (run-command-in-test "/lookup" '("test-fun"))
      (is (= 1 (length hactar::*docs-context*)))
      (is (string= "doc-1" (cdr (assoc :id (first hactar::*docs-context*))))))))
