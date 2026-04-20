(in-package :hactar-tests)

(def-suite lsp-tests :description "LSP server tests.")
(in-suite lsp-tests)

;;* Provider registry

(test lsp-provider-registration
  "Test that deflsp-provider registers a provider."
  (let ((hactar::*lsp-providers* (make-hash-table :test 'equal)))
    (setf (gethash "test-provider" hactar::*lsp-providers*)
          (hactar::make-lsp-provider
           :name "test-provider"
           :enabled t
           :function (lambda (source file-path)
                       (declare (ignore source file-path))
                       (list (hactar::make-lsp-diagnostic
                              :severity 2
                              :message "test warning")))
           :languages '("lisp")))
    (is (= 1 (hash-table-count hactar::*lsp-providers*)))
    (let ((p (gethash "test-provider" hactar::*lsp-providers*)))
      (is-true p)
      (is (string= "test-provider" (hactar::lsp-provider-name p)))
      (is-true (hactar::lsp-provider-enabled p))
      (is (equal '("lisp") (hactar::lsp-provider-languages p))))))

(test lsp-collect-diagnostics-filters-by-language
  "Test that collect-diagnostics filters providers by language ID."
  (let ((hactar::*lsp-providers* (make-hash-table :test 'equal)))
    (setf (gethash "lisp-only" hactar::*lsp-providers*)
          (hactar::make-lsp-provider
           :name "lisp-only"
           :enabled t
           :function (lambda (source file-path)
                       (declare (ignore source file-path))
                       (list (hactar::make-lsp-diagnostic :message "lisp diag")))
           :languages '("lisp")))
    (setf (gethash "all-langs" hactar::*lsp-providers*)
          (hactar::make-lsp-provider
           :name "all-langs"
           :enabled t
           :function (lambda (source file-path)
                       (declare (ignore source file-path))
                       (list (hactar::make-lsp-diagnostic :message "all diag")))
           :languages nil))
    ;; lisp file should get both
    (let ((diags (hactar::lsp-collect-diagnostics "" "test.lisp" "lisp")))
      (is (= 2 (length diags))))
    ;; js file should get only the all-langs one
    (let ((diags (hactar::lsp-collect-diagnostics "" "test.js" "javascript")))
      (is (= 1 (length diags)))
      (is (string= "all diag" (hactar::lsp-diagnostic-message (first diags)))))))

(test lsp-collect-diagnostics-skips-disabled
  "Test that disabled providers are skipped."
  (let ((hactar::*lsp-providers* (make-hash-table :test 'equal)))
    (setf (gethash "disabled" hactar::*lsp-providers*)
          (hactar::make-lsp-provider
           :name "disabled"
           :enabled nil
           :function (lambda (source file-path)
                       (declare (ignore source file-path))
                       (list (hactar::make-lsp-diagnostic :message "should not appear")))))
    (let ((diags (hactar::lsp-collect-diagnostics "" "test.lisp" "lisp")))
      (is (= 0 (length diags))))))

;;* Diagnostic conversion

(test lsp-diagnostic-to-alist-structure
  "Test diagnostic to LSP alist conversion."
  (let* ((d (hactar::make-lsp-diagnostic
             :range-start-line 5
             :range-start-char 2
             :range-end-line 5
             :range-end-char 10
             :severity 2
             :source "hactar-checker"
             :message "test message"
             :code "test-code"))
         (a (hactar::lsp-diagnostic-to-alist d)))
    (let* ((range (cdr (assoc "range" a :test #'string=)))
           (start (cdr (assoc "start" range :test #'string=)))
           (end (cdr (assoc "end" range :test #'string=))))
      (is (= 5 (cdr (assoc "line" start :test #'string=))))
      (is (= 2 (cdr (assoc "character" start :test #'string=))))
      (is (= 5 (cdr (assoc "line" end :test #'string=))))
      (is (= 10 (cdr (assoc "character" end :test #'string=)))))
    (is (= 2 (cdr (assoc "severity" a :test #'string=))))
    (is (string= "hactar-checker" (cdr (assoc "source" a :test #'string=))))
    (is (string= "test message" (cdr (assoc "message" a :test #'string=))))
    (is (string= "test-code" (cdr (assoc "code" a :test #'string=))))))

(test lsp-diagnostic-to-alist-no-code
  "Test diagnostic without code omits code field."
  (let* ((d (hactar::make-lsp-diagnostic :message "no code" :code nil))
         (a (hactar::lsp-diagnostic-to-alist d)))
    (is (null (assoc "code" a :test #'string=)))))

;;* URI helpers

(test lsp-uri-to-path-basic
  "Test file URI to path conversion."
  (is (string= "/home/user/test.lisp"
               (hactar::lsp-uri-to-path "file:///home/user/test.lisp"))))

(test lsp-uri-to-path-spaces
  "Test URI with %20 encoding."
  (is (string= "/home/user/my file.lisp"
               (hactar::lsp-uri-to-path "file:///home/user/my%20file.lisp"))))

(test lsp-path-to-uri-basic
  "Test path to URI conversion."
  (is (string= "file:///home/user/test.lisp"
               (hactar::lsp-path-to-uri "/home/user/test.lisp"))))

;;* Language detection

(test lsp-detect-language-lisp
  "Detect lisp language from URI."
  (is (string= "lisp" (hactar::lsp-detect-language-id "file:///test.lisp"))))

(test lsp-detect-language-tsx
  "Detect typescriptreact from .tsx URI."
  (is (string= "typescriptreact" (hactar::lsp-detect-language-id "file:///app.tsx"))))

(test lsp-detect-language-js
  "Detect javascript from .js URI."
  (is (string= "javascript" (hactar::lsp-detect-language-id "file:///index.js"))))

(test lsp-detect-language-unknown
  "Unknown extension returns plaintext."
  (is (string= "plaintext" (hactar::lsp-detect-language-id "file:///readme.xyz"))))

;;* Checker provider integration

(test lsp-checker-provider-finds-issues
  "Test the built-in checker provider returns diagnostics for bad code."
  (let ((source "(target :redwood) (a :class \"x\" \"Click\")"))
    (let ((diags (hactar::lsp-collect-diagnostics source "test.tsx" "typescriptreact")))
      (is (>= (length diags) 1))
      (is (= 2 (hactar::lsp-diagnostic-severity (first diags))))
      (is (search "href" (hactar::lsp-diagnostic-message (first diags)))))))

(test lsp-checker-provider-clean-code
  "Test the checker provider returns no diagnostics for clean code."
  (let ((source "(target :redwood) (a :href \"/about\" \"About\")"))
    (let ((diags (hactar::lsp-collect-diagnostics source "test.tsx" "typescriptreact")))
      (is (= 0 (length diags))))))

;;* Initialize handler

(test lsp-handle-initialize-response
  "Test initialize returns capabilities."
  (let ((hactar::*lsp-initialized* nil)
        (hactar::*lsp-client-capabilities* nil)
        (output-json nil))
    (mockingbird:with-dynamic-stubs ((hactar::lsp-write-message
                                      (lambda (json-str) (setf output-json json-str))))
      (hactar::lsp-handle-initialize
       42
       '(("capabilities" . (("textDocument" . t)))))
      (is-true hactar::*lsp-initialized*)
      (is-true output-json)
      (let* ((shasht:*read-default-object-format* :alist)
             (parsed (shasht:read-json output-json)))
        (is (= 42 (cdr (assoc "id" parsed :test #'string=))))
        (let ((result (cdr (assoc "result" parsed :test #'string=))))
          (is-true (assoc "capabilities" result :test #'string=))
          (let ((server-info (cdr (assoc "serverInfo" result :test #'string=))))
            (is (string= "hactar-lsp" (cdr (assoc "name" server-info :test #'string=))))))))))

;;* Document lifecycle

(test lsp-did-open-stores-document
  "Test didOpen stores document and publishes diagnostics."
  (let ((hactar::*lsp-documents* (make-hash-table :test 'equal))
        (published-uri nil))
    (mockingbird:with-dynamic-stubs ((hactar::lsp-publish-diagnostics
                                      (lambda (uri) (setf published-uri uri))))
      (hactar::lsp-handle-did-open
       `(("textDocument" . (("uri" . "file:///test.lisp")
                            ("languageId" . "lisp")
                            ("version" . 1)
                            ("text" . "(defvar x 1)")))))
      (let ((doc (gethash "file:///test.lisp" hactar::*lsp-documents*)))
        (is-true doc)
        (is (string= "(defvar x 1)" (cdr (assoc :source doc))))
        (is (string= "lisp" (cdr (assoc :language-id doc)))))
      (is (string= "file:///test.lisp" published-uri)))))

(test lsp-did-close-removes-document
  "Test didClose removes document and clears diagnostics."
  (let ((hactar::*lsp-documents* (make-hash-table :test 'equal))
        (notifications nil))
    (setf (gethash "file:///test.lisp" hactar::*lsp-documents*)
          '((:source . "(defvar x 1)") (:language-id . "lisp")))
    (mockingbird:with-dynamic-stubs ((hactar::lsp-notify
                                      (lambda (method params)
                                        (push (cons method params) notifications))))
      (hactar::lsp-handle-did-close
       `(("textDocument" . (("uri" . "file:///test.lisp")))))
      (is (null (gethash "file:///test.lisp" hactar::*lsp-documents*)))
      (is (= 1 (length notifications)))
      (is (string= "textDocument/publishDiagnostics" (car (first notifications)))))))

;;* Shutdown

(test lsp-handle-shutdown
  "Test shutdown sets flag and responds."
  (let ((hactar::*lsp-shutdown-requested* nil)
        (responded nil))
    (mockingbird:with-dynamic-stubs ((hactar::lsp-respond
                                      (lambda (id result)
                                        (setf responded (cons id result)))))
      (hactar::lsp-handle-shutdown 99)
      (is-true hactar::*lsp-shutdown-requested*)
      (is (= 99 (car responded))))))
