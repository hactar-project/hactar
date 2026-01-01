(in-package :hactar-tests)

(def-suite generator-tests
  :description "Tests for Hactar Generate scaffolding system.")

(in-suite generator-tests)

(test defgenerator-macro-test
  "Test that defgenerator correctly creates and registers a generator."
  ;; Clear existing generators first
  (clrhash hactar::*generators*)
  
  (eval '(hactar::defgenerator test-component
           "Generate a test component"
           :args (name &key props style)
           :infer-from-context
           ((:style-system "detect styling"))
           :operations
           ((:create :file "test.tsx"))))
  
  (let ((gen (first (gethash "test-component" hactar::*generators*))))
    (is-true gen)
    (is (string= (hactar::generator-description gen) "Generate a test component"))
    (is (equal (hactar::generator-args gen) '(name &key props style)))
    (is-true (hactar::generator-infer-from-context gen))
    (is-true (hactar::generator-operations gen))))

(test defpattern-macro-test
  "Test that defpattern correctly creates and registers a pattern."
  ;; Clear existing patterns first
  (clrhash hactar::*patterns*)
  
  (eval '(hactar::defpattern test-pattern
           "A test pattern"
           :requires (:react)
           :package "test-pkg"
           :config-schema
           (:limit (:type :number :default 100))))
  
  (let ((pat (first (gethash "test-pattern" hactar::*patterns*))))
    (is-true pat)
    (is (string= (hactar::pattern-description pat) "A test pattern"))
    (is (equal (hactar::pattern-requires pat) '(:react)))
    (is (string= (hactar::pattern-package pat) "test-pkg"))
    (is-true (hactar::pattern-config-schema pat))))

(test defregistry-macro-test
  "Test that defregistry correctly creates and registers a registry type."
  ;; Clear existing registries first
  (clrhash hactar::*registries*)
  
  (eval '(hactar::defregistry :test-registry
           "A test registry"
           :file-pattern "*/index.ts"
           :entry-pattern "export * from './{name}';"
           :detect-from-framework t
           :variants ((:nextjs :file "app/routes.ts"))))
  
  (let ((reg (gethash :test-registry hactar::*registries*)))
    (is-true reg)
    (is (string= (hactar::registry-description reg) "A test registry"))
    (is (string= (hactar::registry-file-pattern reg) "*/index.ts"))
    (is (string= (hactar::registry-entry-pattern reg) "export * from './{name}';"))
    (is-true (hactar::registry-detect-from-framework-p reg))
    (is-true (hactar::registry-variants reg))))

;;* Context Inference

(test infer-framework-test
  "Test framework inference from stack."
  (let ((hactar::*stack* '("react" "typescript")))
    (is (eq (hactar::infer-framework) :react)))
  
  (let ((hactar::*stack* '("vue" "vite")))
    (is (eq (hactar::infer-framework) :vue)))
  
  (let ((hactar::*stack* '("express" "node")))
    (is (eq (hactar::infer-framework) :express)))
  
  (let ((hactar::*stack* '("react")))
    (is (eq (hactar::infer-framework) :react)))
  
  (let ((hactar::*stack* '("nextjs" "react")))
    (is (eq (hactar::infer-framework) :nextjs)))
  
  (let ((hactar::*stack* '("nextjs")))
    (is (eq (hactar::infer-framework) :nextjs)))
  
  (let ((hactar::*stack* '()))
    (is (null (hactar::infer-framework)))))

(test infer-style-system-test
  "Test style system inference."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp)))
    
    (let ((hactar::*stack* '("tailwind" "react")))
      (is (eq (hactar::infer-style-system) :tailwind)))
    
    (let ((hactar::*stack* '("styled-components" "react")))
      (is (eq (hactar::infer-style-system) :styled-components)))
    
    (let ((hactar::*stack* '("css-modules" "react")))
      (is (eq (hactar::infer-style-system) :css-modules)))
    
    (let ((hactar::*stack* '()))
      (with-open-file (s (merge-pathnames "tailwind.config.js" hactar::*repo-root*)
                         :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format s "module.exports = {}"))
      (is (eq (hactar::infer-style-system) :tailwind)))))

(test infer-test-framework-test
  "Test testing framework inference."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp)))
    
    (let ((hactar::*stack* '("vitest" "react")))
      (is (eq (hactar::infer-test-framework) :vitest)))
    
    (let ((hactar::*stack* '("jest" "react")))
      (is (eq (hactar::infer-test-framework) :jest)))
    
    (let ((hactar::*stack* '("mocha" "node")))
      (is (eq (hactar::infer-test-framework) :mocha)))
    
    (let ((hactar::*stack* '()))
      (is (null (hactar::infer-test-framework))))))

(test infer-package-manager-test
  "Test package manager inference from lock files."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp)))
    (with-open-file (s (merge-pathnames "bun.lockb" hactar::*repo-root*)
                       :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s ""))
    (is (eq (hactar::infer-package-manager) :bun))
    (delete-file (merge-pathnames "bun.lockb" hactar::*repo-root*))
    
    (with-open-file (s (merge-pathnames "pnpm-lock.yaml" hactar::*repo-root*)
                       :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s ""))
    (is (eq (hactar::infer-package-manager) :pnpm))
    (delete-file (merge-pathnames "pnpm-lock.yaml" hactar::*repo-root*))
    
    (with-open-file (s (merge-pathnames "yarn.lock" hactar::*repo-root*)
                       :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s ""))
    (is (eq (hactar::infer-package-manager) :yarn))
    (delete-file (merge-pathnames "yarn.lock" hactar::*repo-root*))
    
    (is (eq (hactar::infer-package-manager) :npm))))

(test infer-context-value-test
  "Test the unified context value inference function."
  (let ((hactar::*stack* '("react" "vitest" "tailwind"))
        (hactar::*language* "typescript"))
    (is (eq (hactar::infer-context-value :framework) :react))
    (is (eq (hactar::infer-context-value :style-system) :tailwind))
    (is (eq (hactar::infer-context-value :test-framework) :vitest))
    (is (string= (hactar::infer-context-value :language) "typescript"))
    (is (null (hactar::infer-context-value :unknown-key)))))

;;* Gen Operation Structs

(test gen-operation-struct-test
  "Test creating gen-operation structs."
  (let ((op (hactar::make-gen-operation
             :type :create
             :file "src/test.ts"
             :content "console.log('hello');")))
    (is (eq (hactar::gen-operation-type op) :create))
    (is (string= (hactar::gen-operation-file op) "src/test.ts"))
    (is (string= (hactar::gen-operation-content op) "console.log('hello');"))
    (is (null (hactar::gen-operation-executed-p op)))))

(test gen-result-struct-test
  "Test creating gen-result structs."
  (let ((result (hactar::make-gen-result
                 :success-p t
                 :files-created '("a.ts" "b.ts")
                 :message "Success")))
    (is-true (hactar::gen-result-success-p result))
    (is (equal (hactar::gen-result-files-created result) '("a.ts" "b.ts")))
    (is (string= (hactar::gen-result-message result) "Success"))))

;;* Plan Parsing

(test parse-plan-response-create-test
  "Test parsing CREATE operations from LLM response."
  (let* ((response "OPERATION: CREATE
FILE: src/components/Button.tsx
CONTENT:
import React from 'react';

export function Button() {
  return <button>Click me</button>;
}
---")
         (ops (hactar::parse-plan-response response)))
    (is (= 1 (length ops)))
    (let ((op (first ops)))
      (is (eq (hactar::gen-operation-type op) :create))
      (is (string= (hactar::gen-operation-file op) "src/components/Button.tsx"))
      (is (search "import React" (hactar::gen-operation-content op)))
      (is (search "Button" (hactar::gen-operation-content op))))))

(test parse-plan-response-modify-test
  "Test parsing MODIFY operations from LLM response."
  (let* ((response "OPERATION: MODIFY
FILE: src/app.ts
SEARCH: const old = 1;
REPLACE: const new = 2;
---")
         (ops (hactar::parse-plan-response response)))
    (is (= 1 (length ops)))
    (let ((op (first ops)))
      (is (eq (hactar::gen-operation-type op) :modify))
      (is (string= (hactar::gen-operation-file op) "src/app.ts"))
      (is (string= (hactar::gen-operation-search op) "const old = 1;"))
      (is (string= (hactar::gen-operation-replace op) "const new = 2;")))))

(test parse-plan-response-install-test
  "Test parsing INSTALL operations from LLM response."
  (let* ((response "OPERATION: INSTALL
PACKAGE: lodash
---")
         (ops (hactar::parse-plan-response response)))
    (is (= 1 (length ops)))
    (let ((op (first ops)))
      (is (eq (hactar::gen-operation-type op) :install))
      (is (string= (hactar::gen-operation-package op) "lodash")))))

(test parse-plan-response-run-test
  "Test parsing RUN operations from LLM response."
  (let* ((response "OPERATION: RUN
COMMAND: npm run build
---")
         (ops (hactar::parse-plan-response response)))
    (is (= 1 (length ops)))
    (let ((op (first ops)))
      (is (eq (hactar::gen-operation-type op) :run))
      (is (string= (hactar::gen-operation-command op) "npm run build")))))

(test parse-plan-response-multiple-ops-test
  "Test parsing multiple operations from LLM response."
  (let* ((response "OPERATION: INSTALL
PACKAGE: react
---
OPERATION: CREATE
FILE: src/index.tsx
CONTENT:
import React from 'react';
---
OPERATION: RUN
COMMAND: npm run dev
---")
         (ops (hactar::parse-plan-response response)))
    (is (= 3 (length ops)))
    (is (eq (hactar::gen-operation-type (first ops)) :install))
    (is (eq (hactar::gen-operation-type (second ops)) :create))
    (is (eq (hactar::gen-operation-type (third ops)) :run))))

(test parse-plan-response-no-trailing-separator-test
  "Test parsing when there's no trailing --- separator."
  (let* ((response "OPERATION: CREATE
FILE: test.txt
CONTENT:
hello world")
         (ops (hactar::parse-plan-response response)))
    (is (= 1 (length ops)))
    (is (string= (hactar::gen-operation-content (first ops)) "hello world"))))

;;* Operation Execution

(test execute-create-op-dry-run-test
  "Test dry-run mode for create operation."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (op (hactar::make-gen-operation
              :type :create
              :file "test/new-file.ts"
              :content "content"))
         (output (make-string-output-stream))
         (*standard-output* output))
    (is-true (hactar::execute-create-op op :dry-run t))
    (let ((out (get-output-stream-string output)))
      (is (search "[CREATE]" out))
      (is (search "test/new-file.ts" out)))))

(test execute-modify-op-dry-run-test
  "Test dry-run mode for modify operation."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (test-file (merge-pathnames "test.ts" hactar::*repo-root*)))
    (with-open-file (s test-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "const old = 1;"))
    
    (let* ((op (hactar::make-gen-operation
                :type :modify
                :file "test.ts"
                :search "const old = 1;"
                :replace "const new = 2;"))
           (output (make-string-output-stream))
           (*standard-output* output))
      (is-true (hactar::execute-modify-op op :dry-run t))
      (let ((out (get-output-stream-string output)))
        (is (search "[MODIFY]" out))
        (is (search "test.ts" out))))))

(test execute-delete-op-dry-run-test
  "Test dry-run mode for delete operation."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (test-file (merge-pathnames "delete-me.ts" hactar::*repo-root*)))
    (with-open-file (s test-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "content"))
    
    (let* ((op (hactar::make-gen-operation
                :type :delete
                :file "delete-me.ts"))
           (output (make-string-output-stream))
           (*standard-output* output))
      (is-true (hactar::execute-delete-op op :dry-run t))
      (let ((out (get-output-stream-string output)))
        (is (search "[DELETE]" out))))))

(test execute-rename-op-dry-run-test
  "Test dry-run mode for rename operation."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (source-file (merge-pathnames "old-name.ts" hactar::*repo-root*)))
    (with-open-file (s source-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "content"))
    
    (let* ((op (hactar::make-gen-operation
                :type :rename
                :source "old-name.ts"
                :target "new-name.ts"))
           (output (make-string-output-stream))
           (*standard-output* output))
      (is-true (hactar::execute-rename-op op :dry-run t))
      (let ((out (get-output-stream-string output)))
        (is (search "[RENAME]" out))
        (is (search "old-name.ts" out))
        (is (search "new-name.ts" out))))))

(test execute-install-op-dry-run-test
  "Test dry-run mode for install operation."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (op (hactar::make-gen-operation
              :type :install
              :package "lodash"))
         (output (make-string-output-stream))
         (*standard-output* output))
    (is-true (hactar::execute-install-op op :dry-run t))
    (let ((out (get-output-stream-string output)))
      (is (search "[INSTALL]" out))
      (is (search "lodash" out)))))

(test execute-run-op-dry-run-test
  "Test dry-run mode for run operation."
  (let* ((op (hactar::make-gen-operation
              :type :run
              :command "echo hello"))
         (output (make-string-output-stream))
         (*standard-output* output))
    (is-true (hactar::execute-run-op op :dry-run t))
    (let ((out (get-output-stream-string output)))
      (is (search "[RUN]" out))
      (is (search "echo hello" out)))))

;;** Actual Operations

(test execute-create-op-actual-test
  "Test actual file creation."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (op (hactar::make-gen-operation
              :type :create
              :file "new-file.ts"
              :content "const x = 1;")))
    (is-true (hactar::execute-create-op op :dry-run nil))
    (let ((created-file (merge-pathnames "new-file.ts" hactar::*repo-root*)))
      (is-true (probe-file created-file))
      (is (string= (uiop:read-file-string created-file) "const x = 1;")))))

(test execute-modify-op-actual-test
  "Test actual file modification."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (test-file (merge-pathnames "modify-me.ts" hactar::*repo-root*)))
    (with-open-file (s test-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "const old = 1;"))
    
    (let ((op (hactar::make-gen-operation
               :type :modify
               :file "modify-me.ts"
               :search "const old = 1;"
               :replace "const new = 2;")))
      (is-true (hactar::execute-modify-op op :dry-run nil))
      (is (string= (uiop:read-file-string test-file) "const new = 2;")))))

(test execute-delete-op-actual-test
  "Test actual file deletion."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (test-file (merge-pathnames "delete-me.ts" hactar::*repo-root*)))
    (with-open-file (s test-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "content"))
    (is-true (probe-file test-file))
    
    (let ((op (hactar::make-gen-operation
               :type :delete
               :file "delete-me.ts")))
      (is-true (hactar::execute-delete-op op :dry-run nil))
      (is (null (probe-file test-file))))))

(test execute-rename-op-actual-test
  "Test actual file renaming."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (source-file (merge-pathnames "old.ts" hactar::*repo-root*))
         (target-file (merge-pathnames "new.ts" hactar::*repo-root*)))
    (with-open-file (s source-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "content"))
    
    (let ((op (hactar::make-gen-operation
               :type :rename
               :source "old.ts"
               :target "new.ts")))
      (is-true (hactar::execute-rename-op op :dry-run nil))
      (is (null (probe-file source-file)))
      (is-true (probe-file target-file))
      (is (string= (uiop:read-file-string target-file) "content")))))

;;* Gen Undo

(test gen-undo-empty-history-test
  "Test gen-undo with empty history."
  (let* ((hactar::*gen-history* '())
         (output (make-string-output-stream))
         (*standard-output* output))
    (is (null (hactar::gen-undo)))
    (is (search "No generation history" (get-output-stream-string output)))))

(test gen-undo-create-operation-test
  "Test undoing a create operation."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (test-file (merge-pathnames "undo-test.ts" hactar::*repo-root*)))
    
    (let ((create-op (hactar::make-gen-operation
                      :type :create
                      :file "undo-test.ts"
                      :content "test content")))
      (hactar::execute-create-op create-op :dry-run nil)
      (is-true (probe-file test-file))
      
      (setf hactar::*gen-history* 
            (list (list :operations (list create-op) :timestamp (get-universal-time))))
      
      (is-true (hactar::gen-undo))
      (is (null (probe-file test-file))))))

;;* Execute Gen Plan

(test execute-gen-plan-dry-run-test
  "Test executing a plan in dry-run mode."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (ops (list (hactar::make-gen-operation
                     :type :create
                     :file "plan-test.ts"
                     :content "content")))
         (result (hactar::execute-gen-plan ops :dry-run t)))
    (is-true (hactar::gen-result-success-p result))
    (is (null (probe-file (merge-pathnames "plan-test.ts" hactar::*repo-root*))))))

(test execute-gen-plan-actual-test
  "Test executing a plan with actual changes."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (ops (list (hactar::make-gen-operation
                     :type :create
                     :file "plan-actual.ts"
                     :content "const x = 1;")))
         (hactar::*gen-history* '())
         (result (hactar::execute-gen-plan ops :dry-run nil)))
    (is-true (hactar::gen-result-success-p result))
    (is (member "plan-actual.ts" (hactar::gen-result-files-created result) :test #'string=))
    (is-true (probe-file (merge-pathnames "plan-actual.ts" hactar::*repo-root*)))
    (is (= 1 (length hactar::*gen-history*)))))

;;* Built-in Generators and Patterns

(test builtin-generators-exist-test
  "Test that built-in generators are registered."
  (eval '(hactar::defgenerator component
           "Generate a React/Vue/Angular component with TypeScript"
           :args (name &key props style test)))
  (eval '(hactar::defgenerator route
           "Generate an API route handler"
           :args (path &key method handler middleware)))
  
  (is-true (gethash "component" hactar::*generators*))
  (is-true (gethash "route" hactar::*generators*)))

(test builtin-patterns-exist-test
  "Test that built-in patterns are registered."
  (eval '(hactar::defpattern rate-limiting
           "Add rate limiting to API endpoints"
           :detect-framework t
           :variants
           ((:express (:package "express-rate-limit"))
            (:fastify (:package "@fastify/rate-limit"))
            (:hono (:package "hono-rate-limiter")))))
  
  (is-true (gethash "rate-limiting" hactar::*patterns*)))

(test getf-string-test
  "Test getf-string helper for parsing command args."
  (let ((args '("--to" "src/*.ts" "--config" "custom")))
    (is (string= (hactar::getf-string args "--to") "src/*.ts"))
    (is (string= (hactar::getf-string args "--config") "custom"))
    (is (null (hactar::getf-string args "--nonexistent")))))

;;* Error Handling

(test execute-modify-op-file-not-found-test
  "Test modify operation when file doesn't exist."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (op (hactar::make-gen-operation
              :type :modify
              :file "nonexistent.ts"
              :search "old"
              :replace "new"))
         (output (make-string-output-stream))
         (*standard-output* output))
    (is (null (hactar::execute-modify-op op :dry-run nil)))
    (is (search "File not found" (get-output-stream-string output)))))

(test execute-modify-op-search-not-found-test
  "Test modify operation when search text doesn't exist."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (test-file (merge-pathnames "search-fail.ts" hactar::*repo-root*)))
    (with-open-file (s test-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "const different = 1;"))
    
    (let* ((op (hactar::make-gen-operation
                :type :modify
                :file "search-fail.ts"
                :search "const old = 1;"
                :replace "const new = 2;"))
           (output (make-string-output-stream))
           (*standard-output* output))
      (is (null (hactar::execute-modify-op op :dry-run nil)))
      (is (search "Search block not found" (get-output-stream-string output))))))

(test execute-delete-op-file-not-found-test
  "Test delete operation when file doesn't exist."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (op (hactar::make-gen-operation
              :type :delete
              :file "nonexistent.ts"))
         (output (make-string-output-stream))
         (*standard-output* output))
    (is (null (hactar::execute-delete-op op :dry-run nil)))
    (is (search "File not found" (get-output-stream-string output)))))

(test execute-rename-op-source-not-found-test
  "Test rename operation when source doesn't exist."
  (let* ((tmp (make-temp-dir))
         (hactar::*repo-root* (uiop:parse-native-namestring tmp))
         (op (hactar::make-gen-operation
              :type :rename
              :source "nonexistent.ts"
              :target "new.ts"))
         (output (make-string-output-stream))
         (*standard-output* output))
    (is (null (hactar::execute-rename-op op :dry-run nil)))
    (is (search "Source not found" (get-output-stream-string output)))))

;;* Gen History Limit

(test gen-history-limit-test
  "Test that gen-history respects the limit."
  (let ((hactar::*gen-history* '())
        (hactar::*gen-history-limit* 3))
    (dotimes (i 5)
      (push (list :operations '() :timestamp i) hactar::*gen-history*))
    
    (is (= 5 (length hactar::*gen-history*)))
    
    (let* ((tmp (make-temp-dir))
           (hactar::*repo-root* (uiop:parse-native-namestring tmp)))
      (hactar::execute-gen-plan 
       (list (hactar::make-gen-operation :type :create :file "test.ts" :content "x"))
       :dry-run nil))
    
    (is (<= (length hactar::*gen-history*) (1+ hactar::*gen-history-limit*)))))
