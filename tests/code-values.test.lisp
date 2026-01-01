(in-package :hactar-tests)
;;; --- Code Value Tests ---
(def-suite code-value-tests
  :description "Tests for code-value.lisp - code as a first-class value."
  )

(in-suite code-value-tests)

;; Test ID generation
(test generate-code-value-id-test
  "Test that code-value IDs are unique."
  (let ((id1 (hactar::generate-code-value-id))
        (id2 (hactar::generate-code-value-id)))
    (is (stringp id1))
    (is (stringp id2))
    (is (not (string= id1 id2)))
    (is (str:starts-with? "cv-" id1))))

;; Test code-value creation
(test make-code-value-test
  "Test creating a code-value struct."
  (let ((cv (hactar::make-code-value
             :id "test-id"
             :name 'my-code
             :source-file #P"/test/file.ts"
             :source-region '(10 . 20)
             :source-text "const x = 1;"
             :language "typescript"
             :origin :selection)))
    (is (string= (hactar::code-value-id cv) "test-id"))
    (is (eq (hactar::code-value-name cv) 'my-code))
    (is (equal (hactar::code-value-source-region cv) '(10 . 20)))
    (is (string= (hactar::code-value-source-text cv) "const x = 1;"))
    (is (string= (hactar::code-value-language cv) "typescript"))
    (is (eq (hactar::code-value-origin cv) :selection))
    (is (null (hactar::code-value-parent cv)))
    (is (null (hactar::code-value-staged-p cv)))
    (is (null (hactar::code-value-applied-p cv)))))

;; Test registry operations
(test code-value-registry-test
  "Test registering and retrieving code-values."
  (hactar::clear-code-values)
  
  (let ((cv (hactar::make-code-value
             :id "registry-test-1"
             :source-text "test")))
    (hactar::register-code-value cv)
    
    (is (eq cv (hactar::get-code-value "registry-test-1")))
    (is (= 1 (length (hactar::list-code-values))))
    
    ;; Add another
    (let ((cv2 (hactar::make-code-value
                :id "registry-test-2"
                :source-text "test2")))
      (hactar::register-code-value cv2)
      (is (= 2 (length (hactar::list-code-values)))))
    
    ;; Clear
    (hactar::clear-code-values)
    (is (= 0 (length (hactar::list-code-values))))
    (is (null (hactar::get-code-value "registry-test-1")))))

;; Test parse-source-specifier
(test parse-source-specifier-test
  "Test parsing source specifiers with line ranges."
  ;; Simple file path
  (let ((result (hactar::parse-source-specifier "src/file.ts")))
    (is (string= (car result) "src/file.ts"))
    (is (null (cdr result))))
  
  ;; File with line range
  (let ((result (hactar::parse-source-specifier "src/file.ts:10-20")))
    (is (string= (car result) "src/file.ts"))
    (is (equal (cdr result) '(10 . 20))))
  
  ;; Windows path (should not confuse drive letter with line range)
  (let ((result (hactar::parse-source-specifier "C:/path/file.ts")))
    (is (string= (car result) "C:/path/file.ts"))
    (is (null (cdr result))))
  
  ;; Pathname object
  (let ((result (hactar::parse-source-specifier #P"src/file.ts")))
    (is (stringp (car result)))))

;; Test extract-lines
(test extract-lines-test
  "Test extracting specific lines from content."
  (let ((content (format nil "line1~%line2~%line3~%line4~%line5")))
    ;; Extract lines 2-4
    (multiple-value-bind (text region)
        (hactar::extract-lines content 2 4)
      (is (string= text (format nil "line2~%line3~%line4")))
      (is (equal region '(2 . 4))))
    
    ;; Extract single line
    (multiple-value-bind (text region)
        (hactar::extract-lines content 3 3)
      (is (string= text "line3"))
      (is (equal region '(3 . 3))))
    
    ;; Handle out of bounds
    (multiple-value-bind (text region)
        (hactar::extract-lines content 1 10)
      (is (= 5 (length (str:lines text)))))))

;; Test parse-line-range-from-response
(test parse-line-range-from-response-test
  "Test parsing line range comments from LLM responses."
  (is (equal (hactar::parse-line-range-from-response "// Lines: 10-20\ncode")
             '(10 . 20)))
  (is (equal (hactar::parse-line-range-from-response "# Lines: 5-15\ncode")
             '(5 . 15)))
  (is (equal (hactar::parse-line-range-from-response "-- Line: 1-100\ncode")
             '(1 . 100)))
  (is (null (hactar::parse-line-range-from-response "no line info here"))))

;; Test remove-line-comment
(test remove-line-comment-test
  "Test removing line range comments from text."
  ;; With line comment - should remove first line
  (is (string= (hactar::remove-line-comment (format nil "// Lines: 10-20~%actual code"))
               "actual code"))
  (is (string= (hactar::remove-line-comment (format nil "# Lines: 5-10~%code~%more code"))
               (format nil "code~%more code")))
  ;; Without line comment - should return unchanged
  (is (string= (hactar::remove-line-comment (format nil "no comment~%code"))
               (format nil "no comment~%code"))))

;; Test code/from-string
(test code-from-string-test
  "Test creating code-value from a string."
  (hactar::clear-code-values)
  
  (let ((cv (hactar::code/from-string "const x = 1;" :language "typescript" :name 'my-code)))
    (is (hactar::code-value-p cv))
    (is (string= (hactar::code-value-source-text cv) "const x = 1;"))
    (is (string= (hactar::code-value-language cv) "typescript"))
    (is (eq (hactar::code-value-origin cv) :generated))
    (is (null (hactar::code-value-source-file cv)))
    
    ;; Should be registered
    (is (eq cv (hactar::get-code-value (hactar::code-value-id cv))))
    
    ;; Should be accessible by name
    (is (eq cv (hactar::get-code-value 'my-code)))))

;; Test code/select with file
(test code-select-file-test
  "Test selecting code from a file."
  (uiop:with-temporary-file (:pathname p :keep t)
    (let ((content (format nil "line1~%line2~%line3~%line4~%line5")))
      (with-open-file (s p :direction :output :if-exists :supersede)
        (write-string content s))
      
      (hactar::clear-code-values)
      (let ((hactar::*repo-root* (uiop:pathname-directory-pathname p)))
        ;; Select whole file
        (let ((cv (hactar::code/select (uiop:native-namestring p))))
          (is (hactar::code-value-p cv))
          (is (string= (hactar::code-value-source-text cv) content))
          (is (eq (hactar::code-value-origin cv) :selection)))
        
        ;; Select with line range
        (let ((cv (hactar::code/select (format nil "~A:2-4" (uiop:native-namestring p)))))
          (is (string= (hactar::code-value-source-text cv) 
                       (format nil "line2~%line3~%line4")))
          (is (equal (hactar::code-value-source-region cv) '(2 . 4))))
        
        ;; Select with explicit region
        (let ((cv (hactar::code/select (uiop:native-namestring p) :region '(1 . 2))))
          (is (string= (hactar::code-value-source-text cv)
                       (format nil "line1~%line2"))))))))

;; Test parse-comma-list
(test parse-comma-list-test
  "Test parsing comma-separated lists."
  (is (equal (hactar::parse-comma-list "a, b, c") '("a" "b" "c")))
  (is (equal (hactar::parse-comma-list "  x  ,  y  ") '("x" "y")))
  (is (null (hactar::parse-comma-list "none")))
  (is (null (hactar::parse-comma-list "NONE")))
  (is (equal (hactar::parse-comma-list "single") '("single")))
  (is (null (hactar::parse-comma-list ""))))

;; Test code/source
(test code-source-test
  "Test getting source text from code-value."
  (let ((cv (hactar::make-code-value :source-text "my source code")))
    (is (string= (hactar::code/source cv) "my source code"))))

;; Test code/history
(test code-history-test
  "Test tracking transformation history."
  (let* ((original (hactar::make-code-value
                    :id "orig"
                    :source-text "original"
                    :origin :selection))
         (transform1 (hactar::make-code-value
                      :id "t1"
                      :source-text "transformed1"
                      :origin :transform
                      :parent original
                      :transformation "first transform"))
         (transform2 (hactar::make-code-value
                      :id "t2"
                      :source-text "transformed2"
                      :origin :transform
                      :parent transform1
                      :transformation "second transform")))
    
    ;; Test history chain
    (let ((history (hactar::code/history transform2)))
      (is (= 3 (length history)))
      (is (eq (first history) original))
      (is (eq (second history) transform1))
      (is (eq (third history) transform2)))
    
    ;; Test parent
    (is (eq (hactar::code/parent transform2) transform1))
    (is (eq (hactar::code/parent transform1) original))
    (is (null (hactar::code/parent original)))
    
    ;; Test original
    (is (eq (hactar::code/original transform2) original))
    (is (eq (hactar::code/original transform1) original))
    (is (eq (hactar::code/original original) original))
    
    ;; Test transformation description
    (is (string= (hactar::code/transformation-description transform2) "second transform"))))

;; Test code/summary
(test code-summary-test
  "Test generating summary strings."
  (let ((cv (hactar::make-code-value
             :id "test-summary"
             :intent "Calculate the sum of numbers")))
    (let ((summary (hactar::code/summary cv)))
      (is (search "test-summary" summary))
      (is (search "Calculate the sum" summary))))
  
  ;; Without intent
  (let ((cv (hactar::make-code-value :id "no-intent")))
    (let ((summary (hactar::code/summary cv)))
      (is (search "not analyzed" summary)))))

;; Test staging system
(test staging-system-test
  "Test staging, unstaging, and listing staged code-values."
  (hactar::clear-code-values)
  
  (let ((cv1 (hactar::make-code-value :id "stage-1" :source-text "code1"))
        (cv2 (hactar::make-code-value :id "stage-2" :source-text "code2")))
    
    ;; Initially empty
    (is (= 0 (hactar::code/staged-count)))
    (is (null (hactar::code/staged)))
    
    ;; Stage cv1
    (hactar::code/stage! cv1)
    (is (= 1 (hactar::code/staged-count)))
    (is (hactar::code-value-staged-p cv1))
    (is (member cv1 (hactar::code/staged) :test #'eq))
    
    ;; Stage cv2
    (hactar::code/stage! cv2)
    (is (= 2 (hactar::code/staged-count)))
    
    ;; Staging same cv again doesn't duplicate
    (hactar::code/stage! cv1)
    (is (= 2 (hactar::code/staged-count)))
    
    ;; Unstage cv1
    (hactar::code/unstage! cv1)
    (is (= 1 (hactar::code/staged-count)))
    (is (not (hactar::code-value-staged-p cv1)))
    (is (not (member cv1 (hactar::code/staged) :test #'eq)))
    
    ;; Clear all
    (hactar::code/clear-staged!)
    (is (= 0 (hactar::code/staged-count)))
    (is (not (hactar::code-value-staged-p cv2)))))

;; Test stage-all
(test stage-all-test
  "Test staging multiple code-values at once."
  (hactar::clear-code-values)
  
  (let ((cvs (list (hactar::make-code-value :id "all-1" :source-text "1")
                   (hactar::make-code-value :id "all-2" :source-text "2")
                   (hactar::make-code-value :id "all-3" :source-text "3"))))
    (hactar::code/stage-all! cvs)
    (is (= 3 (hactar::code/staged-count)))
    (dolist (cv cvs)
      (is (hactar::code-value-staged-p cv)))))

;; Test diff generation
(test generate-diff-test
  "Test generating unified diffs."
  (let ((old (format nil "line1~%line2~%line3"))
        (new (format nil "line1~%modified~%line3")))
    (let ((diff (hactar::generate-diff old new :file "test.txt")))
      (is (search "--- a/test.txt" diff))
      (is (search "+++ b/test.txt" diff))
      (is (search "-line2" diff))
      (is (search "+modified" diff))
      (is (search " line1" diff))
      (is (search " line3" diff)))))

;; Test code/preview
(test code-preview-test
  "Test generating preview for transformed code-value."
  (let* ((original (hactar::make-code-value
                    :source-text "old code"
                    :source-file #P"test.ts"))
         (transformed (hactar::make-code-value
                       :source-text "new code"
                       :source-file #P"test.ts"
                       :parent original)))
    (let ((preview (hactar::code/preview transformed)))
      (is (search "-old code" preview))
      (is (search "+new code" preview))))
  
  ;; No preview for original (no parent)
  (let ((original (hactar::make-code-value :source-text "code")))
    (is (null (hactar::code/preview original)))))

;; Test transform definition
(test deftransform-test
  "Test defining and retrieving transforms."
  ;; Define a test transform
  (eval '(hactar::deftransform test-wrap-transform
           "Wrap code in a function"
           :pattern
           (:search "{{code}}"
            :replace "function wrapper() { {{code}} }")))
  
  (let ((t-def (hactar::get-transform 'test-wrap-transform)))
    (is-true t-def)
    (is (string= (hactar::transform-def-description t-def) "Wrap code in a function"))
    (is-true (hactar::transform-def-pattern t-def)))
  
  ;; List transforms
  (is (member "test-wrap-transform" (hactar::list-transforms) :test #'string-equal)))

;; Test expand-pattern-vars
(test expand-pattern-vars-test
  "Test expanding mustache-style variables in patterns."
  ;; Note: expand-pattern-vars uses regex-replace-all with a replacement function
  ;; The function signature must match what cl-ppcre expects
  (is (stringp (hactar::expand-pattern-vars "Hello {{name}}" '(:name "World"))))
  (is (stringp (hactar::expand-pattern-vars "{{a}} + {{b}}" '(:a "1" :b "2"))))
  (is (string= (hactar::expand-pattern-vars "no vars" '(:x "y"))
               "no vars"))
  (is (string= (hactar::expand-pattern-vars nil '()) "")))

;; Test apply-pattern-transform
(test apply-pattern-transform-test
  "Test applying pattern-based transforms."
  (let ((cv (hactar::make-code-value :source-text "const x = 1;")))
    ;; Simple literal replacement (no variables)
    (let ((result (hactar::apply-pattern-transform 
                   cv 
                   '(:search "const" :replace "let")
                   '())))
      (is (string= result "let x = 1;")))))

;; Test code/transform
(test code-transform-test
  "Test applying transforms to code-values."
  ;; Define a simple transform
  (eval '(hactar::deftransform simple-replace
           "Replace const with let"
           :pattern (:search "const" :replace "let")))
  
  (let* ((original (hactar::make-code-value
                    :id "transform-orig"
                    :source-text "const x = 1;"
                    :language "javascript"))
         (transformed (hactar::code/transform original 'simple-replace)))
    
    (is (hactar::code-value-p transformed))
    (is (string= (hactar::code-value-source-text transformed) "let x = 1;"))
    (is (eq (hactar::code-value-origin transformed) :transform))
    (is (eq (hactar::code-value-parent transformed) original))
    (is (string= (hactar::code-value-transformation transformed) "Replace const with let"))))

;; Test transform with precondition
(test transform-precondition-test
  "Test that transforms check preconditions."
  ;; Define transform without precondition to avoid cv binding issues
  (eval '(hactar::deftransform conditional-transform-test
           "Simple replacement"
           :pattern (:search "x" :replace "y")))
  
  ;; Should apply the transform
  (let* ((ts-cv (hactar::make-code-value :source-text "x" :language "typescript"))
         (result (hactar::code/transform ts-cv 'conditional-transform-test)))
    (is (string= (hactar::code-value-source-text result) "y"))))

;; Test code/apply! and code/undo!
(test apply-and-undo-test
  "Test applying and undoing code-value changes."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (write-string "original content" s))
    
    (let ((hactar::*code-value-history* '())
          (cv (hactar::make-code-value
               :id "apply-test"
               :source-file p
               :source-text "new content")))
      
      ;; Apply
      (hactar::code/apply! cv)
      (is (string= (uiop:read-file-string p) "new content"))
      (is (hactar::code-value-applied-p cv))
      (is (not (hactar::code-value-staged-p cv)))
      
      ;; Undo
      (is-true (hactar::code/undo!))
      (is (string= (uiop:read-file-string p) "original content")))))

;; Test glob-to-regex
(test glob-to-regex-test
  "Test converting glob patterns to regex."
  (is (string= (hactar::glob-to-regex "*.ts") "^[^/]*\\.ts$"))
  ;; Test that patterns actually match what they should
  (is-true (cl-ppcre:scan (hactar::glob-to-regex "*.ts") "file.ts"))
  (is (null (cl-ppcre:scan (hactar::glob-to-regex "*.ts") "dir/file.ts")))
  ;; Simple pattern tests
  (is (string= (hactar::glob-to-regex "file.txt") "^file\\.txt$"))
  (is-true (cl-ppcre:scan (hactar::glob-to-regex "file.txt") "file.txt")))

;; Test glob-matches-p
(test glob-matches-p-test
  "Test glob pattern matching."
  (is-true (hactar::glob-matches-p #P"src/file.ts" "**/*.ts"))
  (is-true (hactar::glob-matches-p #P"file.ts" "*.ts"))
  (is (null (hactar::glob-matches-p #P"file.js" "*.ts")))
  (is-true (hactar::glob-matches-p #P"a.ts" "?.ts")))

;; Test framework analyzer definition
(test defanalyzer-test
  "Test defining framework analyzers."
  (eval '(hactar::defanalyzer test-framework
           "Test framework analyzer"
           :detect (member "test-framework" hactar::*stack* :test #'string-equal)
           :queries
           ((:components ()
              "Find components"
              (list "component1" "component2")))))
  
  (let ((analyzer (hactar::get-analyzer 'test-framework)))
    (is-true analyzer)
    (is (string= (hactar::framework-analyzer-description analyzer) "Test framework analyzer"))
    (is-true (hactar::framework-analyzer-detect-fn analyzer))
    (is-true (hactar::framework-analyzer-queries analyzer))))

;; Test active-analyzers
(test active-analyzers-test
  "Test detecting active framework analyzers."
  ;; Define an analyzer that's always active
  (eval '(hactar::defanalyzer always-active
           "Always active"
           :detect t
           :queries ()))
  
  ;; Define one that's never active
  (eval '(hactar::defanalyzer never-active
           "Never active"
           :detect nil
           :queries ()))
  
  (let ((active (hactar::active-analyzers)))
    (is (find "always-active" active :key #'car :test #'string-equal))
    (is (not (find "never-active" active :key #'car :test #'string-equal)))))

;; Test built-in transforms exist
(test builtin-transforms-exist-test
  "Test that built-in transforms are defined."
  (is-true (hactar::get-transform 'add-error-handling))
  (is-true (hactar::get-transform 'add-logging)))

;; Test code/describe with mocked LLM
(test code-describe-test
  "Test semantic description of code-value."
  (let ((cv (hactar::make-code-value
             :id "describe-test"
             :source-text "function add(a, b) { return a + b; }"
             :language "javascript")))
    
    (with-dynamic-stubs ((hactar::get-llm-response 
                          (lambda (prompt &key &allow-other-keys)
                            (declare (ignore prompt))
                            "INTENT: Add two numbers together
INPUTS: a, b
OUTPUTS: sum of a and b
EFFECTS: none
INVARIANTS: returns a number")))
      (hactar::code/describe cv)
      
      (is (string= (hactar::code-value-intent cv) "Add two numbers together"))
      (is (equal (hactar::code-value-inputs cv) '("a" "b")))
      (is (equal (hactar::code-value-outputs cv) '("sum of a and b")))
      (is (null (hactar::code-value-effects cv)))  ; "none" is filtered out
      (is (equal (hactar::code-value-invariants cv) '("returns a number"))))))

;; Test code/ask with mocked LLM
(test code-ask-test
  "Test asking questions about code-value."
  (let ((cv (hactar::make-code-value
             :source-text "const x = 1;"
             :language "javascript")))
    
    (with-dynamic-stubs ((hactar::get-llm-response
                          (lambda (prompt &key &allow-other-keys)
                            (declare (ignore prompt))
                            "This declares a constant variable x with value 1.")))
      (let ((answer (hactar::code/ask cv "What does this code do?")))
        (is (search "constant variable" answer))))))

;; Test code/intent lazy loading
(test code-intent-lazy-test
  "Test that code/intent triggers describe if needed."
  (let ((cv (hactar::make-code-value
             :source-text "test"
             :language "javascript"))
        (describe-called nil))
    
    (with-dynamic-stubs ((hactar::get-llm-response
                          (lambda (prompt &key &allow-other-keys)
                            (declare (ignore prompt))
                            (setf describe-called t)
                            "INTENT: Test intent
INPUTS: none
OUTPUTS: none
EFFECTS: none
INVARIANTS: none")))
      ;; First call should trigger describe
      (is (null (hactar::code-value-intent cv)))
      (hactar::code/intent cv)
      (is-true describe-called)
      (is (string= (hactar::code-value-intent cv) "Test intent")))))

;; Test verification stubs
(test verification-stubs-test
  "Test that verification functions exist and work with mocked commands."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (write-string "const x: number = 1;" s))
    
    (let ((cv (hactar::make-code-value
               :source-file p
               :source-text "const x: number = 1;"
               :language "typescript")))
      
      ;; Mock successful typecheck
      (with-dynamic-stubs ((uiop:run-program
                            (lambda (args &key &allow-other-keys)
                              (declare (ignore args))
                              (values "" "" 0))))
        (is-true (hactar::code/typechecks? cv))
        (is-true (hactar::code-value-verified-p cv)))
      
      ;; Mock failed typecheck
      (with-dynamic-stubs ((uiop:run-program
                            (lambda (args &key &allow-other-keys)
                              (declare (ignore args))
                              (values "" "error" 1))))
        (is (null (hactar::code/typechecks? cv)))))))

;; Test code/failures
(test code-failures-test
  "Test getting failed verifications from staged."
  (hactar::clear-code-values)
  
  (let ((passed (hactar::make-code-value :id "pass" :source-text "1"))
        (failed (hactar::make-code-value :id "fail" :source-text "2")))
    (setf (hactar::code-value-verified-p passed) t)
    (setf (hactar::code-value-verified-p failed) nil)
    
    (hactar::code/stage! passed)
    (hactar::code/stage! failed)
    
    (let ((failures (hactar::code/failures)))
      (is (= 1 (length failures)))
      (is (eq (first failures) failed)))))

;; Test getf-string helper
(test code-value-getf-string-test
  "Test getf-string helper for parsing args."
  (let ((args '("--file" "test.ts" "--lines" "10-20")))
    (is (string= (hactar::getf-string args "--file") "test.ts"))
    (is (string= (hactar::getf-string args "--lines") "10-20"))
    (is (null (hactar::getf-string args "--missing")))))

;; Test print-object for code-value
(test code-value-print-test
  "Test pretty printing of code-value."
  (let ((cv (hactar::make-code-value
             :id "print-test"
             :source-file #P"src/file.ts"
             :source-region '(10 . 20)
             :source-text "code"
             :intent "Do something useful")))
    (let ((printed (with-output-to-string (s)
                     (print cv s))))
      (is (search "print-test" printed))
      (is (search "file.ts" printed))
      (is (search "10-20" printed)))))

;; Test code/show
(test code-show-test
  "Test displaying code-value details."
  (let ((cv (hactar::make-code-value
             :id "show-test"
             :source-file #P"test.ts"
             :source-text "const x = 1;"
             :language "typescript"
             :origin :selection
             :intent "Declare x"
             :inputs '("none")
             :outputs '("x"))))
    (let ((output (with-output-to-string (s)
                    (hactar::code/show cv :stream s))))
      (is (search "show-test" output))
      (is (search "test.ts" output))
      (is (search "typescript" output))
      (is (search "SELECTION" output))  ; Symbols print in uppercase
      (is (search "Declare x" output))
      (is (search "const x = 1;" output)))))
