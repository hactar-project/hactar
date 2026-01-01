(in-package :hactar-tests)

;;; --- Lisp Mode Tests ---
(def-suite lisp-mode-tests
  :description "Tests for lisp-mode.lisp - Lisp-only response mode."
  )

(in-suite lisp-mode-tests)

;; Test API surface generation
(test generate-hactar-api-surface-test
  "Test that API surface generation produces expected content."
  (let ((surface (hactar::generate-hactar-api-surface)))
    (is (stringp surface))
    ;; Check for core sections
    (is (search "# Hactar Lisp API Surface" surface))
    (is (search "## Core Functions" surface))
    (is (search "## File Operations" surface))
    (is (search "## Generators" surface))
    (is (search "## Context Inference" surface))
    (is (search "## LLM Interaction" surface))
    (is (search "## Git Operations" surface))
    (is (search "## Utilities" surface))
    (is (search "## Templates" surface))
    (is (search "## Generation Operations" surface))
    (is (search "## Project Variables" surface))
    ;; Check for specific functions
    (is (search "code/select" surface))
    (is (search "code/from-string" surface))
    (is (search "run-generator" surface))
    (is (search "infer-framework" surface))
    (is (search "get-llm-response" surface))
    (is (search "git-add" surface))
    (is (search "git-commit" surface))))

(test get-lisp-mode-api-surface-caching-test
  "Test that API surface is cached."
  (let ((hactar::*lisp-mode-api-surface* nil))
    (let ((surface1 (hactar::get-lisp-mode-api-surface))
          (surface2 (hactar::get-lisp-mode-api-surface)))
      (is (eq surface1 surface2))  ; Same object (cached)
      (is-true hactar::*lisp-mode-api-surface*))))

;; Test Lisp extraction
(test extract-lisp-from-response-plain-test
  "Test extracting Lisp when response is plain code."
  (let ((response "  (+ 1 2)  "))
    (is (string= (hactar::extract-lisp-from-response response) "(+ 1 2)"))))

(test extract-lisp-from-response-code-block-test
  "Test extracting Lisp from markdown code block."
  (let ((response "Here's the code:
```lisp
(defun hello ()
  (print \"Hello\"))
```
That's it."))
    (let ((extracted (hactar::extract-lisp-from-response response)))
      (is (search "defun hello" extracted))
      (is (search "print" extracted)))))

(test extract-lisp-from-response-no-lang-test
  "Test extracting from code block without language specifier."
  (let ((response "```
(list 1 2 3)
```"))
    (is (string= (hactar::extract-lisp-from-response response) "(list 1 2 3)"))))

;; Test Lisp syntax validation
(test validate-lisp-syntax-valid-test
  "Test validation of valid Lisp syntax."
  (multiple-value-bind (valid-p forms)
      (hactar::validate-lisp-syntax "(+ 1 2)")
    (is-true valid-p)
    (is (= 1 (length forms)))
    (is (equal (first forms) '(+ 1 2)))))

(test validate-lisp-syntax-multiple-forms-test
  "Test validation of multiple valid forms."
  (multiple-value-bind (valid-p forms)
      (hactar::validate-lisp-syntax "(setf x 1) (setf y 2) (+ x y)")
    (is-true valid-p)
    (is (= 3 (length forms)))))

(test validate-lisp-syntax-invalid-test
  "Test validation of invalid Lisp syntax."
  (multiple-value-bind (valid-p error-msg)
      (hactar::validate-lisp-syntax "(defun broken (")
    (is (null valid-p))
    (is (stringp error-msg))))

(test validate-lisp-syntax-unbalanced-parens-test
  "Test validation catches unbalanced parentheses."
  (multiple-value-bind (valid-p error-msg)
      (hactar::validate-lisp-syntax "((+ 1 2)")
    (is (null valid-p))
    (is (stringp error-msg))))

(test validate-lisp-syntax-empty-test
  "Test validation of empty string."
  (multiple-value-bind (valid-p forms)
      (hactar::validate-lisp-syntax "")
    (is-true valid-p)
    (is (null forms))))

;; Test safe evaluation
(test eval-lisp-safely-simple-test
  "Test safe evaluation of simple expression."
  (multiple-value-bind (result error)
      (hactar::eval-lisp-safely "(+ 1 2 3)")
    (is (= result 6))
    (is (null error))))

(test eval-lisp-safely-multiple-forms-test
  "Test safe evaluation returns last form's result."
  (multiple-value-bind (result error)
      (hactar::eval-lisp-safely "(setf x 10) (setf y 20) (+ x y)")
    (is (= result 30))
    (is (null error))))

(test eval-lisp-safely-syntax-error-test
  "Test safe evaluation handles syntax errors."
  (multiple-value-bind (result error)
      (hactar::eval-lisp-safely "(defun broken")
    (is (null result))
    (is (stringp error))))

(test eval-lisp-safely-runtime-error-test
  "Test safe evaluation handles runtime errors."
  (multiple-value-bind (result error)
      (hactar::eval-lisp-safely "(/ 1 0)")
    (is (null result))
    (is (stringp error))
    (is (search "error" (string-downcase error)))))

(test eval-lisp-safely-undefined-function-test
  "Test safe evaluation handles undefined function errors."
  (multiple-value-bind (result error)
      (hactar::eval-lisp-safely "(nonexistent-function-xyz 1 2 3)")
    (is (null result))
    (is (stringp error))))

;; Test system prompt generation
(test lisp-mode-system-prompt-test
  "Test that system prompt includes necessary context."
  (let ((hactar::*repo-root* #P"/test/repo/")
        (hactar::*language* "typescript")
        (hactar::*stack* '("react" "tailwind"))
        (hactar::*files* '("file1.ts" "file2.ts")))
    (let ((prompt (hactar::lisp-mode-system-prompt)))
      (is (stringp prompt))
      ;; Check for key instructions
      (is (search "ONLY respond with executable Common Lisp code" prompt))
      (is (search "EVAL or REJECT" prompt))
      ;; Check for context info
      (is (search "/test/repo/" prompt))
      (is (search "typescript" prompt))
      (is (search "react" prompt))
      (is (search "tailwind" prompt))
      (is (search "2" prompt))  ; Number of files
      ;; Check for examples
      (is (search "read-file-content" prompt))
      (is (search "execute-gen-plan" prompt)))))

;; Test display functions
(test display-lisp-code-test
  "Test that display-lisp-code outputs formatted code."
  (let ((output (with-output-to-string (*standard-output*)
                  (hactar::display-lisp-code "(+ 1 2)"))))
    (is (search "(+ 1 2)" output))
    (is (search "Generated Lisp Code" output))
    ;; Should have visual separators
    (is (search "━" output))))

;; Test lisp-mode toggle state
(test lisp-mode-toggle-test
  "Test toggling lisp mode on and off."
  (let ((hactar::*lisp-mode-enabled* nil))
    ;; Get the command function from *commands* hash table
    (let ((cmd-info (gethash "/lisp-mode" hactar::*commands*)))
      (is-true cmd-info)
      (let ((cmd-fn (first cmd-info)))
        ;; Turn on
        (let ((output (with-output-to-string (*standard-output*)
                        (funcall cmd-fn '("on")))))
          (declare (ignore output))
          (is-true hactar::*lisp-mode-enabled*))
        
        ;; Turn off
        (let ((output (with-output-to-string (*standard-output*)
                        (funcall cmd-fn '("off")))))
          (declare (ignore output))
          (is (null hactar::*lisp-mode-enabled*)))
        
        ;; Check status
        (let ((output (with-output-to-string (*standard-output*)
                        (funcall cmd-fn '()))))
          (is (search "DISABLED" output)))))))

;; Test lisp-mode-intercept
(test lisp-mode-intercept-disabled-test
  "Test that intercept returns NIL when mode is disabled."
  (let ((hactar::*lisp-mode-enabled* nil))
    (is (null (hactar::lisp-mode-intercept "test prompt")))))

(test lisp-mode-intercept-enabled-test
  "Test that intercept handles prompts when mode is enabled."
  (let ((hactar::*lisp-mode-enabled* t)
        (hactar::*chat-history* '()))
    ;; Mock get-llm-response to return valid Lisp
    (with-dynamic-stubs ((hactar::get-llm-response 
                          (lambda (prompt &key stream add-to-history custom-system-prompt)
                            (declare (ignore prompt stream add-to-history custom-system-prompt))
                            "(+ 1 2)"))
                         ;; Mock user choosing reject
                         (hactar::prompt-eval-or-reject (lambda () :reject)))
      (let ((result (hactar::lisp-mode-intercept "test")))
        (is-true result)))))  ; Returns T because it handled the prompt

;; Test handle-lisp-mode-response with eval
(test handle-lisp-mode-response-eval-test
  "Test handling response when user chooses to eval."
  (let ((hactar::*chat-history* '()))
    (with-dynamic-stubs ((hactar::get-llm-response 
                          (lambda (prompt &key stream add-to-history custom-system-prompt)
                            (declare (ignore prompt stream add-to-history custom-system-prompt))
                            "(* 6 7)"))
                         (hactar::prompt-eval-or-reject (lambda () :eval))
                         (hactar::add-to-chat-history (lambda (role content)
                                                        (declare (ignore role content))
                                                        nil)))
      (let ((result (hactar::handle-lisp-mode-response "multiply")))
        (is (= result 42))))))

(test handle-lisp-mode-response-reject-test
  "Test handling response when user chooses to reject."
  (with-dynamic-stubs ((hactar::get-llm-response 
                        (lambda (prompt &key stream add-to-history custom-system-prompt)
                          (declare (ignore prompt stream add-to-history custom-system-prompt))
                          "(dangerous-operation)"))
                       (hactar::prompt-eval-or-reject (lambda () :reject)))
    (let ((result (hactar::handle-lisp-mode-response "do something")))
      (is (null result)))))

(test handle-lisp-mode-response-no-code-test
  "Test handling when LLM returns no code."
  (with-dynamic-stubs ((hactar::get-llm-response 
                        (lambda (prompt &key stream add-to-history custom-system-prompt)
                          (declare (ignore prompt stream add-to-history custom-system-prompt))
                          nil)))
    (let* ((output (make-string-output-stream))
           (*standard-output* output)
           (result (hactar::handle-lisp-mode-response "test")))
      (is (null result))
      (is (search "No code generated" (get-output-stream-string output))))))

(test handle-lisp-mode-response-invalid-syntax-test
  "Test handling when LLM returns invalid Lisp."
  (with-dynamic-stubs ((hactar::get-llm-response 
                        (lambda (prompt &key stream add-to-history custom-system-prompt)
                          (declare (ignore prompt stream add-to-history custom-system-prompt))
                          "(defun broken (")))
    (let* ((output (make-string-output-stream))
           (*standard-output* output)
           (result (hactar::handle-lisp-mode-response "test")))
      (is (null result))
      (is (search "syntax error" (string-downcase (get-output-stream-string output)))))))

;; Test copy functionality in prompt loop
(test handle-lisp-mode-response-copy-test
  "Test copy action in the eval/reject loop."
  (let ((copied-text nil)
        (call-count 0))
    (with-dynamic-stubs ((hactar::get-llm-response 
                          (lambda (prompt &key stream add-to-history custom-system-prompt)
                            (declare (ignore prompt stream add-to-history custom-system-prompt))
                            "(list 1 2 3)"))
                         (hactar::prompt-eval-or-reject 
                          (lambda () 
                            (incf call-count)
                            (if (= call-count 1) :copy :reject)))
                         (hactar::copy-to-clipboard 
                          (lambda (text) 
                            (setf copied-text text))))
      (hactar::handle-lisp-mode-response "test")
      (is (string= copied-text "(list 1 2 3)")))))

;; Test view functionality in prompt loop
(test handle-lisp-mode-response-view-test
  "Test view action redisplays the code."
  (let ((display-count 0)
        (call-count 0))
    (with-dynamic-stubs ((hactar::get-llm-response 
                          (lambda (prompt &key stream add-to-history custom-system-prompt)
                            (declare (ignore prompt stream add-to-history custom-system-prompt))
                            "(+ 1 1)"))
                         (hactar::prompt-eval-or-reject 
                          (lambda () 
                            (incf call-count)
                            (if (= call-count 1) :view :reject)))
                         (hactar::display-lisp-code 
                          (lambda (code) 
                            (declare (ignore code))
                            (incf display-count))))
      (hactar::handle-lisp-mode-response "test")
      ;; Should be called twice: initial + view
      (is (= display-count 2)))))

;; Test /lisp command
(test lisp-command-with-args-test
  "Test /lisp command executes single prompt in lisp mode."
  (let ((cmd-info (gethash "/lisp" hactar::*commands*)))
    (is-true cmd-info)
    (let ((cmd-fn (first cmd-info)))
      (with-dynamic-stubs ((hactar::handle-lisp-mode-response 
                            (lambda (prompt) 
                              (is (string= prompt "calculate something"))
                              42)))
        (let ((result (funcall cmd-fn '("calculate" "something"))))
          (is (= result 42)))))))

(test lisp-command-no-args-test
  "Test /lisp command without args shows usage."
  (let ((cmd-info (gethash "/lisp" hactar::*commands*)))
    (is-true cmd-info)
    (let ((cmd-fn (first cmd-info)))
      (let* ((output (make-string-output-stream))
             (*standard-output* output))
        (funcall cmd-fn '())
        (is (search "Usage:" (get-output-stream-string output)))))))

;; Test /lisp-api command
(test lisp-api-command-test
  "Test /lisp-api command displays API surface."
  (let ((cmd-info (gethash "/lisp-api" hactar::*commands*)))
    (is-true cmd-info)
    (let ((cmd-fn (first cmd-info)))
      (let* ((output (make-string-output-stream))
             (*standard-output* output))
        (funcall cmd-fn '())
        (let ((out (get-output-stream-string output)))
          (is (search "Hactar Lisp API Surface" out))
          (is (search "Core Functions" out)))))))

;; Test edge cases
(test extract-lisp-nested-code-blocks-test
  "Test extraction handles nested or multiple code blocks."
  (let ((response "First block:
```lisp
(first-form)
```
Second block:
```lisp
(second-form)
```"))
    ;; Should extract first block only
    (let ((extracted (hactar::extract-lisp-from-response response)))
      (is (search "first-form" extracted))
      (is (null (search "second-form" extracted))))))

(test validate-lisp-with-reader-macros-test
  "Test validation handles reader macros."
  (multiple-value-bind (valid-p forms)
      (hactar::validate-lisp-syntax "'(1 2 3)")
    (is-true valid-p)
    (is (= 1 (length forms))))
  
  (multiple-value-bind (valid-p forms)
      (hactar::validate-lisp-syntax "#'car")
    (is-true valid-p)
    (is (= 1 (length forms)))))

(test validate-lisp-with-strings-test
  "Test validation handles strings with special characters."
  (multiple-value-bind (valid-p forms)
      (hactar::validate-lisp-syntax "(format nil \"Hello~%World\")")
    (is-true valid-p)
    (is (= 1 (length forms)))))

(defvar *side-effect-test-var* nil
  "Variable used for testing side effects in eval-lisp-safely.")

(test eval-lisp-safely-with-side-effects-test
  "Test that evaluation with side effects works."
  (setf *side-effect-test-var* nil)
  (multiple-value-bind (result error)
      (hactar::eval-lisp-safely "(progn (setf hactar-tests::*side-effect-test-var* 'modified) 'done)")
    (is (eq result 'done))
    (is (null error))
    (is (eq *side-effect-test-var* 'modified))))
