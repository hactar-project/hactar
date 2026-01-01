(in-package :hactar-tests)

;;* Lisp-RPC Tests

(def-suite lisp-rpc-tests
  :description "Tests for Lisp-RPC system, API, and protocol")

(in-suite lisp-rpc-tests)

;;* API Registry Tests

(test defapi-registers-function
  "Test that defapi correctly registers an API function."
  (let ((orig-registry (alexandria:copy-hash-table hactar::*api-registry*))
        (hactar::*silent* t))
    (unwind-protect
         (progn
           (clrhash hactar::*api-registry*)
           (eval '(hactar::defapi hactar::test-rpc-echo (message)
                    "Echo back the message."
                    :permissions :auto
                    :category :general
                    :returns "The message string"
                    message))
           (let ((api-def (gethash "test-rpc-echo" hactar::*api-registry*)))
             (is-true api-def "API function should be registered")
             (is (string= "Echo back the message."
                          (hactar::api-definition-docstring api-def)))
             (is (eq :auto (hactar::api-definition-permissions api-def)))
             (is (eq :general (hactar::api-definition-category api-def)))
             (is (string= "The message string" (hactar::api-definition-returns api-def)))
             (is (equal '(message) (hactar::api-definition-lambda-list api-def)))))
      (setf hactar::*api-registry* orig-registry))))

(test defapi-with-all-options
  "Test that defapi handles all keyword options."
  (let ((orig-registry (alexandria:copy-hash-table hactar::*api-registry*))
        (hactar::*silent* t))
    (unwind-protect
         (progn
           (clrhash hactar::*api-registry*)
           (eval '(hactar::defapi hactar::test-rpc-dangerous (cmd)
                    "A dangerous operation."
                    :permissions :confirm
                    :category :shell
                    :dangerous t
                    :side-effects "Runs arbitrary command"
                    :returns "Command output"
                    :examples '((hactar::test-rpc-dangerous "ls"))
                    cmd))
           (let ((api-def (gethash "test-rpc-dangerous" hactar::*api-registry*)))
             (is-true api-def)
             (is (eq :confirm (hactar::api-definition-permissions api-def)))
             (is (eq :shell (hactar::api-definition-category api-def)))
             (is-true (hactar::api-definition-dangerous api-def))
             (is (string= "Runs arbitrary command" (hactar::api-definition-side-effects api-def)))
             (is (string= "Command output" (hactar::api-definition-returns api-def)))
             (is-true (hactar::api-definition-examples api-def))))
      (setf hactar::*api-registry* orig-registry))))

(test defapi-creates-hactar-symbol
  "Test that defapi creates and exports a symbol in HACTAR package."
  (let ((orig-registry (alexandria:copy-hash-table hactar::*api-registry*))
        (hactar::*silent* t))
    (unwind-protect
         (progn
           (clrhash hactar::*api-registry*)
           (eval '(hactar::defapi hactar::test-rpc-sym-check (x)
                    "Check symbol creation."
                    :permissions :auto
                    :category :general
                    x))
           (multiple-value-bind (sym status)
               (find-symbol "TEST-RPC-SYM-CHECK" :hactar)
             (is-true sym "Symbol should exist in HACTAR")
             (is (eq :external status) "Symbol should be external")))
      (setf hactar::*api-registry* orig-registry))))

(test defapi-function-is-callable
  "Test that defapi creates a callable function."
  (let ((orig-registry (alexandria:copy-hash-table hactar::*api-registry*))
        (hactar::*silent* t))
    (unwind-protect
         (progn
           (clrhash hactar::*api-registry*)
           ;; Override resolve-permission to always allow
           (let ((orig-fn #'hactar::resolve-permission))
             (setf (fdefinition 'hactar::resolve-permission)
                   (lambda (tool-name args) (declare (ignore tool-name args)) :allow))
             (unwind-protect
                  (progn
                    (eval '(hactar::defapi hactar::test-rpc-adder (a b)
                             "Add two numbers."
                             :permissions :auto
                             :category :general
                             (+ a b)))
                    (let ((api-def (gethash "test-rpc-adder" hactar::*api-registry*)))
                      (is-true (hactar::api-definition-function api-def))
                      ;; Call the implementation function directly
                      (is (= 7 (funcall (hactar::api-definition-function api-def) 3 4)))))
               (setf (fdefinition 'hactar::resolve-permission) orig-fn))))
      (setf hactar::*api-registry* orig-registry))))

;;* Symbol Whitelist / Security Tests

(test lisp-rpc-allowed-cl-symbols
  "Test that whitelisted CL symbols are allowed."
  (is-true (hactar::lisp-rpc-symbol-allowed-p 'cl:progn))
  (is-true (hactar::lisp-rpc-symbol-allowed-p 'cl:let))
  (is-true (hactar::lisp-rpc-symbol-allowed-p 'cl:format))
  (is-true (hactar::lisp-rpc-symbol-allowed-p 'cl:+))
  (is-true (hactar::lisp-rpc-symbol-allowed-p 'cl:list))
  (is-true (hactar::lisp-rpc-symbol-allowed-p 'cl:if))
  (is-true (hactar::lisp-rpc-symbol-allowed-p 'cl:mapcar))
  (is-true (hactar::lisp-rpc-symbol-allowed-p 'cl:concatenate)))

(test lisp-rpc-blocked-cl-symbols
  "Test that blocked CL symbols are rejected."
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:eval))
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:compile))
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:load))
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:open))
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:with-open-file))
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:read-from-string))
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:defun))
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:setf))
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:delete-file))
  (is-false (hactar::lisp-rpc-symbol-allowed-p 'cl:intern)))

(test lisp-rpc-keyword-symbols-allowed
  "Test that keyword symbols are always allowed."
  (is-true (hactar::lisp-rpc-symbol-allowed-p :foo))
  (is-true (hactar::lisp-rpc-symbol-allowed-p :test))
  (is-true (hactar::lisp-rpc-symbol-allowed-p :auto)))

(test lisp-rpc-unknown-package-symbols-blocked
  "Test that symbols from unknown packages are blocked."
  (let ((sym (intern "SOME-RANDOM-THING" (or (find-package :hactar-test-dummy)
                                              (make-package :hactar-test-dummy :use nil)))))
    (is-false (hactar::lisp-rpc-symbol-allowed-p sym))))

;;* Form Validation Tests

(test validate-safe-form
  "Test that safe forms pass validation."
  (is-true (hactar::validate-lisp-rpc-form '(cl:+ 1 2)))
  (is-true (hactar::validate-lisp-rpc-form '(cl:format cl:nil "hello ~A" "world")))
  (is-true (hactar::validate-lisp-rpc-form '(cl:list 1 2 3)))
  (is-true (hactar::validate-lisp-rpc-form 42))
  (is-true (hactar::validate-lisp-rpc-form "hello"))
  (is-true (hactar::validate-lisp-rpc-form :keyword)))

(test validate-blocks-dangerous-forms
  "Test that dangerous forms are rejected."
  (signals hactar::lisp-rpc-security-error
    (hactar::validate-lisp-rpc-form '(cl:eval '(+ 1 2))))
  (signals hactar::lisp-rpc-security-error
    (hactar::validate-lisp-rpc-form '(cl:load "evil.lisp")))
  (signals hactar::lisp-rpc-security-error
    (hactar::validate-lisp-rpc-form '(cl:defun hack () nil)))
  (signals hactar::lisp-rpc-security-error
    (hactar::validate-lisp-rpc-form '(cl:delete-file "/etc/passwd"))))

(test validate-quoted-forms
  "Test that quoted forms are validated appropriately."
  (is-true (hactar::validate-lisp-rpc-form '(cl:quote (1 2 3))))
  (is-true (hactar::validate-lisp-rpc-form '(cl:quote (:a :b :c))))
  ;; Quoted symbols are just data, should be fine
  (is-true (hactar::validate-lisp-rpc-quoted '(foo bar baz)))
  (is-true (hactar::validate-lisp-rpc-quoted 42))
  (is-true (hactar::validate-lisp-rpc-quoted "string")))

(test validate-nil-and-t
  "Test that nil and t pass validation."
  (is-true (hactar::validate-lisp-rpc-form cl:nil))
  (is-true (hactar::validate-lisp-rpc-form cl:t)))

;;* Restricted Reader Tests

(test lisp-rpc-read-form-basic
  "Test reading basic Lisp forms with restricted readtable."
  (let ((form (hactar::lisp-rpc-read-form "(+ 1 2)")))
    ;; The reader reads in HACTAR-API package, so + resolves there or CL
    (is-true (consp form))))

(test lisp-rpc-read-form-blocks-read-eval
  "Test that #. read-time evaluation is blocked."
  (signals hactar::lisp-rpc-security-error
    (hactar::lisp-rpc-read-form "#.(+ 1 2)")))

(test lisp-rpc-read-form-strings
  "Test reading forms with strings."
  (let ((form (hactar::lisp-rpc-read-form "\"hello world\"")))
    (is (string= "hello world" form))))

(test lisp-rpc-read-form-keywords
  "Test reading forms with keywords."
  (let ((form (hactar::lisp-rpc-read-form ":test")))
    (is (eq :test form))))

;;* Sandboxed Evaluator Tests

(test lisp-rpc-eval-arithmetic
  "Test evaluating simple arithmetic."
  (multiple-value-bind (result output error-msg)
      (hactar::lisp-rpc-eval '(cl:+ 1 2 3))
    (declare (ignore output))
    (is (= 6 result))
    (is (null error-msg))))

(test lisp-rpc-eval-string-operations
  "Test evaluating string operations."
  (multiple-value-bind (result output error-msg)
      (hactar::lisp-rpc-eval '(cl:concatenate 'cl:string "hello" " " "world"))
    (declare (ignore output))
    (is (string= "hello world" result))
    (is (null error-msg))))

(test lisp-rpc-eval-list-operations
  "Test evaluating list operations."
  (multiple-value-bind (result output error-msg)
      (hactar::lisp-rpc-eval '(cl:list 1 2 3))
    (declare (ignore output))
    (is (equal '(1 2 3) result))
    (is (null error-msg))))

(test lisp-rpc-eval-blocks-dangerous
  "Test that dangerous forms are blocked in evaluation."
  (multiple-value-bind (result output error-msg)
      (hactar::lisp-rpc-eval '(cl:eval '(+ 1 2)))
    (declare (ignore result output))
    (is-true error-msg "Should have an error message")))

(test lisp-rpc-eval-from-string
  "Test evaluating from a string."
  (multiple-value-bind (result output error-msg)
      (hactar::lisp-rpc-eval "(cl:+ 10 20)")
    (declare (ignore output))
    (is (= 30 result))
    (is (null error-msg))))

(test lisp-rpc-eval-captures-output
  "Test that output is captured during evaluation."
  (multiple-value-bind (result output error-msg)
      (hactar::lisp-rpc-eval '(cl:progn (cl:format cl:t "hello") 42))
    (is (= 42 result))
    (is (search "hello" output))
    (is (null error-msg))))

(test lisp-rpc-eval-handles-runtime-errors
  "Test that runtime errors are caught and reported."
  (multiple-value-bind (result output error-msg)
      (hactar::lisp-rpc-eval '(cl:/ 1 0))
    (declare (ignore result output))
    (is-true error-msg "Division by zero should produce error")))

(test lisp-rpc-eval-logging
  "Test that evaluations are logged."
  (let ((hactar::*lisp-rpc-eval-log* '()))
    (hactar::lisp-rpc-eval '(cl:+ 1 1))
    (is (= 1 (length hactar::*lisp-rpc-eval-log*)))
    (let ((entry (first hactar::*lisp-rpc-eval-log*)))
      (is (equal '(cl:+ 1 1) (getf entry :form)))
      (is (= 2 (getf entry :result)))
      (is (null (getf entry :error)))
      (is (numberp (getf entry :elapsed)))
      (is (numberp (getf entry :timestamp))))))

(test lisp-rpc-eval-log-max-size
  "Test that eval log is trimmed to max size."
  (let ((hactar::*lisp-rpc-eval-log* '())
        (hactar::*lisp-rpc-eval-log-max* 5))
    (dotimes (i 10)
      (hactar::lisp-rpc-eval `(cl:+ ,i 1)))
    (is (<= (length hactar::*lisp-rpc-eval-log*) 5))))

;;* Extract Forms Tests

(test extract-lisp-rpc-forms-fenced
  "Test extracting Lisp forms from ```lisp fenced blocks."
  (let ((text "Here's what I'll do:
```lisp
(hactar:read-file \"src/main.lisp\")
```
That should work."))
    (let ((forms (hactar::extract-lisp-rpc-forms text)))
      (is (= 1 (length forms)))
      (is (search "read-file" (first forms))))))

(test extract-lisp-rpc-forms-tagged
  "Test extracting Lisp forms from <lisp> tagged blocks."
  (let ((text "Let me read the file:
<lisp>(hactar:read-file \"config.yaml\")</lisp>
Done."))
    (let ((forms (hactar::extract-lisp-rpc-forms text)))
      (is (= 1 (length forms)))
      (is (search "read-file" (first forms))))))

(test extract-lisp-rpc-forms-multiple
  "Test extracting multiple Lisp forms."
  (let ((text "First:
```lisp
(hactar:read-file \"a.lisp\")
```
Second:
```lisp
(hactar:read-file \"b.lisp\")
```"))
    (let ((forms (hactar::extract-lisp-rpc-forms text)))
      (is (= 2 (length forms))))))

(test extract-lisp-rpc-forms-mixed
  "Test extracting forms from mixed fenced and tagged blocks."
  (let ((text "```lisp
(cl:+ 1 2)
```
And also: <lisp>(cl:* 3 4)</lisp>"))
    (let ((forms (hactar::extract-lisp-rpc-forms text)))
      (is (= 2 (length forms))))))

(test extract-lisp-rpc-forms-none
  "Test that no forms are extracted from text without code blocks."
  (let ((text "Just some regular text with no code blocks."))
    (let ((forms (hactar::extract-lisp-rpc-forms text)))
      (is (= 0 (length forms))))))

(test extract-lisp-rpc-forms-multiline
  "Test extracting multiline forms from fenced blocks."
  (let ((text "```lisp
(cl:progn
  (cl:let ((x 1)
           (y 2))
    (cl:+ x y)))
```"))
    (let ((forms (hactar::extract-lisp-rpc-forms text)))
      (is (= 1 (length forms)))
      (is (search "progn" (first forms))))))

;;* Process Response Tests

(test lisp-rpc-process-response-basic
  "Test processing a response with a simple safe form."
  (let ((text "```lisp
(cl:+ 10 20)
```"))
    (let ((results (hactar::lisp-rpc-process-response text)))
      (is (= 1 (length results)))
      (let ((r (first results)))
        (is-true (getf r :result))
        (is (null (getf r :error)))))))

(test lisp-rpc-process-response-error
  "Test processing a response with a form that causes an error."
  (let ((text "```lisp
(cl:/ 1 0)
```"))
    (let ((results (hactar::lisp-rpc-process-response text)))
      (is (= 1 (length results)))
      (let ((r (first results)))
        (is-true (getf r :error))))))

(test lisp-rpc-process-response-security-violation
  "Test processing a response with a blocked form."
  (let ((text "```lisp
(cl:eval '(+ 1 2))
```"))
    (let ((results (hactar::lisp-rpc-process-response text)))
      (is (= 1 (length results)))
      (let ((r (first results)))
        (is-true (getf r :error))))))

(test lisp-rpc-process-response-no-forms
  "Test processing a response with no Lisp forms."
  (let ((text "Just a regular message with no code."))
    (let ((results (hactar::lisp-rpc-process-response text)))
      (is (= 0 (length results))))))

;;* Format Results Tests

(test format-lisp-rpc-results-success
  "Test formatting successful results."
  (let ((results (list (list :form "(cl:+ 1 2)"
                             :result "3"
                             :output ""
                             :error nil))))
    (let ((formatted (hactar::format-lisp-rpc-results results)))
      (is-true formatted)
      (is (search "Evaluation results" formatted))
      (is (search "Result: 3" formatted)))))

(test format-lisp-rpc-results-error
  "Test formatting error results."
  (let ((results (list (list :form "(cl:/ 1 0)"
                             :result nil
                             :output ""
                             :error "Division by zero"))))
    (let ((formatted (hactar::format-lisp-rpc-results results)))
      (is-true formatted)
      (is (search "Error: Division by zero" formatted)))))

(test format-lisp-rpc-results-with-output
  "Test formatting results with captured output."
  (let ((results (list (list :form "(cl:format t \"hi\")"
                             :result "NIL"
                             :output "hi"
                             :error nil))))
    (let ((formatted (hactar::format-lisp-rpc-results results)))
      (is-true formatted)
      (is (search "Output: hi" formatted)))))

(test format-lisp-rpc-results-nil-returns-nil
  "Test that nil results list returns nil."
  (is (null (hactar::format-lisp-rpc-results nil))))

;;* API Exposure Tests

(test lisp-rpc-expose-all
  "Test exposing all categories."
  (let ((hactar::*lisp-rpc-exposed-categories* nil)
        (hactar::*silent* t))
    (hactar::lisp-rpc-expose :all)
    (is (equal '(:all) hactar::*lisp-rpc-exposed-categories*))))

(test lisp-rpc-expose-specific-categories
  "Test exposing specific categories."
  (let ((hactar::*lisp-rpc-exposed-categories* nil)
        (hactar::*silent* t))
    (hactar::lisp-rpc-expose :filesystem :context)
    (is (equal '(:filesystem :context) hactar::*lisp-rpc-exposed-categories*))))

(test lisp-rpc-api-exposed-p-all
  "Test that all APIs are exposed when :all is set."
  (let ((hactar::*lisp-rpc-exposed-categories* '(:all))
        (api-def (hactar::make-api-definition
                  :name 'test-fn
                  :category :filesystem
                  :permissions :auto)))
    (is-true (hactar::lisp-rpc-api-exposed-p api-def))))

(test lisp-rpc-api-exposed-p-by-category
  "Test exposure by category."
  (let ((hactar::*lisp-rpc-exposed-categories* '(:filesystem)))
    (is-true (hactar::lisp-rpc-api-exposed-p
              (hactar::make-api-definition :name 'test-fn :category :filesystem :permissions :auto)))
    (is-false (hactar::lisp-rpc-api-exposed-p
               (hactar::make-api-definition :name 'test-fn :category :shell :permissions :auto)))))

(test lisp-rpc-api-exposed-p-by-name
  "Test exposure by specific function name."
  (let ((hactar::*lisp-rpc-exposed-categories* '(my-special-fn)))
    (is-true (hactar::lisp-rpc-api-exposed-p
              (hactar::make-api-definition :name 'my-special-fn :category :general :permissions :auto)))
    (is-false (hactar::lisp-rpc-api-exposed-p
               (hactar::make-api-definition :name 'other-fn :category :general :permissions :auto)))))

;;* API Surface Tests

(test lisp-rpc-api-surface
  "Test getting the API surface for inspection."
  (let ((orig-registry (alexandria:copy-hash-table hactar::*api-registry*))
        (hactar::*silent* t)
        (hactar::*lisp-rpc-exposed-categories* '(:all)))
    (unwind-protect
         (progn
           (clrhash hactar::*api-registry*)
           (eval '(hactar::defapi hactar::surface-test-fn (x)
                    "A test function."
                    :permissions :auto
                    :category :general
                    x))
           (let ((surface (hactar::lisp-rpc-api-surface)))
             (is (= 1 (length surface)))
             (let ((entry (first surface)))
               (is (string= "surface-test-fn" (cdr (assoc :name entry))))
               (is (eq :general (cdr (assoc :category entry))))
               (is (eq :auto (cdr (assoc :permissions entry))))
               (is-true (cdr (assoc :exposed entry))))))
      (setf hactar::*api-registry* orig-registry))))

;;* System Prompt Generation Tests

(test generate-lisp-api-prompt-with-apis
  "Test generating the Lisp API prompt section."
  (let ((orig-registry (alexandria:copy-hash-table hactar::*api-registry*))
        (hactar::*silent* t)
        (hactar::*lisp-rpc-exposed-categories* '(:all)))
    (unwind-protect
         (progn
           (clrhash hactar::*api-registry*)
           (eval '(hactar::defapi hactar::prompt-test-read (path)
                    "Read a file."
                    :permissions :auto
                    :category :filesystem
                    :returns "File content"
                    path))
           (let ((prompt (hactar::generate-lisp-api-prompt)))
             (is-true prompt)
             (is (search "Lisp API" prompt))
             (is (search "prompt-test-read" prompt))
             (is (search "Read a file." prompt))
             (is (search "hactar:" prompt))))
      (setf hactar::*api-registry* orig-registry))))

(test generate-lisp-api-prompt-empty
  "Test that empty API registry produces empty prompt."
  (let ((orig-registry (alexandria:copy-hash-table hactar::*api-registry*))
        (hactar::*silent* t))
    (unwind-protect
         (progn
           (clrhash hactar::*api-registry*)
           (let ((prompt (hactar::generate-lisp-api-prompt)))
             (is (string= "" prompt))))
      (setf hactar::*api-registry* orig-registry))))

(test generate-lisp-api-prompt-shows-permissions
  "Test that confirm-required APIs show permission note."
  (let ((orig-registry (alexandria:copy-hash-table hactar::*api-registry*))
        (hactar::*silent* t)
        (hactar::*lisp-rpc-exposed-categories* '(:all)))
    (unwind-protect
         (progn
           (clrhash hactar::*api-registry*)
           (eval '(hactar::defapi hactar::prompt-confirm-test (cmd)
                    "Run a command."
                    :permissions :confirm
                    :category :shell
                    :side-effects "Runs command"
                    :examples '((hactar::prompt-confirm-test "ls -la"))
                    cmd))
           (let ((prompt (hactar::generate-lisp-api-prompt)))
             (is (search "permission confirmation" prompt))
             (is (search "Side effects:" prompt))))
      (setf hactar::*api-registry* orig-registry))))

;;* Security Error Condition Tests

(test lisp-rpc-security-error-condition
  "Test the security error condition."
  (handler-case
      (error 'hactar::lisp-rpc-security-error
             :form '(cl:eval 'bad)
             :reason "eval is blocked")
    (hactar::lisp-rpc-security-error (e)
      (is (equal '(cl:eval 'bad) (hactar::security-error-form e)))
      (is (string= "eval is blocked" (hactar::security-error-reason e)))
      (is (search "security violation" (format nil "~A" e))))))

;;* RPC Protocol Tests

(test rpc-emit-only-in-rpc-mode
  "Test that rpc-emit only outputs when *lisp-rpc-mode* is active."
  (let ((hactar::*lisp-rpc-mode* nil))
    (let* ((s (make-string-output-stream))
           (hactar::*rpc-output-stream* s))
      (hactar::rpc-emit '(test-form 1 2 3))
      (let ((output (get-output-stream-string s)))
        (is (string= "" output))))))

(test rpc-emit-outputs-in-rpc-mode
  "Test that rpc-emit outputs s-expression when mode is active."
  (let ((hactar::*lisp-rpc-mode* t))
    (let* ((s (make-string-output-stream))
           (hactar::*rpc-output-stream* s))
      (hactar::rpc-emit '(test-form 1 2 3))
      (let ((output (get-output-stream-string s)))
        (is (search "test-form" output))
        (is (search "1" output))
        (is (search "2" output))
        (is (search "3" output))))))

(test rpc-hello-emits-form
  "Test rpc-hello output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-hello "1.0" "gpt-4")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-hello" output))
      (is (search "1.0" output))
      (is (search "gpt-4" output)))))

(test rpc-ready-emits-form
  "Test rpc-ready output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-ready)
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-ready" output)))))

(test rpc-response-emits-form
  "Test rpc-response output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-response "Hello world")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-response" output))
      (is (search "Hello world" output)))))

(test rpc-error-emits-form
  "Test rpc-error output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-error "Something went wrong")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-error" output))
      (is (search "Something went wrong" output)))))

(test rpc-status-emits-form
  "Test rpc-status output with extra args."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-status "waiting" :model "claude")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-status" output))
      (is (search "waiting" output)))))

(test rpc-eval-result-emits-form
  "Test rpc-eval-result output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-eval-result "42" "(+ 40 2)")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-eval-result" output)))))

(test rpc-eval-error-emits-form
  "Test rpc-eval-error output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-eval-error "division by zero" "(/ 1 0)")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-eval-error" output))
      (is (search "division by zero" output)))))

(test rpc-eval-output-emits-form
  "Test rpc-eval-output output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-eval-output "printed text" "(format t ...)")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-eval-output" output)))))

(test rpc-cancelled-emits-form
  "Test rpc-cancelled output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-cancelled)
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-cancelled" output)))))

(test rpc-interrupted-emits-form
  "Test rpc-interrupted output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-interrupted)
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-interrupted" output)))))

(test rpc-exit-emits-form
  "Test rpc-exit output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-exit "shutting down")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-exit" output))
      (is (search "shutting down" output)))))

(test rpc-model-changed-emits-form
  "Test rpc-model-changed output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-model-changed "claude-3.5")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-model-changed" output))
      (is (search "claude-3.5" output)))))

(test rpc-model-not-found-emits-form
  "Test rpc-model-not-found output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-model-not-found "nonexistent")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-model-not-found" output)))))

(test rpc-history-compressed-emits-form
  "Test rpc-history-compressed output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-history-compressed)
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-history-compressed" output)))))

(test rpc-command-output-emits-form
  "Test rpc-command-output output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-command-output "file.txt" "ls")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-command-output" output)))))

(test rpc-permission-request-emits-form
  "Test rpc-permission-request output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-permission-request "write-file" '("test.txt") "Write to file" '(:allow :deny))
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-permission-request" output))
      (is (search "write-file" output)))))

(test rpc-event-emits-form
  "Test rpc-event output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-event :file-changed :path "src/main.lisp")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-event" output))
      (is (search "file-changed" output)))))

(test rpc-log-emits-form
  "Test rpc-log output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-log :info "Starting up" :version "1.0")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-log" output))
      (is (search "info" output))
      (is (search "Starting up" output)))))

(test rpc-warning-emits-form
  "Test rpc-warning output."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-warning "Low memory" :available "100MB")
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-warning" output))
      (is (search "Low memory" output)))))

;;* RPC Output Readability Tests

(test rpc-output-is-readable
  "Test that RPC output can be read back as s-expressions."
  (let* ((hactar::*lisp-rpc-mode* t)
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::rpc-response "test message")
    (let* ((output (get-output-stream-string s))
           (form (read-from-string (string-trim '(#\Newline #\Space) output))))
      (is (string= "hactar-response" (string-downcase (symbol-name (first form)))))
      (is (string= "test message" (second form))))))

(test rpc-output-forms-are-readable
  "Test that various RPC outputs are valid s-expressions."
  (let ((hactar::*lisp-rpc-mode* t))
    (dolist (emitter (list (lambda () (hactar::rpc-ready))
                           (lambda () (hactar::rpc-cancelled))
                           (lambda () (hactar::rpc-interrupted))
                           (lambda () (hactar::rpc-error "test"))
                           (lambda () (hactar::rpc-hello "1.0" "model"))))
      (let* ((s (make-string-output-stream))
             (hactar::*rpc-output-stream* s))
        (funcall emitter)
        (let ((trimmed (string-trim '(#\Newline #\Space) (get-output-stream-string s))))
          (when (> (length trimmed) 0)
            (is-true (handler-case (progn (read-from-string trimmed) t)
                       (error () nil))
                     (format nil "Output should be readable: ~A" trimmed))))))))

;;* Built-in API Tests

(test builtin-api-file-exists-registered
  "Test that file-exists-p API is registered."
  (is-true (gethash "file-exists-p" hactar::*api-registry*)
           "file-exists-p should be in the API registry"))

(test builtin-api-read-file-registered
  "Test that read-file API is registered."
  (is-true (gethash "read-file" hactar::*api-registry*)
           "read-file should be in the API registry"))

(test builtin-api-write-file-registered
  "Test that write-file API is registered."
  (is-true (gethash "write-file" hactar::*api-registry*)
           "write-file should be in the API registry"))

(test builtin-api-sh-registered
  "Test that sh API is registered."
  (is-true (gethash "sh" hactar::*api-registry*)
           "sh should be in the API registry"))

(test builtin-api-git-status-registered
  "Test that git-status API is registered."
  (is-true (gethash "git-status" hactar::*api-registry*)
           "git-status should be in the API registry"))

(test builtin-api-categories
  "Test that built-in APIs have correct categories."
  (let ((read-file (gethash "read-file" hactar::*api-registry*))
        (sh (gethash "sh" hactar::*api-registry*))
        (git-status (gethash "git-status" hactar::*api-registry*))
        (add-ctx (gethash "add-to-context" hactar::*api-registry*)))
    (when read-file
      (is (eq :filesystem (hactar::api-definition-category read-file))))
    (when sh
      (is (eq :shell (hactar::api-definition-category sh))))
    (when git-status
      (is (eq :git (hactar::api-definition-category git-status))))
    (when add-ctx
      (is (eq :context (hactar::api-definition-category add-ctx))))))

(test builtin-api-permissions
  "Test that built-in APIs have correct permission levels."
  (let ((read-file (gethash "read-file" hactar::*api-registry*))
        (write-file (gethash "write-file" hactar::*api-registry*))
        (sh (gethash "sh" hactar::*api-registry*))
        (sh-read (gethash "sh-read" hactar::*api-registry*)))
    (when read-file
      (is (eq :auto (hactar::api-definition-permissions read-file))))
    (when write-file
      (is (eq :confirm (hactar::api-definition-permissions write-file))))
    (when sh
      (is (eq :confirm (hactar::api-definition-permissions sh))))
    (when sh-read
      (is (eq :auto (hactar::api-definition-permissions sh-read))))))

;;* Main Loop Tests

(test lisp-rpc-main-loop-evaluates-stdin
  "Test that lisp-rpc-main-loop reads and evaluates forms from stdin."
  (let* ((hactar::*lisp-rpc-mode* t)
         (hactar::*current-model* nil)
         (hactar::*hactar-version* "test-version")
         (input-string (format nil "(hactar::rpc-response \"hello from stdin\")~%"))
         (*standard-input* (make-string-input-stream input-string))
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::lisp-rpc-main-loop)
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-hello" output))
      (is (search "hactar-ready" output))
      (is (search "hello from stdin" output))
      (is (search "hactar-exit" output))
      (is (search "eof" output)))))

(test lisp-rpc-main-loop-handles-eval-errors
  "Test that lisp-rpc-main-loop catches and reports evaluation errors."
  (let* ((hactar::*lisp-rpc-mode* t)
         (hactar::*current-model* nil)
         (hactar::*hactar-version* "test-version")
         (input-string (format nil "(error \"test error\")~%"))
         (*standard-input* (make-string-input-stream input-string))
         (s (make-string-output-stream))
         (hactar::*rpc-output-stream* s))
    (hactar::lisp-rpc-main-loop)
    (let ((output (get-output-stream-string s)))
      (is (search "hactar-error" output))
      (is (search "test error" output))
      (is (search "hactar-exit" output))
      (is (search "eof" output)))))
