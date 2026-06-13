(in-package :hactar-tests)

(def-suite prompts-tests
  :description "Tests for the prompts registry")

(in-suite prompts-tests)

(test refactor-defprompt-and-render
  "Test that defprompt registers a prompt and render-prompt formats it with mustache."
  (let ((hactar::*prompts* (make-hash-table :test 'equal)))
    (eval '(hactar::defprompt :test-p :template "Hello, {{name}}!"))
    (is (string= "Hello, {{name}}!" (hactar::get-prompt :test-p)))
    (is (string= "Hello, World!" (hactar::render-prompt :test-p '((name . "World")))))))

(test defprompt-with-type-and-listing
  "Test that defprompt handles type argument and keyword, and list-prompts/find-prompt works."
  (let ((hactar::*prompts* (make-hash-table :test 'equal)))
    ;; 1. Test with explicit type argument
    (eval '(hactar::defprompt test-sys system :template "System template"))
    (let ((p (hactar::find-prompt 'test-sys)))
      (is (not (null p)))
      (is (eq (hactar::prompt-type p) 'system))
      (is (string= "System template" (hactar::get-prompt 'test-sys))))

    ;; 2. Test with type keyword
    (eval '(hactar::defprompt test-user :type user :template "User template"))
    (let ((p (hactar::find-prompt 'test-user)))
      (is (not (null p)))
      (is (eq (hactar::prompt-type p) 'user))
      (is (string= "User template" (hactar::get-prompt 'test-user))))

    ;; 3. Test list-prompts
    (let ((all (hactar::list-prompts)))
      (is (= 2 (length all)))
      (is (string= "TEST-SYS" (string (hactar::prompt-name (first all)))))
      (is (string= "TEST-USER" (string (hactar::prompt-name (second all))))))))
