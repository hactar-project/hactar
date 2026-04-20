(in-package :hactar-tests)

(def-suite typescript-target-tests :description "TypeScript target tests.")
(in-suite typescript-target-tests)

;;* Helpers 
(defun ts-compile (source)
  (string-trim '(#\Space #\Newline #\Tab)
               (hactar::compile-file-to-string source :target :typescript)))

;;* Tests
(test ts-typed-defun
      "Function with typed args and return type."
      (let ((result (ts-compile
                     "(defun greet ((name :type 'string)) :return 'string
                    (return (+ \"Hello, \" name \"!\")))")))
	(is (search "function greet(name: string): string" result))
	(is (search "return" result))))

(test ts-typed-let
  "Let bindings with type annotations."
  (let ((result (ts-compile
                 "(let (((count :type 'number) 42)
                        ((is-active :type 'boolean) t))
                    (console.log count))")))
    (is (search "let count: number = 42" result))
    (is (search "let isActive: boolean = true" result))))

(test ts-inherits-js-features
  "TypeScript inherits all JavaScript emitters."
  (let ((result (ts-compile "(@ response status)")))
    (is (search "response.status" result))))
