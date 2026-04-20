(in-package :hactar-tests)

(def-suite javascript-target-tests :description "JavaScript target tests.")
(in-suite javascript-target-tests)

(defun js-compile (source)
  "Helper: compile source to JS string. Prepends (target :javascript) declaration."
  (string-trim '(#\Space #\Newline #\Tab)
               (hactar::compile-file-to-string
                (format nil "(target :javascript) ~A" source))))

(test js-defun
  (let ((result (js-compile "(defun greet (name) (return (+ \"Hello, \" name)))")))
    (is (search "function greet(name)" result))
    (is (search "return" result))))

(test js-defvar
  (let ((result (js-compile "(defvar x 42)")))
    (is (search "var x = 42" result))))

(test js-defconst
  (let ((result (js-compile "(defconst x 42)")))
    (is (search "const x = 42" result))))

(test js-let-bindings
  (let ((result (js-compile "(let ((x 1) (y 2)) (console.log (+ x y)))")))
    (is (search "let x = 1" result))
    (is (search "let y = 2" result))
    (is (search "console.log" result))))

(test js-property-access
  (let ((result (js-compile "(@ response status)")))
    (is (search "response.status" result))))

(test js-object-literal
  (let ((result (js-compile "(create :status 200 :body \"ok\")")))
    (is (search "status: 200" result))
    (is (search "body: \"ok\"" result))))

(test js-new-expression
  (let ((result (js-compile "(new (*Response body opts))")))
    (is (search "new Response(body, opts)" result))))

(test js-import-named
  (let ((result (js-compile "(import (foo bar) :from \"./module\")")))
    (is (search "import { foo, bar } from \"./module\"" result))))

(test js-import-default
  (let ((result (js-compile "(import styles :from \"./style.css?url\")")))
    (is (search "import styles from \"./style.css?url\"" result))))

(test js-export-default
  (let ((result (js-compile "(export-default (create :a 1))")))
    (is (search "export default" result))
    (is (search "a: 1" result))))

(test js-lambda-single-expression
  (let ((result (js-compile "(lambda (x) (+ x 1))")))
    (is (search "=>" result))
    (is (search "x" result))))

(test js-async-await
  (let ((result (js-compile "(async (lambda (req) (let ((res (await (fetch url)))) (return res))))")))
    (is (search "async" result))
    (is (search "await" result))))

(test js-if-else
  (let ((result (js-compile "(if (= x 1) (console.log \"yes\") (console.log \"no\"))")))
    (is (search "if" result))
    (is (search "else" result))))

(test js-comment
  (let ((result (js-compile "(comment \"this is a comment\")")))
    (is (search "// this is a comment" result))))

(test js-boolean-true
  (let ((result (js-compile "(defvar active t)")))
    (is (search "true" result))))

(test emit-funcall-basic
  "emit-funcall emits function call with args."
  (let* ((hactar::*current-vfs* (hactar::make-vfs))
         (_ (hactar::vfs-select-file hactar::*current-vfs* "t.js"))
         (hactar::*current-target* (gethash :javascript hactar::*compiler-targets*)))
    (declare (ignore _))
    (hactar::emit-funcall 'my-func '(1 "hello"))
    (is (string= "myFunc(1, \"hello\")"
                  (hactar::vfs-get-content hactar::*current-vfs* "t.js")))))

(test emit-funcall-no-args
  "emit-funcall with no args emits empty parens."
  (let* ((hactar::*current-vfs* (hactar::make-vfs))
         (_ (hactar::vfs-select-file hactar::*current-vfs* "t.js"))
         (hactar::*current-target* (gethash :javascript hactar::*compiler-targets*)))
    (declare (ignore _))
    (hactar::emit-funcall 'foo nil)
    (is (string= "foo()"
                  (hactar::vfs-get-content hactar::*current-vfs* "t.js")))))

(test emit-separated-basic
  "emit-separated joins compiled forms with separator."
  (let* ((hactar::*current-vfs* (hactar::make-vfs))
         (_ (hactar::vfs-select-file hactar::*current-vfs* "t.js"))
         (hactar::*current-target* (gethash :javascript hactar::*compiler-targets*)))
    (declare (ignore _))
    (hactar::emit-separated ", " '(1 2 3))
    (is (string= "1, 2, 3"
                  (hactar::vfs-get-content hactar::*current-vfs* "t.js")))))

(test emit-separated-single
  "emit-separated with one form emits no separator."
  (let* ((hactar::*current-vfs* (hactar::make-vfs))
         (_ (hactar::vfs-select-file hactar::*current-vfs* "t.js"))
         (hactar::*current-target* (gethash :javascript hactar::*compiler-targets*)))
    (declare (ignore _))
    (hactar::emit-separated " + " '(42))
    (is (string= "42"
                  (hactar::vfs-get-content hactar::*current-vfs* "t.js")))))

(test emit-block-basic
  "emit-block emits braces with indented body."
  (let* ((hactar::*current-vfs* (hactar::make-vfs))
         (_ (hactar::vfs-select-file hactar::*current-vfs* "t.js"))
         (hactar::*current-target* (gethash :javascript hactar::*compiler-targets*))
         (hactar::*indent-level* 0))
    (declare (ignore _))
    (hactar::emit-block '((return 1)))
    (let ((result (hactar::vfs-get-content hactar::*current-vfs* "t.js")))
      (is (search "{" result))
      (is (search "return 1;" result))
      (is (search "}" result)))))

(test emit-atom-types
      "Atoms emit correctly by type."
      (flet ((emit-to-string (atom)
               (let* ((hactar::*current-vfs* (hactar::make-vfs))
                      (_ (hactar::vfs-select-file hactar::*current-vfs* "t.js")))
		 (declare (ignore _))
		 (hactar::emit-atom atom)
		 (hactar::vfs-get-content hactar::*current-vfs* "t.js"))))
	(is (string= "42" (emit-to-string 42)))
	(is (string= "3.14" (emit-to-string 3.14)))
	(is (string= "\"hello\"" (emit-to-string "hello")))
	(is (string= "true" (emit-to-string t)))
	(is (string= "null" (emit-to-string nil)))))

(test js-false-keyword
  ":false emits as false in JavaScript."
  (let ((result (js-compile "(defvar x :false)")))
    (is (search "var x = false" result))))

(test js-true-keyword
  ":true emits as true in JavaScript."
  (let ((result (js-compile "(defvar x :true)")))
    (is (search "var x = true" result))))

(test js-lambda-if-body-uses-block
  "Lambda with if body uses block form, not expression form."
  (let ((result (js-compile "(lambda (prev) (if (= x 1) (return prev) (return (+ prev 1))))")))
    (is (search "=> {" result))
    (is (not (search "=> if" result)))))

(test js-lambda-let-body-uses-block
  "Lambda with let body uses block form."
  (let ((result (js-compile "(lambda (x) (let ((y 1)) (return (+ x y))))")))
    (is (search "=> {" result))))

(test js-no-double-semicolons-progn-in-block
  "progn inside an if block does not produce double semicolons."
  (let ((result (js-compile "(if (= x 1) (progn (foo) (bar)))")))
    (is (not (search ";;" result)))))

(test js-destructuring-const
  (let ((result (js-compile "(destructuring-const (count set-count) (use-state 0))")))
    (is (search "const [count, setCount] = useState(0)" result))))

(test js-lambda-destructuring-arg
  (let ((result (js-compile "(lambda ((:destructure children title)) (+ children title))")))
    (is (search "({ children, title }) =>" result))))
