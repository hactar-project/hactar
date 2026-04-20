(in-package :hactar)

(deftarget :javascript
  :file-extension "js")

;;* Emission Helpers 
(defun emit-name (symbol)
  "Emit a symbol as a JS identifier. Converts kebab-case to camelCase, strips earmuffs.
   Preserves dots in symbol names."
  (let* ((raw (symbol-name symbol))
         (name (string-downcase raw)))
    ;; Check for earmuffs — capitalize first letter (constructor/component)
    (when (and (> (length raw) 1)
               (char= (char raw 0) #\*)
               (not (char= (char raw (1- (length raw))) #\*)))
      ;; Single leading * like *Response -> keep as-is after stripping
      (setf name (string-downcase (subseq raw 1))))
    (when (and (> (length raw) 2)
               (char= (char raw 0) #\*)
               (char= (char raw (1- (length raw))) #\*))
      ;; Double earmuffs *foo* -> strip both
      (setf name (string-downcase (subseq raw 1 (1- (length raw))))))
    ;; Handle dots: split on dots, convert each segment, rejoin
    (if (position #\. name)
        (let* ((segments (cl-ppcre:split "\\." name))
               (converted (mapcar #'kebab-to-camel segments)))
          (compiler-emit (format nil "~{~A~^.~}" converted)))
        ;; Check if name starts with uppercase (earmuff-stripped constructor)
        (let ((camel (kebab-to-camel name)))
          ;; If original had leading *, capitalize
          (when (and (> (length raw) 1)
                     (char= (char raw 0) #\*))
            (let ((stripped (subseq raw 1)))
              (if (and (some #'lower-case-p stripped) (some #'upper-case-p stripped))
                  ;; Mixed case (from :invert reader or pipe-escaping) -> preserve as-is
                  (setf camel stripped)
                  (setf camel (concatenate 'string
                                           (string (char-upcase (char camel 0)))
                                           (subseq camel 1))))))
          (compiler-emit camel)))))

(defun emit-atom (atom)
  "Emit an atom: numbers as-is, strings quoted, T->true, NIL->null, symbols as identifiers."
  (cond
    ((numberp atom) (compiler-emit (format nil "~A" atom)))
    ((stringp atom) (compiler-emit (format nil "\"~A\"" atom)))
    ((eq atom t) (compiler-emit "true"))
    ((null atom) (compiler-emit "null"))
    ((eq atom :true) (compiler-emit "true"))
    ((eq atom :false) (compiler-emit "false"))
    ((keywordp atom) (compiler-emit (format nil "\"~A\"" (string-downcase (symbol-name atom)))))
    ((symbolp atom) (emit-name atom))
    (t (compiler-emit (format nil "~A" atom)))))

(defun emit-funcall (op args)
  "Emit a generic function call: op(arg1, arg2, ...)"
  (emit-name op)
  (compiler-emit "(")
  (loop for (arg . rest) on args
        do (compile-form arg)
        when rest do (compiler-emit ", "))
  (compiler-emit ")"))

(defun emit-separated (separator forms)
  "Compile each form in FORMS, emitting SEPARATOR between them."
  (loop for (form . rest) on forms
        do (compile-form form)
        when rest do (compiler-emit separator)))

(defun emit-block (body)
  "Emit { ... } with indented body statements."
  (compiler-emitln "{")
  (with-indent
    (dolist (form body)
      (emit-indent)
      (compile-form form)
      (if (js-statement-p form)
          (compiler-emitln "")
          (compiler-emitln ";"))))
  (emit-indent)
  (compiler-emit "}"))

(defun emit-js-arg (arg)
  "Emit a function argument, supporting (:destructure a b c) object destructuring."
  (cond
    ((and (listp arg) (eq (first arg) :destructure))
     (compiler-emit "{ ")
     (loop for (item . rest) on (rest arg)
           do (emit-name item)
           when rest do (compiler-emit ", "))
     (compiler-emit " }"))
    ((listp arg)
     (emit-name (first arg)))
    (t
     (emit-name arg))))
(defun js-statement-p (form)
  "Return T if FORM compiles to a JS statement that manages its own semicolons/braces."
  (and (listp form) (symbolp (car form))
       (or (member (car form) '(if cond progn let let* try defun))
           (and (member (car form) '(export export-default async))
                (consp (cadr form))
                (js-statement-p (cadr form))))))

;;* Emitters 
(defemit :javascript defun (name args &body body)
  (let ((clean-body (if (and (keywordp (first body)) (eq (first body) :return))
                        (cddr body)
                        body))
        (clean-args (mapcar (lambda (a) (if (listp a) (first a) a)) args)))
    (compiler-emit "function ")
    (emit-name name)
    (compiler-emit "(")
    (loop for (arg . rest) on clean-args
          do (emit-name arg)
          when rest do (compiler-emit ", "))
    (compiler-emit ") ")
    (emit-block clean-body)))

(defemit :javascript defvar (name &optional value)
  (compiler-emit "var ")
  (emit-name name)
  (when value
    (compiler-emit " = ")
    (compile-form value)))

(defemit :javascript defconst (name &optional value)
  (compiler-emit "const ")
  (emit-name name)
  (when value
    (compiler-emit " = ")
    (compile-form value)))

(defemit :javascript let (bindings &body body)
  (dolist (binding bindings)
    (let ((var-form (if (listp (first binding)) (caar binding) (first binding)))
          (val (second binding)))
      (emit-indent)
      (compiler-emit "let ")
      (emit-name var-form)
      (when val
        (compiler-emit " = ")
        (compile-form val))
      (compiler-emitln ";")))
  (dolist (form body)
    (emit-indent)
    (compile-form form)
    (compiler-emitln ";")))

(defemit :javascript let* (bindings &body body)
  (dolist (binding bindings)
    (let ((var-form (if (listp (first binding)) (caar binding) (first binding)))
          (val (second binding)))
      (emit-indent)
      (compiler-emit "const ")
      (emit-name var-form)
      (when val
        (compiler-emit " = ")
        (compile-form val))
      (compiler-emitln ";")))
  (dolist (form body)
    (emit-indent)
    (compile-form form)
    (compiler-emitln ";")))

(defemit :javascript setf (place value)
  (compile-form place)
  (compiler-emit " = ")
  (compile-form value))

(defemit :javascript if (condition then &optional else)
  (compiler-emit "if (")
  (compile-form condition)
  (compiler-emit ") ")
  (emit-block (list then))
  (when else
    (compiler-emit " else ")
    (emit-block (list else))))

(defemit :javascript cond (&rest clauses)
  (loop for (clause . rest) on clauses
        for first = t then nil
        do (let ((test (first clause))
                 (body (rest clause)))
             (if first
                 (progn
                   (compiler-emit "if (")
                   (if (eq test t)
                       (compiler-emit "true")
                       (compile-form test))
                   (compiler-emit ") ")
                   (emit-block body))
                 (if (eq test t)
                     (progn
                       (compiler-emit " else ")
                       (emit-block body))
                     (progn
                       (compiler-emit " else if (")
                       (compile-form test)
                       (compiler-emit ") ")
                       (emit-block body)))))))

(defjsmacro :javascript when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defjsmacro :javascript unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

(defemit :javascript lambda (args &body body)
  (compiler-emit "(")
  (loop for (arg . rest) on args
        do (emit-js-arg arg)
        when rest do (compiler-emit ", "))
  (compiler-emit ") => ")
  (if (and (= 1 (length body))
           (not (js-statement-p (first body))))
      (compile-form (first body))
      (emit-block body)))

(defemit :javascript async (form)
  (compiler-emit "async ")
  (compile-form form))

(defemit :javascript await (form)
  (compiler-emit "await ")
  (compile-form form))

(defemit :javascript return (form)
  (compiler-emit "return ")
  (compile-form form))

(defemit :javascript + (&rest args)
  (compiler-emit "(")
  (emit-separated " + " args)
  (compiler-emit ")"))

(defemit :javascript - (&rest args)
  (if (= 1 (length args))
      (progn (compiler-emit "(-") (compile-form (first args)) (compiler-emit ")"))
      (progn
        (compiler-emit "(")
        (emit-separated " - " args)
        (compiler-emit ")"))))

(defemit :javascript * (&rest args)
  (compiler-emit "(")
  (emit-separated " * " args)
  (compiler-emit ")"))

(defemit :javascript / (&rest args)
  (compiler-emit "(")
  (emit-separated " / " args)
  (compiler-emit ")"))

(defemit :javascript = (a b)
  (compile-form a)
  (compiler-emit " === ")
  (compile-form b))

(defemit :javascript < (a b)
  (compile-form a)
  (compiler-emit " < ")
  (compile-form b))

(defemit :javascript > (a b)
  (compile-form a)
  (compiler-emit " > ")
  (compile-form b))

(defemit :javascript <= (a b)
  (compile-form a)
  (compiler-emit " <= ")
  (compile-form b))

(defemit :javascript >= (a b)
  (compile-form a)
  (compiler-emit " >= ")
  (compile-form b))

(defemit :javascript and (&rest args)
  (compiler-emit "(")
  (emit-separated " && " args)
  (compiler-emit ")"))

(defemit :javascript or (&rest args)
  (compiler-emit "(")
  (emit-separated " || " args)
  (compiler-emit ")"))

(defemit :javascript not (x)
  (compiler-emit "!(")
  (compile-form x)
  (compiler-emit ")"))

(defemit :javascript @ (obj &rest keys)
  (compile-form obj)
  (dolist (key keys)
    (compiler-emit ".")
    (emit-name key)))

(defemit :javascript create (&rest pairs)
  (compiler-emit "{ ")
  (loop for (key val . rest) on pairs by #'cddr
        do (let ((key-str (if (stringp key)
                              (format nil "\"~A\"" key)
                              (kebab-to-camel (string-downcase (symbol-name key))))))
             (compiler-emit key-str))
           (compiler-emit ": ")
           (compile-form val)
        when rest do (compiler-emit ", "))
  (compiler-emit " }"))

(defemit :javascript new (form)
  (compiler-emit "new ")
  (compile-form form))

(defemit :javascript array (&rest elements)
  (compiler-emit "[")
  (emit-separated ", " elements)
  (compiler-emit "]"))

(defemit :javascript typeof (x)
  (compiler-emit "typeof ")
  (compile-form x))

(defemit :javascript instanceof (x type-form)
  (compile-form x)
  (compiler-emit " instanceof ")
  (compile-form type-form))

(defemit :javascript for-each (binding &body body)
  (let ((item (first binding))
        (arr (second binding)))
    (compile-form arr)
    (compiler-emit ".forEach((")
    (emit-name item)
    (compiler-emit ") => ")
    (emit-block body)
    (compiler-emit ")")))

(defemit :javascript try (&rest forms)
  (let ((body nil)
        (catch-form nil))
    (dolist (f forms)
      (if (and (listp f) (eq (car f) 'catch))
          (setf catch-form f)
          (push f body)))
    (setf body (nreverse body))
    (compiler-emit "try ")
    (emit-block body)
    (when catch-form
      (let ((err-var (first (second catch-form)))
            (handler-body (cddr catch-form)))
        (compiler-emit " catch(")
        (emit-name err-var)
        (compiler-emit ") ")
        (emit-block handler-body)))))

(defemit :javascript throw (form)
  (compiler-emit "throw ")
  (compile-form form))

(defemit :javascript export (form)
  (compiler-emit "export ")
  (compile-form form))

(defemit :javascript export-default (form)
  (compiler-emit "export default ")
  (compile-form form))

(defemit :javascript import (what &key from)
  (if from
      (progn
        (compiler-emit "import ")
        (if (listp what)
            (progn
              (compiler-emit "{ ")
              (loop for (item . rest) on what
                    do (emit-name item)
                    when rest do (compiler-emit ", "))
              (compiler-emit " }"))
            (emit-name what))
        (compiler-emit " from ")
        (compiler-emit (format nil "\"~A\"" from)))
      ;; Dynamic import: import("path")
      (progn
        (compiler-emit "import(")
        (compile-form what)
        (compiler-emit ")"))))

(defemit :javascript comment (text)
  (compiler-emit (format nil "// ~A" text)))

(defemit :javascript in-file (path)
  (vfs-select-file *current-vfs* path))

(defemit :javascript destructuring-const (vars expr)
  (compiler-emit "const [")
  (loop for (var . rest) on vars
        do (emit-name var)
        when rest do (compiler-emit ", "))
  (compiler-emit "] = ")
  (compile-form expr))

(defemit :javascript progn (&rest forms)
  (loop for (form . rest) on forms
        for skip-semi-p = (or (js-statement-p form)
                              (and (listp form) (symbolp (car form))
                                   (member (symbol-name (car form)) '("IN-FILE" "PREPEND-PRAGMA") :test #'string=)))
        do (compile-form form)
           (when (and (not skip-semi-p) (js-file-p))
             (compiler-emit ";"))
        when rest do (compiler-emitln "")))

(defemit :javascript template (&rest parts)
  (compiler-emit "`")
  (dolist (part parts)
    (if (stringp part)
        (compiler-emit part)
        (progn
          (compiler-emit "${")
          (compile-form part)
          (compiler-emit "}"))))
  (compiler-emit "`"))

(defemit :javascript |?:| (condition then else)
  (compile-form condition)
  (compiler-emit " ? ")
  (compile-form then)
  (compiler-emit " : ")
  (compile-form else))

(defemit :javascript chain (obj &rest calls)
  (compile-form obj)
  (dolist (call calls)
    (when (listp call)
      (let* ((method-name (symbol-name (car call)))
             (clean-name (if (char= (char method-name 0) #\.)
                             (subseq method-name 1)
                             method-name)))
        (compiler-emit ".")
        (compiler-emit (kebab-to-camel (string-downcase clean-name)))
        (compiler-emit "(")
        (loop for (arg . rest) on (cdr call)
              do (compile-form arg)
              when rest do (compiler-emit ", "))
        (compiler-emit ")")))))

(defemit :javascript json-stringify (data)
  (compiler-emit "JSON.stringify(")
  (compile-form data)
  (compiler-emit ")"))

(defemit :javascript json-parse (data)
  (compiler-emit "JSON.parse(")
  (compile-form data)
  (compiler-emit ")"))

(defjsmacro :javascript map (arr fn)
  `(chain ,arr (.map ,fn)))

(defjsmacro :javascript filter (arr fn)
  `(chain ,arr (.filter ,fn)))

(defjsmacro :javascript reduce (arr fn &optional initial)
  (if initial
      `(chain ,arr (.reduce ,fn ,initial))
      `(chain ,arr (.reduce ,fn))))
