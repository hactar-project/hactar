(in-package :hactar)

(deftarget :css
  :file-extension "css")

(defvar *css-html-tags*
  '("html" "body" "head" "div" "span" "p" "a" "h1" "h2" "h3" "h4" "h5" "h6"
    "ul" "ol" "li" "table" "tr" "td" "th" "form" "input" "button" "textarea"
    "select" "option" "label" "img" "link" "meta" "title" "script" "style"
    "nav" "header" "footer" "main" "section" "article" "aside"
    "br" "hr" "pre" "code" "blockquote" "strong" "em" "i" "b" "u"
    "iframe" "video" "audio" "source" "canvas" "svg")
  "HTML tag names that should be emitted as bare selectors, not pseudo-selectors.")

(defun css-selector-from-form (selector)
  "Normalize a selector form into a CSS selector string."
  (cond
    ((keywordp selector)
     (let ((name (string-downcase (symbol-name selector))))
       (cond
         ((and (> (length name) 1)
               (char= (char name 0) #\-)
               (char= (char name 1) #\-))
          name)
         ((member name *css-html-tags* :test #'string=)
          name)
         (t (format nil ":~A" name)))))
    ((stringp selector) selector)
    ((symbolp selector) (string-downcase (symbol-name selector)))
    (t (error "Unsupported CSS selector: ~S" selector))))

(defun css-property-name (property)
  "Normalize a property keyword into a CSS property string."
  (unless (keywordp property)
    (error "CSS property must be a keyword, got: ~S" property))
  (string-downcase (symbol-name property)))

(defun css-at-rule-p (selector)
  "Return T when SELECTOR is an at-rule string like @media."
  (and (stringp selector)
       (> (length selector) 0)
       (char= (char selector 0) #\@)))

(defun css-combine-selectors (parent child)
  "Combine PARENT and CHILD selector strings for simple nested selector support."
  (cond
    ((null parent) child)
    ((null child) parent)
    ((and (> (length child) 0)
          (member (char child 0) '(#\> #\+ #\~)))
     (format nil "~A ~A" parent child))
    (t
     (format nil "~A ~A" parent child))))

(defvar *css-unitless-properties*
  '("flex" "flex-grow" "flex-shrink" "order" "z-index" "opacity"
    "font-weight" "line-height" "orphans" "widows" "columns"
    "column-count" "fill-opacity" "stroke-opacity" "tab-size")
  "CSS properties that take unitless numeric values.")

(defun css-property-unitless-p (property)
  "Return T if PROPERTY (a keyword or string) should not have px units added."
  (let ((name (if (keywordp property)
                  (string-downcase (symbol-name property))
                  (string-downcase property))))
    (member name *css-unitless-properties* :test #'string=)))

(defun css-emit-value (value &optional property)
  "Emit a CSS value. Non-zero bare numbers get 'px' units unless the property is unitless."
  (cond
    ((stringp value) (compiler-emit value))
    ((numberp value)
     (cond
       ((zerop value) (compiler-emit "0"))
       ((and property (css-property-unitless-p property))
        (compiler-emit (format nil "~A" value)))
       (t (compiler-emit (format nil "~Apx" value)))))
    ((keywordp value) (compiler-emit (string-downcase (symbol-name value))))
    ((symbolp value) (compiler-emit (string-downcase (symbol-name value))))
    (t (error "Unsupported CSS value: ~S" value))))

(defun css-split-body (body)
  "Split BODY into declaration pairs and nested rule forms."
  (let ((declarations nil)
        (nested-rules nil)
        (remaining body))
    (loop while remaining
          do (let ((item (car remaining)))
               (cond
                 ((keywordp item)
                  (unless (cdr remaining)
                    (error "Missing CSS value for property ~S" item))
                  (push (list item (cadr remaining)) declarations)
                  (setf remaining (cddr remaining)))
                 ((listp item)
                  (push item nested-rules)
                  (setf remaining (cdr remaining)))
                 (t
                  (error "Unsupported CSS rule body item: ~S" item)))))
    (values (nreverse declarations) (nreverse nested-rules))))

(defun register-css-emitters (target)
  "Register CSS emission forms on TARGET."
  (let ((target-keyword target))
    (defemit target-keyword css-decl (property value)
      (emit-indent)
      (compiler-emit (css-property-name property))
      (compiler-emit ": ")
      (css-emit-value value property)
      (compiler-emitln ";"))

    (defemit target-keyword css-rule (selector declarations &rest nested)
      (let ((selector-string (css-selector-from-form selector)))
        (compiler-emit selector-string)
        (compiler-emitln " {")
        (with-indent
          (dolist (declaration declarations)
            (apply (find-emitter *current-target* 'css-decl) declaration))
          (dolist (nested-form nested)
            (emit-indent)
            (compile-form nested-form)
            (compiler-emitln "")))
        (compiler-emit "}")))

    (defemit target-keyword css-nested-rule (parent selector declarations &rest nested)
      (let* ((selector-string (css-selector-from-form selector))
             (combined-selector (css-combine-selectors parent selector-string)))
        (apply (find-emitter *current-target* 'css-rule)
               combined-selector declarations nested)))

    (defemit target-keyword css-at-rule (at-rule selector declarations &rest nested)
      (compiler-emit at-rule)
      (compiler-emitln " {")
      (with-indent
        (emit-indent)
        (apply (find-emitter *current-target* 'css-rule)
               selector declarations nested)
        (compiler-emitln ""))
      (compiler-emit "}"))

    (defemit target-keyword css-at-rule-bare (at-rule declarations)
      (compiler-emit at-rule)
      (compiler-emitln " {")
      (with-indent
        (dolist (declaration declarations)
          (apply (find-emitter *current-target* 'css-decl) declaration)))
      (compiler-emit "}"))

    (defemit target-keyword css-stylesheet (&rest rules)
      (loop for (rule . rest) on rules
            do (compile-form rule)
               (when rest
                 (compiler-emitln "")
                 (compiler-emitln ""))))

    (setf (gethash 'defcss (target-definition-macros (gethash target-keyword *compiler-targets*)))
          (lambda (name &rest rules)
            (declare (ignore name))
            `(css-stylesheet
              ,@(loop for rule in rules
                      append (css-expand-rule rule)))))))

(defun css-expand-rule (rule &optional parent-selector)
  "Expand a user rule into normalized CSS forms."
  (unless (and (listp rule) (consp rule))
    (error "CSS rule must be a non-empty list, got: ~S" rule))
  (let ((selector (first rule))
        (body (rest rule)))
    (multiple-value-bind (declarations nested-rules)
        (css-split-body body)
      (let ((normalized-selector (css-selector-from-form selector)))
        (append
         (list
          (if (css-at-rule-p normalized-selector)
              (if parent-selector
                  `(css-at-rule ,normalized-selector ,parent-selector ,declarations)
                  `(css-at-rule-bare ,normalized-selector ,declarations))
              (if parent-selector
                  `(css-nested-rule ,parent-selector ,selector ,declarations)
                  `(css-rule ,selector ,declarations))))
         (loop for nested-rule in nested-rules
               append
               (if (css-at-rule-p (css-selector-from-form (first nested-rule)))
                   (let ((nested-selector (or parent-selector normalized-selector)))
                     (list `(css-at-rule ,(css-selector-from-form (first nested-rule))
                                         ,nested-selector
                                         ,(multiple-value-bind (nested-decls nested-children)
                                              (css-split-body (rest nested-rule))
                                            (declare (ignore nested-children))
                                            nested-decls))))
                   (css-expand-rule nested-rule
                                    (if parent-selector
                                        (css-combine-selectors parent-selector normalized-selector)
                                        normalized-selector)))))))))
(register-css-emitters :css)

(defjsmacro :css defcss (name &rest rules)
  (declare (ignore name))
  `(css-stylesheet
     ,@(loop for rule in rules
             append (css-expand-rule rule))))
