(in-package :hactar)

(defstruct fern-node
  type          ; keyword or symbol
  start-byte    ; integer
  end-byte      ; integer
  start-point   ; (row . col)
  end-point     ; (row . col)
  children      ; list
  parent        ; parent node reference
  named-p       ; boolean
  text)         ; text content

(defstruct fern-tree
  root-node
  source
  language
  version
  memo-table)

(defstruct fern-edit
  start-byte
  old-end-byte
  new-end-byte
  start-point
  old-end-point
  new-end-point)

(defvar *fern-grammars* (make-hash-table :test 'eq)
  "Registry of Fern grammars.")

(defclass fern-grammar ()
  ((name :initarg :name :accessor grammar-name)
   (extensions :initarg :extensions :accessor grammar-extensions)
   (rules :initarg :rules :accessor grammar-rules)
   (inherits :initarg :inherits :initform nil :accessor grammar-inherits)))

(defmacro defgrammar (name &rest options)
  "Define a Fern PEG grammar."
  (let ((rules (getf options :rules))
        (inherits (getf options :inherits))
        (extensions (getf options :file-extensions))
        (gname (getf options :name)))
    `(progn
       (setf (gethash ,name *fern-grammars*)
             (make-instance 'fern-grammar
                            :name ,gname
                            :extensions ',(if (listp extensions) extensions (list extensions))
                            :rules ',rules
                            :inherits ,inherits))
       ,name)))

;; Define TypeScript and TSX grammars
(defgrammar :typescript
  :name "TypeScript"
  :file-extensions ("ts" "mts" "cts")
  :rules
  ((program         (repeat statement))
   (statement       (choice import-statement
                             export-statement
                             function-declaration
                             class-declaration
                             variable-declaration
                             expression-statement))
   (import-statement (seq "import"
                           (choice (seq (named :default-import identifier)
                                        (optional (seq "," (named :named-imports named-import-list))))
                                   (named :named-imports named-import-list)
                                   (seq "*" "as" (named :namespace identifier)))
                           "from" (named :source string-literal) (optional ";")))
   (export-statement (seq "export"
                           (choice (seq "default" (named :value expression))
                                   (named :declaration (choice function-declaration
                                                               class-declaration
                                                               variable-declaration))
                                   (named :named-exports named-export-list))))
   (function-declaration (seq (optional "async")
                               "function"
                               (optional (named :name identifier))
                               (named :params parameter-list)
                               (named :body block)))
   (class-declaration (seq "class" (named :name identifier) (named :body block)))
   (variable-declaration (seq (choice "const" "let" "var")
                               (named :name identifier)
                               (optional (seq "=" (named :value expression)))
                               (optional ";")))
   (expression-statement (seq (named :expr expression) (optional ";")))
   (expression (choice call-expression identifier string-literal number-literal))
   (call-expression (seq (named :fn identifier) (named :args arguments)))
   (arguments (seq "(" (repeat (choice expression (seq "," expression))) ")"))
   (parameter-list (seq "(" (repeat (choice identifier (seq "," identifier))) ")"))
   (named-import-list (seq "{" (repeat (choice identifier (seq "," identifier))) "}"))
   (named-export-list (seq "{" (repeat (choice identifier (seq "," identifier))) "}"))
   (block (seq "{" (repeat statement) "}"))
   (identifier (seq (repeat (choice #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                                    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                                    #\_ #\$))))
   (string-literal (choice (seq "\"" (repeat (choice #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                                                    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                                                    #\/ #\. #\_ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\Space)) "\"")
                           (seq "'" (repeat (choice #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                                                    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                                                    #\/ #\. #\_ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\Space)) "'")))
   (number-literal (repeat (choice #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))))

(defgrammar :tsx
  :name "TSX"
  :file-extensions ("tsx")
  :inherits :typescript
  :rules
  ((expression      (choice call-expression jsx-element identifier string-literal number-literal))
   (jsx-element     (choice jsx-self-closing jsx-opening-closing))
   (jsx-self-closing (seq "<" (named :tag jsx-tag-name)
                           (repeat (named :attributes jsx-attribute))
                           "/>"))
   (jsx-opening-closing (seq (named :open jsx-opening-tag)
                              (repeat (named :children (choice jsx-element jsx-expression jsx-text)))
                              (named :close jsx-closing-tag)))
   (jsx-opening-tag (seq "<" (named :tag jsx-tag-name)
                         (repeat (named :attributes jsx-attribute))
                         ">"))
   (jsx-closing-tag (seq "</" (named :tag jsx-tag-name) ">"))
   (jsx-tag-name identifier)
   (jsx-attribute   (seq (named :name identifier)
                          (optional (seq "=" (named :value (choice string-literal jsx-expression))))))
   (jsx-expression  (seq "{" expression "}"))
   (jsx-text (repeat (choice #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                             #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                             #\Space #\Tab #\Newline #\Return)))))

;; Fern Parser Engine Implementation

(defun get-grammar-rules (lang)
  "Get all rules for a language, including inherited rules."
  (let ((grammar (gethash lang *fern-grammars*)))
    (when grammar
      (if (grammar-inherits grammar)
          (append (grammar-rules grammar)
                  (get-grammar-rules (grammar-inherits grammar)))
          (grammar-rules grammar)))))

(defun compute-row-col (source byte-index)
  "Convert byte index to (row . col) 1-based line/col."
  (let ((row 1)
        (col 1))
    (loop for i from 0 to (min (1- byte-index) (1- (length source))) do
          (if (char= (char source i) #\Newline)
              (progn
                (incf row)
                (setf col 1))
              (incf col)))
    (cons row col)))

(defun skip-whitespace (source pos)
  "Skip spaces, tabs, and newlines."
  (loop while (and (< pos (length source))
                   (member (char source pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

(defun parse-rule (rule-name source pos grammar memo)
  "Parse a rule at a position, returning (values node new-pos)."
  (let* ((memo-key (cons rule-name pos))
         (cached (gethash memo-key memo)))
    (when cached
      (return-from parse-rule (values (car cached) (cdr cached))))
    (let* ((pos-clean (skip-whitespace source pos))
           (rule-body (second (assoc rule-name grammar))))
      (unless rule-body
        (error "Grammar rule not found: ~A" rule-name))
      (multiple-value-bind (node new-pos) (parse-pattern rule-body source pos-clean grammar memo)
        (if node
            (let ((final-node (make-fern-node
                               :type rule-name
                               :start-byte pos-clean
                               :end-byte new-pos
                               :start-point (compute-row-col source pos-clean)
                               :end-point (compute-row-col source new-pos)
                               :children (if (listp (fern-node-children node))
                                             (fern-node-children node)
                                             (list node))
                               :named-p t
                               :text (subseq source pos-clean new-pos))))
              ;; Set parents
              (dolist (child (fern-node-children final-node))
                (when (fern-node-p child)
                  (setf (fern-node-parent child) final-node)))
              (setf (gethash memo-key memo) (cons final-node new-pos))
              (values final-node new-pos))
            (progn
              (setf (gethash memo-key memo) (cons nil pos))
              (values nil pos)))))))

(defun parse-pattern (pattern source pos grammar memo)
  "Parse a PEG pattern at a position, returning (values node-or-children new-pos)."
  (cond
    ((stringp pattern)
     ;; Literal string match
     (let ((len (length pattern)))
       (if (and (<= (+ pos len) (length source))
                (string= pattern (subseq source pos (+ pos len))))
           (let ((new-pos (+ pos len)))
             (values (make-fern-node
                      :type :literal
                      :start-byte pos
                      :end-byte new-pos
                      :start-point (compute-row-col source pos)
                      :end-point (compute-row-col source new-pos)
                      :named-p nil
                      :text pattern)
                     new-pos))
           (values nil pos))))
    ((characterp pattern)
     ;; Literal character match
     (if (and (< pos (length source))
              (char= pattern (char source pos)))
         (let ((new-pos (1+ pos)))
           (values (make-fern-node
                    :type :literal
                    :start-byte pos
                    :end-byte new-pos
                    :start-point (compute-row-col source pos)
                    :end-point (compute-row-col source new-pos)
                    :named-p nil
                    :text (string pattern))
                   new-pos))
         (values nil pos)))
    ((symbolp pattern)
     ;; Reference to another rule
     (parse-rule pattern source pos grammar memo))
    ((consp pattern)
     (let ((op (car pattern))
           (args (cdr pattern)))
       (case op
         (seq
          (let ((pos-curr pos)
                (children '()))
            (dolist (pat args)
              (let ((pos-clean (skip-whitespace source pos-curr)))
                (multiple-value-bind (res new-pos) (parse-pattern pat source pos-clean grammar memo)
                  (if res
                      (progn
                        (if (and (fern-node-p res) (eq (fern-node-type res) :seq))
                            (setf children (append children (fern-node-children res)))
                            (push res children))
                        (setf pos-curr new-pos))
                      (return-from parse-pattern (values nil pos))))))
            (values (make-fern-node :type :seq :children (nreverse children)) pos-curr)))
         (choice
          (dolist (pat args)
            (multiple-value-bind (res new-pos) (parse-pattern pat source pos grammar memo)
              (when res
                (return-from parse-pattern (values res new-pos)))))
          (values nil pos))
         (optional
          (multiple-value-bind (res new-pos) (parse-pattern (first args) source pos grammar memo)
            (if res
                (values res new-pos)
                (values (make-fern-node :type :optional) pos))))
         (repeat
          (let ((pos-curr pos)
                (children '()))
            (loop
              (let ((pos-clean (skip-whitespace source pos-curr)))
                (multiple-value-bind (res new-pos) (parse-pattern (first args) source pos-clean grammar memo)
                  (if (and res (/= new-pos pos-curr))
                      (progn
                        (push res children)
                        (setf pos-curr new-pos))
                      (return)))))
            (values (make-fern-node :type :repeat :children (nreverse children)) pos-curr)))
         (named
          (let ((name (first args))
                (pat (second args)))
            (multiple-value-bind (res new-pos) (parse-pattern pat source pos grammar memo)
              (if res
                  (progn
                    ;; Wrap or tag the result node with the name keyword
                    (let ((node (if (fern-node-p res)
                                    res
                                    (make-fern-node :type :seq :children (if (listp res) res (list res))))))
                      (setf (fern-node-type node) name)
                      (values node new-pos)))
                  (values nil pos))))))))))

(defun fern-parse (lang source &optional old-tree)
  "Parse source string using the grammar for language LANG."
  (declare (ignore old-tree))
  (let* ((grammar (get-grammar-rules lang))
         (memo (make-hash-table :test 'equal))
         (top-rule (car (first grammar))))
    (multiple-value-bind (root-node end-pos) (parse-rule top-rule source 0 grammar memo)
      (make-fern-tree
       :root-node root-node
       :source source
       :language lang
       :version 1
       :memo-table memo))))

(defun fern-edit (tree edit)
  "Incremental reparse of a tree after applying an edit.
   In this implementation, we invalidate memo table entries that overlap the edit region
   and shift positions of entries that appear after the edit."
  (let* ((start (fern-edit-start-byte edit))
         (old-end (fern-edit-old-end-byte edit))
         (new-end (fern-edit-new-end-byte edit))
         (diff (- new-end old-end))
         (old-memo (fern-tree-memo-table tree))
         (new-memo (make-hash-table :test 'equal))
         (source (fern-tree-source tree))
         (grammar (get-grammar-rules (fern-tree-language tree)))
         (top-rule (car (first grammar))))
    ;; 1. Copy unaffected/shifted memo table entries
    (maphash (lambda (key val)
               (let ((rule-name (car key))
                     (pos (cdr key))
                     (node (car val))
                     (end-pos (cdr val)))
                 (cond
                   ;; Completely before edit range: keep
                   ((< end-pos start)
                    (setf (gethash key new-memo) val))
                   ;; Overlaps edit range: discard (do nothing)
                   ((and (< pos old-end) (>= end-pos start))
                    nil)
                   ;; Completely after edit range: shift positions
                   ((>= pos old-end)
                    (let ((new-key (cons rule-name (+ pos diff)))
                          (new-val (cons node (+ end-pos diff))))
                      ;; Adjust node byte positions
                      (when (fern-node-p node)
                        (incf (fern-node-start-byte node) diff)
                        (incf (fern-node-end-byte node) diff))
                      (setf (gethash new-key new-memo) new-val))))))
             old-memo)
    ;; 2. Run parser using the updated memo table
    (multiple-value-bind (new-root) (parse-rule top-rule source 0 grammar new-memo)
      (make-fern-tree
       :root-node new-root
       :source source
       :language (fern-tree-language tree)
       :version (1+ (fern-tree-version tree))
       :memo-table new-memo))))

;; Fern S-Expression Query Engine

(defun get-node-named-child (node type)
  "Get the child of node that has type equal to TYPE keyword."
  (find type (fern-node-children node) :key #'fern-node-type))

(defun fern-node-text-fallback (node)
  "Return node text or construct it from children."
  (or (fern-node-text node)
      (if (fern-node-children node)
          (str:join " " (mapcar #'fern-node-text-fallback (fern-node-children node)))
          "")))

(defun match-query-pattern (pattern node)
  "Match a pattern query against a node. Returns alist of bindings on success, or NIL."
  (unless node (return-from match-query-pattern nil))
  (cond
    ((symbolp pattern)
     (if (string-prefix-p "@" (symbol-name pattern))
         (list (cons pattern node))
         (when (eq (fern-node-type node) pattern)
           (list (cons :match t)))))
    ((consp pattern)
     (let ((op (car pattern))
           (args (cdr pattern)))
       (cond
         ((eq op :match)
          ;; Constraint check: (:match "@name" "regex")
          (list (cons :constraint pattern)))
         (t
          (when (eq (fern-node-type node) op)
            (let ((bindings '()))
              (dolist (pat args)
                (if (and (consp pat) (eq (car pat) :match))
                    (push (cons :constraint pat) bindings)
                    (let ((matched nil))
                      ;; Try to match any child
                      (dolist (child (fern-node-children node))
                        (let ((res (match-query-pattern pat child)))
                          (when res
                            (setf matched t)
                            (setf bindings (append bindings (remove-if (lambda (b) (eq (car b) :match)) res)))
                            (return))))
                      (unless matched
                        (return-from match-query-pattern nil)))))
              (or bindings (list (cons :match t)))))))))))

(defun evaluate-constraints (bindings source)
  "Evaluate query regex constraints."
  (dolist (b bindings)
    (when (eq (car b) :constraint)
      (let* ((pat (cdr b))
             (var (second pat))
             (regex (third pat))
             (var-sym (intern var :keyword))
             (matched-node (cdr (assoc var-sym bindings))))
        (when matched-node
          (let ((text (fern-node-text-fallback matched-node)))
            (unless (cl-ppcre:scan regex text)
              (return-from evaluate-constraints nil)))))))
  t)

(defun fern-query-node (node pattern source)
  "Recursively search and query a node."
  (let ((results '())
        (match (match-query-pattern pattern node)))
    (when match
      (let ((clean-bindings (remove-if (lambda (b) (or (eq (car b) :match) (eq (car b) :constraint))) match)))
        (when (evaluate-constraints match source)
          (push clean-bindings results))))
    (dolist (child (fern-node-children node))
      (setf results (append results (fern-query-node child pattern source))))
    results))

(defun fern-query (tree pattern)
  "Query the fern tree with pattern, returning a list of bindings."
  (fern-query-node (fern-tree-root-node tree) pattern (fern-tree-source tree)))

(defun fern-query-nodes (tree pattern)
  "Query the fern tree with pattern, returning a list of matching fern-nodes."
  (let ((results '()))
    (labels ((walk (node)
               (when node
                 (when (match-query-pattern pattern node)
                   (push node results))
                 (dolist (child (fern-node-children node))
                   (walk child)))))
      (walk (fern-tree-root-node tree))
      (nreverse results))))
