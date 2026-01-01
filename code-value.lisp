;;; code-value.lisp — Code as a First-Class Value
;;;
;;; Zero LLM default: most operations use AST/patterns, not LLM.
;;; LLM operations (describe, ask, generate) are explicit opt-in.

(in-package :hactar)

;;* code value structure

(defstruct code-value
  "Immutable semantic wrapper around code. Transforms create new values
   with the original as parent. Most fields populated without LLM;
   intent/inputs/outputs/effects/invariants require LLM (via code/describe)."
  (id nil :type (or null string))
  (name nil :type (or null string symbol))
  (source-file nil :type (or null pathname string))
  (source-region nil :type (or null cons))
  (source-text "" :type string)
  (language nil :type (or null string))
  (ast nil)
  (symbols nil :type list)
  (imports nil :type list)
  (exports nil :type list)
  (type-info nil)
  (intent nil :type (or null string))
  (inputs nil :type list)
  (outputs nil :type list)
  (effects nil :type list)
  (invariants nil :type list)
  (origin nil :type (or null keyword))
  (parent nil :type (or null code-value))
  (transformation nil :type (or null string))
  (transform-name nil :type (or null string symbol))
  (staged-p nil :type boolean)
  (applied-p nil :type boolean)
  (verified-p nil :type boolean)
  (verification-result nil)
  (verification-errors nil :type list)
  (timestamp nil :type (or null integer)))

;;* ID generation

(defun generate-code-value-id ()
  "Generate a unique ID for a code-value."
  (incf *code-value-counter*)
  (format nil "cv-~A-~A" (get-universal-time) *code-value-counter*))

;;* code value registry

(defun register-code-value (cv)
  "Register a code-value in the global registry."
  (setf (gethash (code-value-id cv) *code-values*) cv)
  cv)

(defun get-code-value (id)
  "Retrieve a code-value by ID."
  (gethash id *code-values*))

(defun list-code-values ()
  "Return a list of all registered code-values."
  (let ((values '()))
    (maphash (lambda (k v) 
               (declare (ignore k))
               (push v values))
             *code-values*)
    (nreverse values)))

(defun clear-code-values ()
  "Clear all registered code-values."
  (clrhash *code-values*)
  (setf *staged-code-values* '()))

;;* selection operations

(defun code/select (source &key function class method region name)
  "Select code from a source and wrap it as a code-value.
   
   SOURCE can be:
   - A file path (string or pathname)
   - A file path with line range: \"file.py:10-20\"
   
   Keyword arguments for precise selection:
   - :function NAME - Select a specific function
   - :class NAME - Select a specific class
   - :method NAME - Select a specific method (requires :class)
   - :region (START . END) - Select specific line range
   - :name - Optional name to bind this code-value to
   
   Returns the created code-value."
  (let* ((parsed (parse-source-specifier source))
         (file-path (car parsed))
         (line-range (or region (cdr parsed)))
         (full-path (resolve-code-path file-path))
         (content (when (probe-file full-path)
                    (uiop:read-file-string full-path)))
         (selected-text nil)
         (actual-region nil))
    
    (unless content
      (error "Cannot read file: ~A" full-path))
    
    (cond
      (line-range
       (multiple-value-setq (selected-text actual-region)
         (extract-lines content (car line-range) (cdr line-range))))
      
      (function
       (multiple-value-setq (selected-text actual-region)
         (extract-function content function full-path)))
      
      (class
       (multiple-value-setq (selected-text actual-region)
         (extract-class content class method full-path)))
      
      (t
       (setf selected-text content)
       (setf actual-region nil)))
    
    (let ((cv (make-code-value
               :id (generate-code-value-id)
               :source-file full-path
               :source-region actual-region
               :source-text selected-text
               :language (get-language-hint-from-extension 
                          (pathname-type (pathname full-path)))
               :origin :selection
               :timestamp (get-universal-time))))
      
      (register-code-value cv)
      
      (when name
        (setf (gethash name *code-values*) cv))
      
      cv)))

(defun code/from-string (text &key language name)
  "Create a code-value from a string of code.
   Useful for generated code or code from other sources."
  (let ((cv (make-code-value
             :id (generate-code-value-id)
             :source-file nil
             :source-region nil
             :source-text text
             :language language
             :origin :generated
             :timestamp (get-universal-time))))
    
    (register-code-value cv)
    (when name
      (setf (gethash name *code-values*) cv))
    cv))

(defun code/from-context (file-path)
  "Create a code-value from a file already in the Hactar context."
  (let ((native-path (if (pathnamep file-path)
                         (uiop:native-namestring file-path)
                         file-path)))
    (if (member native-path *files* :test #'string=)
        (code/select native-path)
        (progn
          (format t "~&File not in context: ~A~%" file-path)
          nil))))

;;** selection helpers

(defun parse-source-specifier (source)
  "Parse a source specifier like 'file.py:10-20' into (path . (start . end))."
  (let* ((source-str (if (pathnamep source) 
                         (namestring source) 
                         source))
         (colon-pos (position #\: source-str :from-end t))
         (has-line-range (and colon-pos 
                              (> colon-pos 0)
                              (not (and (= colon-pos 1) 
                                        (alpha-char-p (char source-str 0)))))))
    (if has-line-range
        (let* ((path (subseq source-str 0 colon-pos))
               (range-str (subseq source-str (1+ colon-pos)))
               (dash-pos (position #\- range-str)))
          (if dash-pos
              (cons path (cons (parse-integer (subseq range-str 0 dash-pos))
                               (parse-integer (subseq range-str (1+ dash-pos)))))
              (cons path nil)))
        (cons source-str nil))))

(defun resolve-code-path (file-path)
  "Resolve a file path relative to repo root."
  (let ((path (pathname file-path)))
    (if (uiop:absolute-pathname-p path)
        path
        (merge-pathnames path *repo-root*))))

(defun extract-lines (content start-line end-line)
  "Extract lines START-LINE through END-LINE from CONTENT.
   Returns (values extracted-text (start . end))."
  (let* ((lines (str:lines content))
         (start-idx (max 0 (1- start-line)))
         (end-idx (min (length lines) end-line))
         (selected (subseq lines start-idx end-idx)))
    (values (str:join #\Newline selected)
            (cons start-line end-line))))

(defun extract-function (content function-name file-path)
  "Use LLM to extract a function from content.
   Returns (values function-text (start-line . end-line))."
  (let* ((prompt (format nil "Extract the function named '~A' from this code. ~
                              Return ONLY the function code, nothing else. ~
                              Also include a comment on the first line indicating ~
                              the line numbers in format: // Lines: START-END~%~%~A"
                         function-name content))
         (response (get-llm-response prompt 
                                     :stream nil 
                                     :add-to-history nil
                                     :custom-system-prompt "You are a code extraction tool. Extract exactly what is requested.")))
    (if response
        (let ((lines (parse-line-range-from-response response)))
          (values (remove-line-comment response) lines))
        (values nil nil))))

(defun extract-class (content class-name method-name file-path)
  "Use LLM to extract a class (and optionally a specific method) from content.
   Returns (values class-text (start-line . end-line))."
  (let* ((what (if method-name
                   (format nil "method '~A' from class '~A'" method-name class-name)
                   (format nil "class '~A'" class-name)))
         (prompt (format nil "Extract the ~A from this code. ~
                              Return ONLY the code, nothing else. ~
                              Include a comment on the first line: // Lines: START-END~%~%~A"
                         what content))
         (response (get-llm-response prompt 
                                     :stream nil 
                                     :add-to-history nil
                                     :custom-system-prompt "You are a code extraction tool.")))
    (if response
        (let ((lines (parse-line-range-from-response response)))
          (values (remove-line-comment response) lines))
        (values nil nil))))

(defun parse-line-range-from-response (response)
  "Parse a line range comment like '// Lines: 10-20' from response."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "(?://|#|--) Lines?: (\\d+)-(\\d+)" response)
    (when (and match groups (>= (length groups) 2))
      (cons (parse-integer (aref groups 0))
            (parse-integer (aref groups 1))))))

(defun remove-line-comment (text)
  "Remove the first line if it's a Lines: comment."
  (let ((lines (str:lines text)))
    (if (and lines 
             (cl-ppcre:scan "(?://|#|--) Lines?:" (first lines)))
        (str:join #\Newline (rest lines))
        text)))

;;* inspection operations

(defun code/describe (cv &key force)
  "Populate semantic fields of a code-value using LLM.
   Does nothing if already populated unless FORCE is t.
   Returns the code-value with semantic fields filled."
  (when (or force (null (code-value-intent cv)))
    (let* ((prompt (format nil "Analyze this code and provide:
1. INTENT: A one-sentence description of what this code does
2. INPUTS: What inputs/arguments it expects (list each)
3. OUTPUTS: What it returns or produces
4. EFFECTS: Any side effects (IO, state changes, etc.)
5. INVARIANTS: What must always be true about this code

Code:
```~A
~A
```

Respond in this exact format:
INTENT: <description>
INPUTS: <comma-separated list>
OUTPUTS: <comma-separated list>
EFFECTS: <comma-separated list or 'none'>
INVARIANTS: <comma-separated list or 'none'>"
                           (or (code-value-language cv) "")
                           (code-value-source-text cv)))
           (response (get-llm-response prompt 
                                       :stream nil 
                                       :add-to-history nil
                                       :custom-system-prompt "You are a code analysis tool. Be precise and concise.")))
      (when response
        (parse-describe-response cv response))))
  cv)

(defun parse-describe-response (cv response)
  "Parse the LLM response and populate code-value fields."
  (let ((lines (str:lines response)))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ((str:starts-with? "INTENT:" trimmed)
           (setf (code-value-intent cv) 
                 (string-trim '(#\Space) (subseq trimmed 7))))
          ((str:starts-with? "INPUTS:" trimmed)
           (setf (code-value-inputs cv)
                 (parse-comma-list (subseq trimmed 7))))
          ((str:starts-with? "OUTPUTS:" trimmed)
           (setf (code-value-outputs cv)
                 (parse-comma-list (subseq trimmed 8))))
          ((str:starts-with? "EFFECTS:" trimmed)
           (setf (code-value-effects cv)
                 (parse-comma-list (subseq trimmed 8))))
          ((str:starts-with? "INVARIANTS:" trimmed)
           (setf (code-value-invariants cv)
                 (parse-comma-list (subseq trimmed 11))))))))
  cv)

(defun parse-comma-list (str)
  "Parse a comma-separated list, filtering out 'none'."
  (let* ((trimmed (string-trim '(#\Space #\Tab) str))
         (items (str:split "," trimmed)))
    (remove-if (lambda (s) 
                 (or (string= s "")
                     (string-equal (string-trim '(#\Space) s) "none")))
               (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s)) items))))

(defun code/source (cv)
  "Return the source text of a code-value."
  (code-value-source-text cv))

(defun code/intent (cv)
  "Return the intent of a code-value, computing if necessary."
  (unless (code-value-intent cv)
    (code/describe cv))
  (code-value-intent cv))

(defun code/ask (cv question)
  "Ask a question about the code-value.
   Returns the LLM's answer as a string."
  (let* ((prompt (format nil "About this code:
```~A
~A
```

Question: ~A"
                         (or (code-value-language cv) "")
                         (code-value-source-text cv)
                         question))
         (response (get-llm-response prompt 
                                     :stream nil 
                                     :add-to-history nil)))
    response))

;;* history & provenance

(defun code/history (cv)
  "Return the chain of transformations that led to this code-value.
   Returns a list from oldest (original) to newest (cv)."
  (let ((chain '()))
    (loop for current = cv then (code-value-parent current)
          while current
          do (push current chain))
    chain))

(defun code/parent (cv)
  "Return the parent code-value (one step back in history)."
  (code-value-parent cv))

(defun code/original (cv)
  "Return the root/original code-value in the transformation chain."
  (let ((current cv))
    (loop while (code-value-parent current)
          do (setf current (code-value-parent current)))
    current))

(defun code/transformation-description (cv)
  "Return the transformation that created this code-value from its parent."
  (code-value-transformation cv))

;;* display & formatting

(defmethod print-object ((cv code-value) stream)
  "Pretty print a code-value."
  (print-unreadable-object (cv stream :type t)
    (format stream "~A" (code-value-id cv))
    (when (code-value-source-file cv)
      (format stream " ~A" (file-namestring (code-value-source-file cv))))
    (when (code-value-source-region cv)
      (format stream ":~A-~A" 
              (car (code-value-source-region cv))
              (cdr (code-value-source-region cv))))
    (when (code-value-intent cv)
      (format stream " ~S" (str:shorten 40 (code-value-intent cv))))))

(defun code/summary (cv)
  "Return a short summary string for a code-value."
  (format nil "~A: ~A~@[ (~A)~]"
          (code-value-id cv)
          (or (code-value-intent cv) "(not analyzed)")
          (when (code-value-parent cv) "transformed")))

(defun code/show (cv &key (stream *standard-output*))
  "Display a code-value with its metadata."
  (format stream "~&=== Code Value: ~A ===~%" (code-value-id cv))
  (when (code-value-source-file cv)
    (format stream "Source: ~A" (uiop:native-namestring (code-value-source-file cv)))
    (when (code-value-source-region cv)
      (format stream " (lines ~A-~A)" 
              (car (code-value-source-region cv))
              (cdr (code-value-source-region cv))))
    (format stream "~%"))
  (format stream "Language: ~A~%" (or (code-value-language cv) "unknown"))
  (format stream "Origin: ~A~%" (code-value-origin cv))
  (when (code-value-parent cv)
    (format stream "Parent: ~A~%" (code-value-id (code-value-parent cv)))
    (format stream "Transformation: ~A~%" (code-value-transformation cv)))
  (when (code-value-intent cv)
    (format stream "~%Intent: ~A~%" (code-value-intent cv)))
  (when (code-value-inputs cv)
    (format stream "Inputs: ~{~A~^, ~}~%" (code-value-inputs cv)))
  (when (code-value-outputs cv)
    (format stream "Outputs: ~{~A~^, ~}~%" (code-value-outputs cv)))
  (when (code-value-effects cv)
    (format stream "Effects: ~{~A~^, ~}~%" (code-value-effects cv)))
  (format stream "~%--- Source ---~%~A~%--- End ---~%" (code-value-source-text cv))
  cv)

;;* REPL commands

(define-command code/select (args)
  "Select code and create a code-value.
Usage: /code/select <file> [--function NAME] [--class NAME] [--lines START-END] [--name NAME]
Examples:
  /code/select src/auth.ts
  /code/select src/auth.ts --function validateToken
  /code/select src/auth.ts --lines 10-50 --name my-selection"
  (let* ((file (first args))
         (rest-args (rest args))
         (function-name (getf-string rest-args "--function"))
         (class-name (getf-string rest-args "--class"))
         (lines-str (getf-string rest-args "--lines"))
         (name (getf-string rest-args "--name"))
         (region (when lines-str
                   (let ((parts (str:split "-" lines-str)))
                     (when (= (length parts) 2)
                       (cons (parse-integer (first parts))
                             (parse-integer (second parts))))))))
    (if file
        (let ((cv (code/select file 
                               :function function-name 
                               :class class-name
                               :region region
                               :name name)))
          (when cv
            (format t "~&Created: ~A~%" cv)
            (when (code-value-source-region cv)
              (format t "Selected lines ~A-~A~%" 
                      (car (code-value-source-region cv))
                      (cdr (code-value-source-region cv))))
            (format t "~A characters of ~A code~%" 
                    (length (code-value-source-text cv))
                    (or (code-value-language cv) "unknown"))))
        (format t "Usage: /cv/select <file> [--function NAME] [--class NAME] [--lines START-END]~%"))))

(define-command code/list (args)
  "List all registered code-values."
  (declare (ignore args))
  (let ((values (list-code-values)))
    (if values
        (dolist (cv values)
          (format t "~&~A~%" (code/summary cv)))
        (format t "~&No code-values registered.~%"))))

(define-command code/show (args)
  "Show details of a code-value by ID or name.
Usage: /code/show <id-or-name>"
  (let* ((id (first args))
         (cv (when id (get-code-value id))))
    (if cv
        (code/show cv)
        (format t "~&Code-value not found: ~A~%" id))))

(define-command code/describe (args)
  "Analyze a code-value semantically using LLM.
Usage: /code/describe <id-or-name>"
  (let* ((id (first args))
         (cv (when id (get-code-value id))))
    (if cv
        (progn
          (format t "~&Analyzing code...~%")
          (code/describe cv :force t)
          (code/show cv))
        (format t "~&Code-value not found: ~A~%" id))))

(define-command code/ask (args)
  "Ask a question about a code-value.
Usage: /code/ask <id-or-name> <question>"
  (let* ((id (first args))
         (question (str:join " " (rest args)))
         (cv (when id (get-code-value id))))
    (if cv
        (if (and question (not (string= question "")))
            (let ((answer (code/ask cv question)))
              (format t "~&~A~%" answer))
            (format t "~&Please provide a question.~%"))
        (format t "~&Code-value not found: ~A~%" id))))

(define-command code/clear (args)
  "Clear all registered code-values."
  (declare (ignore args))
  (when (confirm-action "Clear all code-values?")
    (clear-code-values)
    (format t "~&All code-values cleared.~%")))

;;* transform definition system (zero LLM)

(defstruct transform-def
  "A reusable code transformation."
  (name nil :type (or null string symbol))
  (description nil :type (or null string))
  (when-fn nil :type (or null function))     ; Predicate: should this apply?
  (pattern nil)                               ; Search/replace pattern
  (template nil :type (or null string))       ; Template to apply
  (template-vars nil :type list)              ; How to extract template variables
  (compose nil :type list)                    ; Other transforms to compose
  (defaults nil :type list))                  ; Default values for parameters

(defmacro deftransform (name description &body body)
  "Define a reusable code transformation (zero LLM).
   
   Usage:
   (deftransform add-rate-limit
     \"Add rate limiting middleware\"
     :when (and (express/handler? cv) (not (has-rate-limit? cv)))
     :pattern (:search \"router.{{method}}\" :replace \"router.{{method}}, rateLimit,\")
     :defaults (:max-requests 100))"
  (let ((when-clause (getf body :when))
        (pattern (getf body :pattern))
        (template (getf body :template))
        (template-vars (getf body :template-vars))
        (compose (getf body :compose))
        (defaults (getf body :defaults)))
    `(setf (gethash ,(if (stringp name) name (string-downcase (symbol-name name)))
                    *transforms*)
           (make-transform-def
            :name ',name
            :description ,description
            :when-fn ,(when when-clause `(lambda (cv) ,when-clause))
            :pattern ',pattern
            :template ,template
            :template-vars ',template-vars
            :compose ',compose
            :defaults ',defaults))))

(defun get-transform (name)
  "Get a transform definition by name."
  (gethash (if (stringp name) name (string-downcase (symbol-name name)))
           *transforms*))

(defun list-transforms ()
  "Return a list of all defined transform names."
  (let ((names '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *transforms*)
    (nreverse names)))

;;* transform application (zero LLM)

(defun code/transform (cv transform &rest args)
  "Apply a transform to a code-value, returning a new code-value.
   TRANSFORM can be a transform name (symbol/string) or a transform-def.
   This operation is ZERO LLM - uses patterns and templates only."
  (let* ((transform-def (if (transform-def-p transform)
                            transform
                            (get-transform transform)))
         (params (append args (when transform-def 
                                (transform-def-defaults transform-def)))))
    (unless transform-def
      (error "Unknown transform: ~A" transform))
    
    (when (and (transform-def-when-fn transform-def)
               (not (funcall (transform-def-when-fn transform-def) cv)))
      (format t "~&Transform ~A precondition not met for ~A~%" 
              (transform-def-name transform-def) (code-value-id cv))
      (return-from code/transform cv))
    
    (when (transform-def-compose transform-def)
      (return-from code/transform
        (reduce (lambda (current-cv sub-transform)
                  (apply #'code/transform current-cv sub-transform 
                         (when (listp sub-transform) (rest sub-transform))))
                (transform-def-compose transform-def)
                :initial-value cv)))
    
    (let ((new-text (cond
                      ((transform-def-pattern transform-def)
                       (apply-pattern-transform cv 
                                                (transform-def-pattern transform-def)
                                                params))
                      ((transform-def-template transform-def)
                       (apply-template-transform cv
                                                 (transform-def-template transform-def)
                                                 (transform-def-template-vars transform-def)
                                                 params))
                      (t (code-value-source-text cv)))))
      
      (make-code-value
       :id (generate-code-value-id)
       :source-file (code-value-source-file cv)
       :source-region (code-value-source-region cv)
       :source-text new-text
       :language (code-value-language cv)
       :origin :transform
       :parent cv
       :transformation (transform-def-description transform-def)
       :transform-name (transform-def-name transform-def)
       :timestamp (get-universal-time)))))

(defun apply-pattern-transform (cv pattern params)
  "Apply a search/replace pattern to code-value text."
  (let* ((search (getf pattern :search))
         (replace (getf pattern :replace))
         (text (code-value-source-text cv)))
    ;; Expand mustache-style placeholders in search and replace
    (let ((expanded-search (expand-pattern-vars search params))
          (expanded-replace (expand-pattern-vars replace params)))
      (cl-ppcre:regex-replace-all expanded-search text expanded-replace))))

(defun apply-template-transform (cv template-name template-vars params)
  "Apply a template to a code-value."
  (let* ((template-path (get-transform-template-path template-name))
         (template-content (when (probe-file template-path)
                             (uiop:read-file-string template-path)))
         (vars (extract-template-vars cv template-vars params)))
    (if template-content
        (mustache:render* template-content vars)
        (progn
          (format t "~&Warning: Template not found: ~A~%" template-name)
          (code-value-source-text cv)))))

(defun get-transform-template-path (template-name)
  "Get the path to a transform template."
  (merge-pathnames (format nil "templates/~A" template-name)
                   *hactar-data-path*))

(defun expand-pattern-vars (pattern params)
  "Expand {{var}} placeholders in a pattern string."
  (if pattern
      (cl-ppcre:regex-replace-all 
       "\\{\\{(\\w+)\\}\\}"
       pattern
       (lambda (target-string start end match-start match-end reg-starts reg-ends)
         (declare (ignore target-string start end match-start match-end))
         (let* ((var-start (aref reg-starts 0))
                (var-end (aref reg-ends 0))
                (var (subseq pattern var-start var-end))
                (key (intern (string-upcase var) :keyword)))
           (or (getf params key) ""))))
      ""))

(defun extract-template-vars (cv template-vars params)
  "Extract template variables from a code-value."
  (let ((vars params))
    (dolist (spec template-vars)
      (when (and (consp spec) (symbolp (car spec)))
        (let ((key (car spec))
              (extractor (cdr spec)))
          (setf (getf vars key)
                (if (functionp extractor)
                    (funcall extractor cv)
                    (code-value-source-text cv))))))
    vars))

;;* staging system

(defun code/stage! (cv)
  "Stage a code-value for application."
  (unless (member cv *staged-code-values* :test #'eq)
    (setf (code-value-staged-p cv) t)
    (push cv *staged-code-values*)
    (register-code-value cv))
  cv)

(defun code/stage-all! (cvs)
  "Stage multiple code-values."
  (dolist (cv cvs)
    (code/stage! cv))
  cvs)

(defun code/unstage! (cv)
  "Remove a code-value from staging."
  (setf (code-value-staged-p cv) nil)
  (setf *staged-code-values* (remove cv *staged-code-values* :test #'eq))
  cv)

(defun code/staged ()
  "Return list of all staged code-values."
  (copy-list *staged-code-values*))

(defun code/staged-count ()
  "Return count of staged code-values."
  (length *staged-code-values*))

(defun code/clear-staged! ()
  "Clear all staged code-values."
  (dolist (cv *staged-code-values*)
    (setf (code-value-staged-p cv) nil))
  (setf *staged-code-values* '()))

(defun code/preview (cv)
  "Generate a diff preview for a single code-value."
  (when (code-value-parent cv)
    (generate-diff (code-value-source-text (code-value-parent cv))
                   (code-value-source-text cv)
                   :file (code-value-source-file cv))))

(defun code/preview-staged ()
  "Generate a unified diff preview of all staged changes."
  (with-output-to-string (s)
    (dolist (cv (reverse *staged-code-values*))
      (let ((diff (code/preview cv)))
        (when diff
          (format s "~A~%~%" diff))))))

(defun generate-diff (old-text new-text &key file)
  "Generate a unified diff between two strings."
  (let ((old-lines (str:lines old-text))
        (new-lines (str:lines new-text)))
    (with-output-to-string (s)
      (when file
        (format s "--- a/~A~%" (if (pathnamep file) (namestring file) file))
        (format s "+++ b/~A~%" (if (pathnamep file) (namestring file) file)))
      (let ((i 0) (j 0))
        (loop while (or (< i (length old-lines)) (< j (length new-lines)))
              do (cond
                   ((>= i (length old-lines))
                    (format s "+~A~%" (nth j new-lines))
                    (incf j))
                   ((>= j (length new-lines))
                    (format s "-~A~%" (nth i old-lines))
                    (incf i))
                   ((string= (nth i old-lines) (nth j new-lines))
                    (format s " ~A~%" (nth i old-lines))
                    (incf i)
                    (incf j))
                   (t
                    (format s "-~A~%" (nth i old-lines))
                    (format s "+~A~%" (nth j new-lines))
                    (incf i)
                    (incf j))))))))

;;* application system

(defun code/apply! (cv)
  "Apply a single code-value's changes to the filesystem."
  (let ((file (code-value-source-file cv))
        (new-text (code-value-source-text cv)))
    (when file
      (let ((full-path (if (pathnamep file) file (pathname file))))
        (push (list :cv cv 
                    :original-text (when (probe-file full-path)
                                     (uiop:read-file-string full-path))
                    :timestamp (get-universal-time))
              *code-value-history*)
        (when (> (length *code-value-history*) *code-value-history-limit*)
          (setf *code-value-history* 
                (subseq *code-value-history* 0 *code-value-history-limit*)))
        
        (ensure-directories-exist full-path)
        (write-file-content (uiop:native-namestring full-path) new-text)
        
        (setf (code-value-applied-p cv) t)
        (setf (code-value-staged-p cv) nil)
        (setf *staged-code-values* (remove cv *staged-code-values* :test #'eq))
        
        (format t "~&Applied: ~A~%" (uiop:native-namestring full-path))
        cv))))

(defun code/apply-staged! ()
  "Apply all staged code-values."
  (let ((applied '()))
    (dolist (cv (reverse *staged-code-values*))
      (code/apply! cv)
      (push cv applied))
    (nreverse applied)))

(defun code/commit! (message)
  "Apply all staged changes and create a git commit."
  (let ((applied (code/apply-staged!)))
    (when applied
      (let ((files (remove-duplicates 
                    (mapcar #'code-value-source-file applied)
                    :test #'equal)))
        (git-add files)
        (git-commit message)
        (format t "~&Committed ~A files: ~A~%" (length files) message)))
    applied))

(defun code/undo! ()
  "Undo the last applied code-value operation."
  (let ((last-op (pop *code-value-history*)))
    (when last-op
      (let* ((cv (getf last-op :cv))
             (original-text (getf last-op :original-text))
             (file (code-value-source-file cv)))
        (when (and file original-text)
          (write-file-content (uiop:native-namestring file) original-text)
          (setf (code-value-applied-p cv) nil)
          (format t "~&Undone: ~A~%" (uiop:native-namestring file))
          t)))))

;;* verification system (zero LLM)

(defun code/typechecks? (cv)
  "Check if the code-value passes type checking.
   This is ZERO LLM - runs external typechecker."
  (let ((file (code-value-source-file cv))
        (language (code-value-language cv)))
    (when file
      (let* ((full-path (uiop:native-namestring file))
             (original (when (probe-file full-path)
                         (uiop:read-file-string full-path))))
        (unwind-protect
            (progn
              (write-file-content full-path (code-value-source-text cv))
              (let ((result (run-typecheck language full-path)))
                (setf (code-value-verified-p cv) result)
                (setf (code-value-verification-result cv) 
                      (if result :passed :failed))
                result))
          (when original
            (write-file-content full-path original)))))))

(defun run-typecheck (language file)
  "Run the appropriate typechecker for the language."
  (cond
    ((member language '("typescript" "ts" "tsx") :test #'string-equal)
     (zerop (nth-value 2 
              (uiop:run-program (list "tsc" "--noEmit" file)
                                :ignore-error-status t))))
    ((member language '("python" "py") :test #'string-equal)
     (zerop (nth-value 2
              (uiop:run-program (list "mypy" file)
                                :ignore-error-status t))))
    (t t)))  ; Unknown language, assume OK

(defun code/lints? (cv)
  "Check if the code-value passes linting. Zero LLM."
  (let ((file (code-value-source-file cv)))
    (when file
      (let* ((full-path (uiop:native-namestring file))
             (original (when (probe-file full-path)
                         (uiop:read-file-string full-path))))
        (unwind-protect
            (progn
              (write-file-content full-path (code-value-source-text cv))
              (zerop (nth-value 2
                       (uiop:run-program (list "eslint" full-path)
                                         :ignore-error-status t))))
          (when original
            (write-file-content full-path original)))))))

(defun code/verify (cv)
  "Run all verification checks on a code-value. Zero LLM."
  (let ((type-ok (code/typechecks? cv))
        (lint-ok (code/lints? cv)))
    (setf (code-value-verified-p cv) (and type-ok lint-ok))
    (setf (code-value-verification-errors cv)
          (append (unless type-ok '("Type check failed"))
                  (unless lint-ok '("Lint check failed"))))
    (code-value-verified-p cv)))

(defun code/failures ()
  "Return staged code-values that failed verification."
  (remove-if #'code-value-verified-p *staged-code-values*))

;;* query system (zero LLM)

(defun code/query (&rest query-spec)
  "Query for code-values matching the specification.
   This is ZERO LLM - uses file scanning and pattern matching.
   
   Supported keys:
   - :type - Symbol type (function, class, handler, etc.)
   - :in - File glob pattern
   - :contains - Text pattern to match
   - :matches - Regex pattern to match
   - :having - Feature the code must have
   - :not-having - Feature the code must not have"
  (let* ((type (getf query-spec :type))
         (in-pattern (getf query-spec :in))
         (contains (getf query-spec :contains))
         (matches (getf query-spec :matches))
         (having (getf query-spec :having))
         (not-having (getf query-spec :not-having))
         (files (find-files-matching in-pattern))
         (results '()))
    
    (dolist (file files)
      (let ((content (uiop:read-file-string file)))
        ;; Check contains/matches
        (when (and (or (null contains) (search contains content))
                   (or (null matches) (cl-ppcre:scan matches content)))
          ;; TODO: Parse and extract specific types
          ;; For now, return whole file as code-value
          (push (code/select file) results))))
    
    (nreverse results)))

(defun find-files-matching (pattern)
  "Find files matching a glob pattern."
  (if pattern
      (let ((files '()))
        (uiop:collect-sub*directories 
         *repo-root*
         (constantly t)
         (constantly t)
         (lambda (dir)
           (dolist (file (uiop:directory-files dir))
             (when (glob-matches-p (pathname file) pattern)
               (push file files)))))
        files)
      (mapcar #'pathname *files*)))

(defun glob-matches-p (pathname pattern)
  "Check if pathname matches a glob-like pattern."
  (let ((name (namestring pathname)))
    (cl-ppcre:scan (glob-to-regex pattern) name)))

(defun glob-to-regex (glob)
  "Convert a glob pattern to a regex."
  (let ((regex glob))
    ;; Use placeholder for ** to avoid * replacement affecting it
    (setf regex (cl-ppcre:regex-replace-all "\\*\\*" regex "\x00STARSTAR\x00"))
    (setf regex (cl-ppcre:regex-replace-all "\\." regex "\\."))
    (setf regex (cl-ppcre:regex-replace-all "\\*" regex "[^/]*"))
    (setf regex (cl-ppcre:regex-replace-all "\\?" regex "."))
    ;; Restore ** as .*
    (setf regex (cl-ppcre:regex-replace-all "\x00STARSTAR\x00" regex ".*"))
    (format nil "^~A$" regex)))

;;* framework analyzer system

(defvar *framework-analyzers* (make-hash-table :test 'equal)
  "Hash table of defined framework analyzers.")

(defstruct framework-analyzer
  "A framework-specific code analyzer."
  (name nil :type (or null string symbol))
  (description nil :type (or null string))
  (detect-fn nil :type (or null function))
  (queries nil :type list))  ; Alist of (query-name . query-fn)

(defmacro defanalyzer (name description &body body)
  "Define a framework-specific analyzer."
  (let ((detect (getf body :detect))
        (queries (getf body :queries)))
    `(setf (gethash ,(if (stringp name) name (string-downcase (symbol-name name)))
                    *framework-analyzers*)
           (make-framework-analyzer
            :name ',name
            :description ,description
            :detect-fn (lambda () ,detect)
            :queries (list ,@(mapcar (lambda (q)
                                       `(cons ',(first q) 
                                              (lambda () ,@(cddr q))))
                                     queries))))))

(defun get-analyzer (name)
  "Get an analyzer by name."
  (gethash (if (stringp name) name (string-downcase (symbol-name name)))
           *framework-analyzers*))

(defun active-analyzers ()
  "Return list of analyzers that apply to the current project."
  (let ((active '()))
    (maphash (lambda (name analyzer)
               (when (and (framework-analyzer-detect-fn analyzer)
                          (funcall (framework-analyzer-detect-fn analyzer)))
                 (push (cons name analyzer) active)))
             *framework-analyzers*)
    active))

;;* REPL commands - staging

(define-command code/stage (args)
  "Stage a code-value for application.
Usage: /code/stage <id>"
  (let* ((id (first args))
         (cv (when id (get-code-value id))))
    (if cv
        (progn
          (code/stage! cv)
          (format t "~&Staged: ~A~%" cv))
        (format t "~&Code-value not found: ~A~%" id))))

(define-command code/staged (args)
  "List all staged code-values."
  (declare (ignore args))
  (let ((staged (code/staged)))
    (if staged
        (progn
          (format t "~&Staged code-values (~A):~%" (length staged))
          (dolist (cv staged)
            (format t "  ~A~%" (code/summary cv))))
        (format t "~&No staged code-values.~%"))))

(define-command code/preview (args)
  "Preview staged changes.
Usage: /code/preview [id]  - Preview specific or all staged"
  (if args
      (let ((cv (get-code-value (first args))))
        (if cv
            (format t "~A~%" (code/preview cv))
            (format t "~&Code-value not found: ~A~%" (first args))))
      (format t "~A~%" (code/preview-staged))))

(define-command code/apply (args)
  "Apply staged code-values.
Usage: /code/apply       - Apply all staged
       /code/apply <id>  - Apply specific"
  (if args
      (let ((cv (get-code-value (first args))))
        (if cv
            (code/apply! cv)
            (format t "~&Code-value not found: ~A~%" (first args))))
      (let ((applied (code/apply-staged!)))
        (format t "~&Applied ~A code-values.~%" (length applied)))))

(define-command code/commit (args)
  "Apply staged changes and create a git commit.
Usage: /code/commit <message>"
  (let ((message (str:join " " args)))
    (if (and message (not (string= message "")))
        (code/commit! message)
        (format t "~&Please provide a commit message.~%"))))

(define-command code/undo (args)
  "Undo the last applied code-value operation."
  (declare (ignore args))
  (if (code/undo!)
      (format t "~&Undo successful.~%")
      (format t "~&Nothing to undo.~%")))

(define-command code/transform (args)
  "Transform a code-value.
Usage: /code/transform <id> <transform-name>"
  (let* ((id (first args))
         (transform-name (second args))
         (cv (when id (get-code-value id))))
    (if cv
        (if transform-name
            (let ((transformed (code/transform cv transform-name)))
              (register-code-value transformed)
              (format t "~&Created: ~A~%" transformed)
              (code/show transformed))
            (format t "~&Please specify a transform name.~%"))
        (format t "~&Code-value not found: ~A~%" id))))

(define-command code/transforms (args)
  "List available transforms."
  (declare (ignore args))
  (let ((transforms (list-transforms)))
    (if transforms
        (progn
          (format t "~&Available transforms:~%")
          (dolist (name transforms)
            (let ((t-def (get-transform name)))
              (format t "  ~A - ~A~%" name 
                      (or (transform-def-description t-def) "No description")))))
        (format t "~&No transforms defined.~%"))))

(define-command code/generate (args)
  "Generate a transformation using LLM (explicit LLM call).
Usage: /code/generate <id> <description>"
  (let* ((id (first args))
         (description (str:join " " (rest args)))
         (cv (when id (get-code-value id))))
    (if cv
        (if (and description (not (string= description "")))
            (let ((transformed (code/generate-transform description :for cv)))
              (when transformed
                (format t "~&Created: ~A~%" transformed)
                (code/show transformed)))
            (format t "~&Please provide a transformation description.~%"))
        (format t "~&Code-value not found: ~A~%" id))))

;;* built-in framework analyzers (zero LLM)

(defanalyzer react
  "React framework analyzer"
  :detect (member "react" *stack* :test #'string-equal)
  :queries
  ((:components ()
    "Find all React components"
    (code/query :matches "function\\s+[A-Z]\\w*\\s*\\(" 
                :in "**/*.tsx"))
   
   (:hooks ()
    "Find all custom hooks"
    (code/query :matches "function\\s+use[A-Z]\\w*\\s*\\(" 
                :in "**/*.ts"))
   
   (:routes ()
    "Find route definitions"
    (code/query :contains "createBrowserRouter\\|Route\\|Routes"
                :in "**/*.tsx"))))

(defanalyzer express
  "Express.js framework analyzer"
  :detect (member "express" *stack* :test #'string-equal)
  :queries
  ((:routes ()
    "Find all route definitions"
    (code/query :matches "router\\.(get|post|put|delete|patch)"
                :in "**/*.ts"))
   
   (:middleware ()
    "Find middleware functions"
    (code/query :matches "app\\.use\\|router\\.use"
                :in "**/*.ts"))
   
   (:handlers ()
    "Find route handler functions"
    (code/query :matches "(req,\\s*res)\\s*=>"
                :in "**/*.ts"))))

;;* built-in transforms (zero LLM)

(deftransform add-error-handling
  "Wrap code in try-catch error handling"
  :pattern
  (:search "{{body}}"
   :replace "try {
  {{body}}
} catch (error) {
  console.error('Error:', error);
  throw error;
}"))

(deftransform add-logging
  "Add logging to function entry/exit"
  :pattern
  (:search "function {{name}}({{params}}) {"
   :replace "function {{name}}({{params}}) {
  console.log('Entering {{name}}', { {{params}} });"))

;;* framework query shortcuts

(defun react/components ()
  "Query React components. Zero LLM."
  (let ((analyzer (get-analyzer 'react)))
    (when analyzer
      (let ((query-fn (cdr (assoc :components 
                                  (framework-analyzer-queries analyzer)))))
        (when query-fn (funcall query-fn))))))

(defun react/hooks ()
  "Query React hooks. Zero LLM."
  (let ((analyzer (get-analyzer 'react)))
    (when analyzer
      (let ((query-fn (cdr (assoc :hooks
                                  (framework-analyzer-queries analyzer)))))
        (when query-fn (funcall query-fn))))))

(defun express/routes ()
  "Query Express routes. Zero LLM."
  (let ((analyzer (get-analyzer 'express)))
    (when analyzer
      (let ((query-fn (cdr (assoc :routes
                                  (framework-analyzer-queries analyzer)))))
        (when query-fn (funcall query-fn))))))

(defun express/handlers ()
  "Query Express handlers. Zero LLM."
  (let ((analyzer (get-analyzer 'express)))
    (when analyzer
      (let ((query-fn (cdr (assoc :handlers
                                  (framework-analyzer-queries analyzer)))))
        (when query-fn (funcall query-fn))))))

;;* LLM transform generation (explicit LLM)

(defun code/generate-transform (description &key for examples)
  "Generate a transformation using LLM.
   This is an EXPLICIT LLM operation.
   
   DESCRIPTION: What transformation to perform
   FOR: The code-value to transform
   EXAMPLES: Optional list of before/after examples"
  (let* ((source-text (when for (code-value-source-text for)))
         (language (when for (code-value-language for)))
         (examples-text (when examples
                          (format nil "~%Examples:~%~{~A~%~}" examples)))
         (prompt (format nil "Transform this code: ~A

~@[Language: ~A~]
~@[~%Code:~%```~%~A~%```~]
~@[~A~]

Return ONLY the transformed code, no explanations."
                         description
                         language
                         source-text
                         examples-text))
         (response (get-llm-response prompt 
                                     :stream nil 
                                     :add-to-history nil
                                     :custom-system-prompt "You are a code transformation tool. Return only code.")))
    (when response
      (let ((new-text (let ((block (extract-md-fenced-code-block response)))
                        (if block
                            (cdr (assoc :contents block))
                            (string-trim '(#\Space #\Tab #\Newline #\Return #\`) response)))))
        (if for
            (let ((cv (make-code-value
                       :id (generate-code-value-id)
                       :source-file (code-value-source-file for)
                       :source-region (code-value-source-region for)
                       :source-text new-text
                       :language language
                       :origin :transform
                       :parent for
                       :transformation description
                       :timestamp (get-universal-time))))
              (register-code-value cv)
              cv)
            new-text)))))

;;* helper for parsing command args

(defun getf-string (plist key)
  "Get value after KEY in a list of strings."
  (loop for (k v) on plist by #'cddr
        when (string= k key)
        return v))
