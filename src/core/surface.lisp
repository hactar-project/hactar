(in-package #:hactar-rebuild)

;;; ---------------------------------------------------------------------------
;;; DSL surface structs
;;; ---------------------------------------------------------------------------

(defstruct hook-def name args doc (handlers '()))
(defstruct mode-def name when rules skills commands detect stack priority stack-commands forms)
(defstruct starter-def name title stack language description files prompts)
(defstruct agent-def name model tools skills sandbox instructions)
(defstruct interface-def name path format render parse)
(defstruct command-def name doc handler)
(defstruct rule-def name doc body)
(defstruct skill-def name doc docs rules)
(defstruct prompt-def name args template)
(defstruct action-def name doc prompt tools skills apply when)
(defstruct tool-def name doc fn sandbox permissions args)
(defstruct route-def name pattern coerce result-template)
(defstruct hypertext-def name uri content)
(defstruct doc-def name uri text file tags)
(defstruct sandbox-def name kind allow deny cwd)

(defun key-string (name)
  "Return the normalized registry key for NAME."
  (string-downcase (string name)))

(defun keyword-for-symbol (symbol)
  (intern (string-upcase (symbol-name symbol)) :keyword))

;;; ---------------------------------------------------------------------------
;;; generated region wrapper (load-time)
;;; ---------------------------------------------------------------------------

(defmacro generated (&rest body)
  "Load-time wrapper for generated forms. Leading :UPDATED-AT <string> is ignored."
  (when (eq (first body) :updated-at)
    (setf body (cddr body)))
  `(progn ,@body))

;;; ---------------------------------------------------------------------------
;;; project data forms
;;; ---------------------------------------------------------------------------

(defmacro defproject (&rest plist)
  "Declare project identity/stack. Exactly one per .hactar.lisp."
  `(setf *project* (list ,@(loop for x in plist collect (list 'quote x)))))

(defmacro defmodels (&rest entries)
  `(setf *models* ',entries))

(defmacro defroutes (&rest entries)
  `(setf *routes-data* ',entries))

;;; ---------------------------------------------------------------------------
;;; registry-populating macros
;;; ---------------------------------------------------------------------------

(defmacro defhook (name &key args doc)
  `(setf (gethash ,(key-string name) *hooks*)
         (make-hook-def :name ',name :args ',args :doc ,doc)))

(defmacro defmode (name &key when rules skills commands detect stack (priority 50) stack-commands forms)
  `(progn
     (setf (gethash ,(key-string name) *modes*)
           (make-mode-def :name ',name :when ',when :rules ',rules
                          :skills ',skills :commands ',commands
                          :detect ',detect :stack ',stack :priority ,priority
                          :stack-commands ',stack-commands :forms ',forms))
     ,@(when (or detect forms stack stack-commands)
         `((register-stack-mode
            :name ,(key-string name)
            :detect (compile-detect-spec ',detect)
            :forms (let ((f ',forms)) (lambda (root) (declare (ignore root)) f))
            :stack ',stack
            :priority ,priority
            :stack-commands ',stack-commands)))
     ',name))

(defmacro defstarter (name &key title stack language description files prompts)
  `(setf (gethash ,(key-string name) *starters*)
         (make-starter-def :name ',name :title ,title :stack ',stack
                           :language ,language :description ,description
                           :files ',files :prompts ',prompts)))

(defmacro defagent (name &key model tools skills sandbox instructions)
  `(setf (gethash ,(key-string name) *agents*)
         (make-agent-def :name ',name :model ,model :tools ',tools
                         :skills ',skills :sandbox ',sandbox
                         :instructions ,instructions)))

(defmacro definterface (name &key path format render parse)
  `(setf (gethash ,(key-string name) *interfaces*)
         (make-interface-def :name ',name :path ,path :format ,format
                             :render ',render :parse ',parse)))

(defmacro defcommand (name &key doc handler)
  `(setf (gethash ,(key-string name) *commands*)
         (make-command-def :name ',name :doc ,doc :handler ',handler)))

(defmacro defrule (name &key doc body)
  `(setf (gethash ,(key-string name) *rules*)
         (make-rule-def :name ',name :doc ,doc :body ,body)))

(defmacro defskill (name &key doc docs rules)
  `(setf (gethash ,(key-string name) *skills*)
         (make-skill-def :name ',name :doc ,doc :docs ',docs :rules ',rules)))

(defmacro defprompt (name args template)
  `(setf (gethash ,(key-string name) *prompts*)
         (make-prompt-def :name ',name :args ',args :template ,template)))

(defmacro defaction (name &key doc prompt tools skills (apply :create) when)
  `(setf (gethash ,(key-string name) *actions*)
         (make-action-def :name ',name :doc ,doc :prompt ',prompt
                          :tools ',tools :skills ',skills
                          :apply ,apply :when ',when)))

;;; deftool: a tool is a Lisp function with a docstring and a body of forms.
;;; The body may call (shell-cmd ...), universal tools, or (with-agent ...).
(defmacro deftool (name lambda-list &body body)
  "Register a tool as a Lisp function. BODY holds the forms to execute."
  (let* ((key (key-string name))
         (doc (if (stringp (first body)) (first body) "No documentation."))
         (forms (if (and (stringp (first body)) (rest body)) (rest body) body)))
    `(progn
       (defun ,name ,lambda-list ,@forms)
       (setf (gethash ,key *tools*)
             (make-tool-def :name ',name :doc ,doc :fn (function ,name)
                            :args ',lambda-list))
       ',name)))

(defun register-tool-fn (name doc fn &key args)
  "Programmatically register a tool function (used by adapters like hactar-mcp)."
  (let ((key (key-string name)))
    (setf (gethash key *tools*)
          (make-tool-def :name name :doc doc :fn fn :args args))))

(defun list-tools ()
  "Return a list of (:name string :doc string :args list) plists for registered tools."
  (let ((out nil))
    (maphash (lambda (k tool)
               (push (list :name k
                           :doc (or (tool-def-doc tool) "")
                           :args (tool-def-args tool))
                     out))
             *tools*)
    (sort out #'string< :key (lambda (x) (getf x :name)))))

(defun list-commands ()
  "Return a sorted list of (:name string :doc string) plists for registered commands."
  (let ((out nil))
    (maphash (lambda (k cmd)
               (push (list :name k :doc (or (command-def-doc cmd) "")) out))
             *commands*)
    (sort out #'string< :key (lambda (x) (getf x :name)))))

(defmacro defroute (name &key pattern coerce result)
  `(setf (gethash ,(key-string name) *route-parsers*)
         (make-route-def :name ',name :pattern ,pattern :coerce ',coerce
                         :result-template ',result)))

(defmacro defhypertext (name &key uri content)
  `(setf (gethash ,(key-string name) *hypertexts*)
         (make-hypertext-def :name ',name :uri ,uri :content ,content)))

(defmacro defdoc (name &key uri text file tags)
  `(setf (gethash ,(key-string name) *docs*)
         (make-doc-def :name ',name :uri ,uri :text ,text :file ,file
                       :tags ',tags)))

(defmacro defsandbox (name &key (kind :local) allow deny cwd)
  `(setf (gethash ,(key-string name) *sandboxes*)
         (make-sandbox-def :name ',name :kind ,kind :allow ',allow
                           :deny ',deny :cwd ,cwd)))

;;; ---------------------------------------------------------------------------
;;; closed-surface validator
;;; ---------------------------------------------------------------------------

(defparameter *surface-operators*
  '("in-package" "generated" "defproject" "defmodels" "defroutes" "defhook"
    "defmode" "defagent" "definterface" "defcommand" "defrule" "defskill"
    "defprompt" "defaction" "deftool" "defroute" "defhypertext" "defdoc"
    "defsandbox" "defstarter")
  "Closed DSL surface accepted in generated Hactar files and content packs.")

(defun surface-operator-name (operator)
  (when (symbolp operator)
    (string-downcase (symbol-name operator))))

(defun strip-generated-metadata (body)
  (if (eq (first body) :updated-at) (cddr body) body))

(defun validate-forms (forms)
  "Return (values ok-p errors). Every top-level operator must be on the
surface; forms inside GENERATED are recursively checked."
  (let ((errors '()))
    (labels ((validate-one (form)
               (cond
                 ((not (consp form))
                  (push (format nil "Not a form: ~S" form) errors))
                 ((string= (or (surface-operator-name (car form)) "") "generated")
                  (dolist (child (strip-generated-metadata (cdr form)))
                    (validate-one child)))
                 ((not (member (surface-operator-name (car form))
                               *surface-operators* :test #'string=))
                  (push (format nil "~A is not on the Hactar surface" (car form))
                        errors)))))
      (dolist (form forms) (validate-one form)))
    (values (null errors) (nreverse errors))))

;;; ---------------------------------------------------------------------------
;;; route parsing + normalization
;;; ---------------------------------------------------------------------------

(defun ensure-leading-slash (value)
  (if (and (plusp (length value)) (char= (char value 0) #\/))
      value
      (concatenate 'string "/" value)))

(defun placeholder-token-p (token)
  (and (> (length token) 2)
       (char= (char token 0) #\{)
       (char= (char token (1- (length token))) #\})))

(defun parse-placeholder-name (token)
  (subseq token 1 (1- (length token))))

(defun parse-route-params (pattern input)
  "Parse INPUT using a pattern with --flag={name} placeholders."
  (let ((pattern-parts (str:split " " pattern :omit-nulls t))
        (input-parts (str:split " " input :omit-nulls t))
        (params '()))
    (unless (= (length pattern-parts) (length input-parts))
      (return-from parse-route-params nil))
    (loop for p in pattern-parts
          for i in input-parts
          do (cond
               ((search "={" p)
                (let* ((p-pos (position #\= p))
                       (i-pos (position #\= i))
                       (p-flag (subseq p 0 p-pos))
                       (i-flag (and i-pos (subseq i 0 i-pos)))
                       (placeholder (subseq p (1+ p-pos))))
                  (unless (and i-pos (string= p-flag i-flag)
                               (placeholder-token-p placeholder))
                    (return-from parse-route-params nil))
                  (push (keyword-for-symbol
                         (intern (string-upcase (parse-placeholder-name placeholder))))
                        params)
                  (push (subseq i (1+ i-pos)) params)))
               ((not (string= p i))
                (return-from parse-route-params nil))))
    (nreverse params)))

(defun apply-route-coercions (params coercions)
  (loop for (key fn-name) in coercions
        for fn = (and (fboundp fn-name) (symbol-function fn-name))
        do (when fn (setf (getf params key) (funcall fn (getf params key)))))
  params)

(defun resolve-template-value (value params)
  (if (and (symbolp value) (not (keywordp value)))
      (getf params (keyword-for-symbol value))
      value))

(defun render-route-template (template params)
  (loop for (key value) on template by #'cddr
        append (list key (resolve-template-value value params))))

(defun try-route (route input)
  (let ((params (parse-route-params (route-def-pattern route) input)))
    (when params
      (setf params (apply-route-coercions params (route-def-coerce route)))
      (render-route-template (route-def-result-template route) params))))

(defun parse-route (input)
  "Return normalized route data for INPUT, or NIL if no route matches."
  (loop for route being the hash-values of *route-parsers*
        for result = (try-route route input)
        when result do (return result)))
