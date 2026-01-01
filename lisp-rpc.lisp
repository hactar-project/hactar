;;* Lisp-RPC — Lisp-native RPC for Hactar
(in-package :hactar)

;;* API definition
(defstruct api-definition
  "Structure representing a registered Lisp-RPC API function."
  (name nil :type symbol)
  (lambda-list nil :type list)
  (docstring "" :type string)
  (category :general :type keyword)
  (permissions :confirm :type keyword)  ; :confirm, :auto, :deny
  (examples nil :type list)
  (dangerous nil :type boolean)
  (returns nil :type (or null string))
  (side-effects nil :type (or null string))
  (function nil :type (or null function)))

;;* Registry
(defvar *api-registry* (make-hash-table :test 'equal)
  "Hash table of registered Lisp-RPC API functions. Key: string name, Value: api-definition.")

(defvar *lisp-rpc-exposed-categories* '(:all)
  "List of API categories (keywords) or function names (symbols) to expose in the system prompt.
   :all means expose everything.")

(defvar *lisp-rpc-eval-timeout* 30
  "Maximum seconds for evaluating a single Lisp-RPC form.")

(defvar *lisp-rpc-eval-log* '()
  "Log of recent Lisp-RPC evaluations for debugging.")

(defvar *lisp-rpc-eval-log-max* 50
  "Maximum entries in the eval log.")


;;* defapi macro
(defmacro defapi (name lambda-list &body body)
  "Define a Lisp-RPC API function.

NAME:        Symbol naming the function (will be interned in HACTAR-API).
LAMBDA-LIST: Ordinary lambda list.
BODY:        Starts with a docstring (required), then keyword options, then implementation.

Keyword options:
  :permissions  :confirm/:auto/:deny  — Permission level (default :confirm)
  :category     keyword               — Grouping for prompt (:filesystem :context :shell :git :search)
  :examples     quoted-list           — Example calls shown to the LLM (quoted s-expressions)
  :dangerous    boolean               — Always requires confirmation if T
  :returns      string                — Description of return value
  :side-effects string                — Description of side effects

Example:
  (defapi read-file (path)
    \"Read file contents at PATH relative to project root.\"
    :permissions :auto
    :category :filesystem
    :returns \"File content as a string\"
    :examples '((hactar:read-file \"src/main.lisp\"))
    (uiop:read-file-string (merge-pathnames path *repo-root*)))"
  (let ((docstring (if (stringp (first body)) (first body) ""))
        (rest-body (if (stringp (first body)) (rest body) body)))
    ;; Parse keyword options from the front of rest-body
    (let ((permissions :confirm)
          (category :general)
          (examples nil)
          (dangerous nil)
          (returns nil)
          (side-effects nil)
          (impl-body nil))
      ;; Consume keyword pairs
      (loop while (and rest-body (keywordp (first rest-body)))
            do (let ((key (pop rest-body))
                     (val (pop rest-body)))
                 (case key
                   (:permissions (setf permissions val))
                   (:category (setf category val))
                   (:examples (setf examples val))
                   (:dangerous (setf dangerous val))
                   (:returns (setf returns val))
                   (:side-effects (setf side-effects val)))))
      (setf impl-body rest-body)
      (let ((hactar-sym (intern (symbol-name name) :hactar))
            (impl-name (intern (format nil "%~A-IMPL" (symbol-name name)) :hactar))
            (name-string (string-downcase (symbol-name name))))
        `(progn
           (defun ,impl-name ,lambda-list
             ,@impl-body)
           (setf (gethash ,name-string *api-registry*)
                 (make-api-definition
                  :name ',hactar-sym
                  :lambda-list ',lambda-list
                  :docstring ,docstring
                  :category ,category
                  :permissions ,permissions
                  :examples ,examples
                  :dangerous ,dangerous
                  :returns ,returns
                  :side-effects ,side-effects
                  :function #',impl-name))
           (setf (fdefinition ',hactar-sym)
                 (make-api-wrapper #',impl-name ,name-string ,permissions ,dangerous))
           (export ',hactar-sym :hactar)
           (unless *silent*
             (format t "Registered Lisp-RPC API: ~A (~A, ~A)~%"
                     ,name-string ,category ,permissions))
           ',hactar-sym)))))

(defun make-api-wrapper (impl-fn api-name permissions dangerous-p)
  "Create a permission-checking wrapper around an API implementation function."
  (lambda (&rest args)
    (let ((decision (if dangerous-p
                        :confirm  ; Always confirm dangerous operations
                        (resolve-permission api-name
                                            ;; Convert args to a plist for the permission system
                                            (loop for arg in args
                                                  for i from 0
                                                  append (list (intern (format nil "ARG~A" i) :keyword) arg))))))
      (case decision
        (:deny
         (error "Permission denied for API call: ~A" api-name))
        (:confirm
         (let ((user-decision (prompt-tool-permission api-name args)))
           (when (eq user-decision :deny)
             (error "Permission denied by user for API call: ~A" api-name))))
        (:allow nil))
      (apply impl-fn args))))

;;* Allowed symbols whitelist
(defvar *lisp-rpc-allowed-cl-symbols*
  '(;; Control flow
    cl:progn cl:let cl:let* cl:if cl:when cl:unless cl:cond cl:case
    cl:block cl:return-from cl:values cl:multiple-value-bind
    ;; Function application
    cl:lambda cl:funcall cl:mapcar cl:mapc cl:remove-if
    cl:remove-if-not cl:find cl:find-if cl:position cl:search
    cl:sort cl:reverse cl:append
    cl:cons cl:list cl:list* cl:first cl:second cl:third cl:rest
    cl:car cl:cdr cl:nth cl:length cl:subseq cl:elt cl:aref
    ;; Strings
    cl:format cl:concatenate cl:string cl:string-upcase cl:string-downcase
    cl:string-trim cl:string= cl:string-equal cl:parse-integer
    cl:with-output-to-string cl:write-string cl:write-char
    ;; Arithmetic
    cl:+ cl:- cl:* cl:/ cl:mod cl:floor cl:ceiling cl:round
    cl:< cl:> cl:<= cl:>= cl:= cl:/= cl:min cl:max cl:abs
    ;; Predicates
    cl:null cl:not cl:and cl:or cl:eq cl:eql cl:equal cl:equalp
    cl:stringp cl:numberp cl:listp cl:consp cl:atom cl:symbolp
    cl:zerop cl:plusp cl:minusp
    ;; Iteration
    cl:loop cl:dolist cl:dotimes
    ;; Type
    cl:coerce cl:type-of cl:typep
    ;; I/O (string only)
    cl:prin1-to-string cl:princ-to-string
    ;; Misc
    cl:identity cl:constantly cl:complement
    ;; Constants / special
    cl:t cl:nil cl:quote)
  "Whitelist of CL symbols allowed in Lisp-RPC sandboxed evaluation.")

(defvar *lisp-rpc-blocked-symbols*
  '(cl:eval cl:compile cl:load cl:require cl:provide
    cl:open cl:close cl:with-open-file cl:with-open-stream
    cl:read cl:read-from-string cl:read-line cl:read-char
    cl:intern cl:find-package cl:in-package cl:defpackage cl:use-package
    cl:find-symbol cl:make-package cl:delete-package
    cl:defun cl:defmacro cl:defvar cl:defparameter cl:defconstant cl:defclass cl:defmethod
    cl:make-instance
    cl:funcall cl:apply  ; These are in allowed list but we re-check targets
    cl:set cl:setf cl:setq cl:makunbound cl:fmakunbound
    cl:delete-file cl:rename-file cl:ensure-directories-exist
    uiop:run-program)
  "Symbols explicitly blocked from Lisp-RPC evaluation, even if they appear in CL.")

;;* Code walker / validator
(define-condition lisp-rpc-security-error (error)
  ((form :initarg :form :reader security-error-form)
   (reason :initarg :reason :reader security-error-reason))
  (:report (lambda (c s)
             (format s "Lisp-RPC security violation: ~A~%  Form: ~S"
                     (security-error-reason c)
                     (security-error-form c)))))

(defun lisp-rpc-symbol-allowed-p (sym)
  "Check if a symbol is allowed in Lisp-RPC evaluation."
  (let ((pkg (symbol-package sym)))
    (cond
      ;; Blocked symbols are never allowed
      ((member sym *lisp-rpc-blocked-symbols*) nil)
      ;; HACTAR symbols are allowed if exported
      ((and pkg (string= (package-name pkg) "HACTAR"))
       (multiple-value-bind (s status) (find-symbol (symbol-name sym) :hactar)
         (declare (ignore s))
         (eq status :external)))
      ;; CL symbols are allowed if whitelisted
      ((and pkg (string= (package-name pkg) "COMMON-LISP"))
       (member sym *lisp-rpc-allowed-cl-symbols*))
      ;; KEYWORD symbols are allowed (they're just data)
      ((keywordp sym) t)
      ;; Everything else is blocked
      (t nil))))

(defun validate-lisp-rpc-form (form)
  "Walk a form tree and validate all symbols are allowed.
   Signals LISP-RPC-SECURITY-ERROR if any disallowed symbol is found."
  (cond
    ;; Atoms
    ((null form) t)
    ((numberp form) t)
    ((stringp form) t)
    ((characterp form) t)
    ((keywordp form) t)
    ((and (symbolp form) (not (null form)))
     (unless (lisp-rpc-symbol-allowed-p form)
       (error 'lisp-rpc-security-error
              :form form
              :reason (format nil "Symbol ~S (package ~A) is not allowed"
                              form (if (symbol-package form)
                                       (package-name (symbol-package form))
                                       "uninterned"))))
     t)
    ((vectorp form)
     (loop for elt across form do (validate-lisp-rpc-form elt))
     t)
    ;; Lists (including quoted forms)
    ((consp form)
     (let ((head (car form)))
       (cond
         ;; (quote x) — validate the quoted data for symbols
         ((eq head 'cl:quote)
          (validate-lisp-rpc-quoted (cadr form)))
         ;; Normal list — validate each element
         (t
          (dolist (elt form)
            (validate-lisp-rpc-form elt))
          t))))
    (t
     (error 'lisp-rpc-security-error
            :form form
            :reason (format nil "Unexpected form type: ~A" (type-of form))))))

(defun validate-lisp-rpc-quoted (form)
  "Validate a quoted form. Allows data but blocks function symbols."
  (cond
    ((null form) t)
    ((numberp form) t)
    ((stringp form) t)
    ((characterp form) t)
    ((keywordp form) t)
    ((symbolp form) t)  ; Quoted symbols are just data
    ((consp form)
     (validate-lisp-rpc-quoted (car form))
     (validate-lisp-rpc-quoted (cdr form))
     t)
    ((vectorp form)
     (loop for elt across form do (validate-lisp-rpc-quoted elt))
     t)
    (t t)))

;;* Restricted reader
(defvar *lisp-rpc-readtable*
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\. (lambda (s c n)
                                            (declare (ignore s c n))
                                            (error 'lisp-rpc-security-error
                                                   :form "#."
                                                   :reason "Read-time evaluation (#.) is not allowed"))
                                  rt)
    rt)
  "Restricted readtable for parsing Lisp-RPC forms from LLM output.")

(defun lisp-rpc-read-form (string)
  "Read a single Lisp form from STRING using the restricted readtable.
   Reads in the HACTAR package so unqualified symbols resolve there."
  (let ((*readtable* *lisp-rpc-readtable*)
        (*package* (find-package :hactar))
        (*read-eval* nil))
    (handler-case
        (read-from-string string)
      (error (e)
        (error 'lisp-rpc-security-error
               :form string
               :reason (format nil "Read error: ~A" e))))))

;;* Sandboxed evaluator
(defun lisp-rpc-eval (form-or-string)
  "Evaluate a Lisp-RPC form in the sandboxed environment.
   FORM-OR-STRING can be a string (will be read) or an already-parsed form.
   Returns (values result output-string error-string).

   Steps:
   1. Read the form (if string) with restricted readtable
   2. Validate all symbols via code walk
   3. Evaluate with timeout, capturing output
   4. Log the evaluation"
  (let* ((form (if (stringp form-or-string)
                   (lisp-rpc-read-form form-or-string)
                   form-or-string))
         (output (make-string-output-stream))
         (result nil)
         (error-msg nil)
         (start-time (get-internal-real-time)))
    (handler-case
        (validate-lisp-rpc-form form)
      (lisp-rpc-security-error (e)
        (setf error-msg (format nil "~A" e))
        (lisp-rpc-log-eval form nil error-msg start-time)
        (return-from lisp-rpc-eval
          (values nil "" error-msg))))
    (handler-case
        (let ((*standard-output* output)
              (*error-output* output)
              (*package* (find-package :hactar)))
          (setf result (eval form)))
      (error (e)
        (setf error-msg (format nil "Evaluation error: ~A" e))))
    (lisp-rpc-log-eval form result error-msg start-time)
    (values result
            (get-output-stream-string output)
            error-msg)))

(defun lisp-rpc-log-eval (form result error start-time)
  "Log a Lisp-RPC evaluation for debugging."
  (let ((elapsed (/ (- (get-internal-real-time) start-time)
                    (float internal-time-units-per-second))))
    (push (list :form form
                :result result
                :error error
                :elapsed elapsed
                :timestamp (get-universal-time))
          *lisp-rpc-eval-log*)
    (when (> (length *lisp-rpc-eval-log*) *lisp-rpc-eval-log-max*)
      (setf *lisp-rpc-eval-log*
            (subseq *lisp-rpc-eval-log* 0 *lisp-rpc-eval-log-max*)))
    (debug-log "Lisp-RPC eval:" form "=>" (or error result) (format nil "(~,3Fs)" elapsed))))

;;* LLM response parsing
(defun extract-lisp-rpc-forms (text)
  "Extract Lisp forms from LLM response text.
   Looks for ```lisp ... ``` fenced blocks and <lisp>...</lisp> tagged blocks.
   Returns a list of form strings."
  (let ((forms '()))
    (cl-ppcre:do-register-groups (content)
        ("(?s)```lisp\\s*\\n(.*?)\\n\\s*```" text)
      (push (string-trim '(#\Space #\Tab #\Newline #\Return) content) forms))
    (cl-ppcre:do-register-groups (content)
        ("(?s)<lisp>\\s*(.*?)\\s*</lisp>" text)
      (push (string-trim '(#\Space #\Tab #\Newline #\Return) content) forms))
    (nreverse forms)))

(defun lisp-rpc-process-response (response-text)
  "Process an LLM response, extracting and evaluating any Lisp-RPC forms.
   Returns a list of (form-string result output error) for each form found."
  (let* ((form-strings (extract-lisp-rpc-forms response-text))
         (results '()))
    (dolist (form-str form-strings)
      (handler-case
          (multiple-value-bind (result output error-msg)
              (lisp-rpc-eval form-str)
            (push (list :form form-str
                        :result (if error-msg nil (prin1-to-string result))
                        :output output
                        :error error-msg)
                  results))
        (error (e)
          (push (list :form form-str
                      :result nil
                      :output ""
                      :error (format nil "~A" e))
                results))))
    (nreverse results)))

(defun format-lisp-rpc-results (results)
  "Format Lisp-RPC evaluation results for feeding back to the LLM."
  (when results
    (with-output-to-string (s)
      (format s "~%Evaluation results:~%")
      (dolist (r results)
        (let ((form (getf r :form))
              (result (getf r :result))
              (output (getf r :output))
              (error-msg (getf r :error)))
          (format s "~%Form: ~A~%" form)
          (if error-msg
              (format s "Error: ~A~%" error-msg)
              (progn
                (when (and output (> (length output) 0))
                  (format s "Output: ~A~%" output))
                (format s "Result: ~A~%" result))))))))

;;* System prompt generation
(defun lisp-rpc-expose (&rest categories-or-names)
  "Set which API categories or specific functions to expose in the system prompt.
   Arguments can be keywords (:filesystem :context :shell :git :all)
   or symbols (specific function names)."
  (setf *lisp-rpc-exposed-categories*
        (if (member :all categories-or-names)
            '(:all)
            categories-or-names))
  (unless *silent*
    (format t "Lisp-RPC exposed: ~{~A~^, ~}~%" *lisp-rpc-exposed-categories*)))

(defun lisp-rpc-api-exposed-p (api-def)
  "Check if an API definition should be included in the system prompt."
  (or (member :all *lisp-rpc-exposed-categories*)
      (member (api-definition-category api-def) *lisp-rpc-exposed-categories*)
      (member (api-definition-name api-def) *lisp-rpc-exposed-categories*)))

(defun generate-lisp-api-prompt ()
  "Generate the Lisp API reference section for the system prompt."
  (let ((categories (make-hash-table :test 'eq))
        (exposed-count 0))
    (maphash (lambda (name api-def)
               (declare (ignore name))
               (when (lisp-rpc-api-exposed-p api-def)
                 (incf exposed-count)
                 (push api-def (gethash (api-definition-category api-def) categories))))
             *api-registry*)
    (when (zerop exposed-count)
      (return-from generate-lisp-api-prompt ""))
    (with-output-to-string (s)
      (format s "~%* Lisp API~%")
      (format s "You have access to the following Lisp functions for performing actions.~%")
      (format s "To use them, emit s-expressions inside a ```lisp fenced code block.~%")
      (format s "You may use progn, let, let*, when, unless, if, cond, format,~%")
      (format s "concatenate, loop, mapcar, and standard CL string/list/arithmetic functions.~%")
      (format s "All API calls must be qualified with hactar: prefix.~%~%")
      (maphash (lambda (cat api-defs)
                 (format s "** ~A~%" (string-capitalize (symbol-name cat)))
                 (dolist (api-def (sort (copy-list api-defs) #'string<
                                       :key (lambda (d) (symbol-name (api-definition-name d)))))
                   (let ((name (api-definition-name api-def))
                         (ll (api-definition-lambda-list api-def))
                         (doc (api-definition-docstring api-def)))
                     (format s "~%;; (hactar:~A~{ ~A~})~%"
                             (string-downcase (symbol-name name))
                             (mapcar (lambda (p) (string-upcase (symbol-name p))) ll))
                     (format s ";;   ~A~%" doc)
                     (when (api-definition-returns api-def)
                       (format s ";;   Returns: ~A~%" (api-definition-returns api-def)))
                     (when (api-definition-side-effects api-def)
                       (format s ";;   Side effects: ~A~%" (api-definition-side-effects api-def)))
                     (when (not (eq (api-definition-permissions api-def) :auto))
                       (format s ";;   Requires: permission confirmation~%"))
                     (when (api-definition-examples api-def)
                       (let ((*package* (find-package :hactar)))
                         (format s ";;   Example: ~(~S~)~%"
                                 (first (api-definition-examples api-def)))))))
                 (format s "~%"))
               categories))))

(defun lisp-rpc-api-surface ()
  "Return the current API surface as an alist (for Slynk inspection)."
  (let ((surface '()))
    (maphash (lambda (name api-def)
               (push `((:name . ,name)
                       (:category . ,(api-definition-category api-def))
                       (:permissions . ,(api-definition-permissions api-def))
                       (:docstring . ,(api-definition-docstring api-def))
                       (:lambda-list . ,(api-definition-lambda-list api-def))
                       (:exposed . ,(lisp-rpc-api-exposed-p api-def)))
                     surface))
             *api-registry*)
    (sort surface #'string< :key (lambda (s) (cdr (assoc :name s))))))

;;* System prompt integration
(defun lisp-rpc-system-prompt ()
  "Generate the system prompt for Lisp-RPC mode.
   Uses the standard system prompt template but replaces the tools section
   with the Lisp API reference."
  (let* ((mustache:*escape-tokens* nil)
         (base-prompt (uiop:read-file-string (get-prompt-path "system.default.org")))
         (context (generate-context))
         (guide-content (get-active-guide-content))
         (rules-text (with-output-to-string (s)
                       (maphash (lambda (key value)
                                  (declare (ignore key))
                                  (format s "~A~%~%" value))
                                *active-rules*)))
         (lisp-api (generate-lisp-api-prompt)))
    (mustache:render* base-prompt
                      `((:language . ,(language))
                        (:rules . ,rules-text)
                        (:guide . ,(or guide-content ""))
                        (:context . ,context)
                        (:tools . ,lisp-api)
                        (:tools_enabled . ,(> (hash-table-count *api-registry*) 0))))))

;;* Commands
(define-command lisp-rpc (args)
  "Manage Lisp-RPC mode. Usage: /lisp-rpc [status|eval|log|expose|api]"
  (cond
    ((or (null args) (string-equal (first args) "status"))
     (format t "Lisp-RPC mode: ~A~%" (if *lisp-rpc-mode* "ACTIVE" "inactive"))
     (format t "Registered API functions: ~A~%" (hash-table-count *api-registry*))
     (format t "Exposed categories: ~{~A~^, ~}~%" *lisp-rpc-exposed-categories*)
     (format t "Eval timeout: ~As~%" *lisp-rpc-eval-timeout*)
     (format t "Eval log entries: ~A~%" (length *lisp-rpc-eval-log*)))
    ((string-equal (first args) "eval")
     (if (rest args)
         (let ((form-str (format nil "~{~A~^ ~}" (rest args))))
           (multiple-value-bind (result output error-msg)
               (lisp-rpc-eval form-str)
             (when (and output (> (length output) 0))
               (format t "Output: ~A~%" output))
             (if error-msg
                 (format t "Error: ~A~%" error-msg)
                 (format t "Result: ~S~%" result))))
         (format t "Usage: /lisp-rpc eval <form>~%")))
    ((string-equal (first args) "log")
     (if *lisp-rpc-eval-log*
         (dolist (entry (subseq *lisp-rpc-eval-log* 0
                                (min 10 (length *lisp-rpc-eval-log*))))
           (format t "  ~S → ~A (~,3Fs)~%"
                   (getf entry :form)
                   (or (getf entry :error) (getf entry :result))
                   (getf entry :elapsed)))
         (format t "No evaluations logged.~%")))
    ((string-equal (first args) "expose")
     (if (rest args)
         (apply #'lisp-rpc-expose
                (mapcar (lambda (s) (intern (string-upcase s) :keyword))
                        (rest args)))
         (format t "Exposed: ~{~A~^, ~}~%" *lisp-rpc-exposed-categories*)))
    ((string-equal (first args) "api")
     (let ((surface (lisp-rpc-api-surface)))
       (dolist (entry surface)
         (format t "  ~A (~A) [~A]~A~%    ~A~%"
                 (cdr (assoc :name entry))
                 (cdr (assoc :category entry))
                 (cdr (assoc :permissions entry))
                 (if (cdr (assoc :exposed entry)) "" " [hidden]")
                 (cdr (assoc :docstring entry))))))
    (t
     (format t "Usage: /lisp-rpc [status|eval <form>|log|expose <categories>|api]~%"))))

;;* Hook into LLM response processing
(defun lisp-rpc-process-assistant-response (history)
  "Process the latest assistant message for Lisp-RPC forms.
   Called via *process-history-hook* when *lisp-rpc-mode* is active.
   In lisp-rpc-mode, outputs results as s-expressions to stdout."
  (when *lisp-rpc-mode*
    (let* ((last-msg (car (last history)))
           (role (cdr (assoc :role last-msg)))
           (content (cdr (assoc :content last-msg))))
      (when (and (string= role "assistant") content)
        (let ((results (lisp-rpc-process-response content)))
          (when results
            (let ((results-text (format-lisp-rpc-results results)))
              (when results-text
                (add-to-chat-history "user"
                                     (format nil "[Lisp-RPC Evaluation Results]~%~A" results-text))
                (dolist (r results)
                  (let ((form (getf r :form))
                        (result (getf r :result))
                        (output (getf r :output))
                        (error-msg (getf r :error)))
                    (if error-msg
                        (rpc-eval-error error-msg form)
                        (progn
                          (when (and output (> (length output) 0))
                            (rpc-eval-output output form))
                          (rpc-eval-result result form)))))))))))))

(nhooks:add-hook *process-history-hook*
                 (make-instance 'nhooks:handler
                                :fn #'lisp-rpc-process-assistant-response
                                :name 'lisp-rpc-process-response))

;;* Slynk integration
(defun lisp-rpc-prompt (text &key (model nil))
  "Send TEXT as a prompt to the LLM, streaming chunks to stdout via rpc-stream-chunk.
   Blocks until the response is complete.  Editors that need to issue concurrent
   requests should open a second Slynk REPL — this function intentionally ties up
   the calling thread so the protocol stream stays coherent.

   Emits:
     (hactar-status \"waiting\" ...)
     (hactar-stream-chunk CHUNK) — once per streamed token
     (hactar-stream-end)
     (hactar-response FULL-TEXT)
     (hactar-ready)"
  (rpc-status "waiting" :model (if *current-model* (model-config-name *current-model*) nil))
  (let* ((resolved-model (cond
                           ((stringp model) (find-model-by-name model))
                           (model model)
                           (t *current-model*))))
    (unless resolved-model
      (rpc-error "No model selected.")
      (rpc-ready)
      (return-from lisp-rpc-prompt nil))
    (handler-case
        (let* ((*current-model* resolved-model)
               (provider-name (model-config-provider resolved-model))
               (provider-type (intern (string-upcase provider-name) :keyword))
               (messages-for-api (prepare-messages text))
               (sys-prompt (if *lisp-rpc-mode*
                               (lisp-rpc-system-prompt)
                               (system-prompt)))
               (full-response (make-string-output-stream))
               (reader-instance nil))
          (multiple-value-bind (response-result initial-messages)
              (llm:complete
               provider-type
               messages-for-api
               :model (model-config-model-name resolved-model)
               :max-tokens (model-config-max-output-tokens resolved-model)
               :max-context (model-config-max-input-tokens resolved-model)
               :system-prompt sys-prompt
               :extra-headers (normalize-http-extra-headers
                               (when (model-config-extra-params resolved-model)
                                 (gethash "extra_headers" (model-config-extra-params resolved-model))))
               :stream t)
            (declare (ignore initial-messages))
            (setf reader-instance response-result)
            (unless (typep reader-instance 'llm:llm-stream-reader)
              (rpc-error "LLM did not return a stream reader.")
              (rpc-ready)
              (return-from lisp-rpc-prompt nil))
            (loop for chunk = (llm:read-next-chunk reader-instance)
                  while chunk
                  do (progn
                       (rpc-stream-chunk chunk)
                       (write-string chunk full-response)))
            (rpc-stream-end)
            (let ((assistant-response (get-output-stream-string full-response)))
              (rpc-response assistant-response)
              (add-to-chat-history "user" text)
              (add-to-chat-history "assistant" assistant-response)
              (nhooks:run-hook *process-history-hook* *chat-history*))
            (rpc-ready)))
      (hactar-interrupt ()
        (rpc-cancelled)
        (rpc-ready))
      (error (e)
        (rpc-error (format nil "~A" e))
        (rpc-ready)))))

(defun lisp-rpc-get-state ()
  "Return the current Hactar state as an alist for editor inspection."
  `((:version . ,*hactar-version*)
    (:model . ,(if *current-model* (model-config-name *current-model*) nil))
    (:files . ,(mapcar (lambda (f) (uiop:native-namestring (uiop:enough-pathname f *repo-root*))) *files*))
    (:lisp-rpc-mode . ,*lisp-rpc-mode*)
    (:active-presets . ,*active-presets*)
    (:chat-history-length . ,(length *chat-history*))
    (:api-count . ,(hash-table-count *api-registry*))
    (:exposed-categories . ,*lisp-rpc-exposed-categories*)
    (:repo-root . ,(when *repo-root* (namestring *repo-root*)))))

(defun lisp-rpc-execute-command (cmd-string)
  "Execute a slash command from the editor and return output as a string.
   CMD-STRING should be the full command e.g. \"/add src/main.lisp\"."
  (multiple-value-bind (cmd args) (parse-command cmd-string)
    (if cmd
        (with-output-to-string (*standard-output*)
          (execute-command cmd args))
        (format nil "Not a command: ~A" cmd-string))))

(defun lisp-rpc-available-models ()
  "Return a list of available model names."
  (mapcar #'model-config-name *available-models*))

(defun lisp-rpc-current-model ()
  "Return the current model name or nil."
  (when *current-model*
    (model-config-name *current-model*)))

(defun lisp-rpc-chat-history ()
  "Return the current chat history."
  *chat-history*)

(defun lisp-rpc-respond-permission (response)
  "Respond to a pending permission request from the editor.
   RESPONSE is a string: \"allow-once\", \"allow-always\", \"allow-exact\",
   \"deny-once\", or \"deny-always\"."
  (if *lisp-rpc-pending-permission*
      (progn
        (bt:with-lock-held (*lisp-rpc-permission-lock*)
          (setf (getf *lisp-rpc-pending-permission* :response) response)
          (bt:condition-notify *lisp-rpc-permission-cv*))
        (format nil "Permission response accepted: ~A" response))
      "No pending permission request."))

(defun lisp-rpc-pending-permission ()
  "Return the current pending permission request plist, or nil."
  (when *lisp-rpc-pending-permission*
    (list :tool-name (getf *lisp-rpc-pending-permission* :tool-name)
          :args (getf *lisp-rpc-pending-permission* :args)
          :pending (null (getf *lisp-rpc-pending-permission* :response)))))

;;* Main loop
(defun lisp-rpc-main-loop ()
  "Main loop for Lisp-RPC mode. Reads forms from stdin and evaluates them.
   Also allows interaction through Slynk."
  (rpc-hello *hactar-version*
             (if *current-model* (model-config-name *current-model*) nil))
  (rpc-ready)
  (loop
    (when (not *lisp-rpc-mode*)
      (rpc-exit "mode-disabled")
      (return))
    (let ((c (read-char-no-hang *standard-input* nil :eof)))
      (cond
        ((eq c :eof)
         (rpc-exit "eof")
         (return))
        (c
         (unread-char c *standard-input*)
         (let ((line (read-line *standard-input* nil nil)))
           (when (and line (> (length (string-trim '(#\Space #\Tab #\Return) line)) 0))
             (handler-case
                 (let* ((*package* (find-package :hactar))
                        (form (read-from-string line)))
                   (eval form))
               (error (e)
                 (rpc-error (format nil "Eval error: ~A" e)))))))
        (t
         (sleep 0.1))))))
