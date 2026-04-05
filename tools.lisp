;; Tools Handling - XML-based and API-based tool calling
(in-package :hactar)

;;* Tool Definition Structure

(defstruct tool-definition
  "Structure representing a defined tool."
  (name "" :type string)
  (description "" :type string)
  (parameters nil :type list)
  (rules nil :type (or null string))
  (permissions :confirm :type keyword)
  (function nil :type (or null function)))

(defstruct tool-parameter
  "Structure representing a tool parameter."
  (name "" :type string)
  (type :string :type keyword)
  (description "" :type string)
  (required t :type boolean)
  (enum nil :type list))

;;* deftool Macro

(defmacro deftool (name (&rest params) &body body)
  "Define a tool that Hactar can use.

NAME: Symbol, the name of the tool (will be converted to snake_case).
PARAMS: Parameter specifications, each being:
        (param-name type &key description required enum)
        or just param-name for a required string parameter.
BODY: The body can start with keyword options:
      :description \"...\" - Tool description (required)
      :rules \"...\" - Optional usage rules/guidelines
      :permissions :confirm/:auto - Permission level (default :confirm)
      Then the actual implementation code.

Example:
  (deftool read-file ((path :string :description \"File path\" :required t))
    :description \"Read contents of a file\"
    :rules \"Only read files within the project directory\"
    :permissions :confirm
    (let ((file-path (getf args :path)))
      (uiop:read-file-string file-path)))"
  (let* ((tool-name-str (string-downcase (substitute #\_ #\- (symbol-name name))))
         (fn-name (intern (format nil "TOOL-~A" (symbol-name name)) :hactar))
         ;; Parse body for keyword options
         (description nil)
         (rules nil)
         (permissions :confirm)
         (impl-body body))
    
    (loop while (and impl-body (keywordp (car impl-body)))
          do (case (car impl-body)
               (:description (setf description (cadr impl-body)))
               (:rules (setf rules (cadr impl-body)))
               (:permissions (setf permissions (cadr impl-body))))
             (setf impl-body (cddr impl-body)))
    
    (unless description
      (error "deftool ~A requires a :description" name))
    
    `(progn
       (defun ,fn-name (hactar::%tool-args%)
         "Tool implementation function. HACTAR::%TOOL-ARGS% is a plist of parameter values."
         (let ((hactar::args hactar::%tool-args%))
           (declare (ignorable hactar::args))
           ,@impl-body))
       
       (setf (gethash ,tool-name-str *defined-tools*)
             (make-tool-definition
              :name ,tool-name-str
              :description ,description
              :parameters (list ,@(mapcar #'parse-param-spec params))
              :rules ,rules
              :permissions ,permissions
              :function #',fn-name))
       
       (unless *silent*
         (format t "Defined tool: ~A~%" ,tool-name-str))
       ',name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-param-spec (spec)
    "Parse a parameter specification into a make-tool-parameter form."
    (if (symbolp spec)
        ;; Simple form: just a name, defaults to required string
        `(make-tool-parameter
          :name ,(string-downcase (symbol-name spec))
          :type :string
          :description ""
          :required t)
        ;; Full form: (name type &key description required enum)
        (destructuring-bind (name type &key (description "") (required t) enum) spec
          `(make-tool-parameter
            :name ,(string-downcase (symbol-name name))
            :type ,type
            :description ,description
            :required ,required
            :enum ',enum)))))

;;* XML Generation for Tools-in-Prompt Mode

(defun generate-tool-xml-description (tool)
  "Generate XML description for a single tool."
  (with-output-to-string (s)
    (format s "<tool name=\"~A\">~%" (tool-definition-name tool))
    (format s "  <description>~A</description>~%" (tool-definition-description tool))
    
    (format s "  <parameters>~%")
    (dolist (param (tool-definition-parameters tool))
      (format s "    <parameter name=\"~A\" type=\"~A\" required=\"~A\">~%"
              (tool-parameter-name param)
              (string-downcase (symbol-name (tool-parameter-type param)))
              (if (tool-parameter-required param) "true" "false"))
      (when (tool-parameter-description param)
        (format s "      <description>~A</description>~%" (tool-parameter-description param)))
      (when (tool-parameter-enum param)
        (format s "      <enum>~{~A~^, ~}</enum>~%" (tool-parameter-enum param)))
      (format s "    </parameter>~%"))
    (format s "  </parameters>~%")
    
    (when (tool-definition-rules tool)
      (format s "  <rules>~%~A~%  </rules>~%" (tool-definition-rules tool)))
    
    (format s "  <usage>~%")
    (format s "<~A>~%" (tool-definition-name tool))
    (dolist (param (tool-definition-parameters tool))
      (format s "<~A>value</~A>~%" (tool-parameter-name param) (tool-parameter-name param)))
    (format s "</~A>~%" (tool-definition-name tool))
    (format s "  </usage>~%")
    
    (format s "</tool>")))

(defun generate-all-tools-xml ()
  "Generate XML descriptions for all defined tools.
   Only generates output when *tools-in-system-prompt* is T."
  (when (and *tool-use-enabled* *tools-in-system-prompt* (> (hash-table-count *defined-tools*) 0))
    (with-output-to-string (s)
      (format s "<tools>~%")
      (maphash (lambda (name tool)
                 (declare (ignore name))
                 (format s "~A~%~%" (generate-tool-xml-description tool)))
               *defined-tools*)
      (format s "</tools>"))))

;;* API-based Tool Definitions (OpenAI/Anthropic format)

(defun tool-parameter-to-json-schema (param)
  "Convert a tool-parameter to JSON schema format."
  (let ((schema `((:type . ,(case (tool-parameter-type param)
                              (:string "string")
                              (:boolean "boolean")
                              (:number "number")
                              (:array "array")
                              (t "string")))
                  (:description . ,(tool-parameter-description param)))))
    (when (tool-parameter-enum param)
      (push `(:enum . ,(coerce (tool-parameter-enum param) 'vector)) schema))
    schema))

(defun tool-definition-to-api-format (tool)
  "Convert a tool-definition to OpenAI/Anthropic API format."
  (let* ((required-params (remove-if-not #'tool-parameter-required
                                          (tool-definition-parameters tool)))
         (properties (loop for param in (tool-definition-parameters tool)
                           collect (cons (intern (string-upcase (tool-parameter-name param)) :keyword)
                                        (tool-parameter-to-json-schema param)))))
    `((:type . "function")
      (:function . ((:name . ,(tool-definition-name tool))
                    (:description . ,(tool-definition-description tool))
                    (:parameters . ((:type . "object")
                                   (:properties . ,properties)
                                   (:required . ,(coerce (mapcar #'tool-parameter-name required-params) 'vector)))))))))

(defun get-tools-for-api ()
  "Get all tools formatted for the LLM API (when *tools-in-system-prompt* is NIL)."
  (when (and *tool-use-enabled* (not *tools-in-system-prompt*))
    (let ((tools '()))
      (maphash (lambda (name tool)
                 (declare (ignore name))
                 (push (tool-definition-to-api-format tool) tools))
               *defined-tools*)
      (when tools
        (coerce (nreverse tools) 'vector)))))

;;* XML Tool Call Parsing

(defun parse-xml-tool-calls (text)
  "Parse XML tool calls from LLM response text.
Returns a list of (tool-name . args-alist) pairs."
  (let ((tool-calls '()))
    (maphash (lambda (tool-name tool-def)
               (declare (ignore tool-def))
               ;; Match <tool_name>...</tool_name> blocks (including multiline content)
               (let ((pattern (format nil "(?s)<~A>([\\s\\S]*?)</~A>" tool-name tool-name)))
                 (cl-ppcre:do-register-groups (content) (pattern text)
                   (let ((args (parse-xml-tool-args content tool-name)))
                     (push (cons tool-name args) tool-calls)))))
             *defined-tools*)
    (let ((result (nreverse tool-calls)))
      (when (and *in-editor* result)
        (editor-output
         (format nil "Parsed ~A tool call~:P:~%~{  - ~A~%~}"
                 (length result)
                 (mapcar #'car result))
         :type "tool-calls-parsed"
         :success "true"
         :print-done nil))
      result)))

(defun parse-xml-tool-args (content tool-name)
  "Parse XML arguments from tool call content."
  (let ((tool (gethash tool-name *defined-tools*))
        (args '()))
    (when tool
      (dolist (param (tool-definition-parameters tool))
        (let* ((param-name (tool-parameter-name param))
               (pattern (format nil "<~A>([\\s\\S]*?)</~A>" param-name param-name)))
          (multiple-value-bind (match groups)
              (cl-ppcre:scan-to-strings pattern content)
            (declare (ignore match))
            (when (and groups (> (length groups) 0))
              (let ((value (string-trim '(#\Space #\Tab #\Newline #\Return) (aref groups 0))))
                (setf value (case (tool-parameter-type param)
                              (:boolean (string-equal value "true"))
                              (:number (parse-integer value :junk-allowed t))
                              (t value)))
                (push (cons (intern (string-upcase param-name) :keyword) value) args)))))))
    (nreverse args)))

;;* Tool Execution

(defun execute-tool (tool-name args)
  "Execute a tool by name with the given arguments.
ARGS can be an alist or plist of parameter values.
Returns (values result success-p error-message).
Uses the permissions system (resolve-permission) instead of simple :confirm checks."
  (let ((tool (gethash tool-name *defined-tools*)))
    (unless tool
      (return-from execute-tool
        (values nil nil (format nil "Tool '~A' not found" tool-name))))
    
    (let ((args-plist (if (and args 
                               (consp (car args)) 
                               (not (null (cdr (car args))))
                               (atom (cdr (car args))))
                          (loop for (key . val) in args
                                append (list key val))
                          args)))
      (let ((decision (resolve-permission tool-name args-plist)))
        (case decision
          (:deny
           (return-from execute-tool
             (values "Tool execution denied by permission rule." t nil)))
          (:confirm
           (let ((user-decision (prompt-tool-permission tool-name args-plist)))
             (when (eq user-decision :deny)
               (return-from execute-tool
                 (values "Tool execution denied by user." t nil)))))
          (:allow nil)))
      (let ((tui-call-id (when *tui-running*
                           (let ((id (format nil "tui_~A_~A" tool-name (get-universal-time))))
                             (tui-add-tool-call id tool-name nil)
                             (tui-update-tool-call id "in_progress")
                             id))))
        (handler-case
            (let ((result (funcall (tool-definition-function tool) args-plist)))
              (when tui-call-id
                (tui-update-tool-call tui-call-id "completed"))
              (when *in-editor*
                (editor-log-write
                 (format nil "Tool: ~A~%Args: ~S~%Result: ~A" tool-name args-plist result)
                 :type "tool-result")
                (editor-done-marker :success "true"))
              (values result t nil))
          (error (e)
            (when tui-call-id
              (tui-update-tool-call tui-call-id "failed"))
            (let ((err-msg (format nil "Error executing tool '~A': ~A" tool-name e)))
              (when *in-editor*
                (editor-log-write
                 (format nil "Tool: ~A~%Args: ~S~%Error: ~A" tool-name args-plist err-msg)
                 :type "tool-error")
                (editor-done-marker :success "false"))
              (values nil nil err-msg))))))))

(defun execute-xml-tool-calls (tool-calls)
  "Execute a list of parsed XML tool calls.
Returns a list of (tool-name result success-p error) for each call."
  (loop for (tool-name . args) in tool-calls
        collect (multiple-value-bind (result success-p error-msg)
                    (execute-tool tool-name args)
                  (list tool-name result success-p error-msg))))

;;* API Tool Call Handling

(defun handle-api-tool-calls (tool-calls-list)
  "Handle tool calls from the LLM API response.
TOOL-CALLS-LIST is the list of tool calls in API format.
Returns a list of tool result messages."
  (let ((results '())
        (all-succeeded t))
    (dolist (tc tool-calls-list)
      (let* ((tool-call-id (or (cdr (assoc :id tc))
                               (format nil "call_~A" (uuid:make-v4-uuid))))
             (function-info (cdr (assoc :function tc)))
             (function-name (cdr (assoc :name function-info)))
             (arguments-json (cdr (assoc :arguments function-info)))
             (args (handler-case 
                       (cl-json:decode-json-from-string arguments-json)
                     (error () nil))))
        
        (multiple-value-bind (result success-p error-msg)
            (execute-tool function-name args)
          (unless success-p (setf all-succeeded nil))
          (let ((content (if success-p
                            (if (stringp result) result (format nil "~S" result))
                            (or error-msg "Unknown error"))))
            (push `((:role . "tool")
                    (:tool_call_id . ,tool-call-id)
                    (:name . ,function-name)
                    (:content . ,content))
                  results)))))
    (when *in-editor*
      (editor-done-marker :success (if all-succeeded "true" "false")))
    (nreverse results)))

;;* Tool Response Formatting

(defun format-tool-results-for-prompt (results)
  "Format tool execution results for inclusion in a follow-up prompt.
RESULTS is a list from execute-xml-tool-calls."
  (let ((formatted (with-output-to-string (s)
                     (format s "<tool_results>~%")
                     (dolist (result results)
                       (destructuring-bind (tool-name output success-p error-msg) result
                         (format s "<result tool=\"~A\" success=\"~A\">~%"
                                 tool-name (if success-p "true" "false"))
                         (if success-p
                             (format s "~A~%" output)
                             (format s "<error>~A</error>~%" error-msg))
                         (format s "</result>~%")))
                     (format s "</tool_results>"))))
    (when *in-editor*
      (let ((all-succeeded (every (lambda (r) (third r)) results)))
        (editor-output formatted :type "tool-results-xml"
                       :success (if all-succeeded "true" "false")
                       :print-done t)))
    formatted))

;;* Commands

(define-command tool-call (args)
  "Manually call a defined tool. Usage: /tool-call <tool_name> [args...]"
  (if (null args)
      (format t "Usage: /tool-call <tool_name> [key=value ...]~%")
      (let* ((tool-name (first args))
             (arg-pairs (rest args))
             (args-plist (loop for pair in arg-pairs
                               for pos = (position #\= pair)
                               when pos
                               append (list (intern (string-upcase (subseq pair 0 pos)) :keyword)
                                           (subseq pair (1+ pos))))))
        (multiple-value-bind (result success-p error-msg)
            (execute-tool tool-name args-plist)
          (if success-p
              (format t "Result: ~A~%" result)
              (format t "Error: ~A~%" error-msg))))))

(define-command tools (args)
  "List available tools and their descriptions."
  (declare (ignore args))
  (if (zerop (hash-table-count *defined-tools*))
      (format t "No tools defined.~%")
      (let ((output (with-output-to-string (s)
                      (format s "Available tools (~A mode):~%" 
                              (if *tools-in-system-prompt* "XML-in-system-prompt" "API"))
                      (format s "~%")
                      (maphash (lambda (name tool)
                                 (declare (ignore name))
                                 (format s "  ~A~%" (tool-definition-name tool))
                                 (format s "    ~A~%" (tool-definition-description tool))
                                 (format s "    Parameters:~%")
                                 (dolist (param (tool-definition-parameters tool))
                                   (format s "      - ~A (~A~A): ~A~%"
                                           (tool-parameter-name param)
                                           (tool-parameter-type param)
                                           (if (tool-parameter-required param) ", required" "")
                                           (tool-parameter-description param)))
                                 (format s "~%"))
                               *defined-tools*))))
        (if *in-editor*
            (editor-output output :type "tools-list" :success "true")
            (format t "~A" output)))))

(define-command tools-mode (args)
  "Toggle or set tools mode. Usage: /tools-mode [prompt|api]"
  (cond
    ((null args)
     (setf *tools-in-system-prompt* (not *tools-in-system-prompt*)))
    ((string-equal (first args) "prompt")
     (setf *tools-in-system-prompt* t))
    ((string-equal (first args) "api")
     (setf *tools-in-system-prompt* nil)))
  (format t "Tools mode: ~A~%" (if *tools-in-system-prompt* "XML-in-system-prompt" "API")))

;;* Built-in Core Tools

(deftool execute-command ((command :string 
                           :description "The CLI command to execute"
                           :required t)
                         (requires-approval :boolean
                           :description "Whether this command requires explicit user approval"
                           :required t))
  :description "Execute a CLI command on the system. Use this when you need to perform system operations or run specific commands."
  :rules "Commands are executed in the current working directory. Use appropriate command chaining syntax for the shell. Prefer complex CLI commands over creating executable scripts. Set requires_approval to true for potentially impactful operations like installing/uninstalling packages, deleting/overwriting files, system configuration changes, or network operations."
  :permissions :confirm
  (let ((cmd (getf args :command))
        (needs-approval (getf args :requires-approval)))
    (when (and needs-approval 
               (not (confirm-action (format nil "Execute command: ~A?" cmd))))
      (return-from tool-execute-command "Command execution denied by user."))
    (when (and *acp-mode* (acp-client-has-capability? '("terminal")))
      (handler-case
          (let* ((cwd (when *repo-root* (namestring *repo-root*)))
                 (terminal-id (acp-terminal-create *shell*
                                :args (list "-c" cmd)
                                :cwd cwd
                                :output-byte-limit 1048576)))
            (when terminal-id
              (let* ((exit-result (acp-terminal-wait-for-exit terminal-id))
                     (output-result (acp-terminal-output terminal-id))
                     (exit-code (cdr (assoc "exitCode" exit-result :test #'string=)))
                     (output (cdr (assoc "output" output-result :test #'string=))))
                (acp-terminal-release terminal-id)
                (return-from tool-execute-command
                  (if (and exit-code (zerop exit-code))
                      (or output "")
                      (format nil "Command failed (exit ~A):~%~A" 
                              (or exit-code "unknown") (or output "")))))))
        (error (e)
          (acp-log "Terminal fallback to local exec: ~A" e))))
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program cmd
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (if (zerop exit-code)
              (format nil "~A~@[~A~]" output (when (> (length error-output) 0) 
                                               (format nil "~%stderr: ~A" error-output)))
              (format nil "Command failed (exit ~A):~%stdout: ~A~%stderr: ~A" 
                      exit-code output error-output)))
      (error (e)
        (format nil "Error executing command: ~A" e)))))

(deftool read-file ((path :string
                     :description "The path of the file to read (relative to current working directory)"
                     :required t))
  :description "Read the contents of a file at the specified path. Use this to examine existing files, analyze code, review text files, or extract information from configuration files."
  :rules "Only read files within the project directory. Automatically handles text files. May not be suitable for binary files."
  :permissions :auto
  (let ((file-path (getf args :path)))
    (handler-case
        (let ((full-path (merge-pathnames file-path *repo-root*)))
          (when (and *acp-mode* (acp-client-has-capability? '("fs" "readTextFile")))
            (let ((content (acp-fs-read-text-file (namestring full-path))))
              (when content
                (add-file-to-context (uiop:native-namestring full-path))
                (return-from tool-read-file
                  (format nil "~A~%~A" file-path content)))))
          (if (probe-file full-path)
              (progn
                (add-file-to-context (uiop:native-namestring full-path))
                (format nil "Added ~A~% to context" file-path))
              (format nil "Error: File not found: ~A" file-path)))
      (error (e)
        (format nil "Error reading file: ~A" e)))))

(deftool write-to-file ((path :string
                         :description "The path of the file to write to (relative to current working directory)"
                         :required t)
                        (content :string
                         :description "The complete content to write to the file"
                         :required t))
  :description "Write content to a file at the specified path. If the file exists, it will be overwritten. If it doesn't exist, it will be created. Automatically creates any needed directories."
  :rules "ALWAYS provide the COMPLETE intended content of the file, without any truncation or omissions. Include ALL parts of the file, even if they haven't been modified."
  :permissions :auto
  (let ((file-path (getf args :path))
        (content (getf args :content)))
    (handler-case
        (let ((full-path (merge-pathnames file-path *repo-root*)))
          (when (and *acp-mode* (acp-client-has-capability? '("fs" "writeTextFile")))
            (when (acp-fs-write-text-file (namestring full-path) content)
              (return-from tool-write-to-file
                (format nil "Successfully wrote ~A bytes to ~A" (length content) file-path))))
          (safe-write-to-file file-path content)
          (format nil "Successfully wrote ~A bytes to ~A" (length content) file-path))
      (error (e)
        (format nil "Error writing file: ~A" e)))))

(deftool replace-in-file ((path :string
                           :description "The path of the file to modify (relative to current working directory)"
                           :required t)
                          (search :string
                           :description "The exact text to search for in the file"
                           :required t)
                          (replace :string
                           :description "The text to replace the search text with"
                           :required t))
  :description "Replace a section of content in an existing file. Searches for the exact text and replaces the first occurrence with the new text. Use this for targeted changes to specific parts of a file."
  :rules "SEARCH content must match EXACTLY character-for-character including whitespace and indentation. Only replaces the first match occurrence. Use empty REPLACE to delete code. For multiple replacements, call the tool multiple times."
  :permissions :auto
  (let ((file-path (getf args :path))
        (search-text (getf args :search))
        (replace-text (getf args :replace)))
    (handler-case
        (if (safe-replace-in-file file-path search-text replace-text)
            (format nil "Successfully replaced text in ~A" file-path)
            (format nil "Error: Search text not found in ~A" file-path))
      (error (e)
        (format nil "Error modifying file: ~A" e)))))
