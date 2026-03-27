;; Command handling macros
(in-package :hactar)

(defvar *commands* (make-hash-table :test 'equal)
                   "Hash table of available slash commands.")

(defvar *sub-commands* (make-hash-table :test 'equal)
                   "Hash table of available sub-commands for the CLI.")

(defvar *acp-commands* (make-hash-table :test 'equal)
  "Hash table of ACP-compatible slash commands. Keys are command names (with /),
   values are handler functions that return an alist suitable for JSON-RPC response.")

(defun %command-fn-name (prefix name-key)
  "Generate an interned symbol for a command function from PREFIX and NAME-KEY."
  (intern (format nil "~A/~A" (string-upcase prefix) (string-upcase name-key)) :hactar))

(defmacro define-slash-command (name args &body body)
  "Define a slash command with the given name and arguments, capturing the docstring.
   NAME can be a symbol or a string (e.g., 'my-cmd', \"my.cmd\", \"/my.other.cmd\").
   Supports :cli-options keyword for defining command-line arguments.
   The command body receives a plist of parsed arguments if cli-options are present."
  (let* ((command-name-key
           (if (stringp name)
               (string-downcase (string-trim '(#\/) name))
               (string-downcase (symbol-name name))))
         (docstring (when (stringp (first body)) (first body)))
         (body-without-docstring (if (stringp (first body)) (rest body) body))
         (keyword-args-pos (position-if (lambda (x) (and (symbolp x) (keywordp x))) body-without-docstring))
         (main-body (if keyword-args-pos (subseq body-without-docstring 0 keyword-args-pos) body-without-docstring))
         (plist (if keyword-args-pos (subseq body-without-docstring keyword-args-pos) nil))
         (cli-options (getf plist :cli-options nil))
         (actual-args (gensym "ACTUAL-ARGS"))
         (fn-name (%command-fn-name "SLASH-CMD" command-name-key))
         (short-map (when cli-options
                      (loop for opt in cli-options
                            when (and (getf opt :short) (getf opt :long))
                            collect (cons (getf opt :short) (getf opt :long))))))
    `(progn
       (defun ,fn-name (,actual-args)
         ,@(when docstring (list docstring))
         (if (or (member "-h" ,actual-args :test #'string=)
                 (member "--help" ,actual-args :test #'string=))
             (progn
               (format t "Command: /~A~%" ,command-name-key)
               (format t "  ~A~%" ,(or docstring "No description available."))
               (when ',cli-options
                 (format t "  Options:~%")
                 (dolist (opt ',cli-options)
                   (let ((opt-short (getf opt :short))
                         (opt-long (getf opt :long))
                         (opt-desc (getf opt :description)))
                     (format t "    ")
                     (when opt-short (format t "-~A" opt-short))
                     (when (and opt-short opt-long) (format t ", "))
                     (when opt-long (format t "--~A" opt-long))
                     (format t " : ~A~%" opt-desc)))))
             (let ((parsed-args (if ',cli-options
                                    (let ((cli-string (format nil "~A ~{~A~^ ~}" ,command-name-key ,actual-args)))
                                      (parse-cli-args-s cli-string ',short-map))
                                    ,actual-args)))
               (destructuring-bind ,args (list parsed-args)
                 ,@main-body))))
       (setf (gethash ,(format nil "/~A" command-name-key) *commands*)
             (list ',fn-name
                   ,(or docstring "No description available.")
                   ',cli-options)))))

(defmacro define-sub-command (name args &body body)
  "Define a sub-command for CLI use. Sets *in-repl* to nil.
   NAME can be a symbol or a string (e.g., 'my-cmd', \"my.cmd\"). 
   Supports :cli-options keyword for defining command-line arguments. 
   The command body receives a plist of parsed arguments."
  (let* ((command-name-key
           (if (stringp name)
               (string-downcase (string-trim '(#\/) name))
               (string-downcase (symbol-name name))))
         (docstring (when (stringp (first body)) (first body)))
         (body-without-docstring (if (stringp (first body)) (rest body) body))
         (keyword-args-pos (position-if (lambda (x) (and (symbolp x) (keywordp x))) body-without-docstring))
         (main-body (if keyword-args-pos (subseq body-without-docstring 0 keyword-args-pos) body-without-docstring))
         (plist (if keyword-args-pos (subseq body-without-docstring keyword-args-pos) nil))
         (cli-options (getf plist :cli-options nil))
         (actual-args (gensym "ACTUAL-ARGS"))
         (fn-name (%command-fn-name "SUB-CMD" command-name-key))
         (short-map (when cli-options
                      (loop for opt in cli-options
                            when (and (getf opt :short) (getf opt :long))
                            collect (cons (getf opt :short) (getf opt :long))))))
    `(progn
       (defun ,fn-name (,actual-args)
         ,@(when docstring (list docstring))
         (let* ((*in-repl* nil)
                (*silent* t))
           (if (or (member "-h" ,actual-args :test #'string=)
                   (member "--help" ,actual-args :test #'string=))
               (progn
                 (format t "Command: ~A~%" ,command-name-key)
                 (format t "  ~A~%" ,(or docstring "No description available."))
                 (when ',cli-options
                   (format t "  Options:~%")
                   (dolist (opt ',cli-options)
                     (let ((opt-short (getf opt :short))
                           (opt-long (getf opt :long))
                           (opt-desc (getf opt :description)))
                       (format t "    ")
                       (when opt-short (format t "-~A" opt-short))
                       (when (and opt-short opt-long) (format t ", "))
                       (when opt-long (format t "--~A" opt-long))
                       (format t " : ~A~%" opt-desc)))))
               (let* ((parsed-args (if ',cli-options
                                       (let ((cli-string (format nil "~A ~{~A~^ ~}" ,command-name-key ,actual-args)))
                                         (parse-cli-args-s cli-string ',short-map))
                                       ,actual-args)))
                 (destructuring-bind ,args (list parsed-args)
                   ,@main-body)))))
       (setf (gethash ,(format nil "~A" command-name-key) *sub-commands*)
             (list ',fn-name
                   ,(or docstring "No description available.")
                   ',cli-options)))))

(defmacro define-command (name args &body body)
  "Define a command, available as a slash command and/or a sub-command.
   Use :slash and :sub keyword arguments at the end of the body to control availability.
   :slash defaults to t, :sub defaults to nil.
   Use :acp to enable ACP (JSON-RPC) compatibility:
     :acp t       — capture stdout from the command body, wrap it in a JSON response
     :acp <form>  — a lambda/function form called with (args) that returns an alist for JSON response
   Use :cli-options to define CLI arguments for the sub-command. 
   When used as a sub-command, the body receives a plist of parsed arguments. 
   When used as a slash command, the body receives a list of string arguments."
  (let* ((docstring (when (stringp (first body)) (first body)))
         (body-without-docstring (if docstring (rest body) body))
         (keyword-args-pos (position-if (lambda (x) (and (symbolp x) (keywordp x))) body-without-docstring))
         (main-body (if keyword-args-pos (subseq body-without-docstring 0 keyword-args-pos) body-without-docstring))
         (plist (if keyword-args-pos (subseq body-without-docstring keyword-args-pos) nil))
         (slash (getf plist :slash t))
         (subby (getf plist :sub nil))
         (acp-opt (getf plist :acp nil))
         (cli-options (getf plist :cli-options nil))
         (slash-body (append (when docstring (list docstring)) main-body (when cli-options `(:cli-options ,cli-options))))
         (sub-body (append (when docstring (list docstring)) main-body (when cli-options `(:cli-options ,cli-options))))
         (command-name-key
           (if (stringp name)
               (string-downcase (string-trim '(#\/) name))
               (string-downcase (symbol-name name))))
         (full-cmd-name (format nil "/~A" command-name-key))
         (acp-fn-name (%command-fn-name "ACP-CMD" command-name-key))
         (slash-fn-name (%command-fn-name "SLASH-CMD" command-name-key)))
    `(progn
       ,@(when slash
           `((define-slash-command ,name ,args ,@slash-body)))
       ,@(when subby
           `((define-sub-command ,name ,args ,@sub-body)))
       ,@(when acp-opt
           (cond
             ((eq acp-opt t)
              ;; :acp t — wrap stdout output in a JSON text response
              `((defun ,acp-fn-name (cmd-args)
                  (declare (ignorable cmd-args))
                  (let ((output (with-output-to-string (*standard-output*)
                                  (funcall ',slash-fn-name cmd-args))))
                    `(("text" . ,output))))
                (setf (gethash ,full-cmd-name *acp-commands*) ',acp-fn-name)))
             (t
              ;; :acp <function-form> — call the function which returns an alist
              `((setf (gethash ,full-cmd-name *acp-commands*)
                      ,acp-opt))))))))

(defmacro def-acp-command (name args &body body)
  "Define an ACP-only command handler. The handler receives ARGS (list of strings)
   and must return an alist that will be sent as the JSON-RPC result.
   Also registers a basic slash command that prints the JSON output in non-ACP mode."
  (let* ((command-name-key
           (if (stringp name)
               (string-downcase (string-trim '(#\/) name))
               (string-downcase (symbol-name name))))
         (full-cmd-name (format nil "/~A" command-name-key))
         (docstring (when (stringp (first body)) (first body)))
         (impl-body (if docstring (rest body) body))
         (fn-name (intern (format nil "ACP-CMD-~A" (string-upcase command-name-key)) :hactar)))
    `(progn
       (defun ,fn-name ,args
         ,@(when docstring (list docstring))
         ,@impl-body)
       (setf (gethash ,full-cmd-name *acp-commands*) ',fn-name)
       ;; Also register as a regular slash command for non-ACP use
       (unless (gethash ,full-cmd-name *commands*)
         (setf (gethash ,full-cmd-name *commands*)
               (list (lambda (cmd-args)
                       (let ((result (funcall ',fn-name cmd-args)))
                         (format t "~A~%" (to-json result))))
                     ,(or docstring "ACP command")
                     nil)))
       ',name)))

(defun acp-command-p (cmd-name)
  "Return T if CMD-NAME (e.g. \"/add\") has an ACP-compatible handler."
  (not (null (gethash cmd-name *acp-commands*))))

(defun execute-acp-command (cmd args)
  "Execute an ACP command handler and return the result alist.
   Returns NIL if no ACP handler is registered."
  (let ((handler (gethash cmd *acp-commands*)))
    (when handler
      (funcall handler args))))

(defun parse-command (input)
  "Parse a command from the input string. Handles /cmd and .cmd formats."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline) input))
         (is-dot-command (and (> (length trimmed) 0) (char= (char trimmed 0) #\.)))
         (is-slash-command (and (> (length trimmed) 0) (char= (char trimmed 0) #\/)))
         (space-pos (position #\Space trimmed)))
    (if (or is-dot-command is-slash-command)
        (if space-pos
            (values (subseq trimmed 0 space-pos) ; Command name (e.g., "/help", ".cat")
                    (tokenize-cli-string (subseq trimmed (1+ space-pos)))) ; Args list
            (values trimmed nil)) ; Command name, no args
        (values nil trimmed)))) ; Not a command, likely a prompt

(defun execute-command (cmd args)
  "Execute a command (slash or dot) with the given arguments."
  (cond
    ((str:starts-with? "/" cmd)
     (let ((command-info (gethash cmd *commands*)))
       (if command-info
           (let ((cmd-fn (first command-info)))
             (funcall cmd-fn args))
           (format t "Unknown slash command: ~A~%" cmd))))
    ((str:starts-with? "." cmd)
     (let ((command-info (gethash cmd *dot-commands*)))
       (if command-info
           (let ((cmd-fn (first command-info)))
             ;; Dot command functions are responsible for their interaction with LLM
             ;; and printing output or triggering processors.
             (funcall cmd-fn args))
           (format t "Unknown dot command: ~A~%" cmd))))
    (t (format t "Invalid command format (expected /cmd or .cmd): ~A~%" cmd))))

(defun command-completer (text start end)
  "Complete command names and arguments."
  (declare (ignore end))
  (if (zerop start)
    ;; Complete command names
    (let ((matches (loop for cmd being the hash-keys of *commands*
                         when (str:starts-with-p text cmd :ignore-case t)
                         collect cmd)))
      (if (cdr matches)
        (cons (str:prefix matches) matches)
        matches))
    ;; Complete arguments based on the command
    (let* ((line (subseq rl:*line-buffer* 0 rl:*point*))
           (words (cl-ppcre:split "\\s+" line))
           (cmd (first words)))
      (cond
        ((member cmd '("/add" "/drop") :test #'string=)
          ;; Complete with filenames
          (let* ((prefix (if (string= text "") "./" text))
                 (dir (directory-namestring prefix))
                 (name-prefix (file-namestring prefix))
                 (files (when (probe-file dir)
                          (mapcar #'namestring
                                  (directory (merge-pathnames
                                               (make-pathname :name :wild :type :wild)
                                               dir))))))
            (remove-if-not (lambda (file)
                             (str:starts-with-p name-prefix (file-namestring file) :ignore-case t))
                           files)))
        ((string= cmd "/docs-add")
          ;; Complete with filenames or suggest @p/
          (let* ((prefix text)
                 (dir (directory-namestring prefix))
                 (name-prefix (file-namestring prefix))
                 (files (when (probe-file dir)
                          (mapcar #'namestring
                                  (directory (merge-pathnames
                                               (make-pathname :name :wild :type :wild)
                                               dir))))))
            (append (list "@p/") ; Suggest package prefix
                    (remove-if-not (lambda (file)
                                     (str:starts-with-p name-prefix (file-namestring file) :ignore-case t))
                                   files))))
        ((string= cmd "/guides")
          nil)
        ((string= cmd "/guide")
	 nil)
        ((string= cmd "/guides-gen")
	 nil)
        ((string= cmd "/model")
          (let ((matches (loop for model in *available-models*
                               when (str:starts-with-p text (model-config-name model) :ignore-case t)
                               collect (model-config-name model))))
            (if (cdr matches)
              (cons (str:prefix matches) matches)
              matches)))
        ((member cmd '("/mold.use" "/mold.show" "/mold.export") :test #'string=)
          (let ((matches (loop for name being the hash-keys of *molds*
                               when (str:starts-with-p text name :ignore-case t)
                               collect name)))
            (if (cdr matches)
              (cons (str:prefix matches) matches)
              matches)))
        ((string= cmd "/mold.pour")
          (let ((matches (when *active-mold*
                           (loop for entity in (mold-definition-entities *active-mold*)
                                 for name = (string-downcase
                                             (if (stringp (mold-entity-name entity))
                                                 (mold-entity-name entity)
                                                 (symbol-name (mold-entity-name entity))))
                                 when (str:starts-with-p text name :ignore-case t)
                                 collect name))))
            (if (cdr matches)
              (cons (str:prefix matches) matches)
              matches)))
        ((string= cmd "/mold.install")
          ;; Complete with filenames for local installs
          (let* ((prefix (if (string= text "") "./" text))
                 (dir (directory-namestring prefix))
                 (name-prefix (file-namestring prefix))
                 (files (when (probe-file dir)
                          (mapcar #'namestring
                                  (directory (merge-pathnames
                                               (make-pathname :name :wild :type :wild)
                                               dir))))))
            (remove-if-not (lambda (file)
                             (str:starts-with-p name-prefix (file-namestring file) :ignore-case t))
                           files)))
        (t nil)))))
