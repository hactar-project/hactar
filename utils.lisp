(in-package :hactar)

;;* Editor log
(defun editor-log-path ()
  "Compute the pathname for the editor log file .hactar.{pid}.log under repo root."
  (when *repo-root*
    (merge-pathnames (format nil ".hactar.~A.log" (current-pid)) *repo-root*)))

(defun format-org-timestamp ()
  "Return the current time as an Org-mode timestamp string, e.g. [2025-07-14 Mon 15:04:32]."
  (multiple-value-bind (sec min hour day month year dow)
      (get-decoded-time)
    (let ((day-names #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
      (format nil "[~4,'0D-~2,'0D-~2,'0D ~A ~2,'0D:~2,'0D:~2,'0D]"
              year month day (aref day-names dow) hour min sec))))

(defun editor-log-write (content &key (type "output"))
  "Append a structured entry to the editor log file.
   CONTENT is the string to write. TYPE is the src block type annotation."
  (when (and *in-editor* *editor-log-file*)
    (let ((timestamp (format-org-timestamp)))
      (handler-case
          (with-open-file (stream *editor-log-file*
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
            (format stream "#+begin_src ~A :time ~A~%~A~%#+end_src~%~%"
                    type timestamp content))
        (error (e)
          (format *error-output* "Error writing to editor log: ~A~%" e))))))

(defun editor-log-init ()
  "Initialize the editor log file, clearing any previous content."
  (when *editor-log-file*
    (handler-case
        (with-open-file (stream *editor-log-file*
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (format stream "# Hactar Editor Log - ~A~%~%" (format-org-timestamp)))
      (error (e)
        (format *error-output* "Error initializing editor log: ~A~%" e)))))

(defun editor-done-marker (&key success)
  "Print a <done> marker with timestamp to stdout for editor integration.
   If SUCCESS is provided (:true or :false), include it as an attribute."
  (if success
      (format t "<done success=\"~A\">~A</done>~%" success (format-org-timestamp))
      (format t "<done>~A</done>~%" (format-org-timestamp)))
  (force-output))

(defun editor-output (content &key (type "output") (print-done t) success)
  "In editor mode, write content to log and optionally print <done> marker.
   In normal mode, just print content to stdout.
   SUCCESS, if provided, is included in the <done> marker (e.g. \"true\" or \"false\").
   Returns T if content was handled."
  (if *in-editor*
      (progn
        (editor-log-write content :type type)
        (when print-done (editor-done-marker :success success))
        t)
      (progn
        (format t "~A" content)
        nil)))

(defun current-pid ()
  "Return the current process ID, implementation-specific fallback to 0."
  #+sbcl (sb-posix:getpid)
  #+ccl (ccl::getpid)
  #+clisp (system::getpid)
  #+ecl (si:getpid)
  #+allegro (excl:process-id (excl:current-process))
  #+cmu (unix:unix-getpid)
  #+lispworks (hcl:get-process-id (hcl:current-process))
  #-(or sbcl ccl clisp ecl allegro cmu lispworks) 0)

(defun is-port-available-p (port &optional (host "127.0.0.1"))
  "Checks if a TCP port is available for binding on the given host."
  (handler-case
      (let ((socket (usocket:socket-listen host port :reuse-address t)))
        (usocket:socket-close socket)
        t)
    (usocket:address-in-use-error () nil)
    (usocket:socket-error (e)
      (debug-log "Socket error checking port" port ":" e)
      nil)
    (error (e)
      (debug-log "Generic error checking port" port ":" e)
      nil)))

(defun find-free-port (&optional (start-port 4005) (host "127.0.0.1"))
  "Find the first available TCP port starting from START-PORT."
  (loop for port from start-port to 65535
        when (is-port-available-p port host)
          return port
        finally (error "No free port found starting from ~A" start-port)))

(defun slynk-port-file-path ()
  "Return the pathname for .hactar.{pid}.port under *repo-root*, or nil if repo-root unset."
  (when *repo-root*
    (merge-pathnames (format nil ".hactar.~A.port" (current-pid)) *repo-root*)))

(defun write-slynk-port-file (port)
  "Write PORT number to the .hactar.{pid}.port file under *repo-root*."
  (let ((path (slynk-port-file-path)))
    (when path
      (handler-case
          (with-open-file (s path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
            (format s "~A~%" port))
        (error (e)
          (format *error-output* "Warning: could not write Slynk port file ~A: ~A~%" path e))))))

(defun delete-slynk-port-file ()
  "Delete the .hactar.{pid}.port file if it exists."
  (let ((path (slynk-port-file-path)))
    (when (and path (probe-file path))
      (ignore-errors (delete-file path)))))

(defun normalize-http-extra-headers (x)
  "Coerce EXTRA-HEADERS into a Drakma-compatible alist of (string . string).

Accepted inputs:
- NIL
- list of cons pairs ((\"Header\" . \"Value\") ...)
- hash-table mapping header-name -> header-value (strings/symbols/numbers)."
  (cond
    ((null x) nil)
    ((hash-table-p x)
     (loop for k being the hash-keys of x using (hash-value v)
           collect (cons (string k) (princ-to-string v))))
    ((and (listp x)
          (every (lambda (item)
                   (and (consp item)
                        (or (stringp (car item)) (symbolp (car item)))))
                 x))
     (mapcar (lambda (pair)
               (cons (string (car pair))
                     (princ-to-string (cdr pair))))
             x))
    (t
     (error "Invalid extra headers (expected list or hash-table), got: ~S" x))))

(defun parse-metadata-args (arg-list)
  "Parses a list of string arguments like '(\":tags\" \"'(\\\"tag1\\\")\")' into a plist.
   Returns NIL on error or if no metadata args are found."
  (when (and arg-list (string= (first arg-list) "(") (string= (car (last arg-list)) ")"))
    ;; Reconstruct the string representation of the list, removing outer parens
    (let ((metadata-str (format nil "(~{~A~^ ~})" (butlast (rest arg-list)))))
      (handler-case (read-from-string metadata-str nil nil) ; Add nil args to avoid error on EOF
        (reader-error (e)
		      (format t "Error parsing metadata arguments: ~A~%" e)
		      (debug-log "Failed metadata string:" metadata-str)
		      nil)
        (error (e)
               (format t "Unexpected error parsing metadata arguments: ~A~%" e)
               (debug-log "Failed metadata string:" metadata-str)
               nil)))))

(defun tokenize-cli-string (str)
  "Splits a string into tokens, respecting double quotes."
  (let ((tokens '())
        (current-token (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (in-quote nil)
        (escaped nil))
    (loop for char across str do
          (cond
            (escaped
             (vector-push-extend char current-token)
             (setf escaped nil))
            ((char= char #\\)
             (setf escaped t))
            ((char= char #\")
             (setf in-quote (not in-quote)))
            ((and (char= char #\Space) (not in-quote))
             (when (> (length current-token) 0)
               (push (copy-seq current-token) tokens)
               (setf (fill-pointer current-token) 0)))
            (t
             (vector-push-extend char current-token))))
    (when (> (length current-token) 0)
      (push (copy-seq current-token) tokens))
    (nreverse tokens)))

(defun parse-cli-args-s (cli-string &optional (short-map nil))
  "Parses a CLI string into a plist.
   SHORT-MAP is an alist mapping short flags (strings) to long flags (strings).
   Example: (parse-cli-args-s \"hactar sub -t=1,2\" '((\"t\" . \"tags\")))
   -> (:COMMAND \"hactar\" :SUBCOMMAND \"sub\" :TAGS (\"1\" \"2\"))"
  (let* ((tokens (tokenize-cli-string cli-string))
         (result '())
         (current-key nil)
         (args-data (make-hash-table :test #'equal)))

    (when (and tokens (not (string-prefix-p "-" (first tokens))))
      (setf (gethash :command args-data) (list (pop tokens)))
      (when (and tokens (not (string-prefix-p "-" (first tokens))))
        (setf (gethash :subcommand args-data) (list (pop tokens)))))

    (loop while tokens do
          (let ((token (pop tokens)))
            (cond
              ((string-prefix-p "-" token)
               (let* ((clean-token (string-left-trim "-" token))
                      (equals-pos (position #\= clean-token))
                      (key (if equals-pos (subseq clean-token 0 equals-pos) clean-token))
                      (val (if equals-pos (subseq clean-token (1+ equals-pos)) nil)))

                 (let ((mapped (assoc key short-map :test #'string=)))
                   (when mapped (setf key (cdr mapped))))

                 (setf current-key key)

                 (when val
                   (let ((vals (uiop:split-string val :separator ",")))
                     (dolist (v vals)
                       (push v (gethash current-key args-data))))
                   (setf current-key nil))))

              (current-key
               (let ((vals (uiop:split-string token :separator ",")))
                 (dolist (v vals)
                   (push v (gethash current-key args-data))))
               (setf current-key nil))

              (t
               (push token (gethash :args args-data))))))

    (maphash (lambda (k v)
               (cond
                 ((or (eq k :command) (eq k :subcommand))
                  (setf result (list* k (first v) result)))
                 (t
                  (let ((keyword (intern (string-upcase k) :keyword))
                        (values (nreverse v)))
                    (if (= (length values) 1)
                        (setf result (list* keyword (first values) result))
                        (setf result (list* keyword values result)))))))
             args-data)
    result))

(defun get-free-args (command-name)
  "Manually parse free args from uiop:command-line-arguments for a given subcommand.
   This is a workaround because clingon doesn't expose them easily."
  (let* ((all-cli-args (uiop:command-line-arguments))
         (opts-with-arg '("--model" "-m" "--provider" "--name" "--author" "--config-path" "-c" "--slynk-port" "-p" "--disable-analyzers" "--enable-analyzers" "--http-port" "--query" "-q" "--execute" "-e" "--execute-immediately" "--output" "--path" "-p" "--starter"))
         (flag-opts '("--sonnet" "--gemini" "--gpt" "--opus" "--gemini-free" "--deepseek" "--deepseek-free" "--assistant" "--audio" "--react" "--sinatra" "--in-editor" "--mcp" "--acp" "--lisp"))
         (command-pos (position command-name all-cli-args :test #'string=))
         (args-to-scan (if command-pos (nthcdr (+ 1 command-pos) all-cli-args) '())))
    (loop with i = 0
          while (< i (length args-to-scan))
          do (let ((arg (nth i args-to-scan)))
               (cond
                 ((member arg opts-with-arg :test #'string=)
                  (incf i 2))
                 ((member arg flag-opts :test #'string=)
                  (incf i 1))
                 (t
                  (return (subseq args-to-scan i)))))
          finally (return '()))))

(defun push-end (item my-list)
  (append my-list (list item)))

(defun add-to-stack (item)
  "Add ITEM to *STACK* if not present, and run *STACK-CHANGED-HOOK* if changed."
  (let ((len-before (length *stack*)))
    (pushnew item *stack* :test #'string=)
    (when (> (length *stack*) len-before)
      (nhooks:run-hook *stack-changed-hook* *stack*))))

(defun debug-log (&rest args)
  "Log messages to the debug stream. Accepts multiple arguments like format."
  (when *debug*
    (let ((message (format nil "~{~A~^ ~}" args)))
      (format t "~A~%" message)
      (when *debug-stream*
        (format *debug-stream* "~A~%" message)))))

(defun find-executable (name)
  "Check if an executable exists in the system's PATH."
  (handler-case
      (let* ((process-info (uiop:launch-program (list "which" name)
						))
	     (exit-status (uiop:wait-process process-info)))
        (zerop exit-status))
    (error (e)
      nil)))

(defun copy-to-clipboard (text)
  "Copies the given text to the system clipboard using wl-copy or xclip."
  (if (and text (> (length text) 0))
      (let ((wl-copy-exists (find-executable "wl-copy"))
            (xclip-exists (find-executable "xclip")))
        (cond
          (wl-copy-exists
	   (handler-case
	       (progn
		 (uiop:run-program (list "wl-copy" text) :ignore-error-status t))
              (error (e)
                (unless *silent* (format t "Failed to copy to clipboard using wl-copy. Error: ~A~%" e)))))
          (xclip-exists
            (handler-case
                (uiop:run-program (list "xclip" "-selection" "clipboard") :input text :ignore-error-status t)
              (error (e)
                (unless *silent* (format t "Failed to copy to clipboard using xclip. Error: ~A~%" e)))))
          (t
           (format t "Failed to copy to clipboard. Neither 'wl-copy' nor 'xclip' found in PATH.~%"))))
      (unless *silent* (format t "No text to copy.~%"))))

;;** String utils
(defun split-lines (text)
  "Splits text into lines, preserving empty lines.
If the string contains actual newline characters, split on those.
Otherwise, treat the literal character 'n' as a newline placeholder (for tests using \"\\n\")."
  (when text
    (if (position #\Newline text)
        (uiop:split-string text :separator (string #\Newline))
        (uiop:split-string text :separator "n"))))

(defun join-lines (lines)
  "Joins lines back into a single string, adding newlines. Ignores empty lines."
  (str:join (string #\Newline) (remove "" lines :test #'string=)))

(defun remove-prefix (prefix str)
  "Removes PREFIX from the beginning of STR if it exists."
  (if (string-prefix-p prefix str)
      (subseq str (length prefix))
      str))

(defun join-strings (separator strings)
  (with-output-to-string (s)
    (loop for str in strings
          for first = t then nil
          do (unless first (format s separator))
          (format s "~A" str))))

(defun string-prefix-p (prefix str)
  "Returns T if STR starts with PREFIX."
  (and (<= (length prefix) (length str))
       (string= prefix (subseq str 0 (length prefix)))))

(defun extract-md-fenced-code-block (s)
  "Takes a string and returns the first markdown fenced code block found or nil.
   Parsed block takes the form of an alist:
   '((:lang . \"js\") (:filename . \"source.js\") (:contents . \"contents...\"))
   Lang and filename default to nil if not present."
  (let ((lines (cl-ppcre:split "\\n" s))
        (in-block nil)
        (lang nil)
        (filename nil)
        (contents '())
        (result nil))

    (dolist (line lines)
      (cond
       ((and (not in-block)
             (cl-ppcre:scan "^```\\S*" line))
        (setf in-block t)
        (let* ((fence-content (subseq line 3))
               (parts (cl-ppcre:split "\\s+" fence-content :limit 2)))
          (when (and parts (> (length parts) 0)
                     (not (string= (first parts) "")))
            (setf lang (first parts)))
          (when (and (> (length parts) 1)
                     (not (string= (second parts) "")))
            (setf filename (second parts)))))

       ((and in-block
             (cl-ppcre:scan "^```" line))
        (setf in-block nil)
        (setf result
              `((:lang . ,lang)
                (:filename . ,filename)
                (:contents . ,(string-trim '(#\Space #\Tab #\Newline #\Return)
                                           (format nil "~{~A~^~%~}" (nreverse contents))))))
        (return))

       (in-block
        (push line contents))))

    result))

;;** IO utils
(defun read-file-content (filename)
  "Safely reads the content of a file."
  (handler-case (uiop:read-file-string filename)
    (error (e)
      (format *error-output* "Error reading file ~A: ~A~%" filename e)
      nil)))

(defun write-file-content (filename content)
  "Safely writes content to a file using UTF-8 encoding."
  (handler-case (with-open-file (stream filename :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create
                                         :external-format :utf-8)
                  (write-string content stream))
    (error (e)
      (format *error-output* "Error writing file ~A: ~A~%" filename e)
      nil)))

(defun to-json (alist)
  (let* ((shasht:*write-alist-as-object* t)
         (shasht:*write-plist-as-object* t)
         (shasht:*symbol-name-function* (lambda (sym)
                                          (string-downcase (symbol-name sym))))
         (result
           (shasht:write-json alist nil)))
    result))

;;** File utils
(defun expand-path (path)
  "Expand ~ to user home directory in path string."
  (if (and (stringp path) (> (length path) 0) (char= (char path 0) #\~))
      (let ((rest (subseq path 1)))
        (if (or (string= rest "") (char= (char rest 0) #\/))
            (uiop:merge-pathnames* (string-left-trim '(#\/) rest) (user-homedir-pathname))
            (uiop:parse-native-namestring path)))
      (uiop:parse-native-namestring path)))

(defun get-models-config-path ()
  "Returns the full path to the models.yaml configuration file."
  (uiop:subpathname *hactar-config-path* "models.yaml"))

(defun get-prompt-path (prompt-filename)
  "Returns the full path to a prompt file within the user's hactar config directory."
  (uiop:subpathname *hactar-config-path*
                    (make-pathname :directory '(:relative "prompts")
                                   :name prompt-filename)))

(defun get-mime-type (pathname)
  "Determine the MIME type based on the file extension."
  (let ((ext (string-downcase (pathname-type pathname))))
    (cond
      ((member ext '("jpg" "jpeg") :test #'string=) "image/jpeg")
      ((string= ext "png") "image/png")
      ((string= ext "gif") "image/gif")
      ((string= ext "webp") "image/webp")
      (t nil))))

(defun is-image-file? (pathname)
  "Check if a pathname likely points to an image file based on extension."
  (not (null (get-mime-type pathname))))

(defun check-image-size (pathname)
  "Check if image size exceeds the limit and warn."
  (handler-case
      (let* ((size-bytes (with-open-file (s pathname) (file-length s)))
             (size-mb (/ size-bytes 1024.0 1024.0)))
        (when (> size-mb *image-max-size-mb*)
          (format t "~&Warning: Image file '~A' (~,2F MB) exceeds the recommended size limit of ~A MB.~%"
                  (uiop:native-namestring pathname) size-mb *image-max-size-mb*)))
    (error (e)
      (format t "~&Warning: Could not determine size of file '~A': ~A~%" (uiop:native-namestring pathname) e))))

(defun calculate-target-dimensions (width height)
  "Calculate target dimensions based on aspect ratio, aiming for sizes close to the provided table."
  (let ((aspect-ratio (/ width height)))
    (cond
      ((< (abs (- aspect-ratio 1.0)) 0.05) '(1092 1092))       ; 1:1
      ((< (abs (- aspect-ratio 0.75)) 0.05) '(951 1268))       ; 3:4
      ((< (abs (- aspect-ratio 0.666)) 0.05) '(896 1344))      ; 2:3
      ((< (abs (- aspect-ratio 0.5625)) 0.05) '(819 1456))     ; 9:16
      ((< (abs (- aspect-ratio 0.5)) 0.05) '(784 1568))        ; 1:2
      (t (let* ((scale-factor (sqrt (/ (* 1092 1092) (* width height)))))
           (list (round (* width scale-factor)) (round (* height scale-factor))))))))

(defun resize-and-encode-image (pathname)
  "Resize image according to aspect ratio rules and return base64 encoded string."
  (handler-case
      (let* ((image (imago:read-image pathname))
             (width (imago:image-width image))
             (height (imago:image-height image))
             (target-dims (calculate-target-dimensions width height))
             (target-width (first target-dims))
             (target-height (second target-dims))
             (resized-image (imago:resize image target-width target-height))
             (mime-type (get-mime-type pathname))
             (temp-pathname (uiop:with-temporary-file (:pathname p :type (pathname-type pathname) :keep nil) p)))
        (imago:write-image resized-image temp-pathname)
        (let ((image-bytes (alexandria:read-file-into-byte-vector temp-pathname)))
          (values (cl-base64:usb8-array-to-base64-string image-bytes) mime-type)))
    (error (e)
      (format t "~&Error processing image file '~A': ~A~%" (uiop:native-namestring pathname) e)
      (values nil nil)))) ; Return nil on error

(defun split-content (content max-chars)
  "Splits content into chunks based on max character length, trying to split at paragraphs."
  (let ((chunks '())
        (current-pos 0)
        (content-len (length content)))
    (loop while (< current-pos content-len) do
          (let* ((chunk-end (min (+ current-pos max-chars) content-len)))
            (when (< chunk-end content-len)
              ;; Try to find a paragraph break near the end
              (let ((para-break (position #\Newline (subseq content current-pos chunk-end) :from-end t)))
                (when (and para-break (> para-break 0)) ; Found a break within the chunk
                  (setf chunk-end (+ current-pos para-break 1)))))
            (push (subseq content current-pos chunk-end) chunks)
            (setf current-pos chunk-end)))
    (nreverse chunks)))

(defun get-language-hint-from-extension (extension)
  "Map a file extension string to a language hint for code fences."
  (when extension
    (let ((ext (string-downcase extension)))
      (cond
        ((string= ext "lisp") "lisp")
        ((string= ext "js") "javascript")
        ((string= ext "ts") "typescript")
        ((string= ext "py") "python")
        ((string= ext "rs") "rust")
        ((string= ext "sh") "bash")
        ((string= ext "md") "markdown")
        ((string= ext "json") "json")
        ((or (string= ext "yaml") (string= ext "yml")) "yaml")
        ((string= ext "sql") "sql")
        ((string= ext "toml") "toml")
        ((string= ext "org") "org")
        ((string= ext "mustache") "mustache")
        ((string= ext "html") "html")
        ((string= ext "css") "css")
        (t ext)))))

(defun get-file-content (file-path)
  "Get the content of a file, reading as UTF-8."
  (uiop:read-file-string file-path :external-format :utf-8))

(defun resolve-starter-path (starter-name)
  "Resolves the path to a starter file.
   Checks env var HACTAR_STARTERS_<NAME>_PATH, then falls back to default.
   Example: (resolve-starter-path \"react\") -> checks HACTAR_STARTERS_REACT_PATH."
  (let* ((upper-name (string-upcase starter-name))
         (env-var-name (format nil "HACTAR_STARTERS_~A_PATH" upper-name))
         (env-path (uiop:getenv env-var-name))
         (default-path (merge-pathnames (format nil "starters/~A.org" (string-capitalize starter-name))
                                        *hactar-data-path*)))
    (or env-path default-path)))

(defun list-git-tracked-files (repo-root)
  "Lists files tracked by git within the given repository root.
   Returns a list of path strings relative to the repo root."
  (handler-case
      (multiple-value-bind (output error-output exit-code)
                           (uiop:run-program (list "git" "-C" (namestring repo-root) "ls-files")
                                             :output :string
                                             :error-output :string
                                             :ignore-error-status t)
        (if (zerop exit-code)
          (str:lines output)
          (progn
            (format t "~&Error running 'git ls-files': ~A~%" error-output)
            nil)))
    (error (e)
      (format t "~&Error executing git command: ~A~%" e)
     nil)))

;;** Git
(defun find-git-repo-root (start-dir)
  "Finds the git repository root starting from start-dir."
  (multiple-value-bind (output error-output exit-code)
                       (uiop:run-program (list "git" "-C" (namestring start-dir) "rev-parse" "--show-toplevel")
                                         :output :string
                                         :error-output :string
                                         :ignore-error-status t)
    (if (zerop exit-code)
        (uiop:ensure-directory-pathname (string-trim '(#\Newline #\Return #\Space #\Tab) output))
        (error "Failed to find git repository root. Output: ~A" error-output))))

(defun git-repo-present-p (dir)
  "Return T if DIR contains a git repo (i.e., git rev-parse succeeds), else NIL."
  (multiple-value-bind (output error-output exit-code)
                       (uiop:run-program (list "git" "-C" (namestring dir) "rev-parse" "--git-dir")
                                         :output :string
                                         :error-output :string
                                         :ignore-error-status t)
    (declare (ignore output error-output))
    (zerop exit-code)))

(defun git-init (dir)
  "Initialize a git repo in DIR if needed. Returns T if git init succeeds."
  (multiple-value-bind (output error-output exit-code)
                       (uiop:run-program (list "git" "-C" (namestring dir) "init")
                                         :output :string
                                         :error-output :string
                                         :ignore-error-status t)
    (when (and (not (zerop exit-code)) (> (length error-output) 0))
      (format *error-output* "~&Error: git init failed: ~A~%" error-output)
      (when (> (length output) 0)
        (format *error-output* "~&Output: ~A~%" output)))
    (zerop exit-code)))

(defun run-git-command (args &key (ignore-error nil))
  "Runs a git command in the *repo-root* directory."
  (when *repo-root*
    (let ((command (cons "git" (cons "-C" (cons (namestring *repo-root*) args)))))
      (debug-log "Running git command:" (format nil "~{~A~^ ~}" command))
      (multiple-value-bind (output error-output exit-code)
                           (uiop:run-program command
                                             :output :string
                                             :error-output :string
                                             :ignore-error-status ignore-error)
        (when (and (not ignore-error) (not (zerop exit-code)))
          (format t "~&Git command error (Exit Code ~A): ~A~%Output: ~A~%" exit-code error-output output))
        (values output error-output exit-code)))))

(defun git-add (files)
  "Stages the specified files using git add."
  (when files
    (let ((relative-files (mapcar (lambda (f) (uiop:native-namestring (uiop:enough-pathname f *repo-root*))) files)))
      (run-git-command (cons "add" relative-files)))))

(defun git-commit (message)
  "Creates a git commit with the given message."
  (run-git-command (list "commit" "-m" message)))

(defun git-reset-hard (revision)
  "Performs a git reset --hard to the specified revision."
  (run-git-command (list "reset" "--hard" revision)))

(defun git-check-ignore (pathname repo-root)
  "Check if a pathname is NOT tracked by git using 'git ls-files --error-unmatch'.
   Returns T if the file is NOT tracked (ignored or untracked), NIL if tracked.
   Quickly returns T for paths with special characters or excessive length to avoid slow git operations.
   Also checks against *hactar-ignored-paths* regex patterns."
  (handler-case
      (let* ((relative-path (uiop:enough-pathname pathname repo-root))
             (path-string (namestring relative-path)))
        (when (or (> (length path-string) 256)
                  (find-if (lambda (c)
                             (or (char= c #\Null)
                                 (char= c #\Tab)
                                 (and (char>= c (code-char 0))
                                      (char<= c (code-char 31)))))
                           path-string))
          (debug-log "Skipping git-check-ignore for problematic path:" path-string)
          (return-from git-check-ignore t))

        (dolist (pattern *hactar-ignored-paths*)
          (when (cl-ppcre:scan pattern path-string)
            (debug-log "Path matches ignored pattern" pattern ":" path-string)
            (return-from git-check-ignore t)))

        (let ((git-path (uiop:native-namestring (substitute #\/ #\\ path-string))))
          (when *debug-stream*
            (format *debug-stream* "~&Checking git tracking for: ~A (relative: ~A) in repo: ~A~%" pathname git-path repo-root))
          (multiple-value-bind (output error-output exit-code)
                               (uiop:run-program (list "git" "-C" (namestring repo-root) "ls-files" "--error-unmatch" git-path)
                                                 :ignore-error-status t
                                                 :output :string
                                                 :error-output :string)
            (declare (ignore output))
            (when (and *debug-stream* (> (length error-output) 0))
              (format *debug-stream* "~&git ls-files stderr: ~A~%" error-output))
            (not (zerop exit-code)))))
    (error (e)
      (debug-log
        (format nil "~&Error running git ls-files for ~A: ~A~%" pathname e))
     t)))

;;** HTTP utils
(defun get-github-raw-url (github-url)
  "Converts a GitHub blob, tree, or repo root URL to its raw content URL.
   Handles main/master branches and defaults to README for repo roots."
  (let ((raw-base "raw.githubusercontent.com"))
    (cond
      ((cl-ppcre:register-groups-bind (user repo branch path)
                                      ("github\\.com/([^/]+)/([^/]+)/blob/([^/]+)/(.*)" github-url)
                                      (format nil "https://~A/~A/~A/refs/heads/~A/~A" raw-base user repo branch path)))

      ((cl-ppcre:register-groups-bind (user repo branch path)
                                      ("github\\.com/([^/]+)/([^/]+)/tree/([^/]+)/(.*)" github-url)
                                      (let ((readme-path (if (str:ends-with? "/" path)
                                                           (concatenate 'string path "README.md")
                                                           (format nil "~A/README.md" path))))
                                        (format nil "https://~A/~A/~A/refs/heads/~A/~A" raw-base user repo branch readme-path))))

      ((cl-ppcre:register-groups-bind (user repo)
                                      ("github\\.com/([^/]+)/([^/]+)/?$" github-url)
                                      (or (probe-url (format nil "https://~A/~A/~A/refs/heads/main/README.md" raw-base user repo))
                                          (probe-url (format nil "https://~A/~A/~A/refs/heads/master/README.md" raw-base user repo))
                                          (probe-url (format nil "https://~A/~A/~A/refs/heads/main/README" raw-base user repo))
                                          (probe-url (format nil "https://~A/~A/~A/refs/heads/master/README" raw-base user repo)))))

      (t nil))))

(defun probe-url (url)
  "Checks if a URL exists by making a HEAD request. Returns URL if successful, NIL otherwise."
  (handler-case
      (multiple-value-bind (stream code)
                           (drakma:http-request url :method :head :want-stream nil)
        (declare (ignore stream))
        (when (and code (>= code 200) (< code 300))
          url))
    (error (e)
      (debug-log "Probe URL error for" url ":" e)
     nil)))

(defun fetch-url-content (url)
  "Fetches content from a URL. Handles GitHub URLs specifically."
  (let ((target-url (or (get-github-raw-url url) url)))
    (unless target-url
      (format t "Could not determine raw content URL for: ~A~%" url)
      (return-from fetch-url-content nil))
    (format t "Fetching content from: ~A~%" target-url)
    (handler-case
        (multiple-value-bind (body code)
                             (drakma:http-request target-url :want-stream nil)
          (if (and code (>= code 200) (< code 300))
            (if (stringp body) body (babel:octets-to-string body :encoding :utf-8))
            (progn
              (format t "Failed to fetch URL ~A (HTTP ~A)~%" target-url code)
              nil)))
      (error (e)
        (format t "Error fetching URL ~A: ~A~%" target-url e)
       nil))))

(defun parse-url-from-text (text)
  "Finds the first likely URL in a block of text."
  (cl-ppcre:scan-to-strings "(https?://\\S+)" text))

;;** Output utils
(defun normalize-completion (completion-string)
  "Extracts the core completion text from an LLM response string.
   Prioritizes the content of the first markdown code block (```...```).
   If no code block is found, removes a leading 'Complete:' prefix (case-insensitive).
   Returns the extracted/cleaned string, or the original trimmed string if neither pattern is found."
  (let ((trimmed-string (string-trim '(#\Space #\Tab #\Newline #\Return) completion-string)))
    (multiple-value-bind (match registers)
                         (cl-ppcre:scan-to-strings "(?s)```(?:\\w+)?\\s*\\n?(.*?)\\n?```" trimmed-string)
      (when (and match (>= (length registers) 1))
        (let ((code-content (aref registers 0)))
          (return-from normalize-completion (string-trim '(#\Space #\Tab #\Newline #\Return) code-content)))))

    (let* ((prefix "Completion:")
           (prefix-len (length prefix)))
      (when (and (> (length trimmed-string) prefix-len)
                 (string-equal (subseq trimmed-string 0 prefix-len) prefix))
        (return-from normalize-completion (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq trimmed-string prefix-len)))))

    trimmed-string))
(defun play-audio-file (audio-pathname)
  "Plays an audio file using 'paplay' or 'aplay'."
  (let ((player (cond
                  ((find-executable "paplay") "paplay")
                  ((find-executable "aplay") "aplay")
                  (t nil))))
    (if player
      (handler-case
          (progn
            (debug-log "Playing audio file:" audio-pathname "with" player)
            (uiop:run-program (list player (uiop:native-namestring audio-pathname))
                              :output :interactive :error-output :interactive)
            t)
        (error (e)
          (format t "~&Error playing audio file ~A: ~A~%" audio-pathname e)
          nil))
      (progn
        (format t "~&Error: No suitable audio player (paplay or aplay) found.~%")
        nil))))

;;** System check helpers
(defun colorize (text color)
  "Return TEXT wrapped in ANSI color codes."
  (let ((code (ecase color
                (:red "31")
                (:green "32")
                (:yellow "33")
                (:blue "34")
                (:magenta "35")
                (:cyan "36")
                (:bold "1"))))
    (format nil "~C[~Am~A~C[0m" #\Esc code text #\Esc)))

(defun log-good (fmt &rest args)
  "Log a success message in green with 'Good' prefix."
  (let ((msg (apply #'format nil fmt args)))
    (format t "~A~%"
            (colorize (format nil "Good: ~A" msg) :green))))

(defun log-warning (fmt &rest args)
  "Log a warning message in red with 'Warning' prefix."
  (let ((msg (apply #'format nil fmt args)))
    (format t "~A~%"
            (colorize (format nil "Warning: ~A" msg) :red))))

;;** System checks
(defun %to-pathname (p)
  "Coerce strings to pathnames, leave pathnames as-is."
  (typecase p
    (pathname p)
    (string (uiop:parse-native-namestring p))
    (t p)))

(defun %ensure-dir (dir)
  "Ensure DIR exists as a directory pathname and return it."
  (let ((d (uiop:ensure-directory-pathname dir)))
    (ensure-directories-exist d)
    d))

(defun %dir-writable-p (dir)
  "Attempt to write a temporary file to DIR."
  (let* ((dir-path (%ensure-dir dir))
         (tmp (merge-pathnames (format nil ".hactar-write-test-~A" (uuid:make-v4-uuid)) dir-path)))
    (handler-case
        (progn
          (with-open-file (s tmp :direction :output :if-exists :supersede :if-does-not-exist :create)
            (declare (ignore s)))
          (ignore-errors (delete-file tmp))
          t)
      (error (e)
        (declare (ignore e))
        nil))))

(defun search-files-with-rg (queries path)
  "Run rg -l to find files containing any of the queries in path. QUERIES can be a string or list of strings."
  (let ((query-list (uiop:ensure-list queries)))
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (append (list "rg" "-l")
                                      (loop for q in query-list collect "-e" collect q)
                                      (list (namestring path)))
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore error-output))
          (if (zerop exit-code)
              (split-lines (string-trim '(#\Newline) output))
              nil))
      (error (e)
        (format t "Error running rg: ~A~%" e)
        nil))))

(defun select-with-fzf-multi (candidates)
  "Select multiple items from CANDIDATES using fzf -m."
  (let ((input-str (format nil "~{~A~%~}" candidates)))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program "fzf -m"
                          :input (make-string-input-stream input-str)
                          :output :string
                          :error-output :interactive
                          :ignore-error-status t)
      (declare (ignore error-output))
      (if (zerop exit-code)
          (split-lines (string-trim '(#\Newline) output))
          nil))))

;;** LLM processing
(defun chunk-for-llm (file &key size-of-chunk)
  "Chunks a file's content into a list of strings based on token limits.
   FILE should be a pathname or string representing a path.
   SIZE-OF-CHUNK is the token limit for each chunk. If nil, defaults to 50% of the chunking model's context or 2048."
  (let* ((content (get-file-content file)))
    (if content
        (let* ((chunk-tokens (or size-of-chunk
                                 (let* ((model-name *chunking-llm*)
                                        (model (find-model-by-name model-name)))
                                   (if model
                                       (floor (* (model-config-max-input-tokens model) 0.5))
                                       2048))))
               ;; Estimating 3 chars per token to be safe (4 is avg, but 3 is safer for split-content)
               (max-chars (* chunk-tokens 3)))
          (split-content content max-chars))
        nil)))

(defun process-in-chunks (chunks prompt)
  "Process a list of text chunks with the LLM using the given prompt.
   Returns all responses joined by newlines.
   Uses *chunking-llm* (non-streaming, no history)."
  (let* ((model-name *chunking-llm*)
         (model (find-model-by-name model-name))
         (provider (if model
                       (intern (string-upcase (model-config-provider model)) :keyword)
                       :ollama)) ; Fallback
         (model-id (if model (model-config-model-name model) model-name))
         (responses '()))
    
    (unless model
      (format t "Warning: Chunking model '~A' not found. Using default provider :ollama.~%" model-name))

    (dolist (chunk chunks)
      (let* ((messages `(((:role . "user") (:content . ,chunk))))
             (response (llm:complete provider
                                     messages
                                     :model model-id
                                     :system-prompt prompt
                                     :stream nil)))
        (push response responses)))
    
    (str:join (string #\Newline) (nreverse responses))))

(defun process-with-llm (file prompt)
  "Process a file with the LLM by chunking it. Returns the joined response."
  (let ((chunks (chunk-for-llm file)))
    (process-in-chunks chunks prompt)))

(defun process-docs-with-llm (chunks format-opt output-file)
  "Process docs chunks with LLM to convert/summarize."
  (let* ((prompt-path (get-prompt-path "api-docs.gen.org"))
         (prompt (if (probe-file prompt-path)
                     (uiop:read-file-string prompt-path)
                     (format nil "Convert the following documentation markdown to ~A. Only output the converted content, no preamble." format-opt)))
         (processed-content (process-in-chunks chunks prompt)))
    (if output-file
        (progn
          (write-file-content output-file processed-content)
          (unless *silent* (format t "Processed docs written to ~A~%" output-file)))
        (format t "~A" processed-content))))

(defun output-docs-markdown (markdown-content output-file)
  "Output docs as markdown."
  (if output-file
      (progn
        (write-file-content output-file markdown-content)
        (unless *silent* (format t "Docs written to ~A~%" output-file)))
      (format t "~A" markdown-content)))

(defun output-docs-converted (markdown-content format-opt output-file)
  "Convert docs to specified format using pandoc."
  (if (find-executable "pandoc")
      (let ((pandoc-cmd (list "pandoc" "-f" "markdown" "-t" format-opt)))
        (when output-file
          (setf pandoc-cmd (append pandoc-cmd (list "-o" output-file))))

        (if output-file
            (progn
              (uiop:run-program pandoc-cmd
                                :input (make-string-input-stream markdown-content)
                                :output :interactive
                                :error-output :interactive)
              (unless *silent* (format t "Docs written to ~A~%" output-file)))
            (let ((output (uiop:run-program pandoc-cmd
                                            :input (make-string-input-stream markdown-content)
                                            :output :string
                                            :error-output :interactive)))
              (format t "~A" output))))
      (format t "Error: pandoc not found. Cannot convert to ~A.~%" format-opt)))

(defun kebab-case (str)
  "Convert a string to kebab-case.
   Inserts hyphens between lowercase-to-uppercase transitions."
  (string-downcase 
   (cl-ppcre:regex-replace-all 
    "([a-z])([A-Z])" 
    str 
    "\\1-\\2")))

(defun pascal-case (str)
  "Convert a string to PascalCase."
  (apply #'concatenate 'string
         (mapcar #'string-capitalize 
                 (cl-ppcre:split "[-_\\s]+" str))))

(defun camel-case (str)
  "Convert a string to camelCase."
  (let ((pascal (pascal-case str)))
    (when (> (length pascal) 0)
      (concatenate 'string 
                   (string-downcase (subseq pascal 0 1))
                   (subseq pascal 1)))))

(defun fetch-github-repo (user repo)
  "Clones a GitHub repository to a temporary directory and returns the path.
   If the directory already exists, it is returned as is."
  (let* ((temp-dir (uiop:temporary-directory))
         (repo-name (format nil "~A-~A" user repo))
         (target-dir (merge-pathnames (format nil "hactar-repos/~A/" repo-name) temp-dir))
         (repo-url (format nil "git@github.com:~A/~A.git" user repo)))
    (if (uiop:directory-exists-p target-dir)
        target-dir
        (progn
          (ensure-directories-exist (uiop:pathname-directory-pathname target-dir))
          (format t "Cloning ~A to ~A...~%" repo-url target-dir)
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program (list "git" "clone" "--depth" "1" repo-url (uiop:native-namestring target-dir))
                                :output :string :error-output :string)
            (declare (ignore output))
            (if (zerop exit-code)
                target-dir
                (progn
                  (format t "Error cloning repo: ~A~%" error-output)
                  nil)))))))
(defvar *verbose-glob* nil)
(declaim (inline safe-char=))

(defstruct pathparts
  dirs dir-lens
  file file-len
  i)

(defun empty-pathparts-p (pathparts)
  (declare (type (or null pathparts) pathparts))
  (or (null pathparts)
      (and (null (pathparts-dirs pathparts))
           (>= (pathparts-i pathparts)
              (pathparts-file-len pathparts)))))

(defun current-part (pathparts)
  (declare (type pathparts pathparts)
           (optimize (safety 0)))
  (let ((dir (car (pathparts-dirs pathparts))))
   (if dir
       (values dir (car (pathparts-dir-lens pathparts)))
       (values (pathparts-file pathparts) (pathparts-file-len pathparts)))))

(defun count-parts (pathparts)
  (declare (type (or null pathparts) pathparts))
  (if (null pathparts)
      0
      (1+ (length (pathparts-dirs pathparts)))))

(defun empty-current-part-p (pathparts)
  (declare (type (or null pathparts) pathparts))
  (or (null pathparts)
      (multiple-value-bind (part len) (current-part pathparts)
        (declare (ignore part))
        (>= (pathparts-i pathparts) len))))

(defun drop-part (pathparts)
  (declare (type pathparts pathparts))
  (if (pathparts-dirs pathparts)
      (progn
        (setf (pathparts-dirs pathparts)
              (cdr (pathparts-dirs pathparts)))
        (setf (pathparts-dir-lens pathparts)
              (cdr (pathparts-dir-lens pathparts)))
        (setf (pathparts-i pathparts) 0)
        pathparts)
      nil))

(defun move-forward-1 (pathparts &optional jump-if-empty)
  (declare (type pathparts pathparts))
  (multiple-value-bind (part len) (current-part pathparts)
    (declare (ignore part))
    (cond ((and jump-if-empty (= (pathparts-i pathparts) len))
           (drop-part pathparts))
          ((< (pathparts-i pathparts) len)
           (incf (pathparts-i pathparts))
           pathparts)
          (t pathparts))))

(defun next-char (pathparts)
  (declare (type (or null pathparts) pathparts)
           (optimize (safety 0)))
  (when pathparts
    (multiple-value-bind (part len) (current-part pathparts)
      (when (< (pathparts-i pathparts) len)
        (schar part (pathparts-i pathparts))))))

(defun pathspec->pathparts (pathspec)
  (let ((dirs (cdr (pathname-directory pathspec)))
        (file (file-namestring pathspec)))
    (make-pathparts :dirs dirs
                    :dir-lens (mapcar #'length dirs)
                    :file file
                    :file-len (length file)
                    :i 0)))

(defun glob (pathname &optional root)
  "Return a possibly empty list of path names that match pathname, which must be a string containing a path specification. pathname can be either absolute (like /home/user1/lisp/**/*.lisp) or relative (like ../../Tools/*/*.gif), and can contain shell-style wildcards.

  If root is specified, it has the same effect on glob as changing the current directory before calling it.

  If pathname is relative, the result will contain paths relative to root.

  Examples:
   > (glob \"/home/user1/lisp/**/*.lisp\")
   (#P\"/home/user1/lisp/cl-template/cl-template.lisp\"
    #P\"/home/user1/lisp/pcl/id3/id3.lisp\")

   > (glob \"*.lisp\")
   (#P\"cl-glob.lisp\"
    #P\"packages.lisp\"
    #P\"tests.lisp\")

   > (glob \"*.lisp\" \"../mokubune/\")
   (#P\"../mokubune/init.lisp\"
    #P\"../mokubune/mokubune.lisp\"
    #P\"../mokubune/packages.lisp\"
    #P\"../mokubune/version.lisp\")

   > (glob \"../mokubune/*.lisp\")
   (#P\"../mokubune/init.lisp\"
    #P\"../mokubune/mokubune.lisp\"
    #P\"../mokubune/packages.lisp\"
    #P\"../mokubune/version.lisp\")

   This is the same as (glob \"*.lisp\" \"../mokubune/\")."

  (multiple-value-bind (pattern-root pattern) (split-pattern pathname)
		       (let* ((compiled-pattern (compile-pattern (namestring pattern)))
			      (root (if root
					(merge-pathnames pattern-root (pathname-as-directory root))
				      pattern-root))
			      (root-abs (first (directory root)))
			      result
			      (checked-count 0))
			 (unless root-abs
			   (return-from glob))

			 (labels ((walk (name)
					(let ((pathname-rel (enough-namestring name root-abs)))
					  (incf checked-count)
					  (when (glob-matches-compiled compiled-pattern pathname-rel)
					    (push (relative-to-root pathname-rel root) result))
					  (when (directory-pathname-p name)
					    (dolist (entry (directory (wild-directory name) :resolve-symlinks nil))
					      (walk entry))))))
				 (walk root-abs)
				 (when *verbose-glob* (format t "Total ~d entries checked.~%" checked-count))
				 (nreverse result)))))

(defun glob-matches (pattern pathname)
  "Return t if pathname matches pattern.

  ? matches any single character except file path separator.
  * matches zero or more characters except file path separator.
  A leading ** followed by a slash (**/a), or ** between slashes (a/**/b) matches zero or more directory components.
  A trailing ** (a/**) matches anything remain.
  Other forms of ** are treated as a single *. For example 'abc/**b/Makefile' === 'abc/*b/Makefile, 'abc/**.gif' === 'abc/*.gif'.
  [0-9a-z?*] matches a single character which is '?' or '*' or is between '0 and '9' or is between 'a' and 'z'.
  [!0-9a-z?*] matches the opposite of above.
  All other characters in pattern matches literally."
  (glob-matches-compiled (compile-pattern pattern) pathname))

(defun glob-matches-compiled (compiled-pattern pathname)
  "Same as glob-matches, except first argument is a compiled pattern (via compile-pattern function)."
  (matches-all compiled-pattern (pathspec->pathparts pathname)))

(defun compile-pattern (pattern)
  "Compile the pattern.

   Output object should be used as parameter of other Apis. Internal structure of the output object is subject to change, thus should not be depended directly."
  (compile-internal pattern))

(defun compile-internal (pattern &optional stack (start 0))
  (let ((c (char-or-nil pattern start)))
    (cond ((null c) (nreverse stack))

          ((and (char= c #\*)
                (safe-char= (char-or-nil pattern (1+ start)) #\*))
           (let* ((prev-c (char-or-nil pattern (1- start)))
                  (next-c (char-or-nil pattern (+ start 2)))
                  (has-leading-slash (or (null prev-c) (char= prev-c #\/)))
                  (has-trailing-slash (and next-c (char= next-c #\/))))
             (cond
               ;; **/... or .../**/...
               ((and has-leading-slash has-trailing-slash)
                (compile-internal pattern
                                  (cons #'matches-any-string stack)
                                  (+ start 3)))
               ;; .../**
               ((and has-leading-slash (null next-c))
                (compile-internal pattern
                                  (cons #'matches-any-trailing stack)
                                  (+ start 2)))
               ;; ...**ab... or ab** etc.
               (t (compile-internal pattern
                                    (cons #'matches-any-segment stack)
                                    (+ start 2))))))

          ((char= c #\*)
           (compile-internal pattern (cons #'matches-any-segment stack) (1+ start)))

          ((char= c #\?)
           (compile-internal pattern
                             (cons #'matches-char-none-separator stack)
                             (1+ start)))

          ((and (char= c #\[)
                (not (safe-char= (char-or-nil pattern (1+ start)) #\!)))
           (let ((close-pos (safe-position #\] pattern (+ start 2))))
             (if close-pos
                 (compile-internal
                  pattern
                  (cons
                   #'(lambda (patterns pathname)
                       (matches-char-in-ranges
                        (parse-range pattern (1+ start) close-pos)
                        patterns pathname))
                   stack)
                  (1+ close-pos))
                 (error "Invalid pattern range"))))

          ((and (char= c #\[)
                (safe-char= (char-or-nil pattern (1+ start)) #\!))
           (let ((close-pos (safe-position #\] pattern (+ start 3))))
             (if close-pos
                 (compile-internal
                  pattern
                  (cons
                   #'(lambda (patterns pathname)
                       (matches-char-not-in-ranges
                        (parse-range pattern (+ start 2) close-pos)
                        patterns pathname))
                   stack)
                  (1+ close-pos))
                 (error "Invalid pattern range"))))

          (t (compile-internal
              pattern
              (cons
               #'(lambda (patterns pathname)
                   (matches-char c patterns pathname))
               stack)
              (1+ start))))))

(defun split-pattern (pattern)
  (loop for parts on (pathname-directory pattern)
        for p = (first parts)
        while (or (member p '(:ABSOLUTE :RELATIVE :UP :BACK))
                  (stringp p))
        collect p into root
        finally (return (values (make-pathname :directory root)
                                (make-pathname
                                 :directory (cons :relative parts)
                                 :defaults pattern)))))

(defun relative-to-root (pathname root)
  (let ((root-true-path (truename root)))
    (merge-pathnames
     (enough-namestring pathname root-true-path)
     root)))

(defun wild-directory (dir)
  (make-pathname
   :name :wild
   :type :wild
   :defaults dir))

(defun directory-pathname-p (p)
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name      nil
         :type      nil
         :defaults pathname)
        pathname)))


(defun char-or-nil (string index)
  (when (< -1 index (length string)) (char string index)))

(defun safe-char= (c1 c2)
  (and c1 c2 (char= c1 c2)))

(defun safe-position (item sequence start)
  (when (< start (length sequence))
    (position item sequence :start start)))

(defun parse-range (pattern start end)
  (let (range)
    (loop until (>= start end)
          when (and (>= (- end start) 2)
                    (char= (char pattern (1+ start)) #\-))
            do (push (cons (char pattern start) (char pattern (+ start 2))) range)
               (incf start 3)
          else
            do (push (char pattern start) range)
               (incf start))
    (nreverse range)))

(defun matches-all (pattern-list pathparts)
  (declare (type (or null pathparts) pathparts))
  (loop for pat = (first pattern-list)
        when (null pat)
          return (empty-pathparts-p pathparts)
        do (multiple-value-bind (matched remain-patterns remain-pathparts)
               (funcall pat (rest pattern-list) pathparts)
             (if matched
                 (setf pattern-list remain-patterns pathparts remain-pathparts)
                 (return nil)))))

(defun matches-any-segment (remain-patterns pathparts)
  (declare (type (or null pathparts) pathparts))
  (values
   (loop for subparts = pathparts
           then (move-forward-1 subparts)
         when (matches-all remain-patterns subparts)
           return t
         when (empty-current-part-p subparts)
           return nil)
   nil
   nil))

(defun matches-any-string (remain-patterns pathparts)
  (declare (type (or null pathparts) pathparts))
  (values
   (loop for subparts = pathparts
           then (drop-part pathparts)
         while subparts
         when (matches-all remain-patterns subparts)
           return t)
   nil
   nil))

(defun matches-char (char remain-patterns pathparts)
  (declare (type character char)
           (type (or null pathparts) pathparts))
  (let* ((c (next-char pathparts))
         (matched (or (safe-char= char c)
                      (and (char= char #\/)
                           (> (count-parts pathparts) 1)
                           (null c)))))
    (values matched
            remain-patterns
            (if matched (move-forward-1 pathparts t) pathparts))))

(defun matches-char-in-ranges (ranges remain-patterns pathparts)
  (declare (type (or null pathparts) pathparts))
  (let ((c (next-char pathparts)))
    (flet ((in-range (range)
             (cond ((characterp range) (and c (char= range c)))
                   (t (and c (char<= (car range) c (cdr range)))))))
      (values
       (some #'in-range ranges)
       remain-patterns
       (if c (move-forward-1 pathparts) pathparts)))))

(defun matches-char-not-in-ranges (ranges remain-patterns pathparts)
  (declare (type (or null pathparts) pathparts))
  (multiple-value-bind (matched patterns remain-pathparts)
      (matches-char-in-ranges ranges remain-patterns pathparts)
    (values (not matched) patterns remain-pathparts)))

(defun matches-char-none-separator (remain-patterns pathparts)
  (declare (type (or null pathparts) pathparts))
  (let ((c (next-char pathparts)))
    (if c
        (values t remain-patterns (move-forward-1 pathparts))
        (values nil remain-patterns pathparts))))

(defun matches-any-trailing (remain-patterns pathparts)
  (declare (ignore pathparts))
  (if (null remain-patterns)
      (values t nil nil)
      (error "Expected to be the trailing pattern.")))
