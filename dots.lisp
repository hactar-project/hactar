;; dot commands operate on the current prompt
(in-package :hactar)
(defvar *dot-commands* (make-hash-table :test 'equal)
  "Hash table of available dot commands.")

(defmacro defdot (name args &body body)
  "Define a dot command with the given name and arguments, capturing the docstring.
   The command name should be the symbol without the leading dot (e.g., 'cat')."
  (let* ((command-name-key (string-downcase (symbol-name name)))
         (docstring (when (stringp (first body)) (first body)))
         (body-without-docstring (if (stringp (first body)) (rest body) body)))
    `(setf (gethash ,(format nil ".~A" command-name-key) *dot-commands*)
           (list (lambda ,args
                   ,@body-without-docstring)
                 ,(or docstring "No description available.")))))

(define-command dump-dot (args)
  "Print out the current dot system prompt."
  (declare (ignore args))
  (dump-dot-system-prompt))
;;** Dot Commands
(defdot set (args)
  "Usage: set key=value
   Set meta details to the operating system. e.g set project.description would set the project description"
  (let ((full-command (format nil "set ~{~A~^ ~}" args)))
    (get-llm-response full-command :dot-command-p t)))

(defdot create (args)
  "Usage: .create <FILE>
Create a new file in the virtual file system. The response should be a SEARCH/REPLACE block with an empty SEARCH section."
  (let ((full-command (format nil "create ~{~A~^ ~}" args)))
    (get-llm-response full-command :dot-command-p t :stream nil)))

(defdot cat (args)
  "Usage: cat <file1> [file2 ...]
Displays content of specified files from the virtual context.
The LLM will act like the OS 'cat' command.
If a colon is passed on the end of filename treat it as a query on the file.
For example:

cat src/components/Cheassboard.tsx:imports should return the imports from that file.
"
  (let ((full-command (format nil "cat ~{~A~^ ~}" args)))
    (get-llm-response full-command :dot-command-p t)))

(defdot ls (args)
	"Usage: ls [path]
Lists directory contents. Acts like the OS 'ls' command.

When used on a file it should act like 'cat' command.
"
	(let ((full-command (format nil "ls ~{~A~^ ~}" args)))
	  (get-llm-response full-command :dot-command-p t)
	  ))

(defdot org (args)
  "Usage: org <file-or-directory> [file-or-direct...]
Like cat/ls but isplays content of specified files, formatted as Org-mode source blocks."
  (let ((full-command (format nil ".org ~{~A~^ ~}" args)))
    (get-llm-response full-command :dot-command-p t)))

(defdot md (args)
  "Usage: md <file-or-directory> [file-or-direct...]
Like cat/ls but isplays content of specified files, formatted as markdown source blocks "
  (let ((full-command (format nil "md ~{~A~^ ~}" args)))
    (get-llm-response full-command :dot-command-p t)
    ))

(defdot mod (args)
  "Usage: modify <filename> <description of changes>
Asks the LLM to generate modifications for the specified file.
The response should be a SEARCH/REPLACE block, which will be processed automatically."
  (let ((full-command (format nil "mod ~{~A~^ ~}" args)))
    (get-llm-response full-command :dot-command-p t :stream nil)))

(defdot convert (args)
  "Usage: convert <filepath> <target_format_or_conversion_description>
Convert the content of the file or files from on format to another. Return each file as a org-mode source block.
Example: convert src/components/Chessboard.tsx mdx should return the Chessboard.tsx as mdx
"
  (if (< (length args) 2)
      (let ((full-command (format nil "convert ~{~A~^ ~}" args)))
        (get-llm-response full-command :dot-command-p t))))

(defdot cmd (args)
  "Usage: cmd <description of command>
Asks the LLM to generate a shell command based on the description.
Prompts for confirmation before running."
  (let* ((description (format nil "~{~A~^ ~}" args))
         (full-command (format nil "cmd ~A" description))
         (shell-command-response (get-llm-response full-command :dot-command-p t :stream nil :add-to-history t)))
    (when (and shell-command-response (not (string= shell-command-response "")))
      (let ((potential-command nil))
        (let ((extracted-block-info (extract-md-fenced-code-block shell-command-response)))
          (if extracted-block-info
              (setf potential-command (cdr (assoc :contents extracted-block-info)))
              (setf potential-command (string-trim '(#\Space #\Tab #\Newline #\Return #\`) shell-command-response))))
        (setf potential-command (string-trim '(#\Space #\Tab #\Newline #\Return) potential-command))

        (if (confirm-action (format nil "Run command: ~A?" potential-command))
          (execute-command "/run" (list potential-command))
          (format t "Command not executed.~%"))))))

(defdot cmd! (args)
  "Usage: cmd! <description of command>
Asks the LLM to generate a shell command and runs it immediately."
  (let* ((description (format nil "~{~A~^ ~}" args))
         (full-command (format nil "cmd! ~A" description))
         (shell-command-response (get-llm-response full-command :dot-command-p t :stream nil :add-to-history t)))
    (when (and shell-command-response (not (string= shell-command-response "")))
      (let ((potential-command nil))
        (let ((extracted-block-info (extract-md-fenced-code-block shell-command-response)))
          (if extracted-block-info
              (setf potential-command (cdr (assoc :contents extracted-block-info)))
              (setf potential-command (string-trim '(#\Space #\Tab #\Newline #\Return #\`) shell-command-response))))
        (setf potential-command (string-trim '(#\Space #\Tab #\Newline #\Return) potential-command))

        (format t "Running command: ~A~%" potential-command)
        (execute-command "/run" (list potential-command))))))

(defun execute-dot-command-for-pipe (command-string)
  "Executes a dot command string and returns its string output for piping.
   Assumes non-streaming, non-modify commands that return simple text."
  (multiple-value-bind (cmd-name cmd-args) (parse-command command-string)
    (when (and cmd-name (str:starts-with? "." cmd-name) (gethash cmd-name *dot-commands*))
      (let ((full-user-cmd (format nil "~A~{ ~A~}" cmd-name cmd-args)))
        ;; Call get-llm-response without streaming and without adding to history for intermediate pipe steps.
        (get-llm-response full-user-cmd :dot-command-p t :stream nil :add-to-history nil)))))

(defdot \| (args)
  "Usage: .| <dot_command_1_with_args> | <dot_command_2_with_args_expecting_input>
Pipes the text output of the first dot command as the final argument to the second dot command.
Example: .| .cat myfile.txt | .modify anotherfile.txt The content is:"
  (let* ((args-string (format nil "~{~A~^ ~}" args))
         (pipe-marker " | ")
         (pipe-pos (search pipe-marker args-string)))
    (if (not pipe-pos)
        (format t "Usage: .| <command1_and_args> | <command2_and_args_prefix>~%Missing ' | ' separator.~%")
        (let* ((cmd1-full-str (string-trim '(#\Space) (subseq args-string 0 pipe-pos)))
               (cmd2-prefix-str (string-trim '(#\Space) (subseq args-string (+ pipe-pos (length pipe-marker)))))
               (output1 (execute-dot-command-for-pipe cmd1-full-str)))
          (if output1
              (let* (;; Construct the full second command string
                     (cmd2-full-str (format nil "~A ~A" cmd2-prefix-str output1)))
                (debug-log "Piping. Cmd1 output:" output1)
                (debug-log "Piping. Executing full Cmd2 string:" cmd2-full-str)

                ;; Parse and execute the second command
                (multiple-value-bind (cmd2-name cmd2-args) (parse-command cmd2-full-str)
                  (if (and cmd2-name (str:starts-with? "." cmd2-name) (gethash cmd2-name *dot-commands*))
                      (let ((cmd-fn (first (gethash cmd2-name *dot-commands*))))
                        (funcall cmd-fn cmd2-args)) ; Call the target command's lambda
                      (format t "Error: Second command in pipe is not a valid dot command or failed to parse: ~A~%" cmd2-prefix-str))))
              (format t "Error: First command in pipe ('~A') failed or produced no output.~%" cmd1-full-str))))))

(define-command dots (args)
                "Display available dot commands and their descriptions."
                (declare (ignore args))
                (format t "Available dot commands:~%")
                (let ((sorted-commands (sort (alexandria:hash-table-keys *dot-commands*) #'string<)))
                  (dolist (cmd sorted-commands)
                    (let* ((command-info (gethash cmd *dot-commands*))
                           (description (second command-info)))
                      (format t "  ~A - ~A~%" cmd description)))))
