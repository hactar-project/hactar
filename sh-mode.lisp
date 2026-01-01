;; shell handling stuff
(in-package :hactar)

(define-sub-command sh (args)
  "Generate a shell command from a query, print it, and copy it to the clipboard."
  (let* ((query (format nil "~{~A~^ ~}" args))
         (shell-command (generate-and-get-shell-command query)))
    (if shell-command
        (progn
          (format t "~A~%" shell-command)
          (copy-to-clipboard shell-command)
          (uiop:quit 0))
        (progn
          (format t "Error: Could not generate a shell command.~%")
          (uiop:quit 1)))))

(define-sub-command sh! (args)
  "Generate a shell command from a query and execute it immediately."
  (let* ((query (format nil "~{~A~^ ~}" args))
         (shell-command (generate-and-get-shell-command query)))
    (if shell-command
        (progn
          (format t "Executing: ~A~%" shell-command)
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program shell-command :output :string :error-output :string :ignore-error-status t)
            (format t "~A" output)
            (when (and error-output (not (string= error-output "")))
              (format t "~A" error-output))
            (uiop:quit exit-code)))
        (progn
          (format t "Error: Could not generate a shell command.~%")
          (uiop:quit 1)))))

(define-command run (args)
                "Run a shell command and optionally add the output to the chat."
                (when args
                  (let* ((add-to-chat (string= (first args) "-a"))
                         (cmd-args (if add-to-chat (rest args) args))
                         (cmd-str (format nil "~{~A~^ ~}" cmd-args)))
                    (multiple-value-bind (output error-output exit-code)
                                         (uiop:run-program cmd-str :output :string :error-output :string :ignore-error-status t)
                      (format t "~A~%" output)
                      (when (not (zerop exit-code))
                        (format t "Error (exit code ~A):~%~A~%" exit-code error-output))
                      (when add-to-chat
                        (add-to-chat-history "user" (format nil "Shell command: ~A~%Output:~%~A" cmd-str output)))))))

(setf (gethash "/!" *commands*) (gethash "/run" *commands*))
(defun generate-and-get-shell-command (query)
  "Asks the LLM to generate a shell command from a query and returns it as a string."
  (when (and *current-model* query (not (string= query "")))
    (let* ((system-prompt-text (uiop:read-file-string (get-prompt-path "generate-shell-command.mustache")))
           (llm-response (get-llm-response query
                                           :custom-system-prompt system-prompt-text
                                           :stream nil
                                           :add-to-history nil)))
      (if llm-response
          (let* ((extracted-block (extract-md-fenced-code-block llm-response))
                 (shell-command (if extracted-block
                                    (cdr (assoc :contents extracted-block))
                                    (string-trim '(#\Space #\Tab #\Newline #\Return #\`) llm-response))))
            (when (and shell-command (not (string= shell-command "")))
              (string-trim '(#\Space #\Tab #\Newline #\Return) shell-command)))
          (progn
            (format t "Error: No response from LLM for shell command generation.~%")
            nil)))))
