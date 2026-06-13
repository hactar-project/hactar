;; LISP mode instructs an to generate LISP
;; Mostly here as an experiment and future diffusion model using experiment
(in-package :hactar)

(defvar *lisp-mode-api-surface* nil
  "Cached string of the Lisp API surface.")

(defun generate-hactar-api-surface ()
  "Generate the markdown string representing the Lisp API surface."
  (with-output-to-string (s)
    (format s "# Hactar Lisp API Surface~%~%")

    (format s "## Core Functions~%")
    (format s "Core functions for selection and creation:~%")
    (format s "  - (code/select source &key function class method region name)~%")
    (format s "  - (code/from-string string &key name language)~%~%")

    (format s "## File Operations~%")
    (format s "File system operations:~%")
    (format s "  - (read-file-content path)~%")
    (format s "  - (write-file-content path content)~%~%")

    (format s "## Generators~%")
    (format s "Generator utilities:~%")
    (format s "  - (run-generator name &rest args)~%~%")

    (format s "## Context Inference~%")
    (format s "Context inference operations:~%")
    (format s "  - (infer-framework)~%~%")

    (format s "## LLM Interaction~%")
    (format s "Functions for direct LLM query:~%")
    (format s "  - (get-llm-response prompt &key stream custom-system-prompt)~%~%")

    (format s "## Git Operations~%")
    (format s "Git version control operations:~%")
    (format s "  - (git-add files)~%")
    (format s "  - (git-commit message)~%~%")

    (format s "## Utilities~%")
    (format s "General utility operations.~%~%")

    (format s "## Templates~%")
    (format s "Template management operations.~%~%")

    (format s "## Generation Operations~%")
    (format s "Generation/planning operations:~%")
    (format s "  - (execute-gen-plan plan)~%~%")

    (format s "## Project Variables~%")
    (format s "Access to project state and variables.~%")))

(defun get-lisp-mode-api-surface ()
  "Get the cached Lisp API surface, generating it if necessary."
  (or *lisp-mode-api-surface*
      (setf *lisp-mode-api-surface* (generate-hactar-api-surface))))

(defun extract-lisp-from-response (response)
  "Extract executable Lisp code from an LLM response."
  (or (cl-ppcre:register-groups-bind (code)
          ("(?s)```(?:lisp|common-lisp|cl)?\\s*(.*?)\\s*```" response)
        code)
      (string-trim '(#\Space #\Tab #\Newline #\Return) response)))

(defun validate-lisp-syntax (string)
  "Validate that a string contains valid Common Lisp forms."
  (handler-case
      (let ((forms '())
            (eof (gensym)))
        (with-input-from-string (s string)
          (loop
            (let ((form (read s nil eof)))
              (if (eq form eof)
                  (return (values t (nreverse forms)))
                  (push form forms))))))
    (serious-condition (c)
      (values nil (format nil "~A" c)))))

(defun eval-lisp-safely (string)
  "Evaluate Lisp forms in a string safely, returning the last form's value."
  (multiple-value-bind (valid-p forms-or-err) (validate-lisp-syntax string)
    (if (not valid-p)
        (values nil forms-or-err)
        (handler-case
            (let ((result nil))
              (dolist (form forms-or-err)
                (setf result (eval form)))
              (values result nil))
          (serious-condition (c)
            (values nil (format nil "~A" c)))))))

(defun lisp-mode-system-prompt ()
  "Generate the system prompt for Lisp mode."
  (format nil
          "You are Hactar in Lisp Response Mode.
You must ONLY respond with executable Common Lisp code wrapped in a ```lisp fenced code block, or as plain code.
No markdown or conversational text outside the code block.
The user will be prompted to EVAL or REJECT your generated Lisp code.

Project Context:
- Repository Root: ~A
- Language: ~A
- Tech Stack: ~{~A~^, ~}
- Files in context (~D): ~{~A~^, ~}

Example API functions you can use:
- (read-file-content \"path/to/file\")
- (execute-gen-plan plan-data)

Respond ONLY with Lisp code."
          (or *repo-root* "")
          (or *language* "")
          (or *stack* '())
          (length *files*)
          (mapcar #'namestring *files*)))

(defun display-lisp-code (code)
  "Display the generated Lisp code with separators."
  (format t "~&━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
  (format t "Generated Lisp Code:~%")
  (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
  (format t "~A~%" code)
  (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%"))

(defun prompt-eval-or-reject ()
  "Prompt the user to evaluate or reject the generated Lisp code."
  (format t "~&[E]val, [R]eject, [C]opy to clipboard, [V]iew code? [e/r/c/v]: ")
  (force-output)
  (let ((input (string-trim '(#\Space #\Tab #\Newline #\Return) (or (read-line *standard-input* nil "") ""))))
    (cond
      ((string-equal input "e") :eval)
      ((string-equal input "c") :copy)
      ((string-equal input "v") :view)
      (t :reject))))

(defun handle-lisp-mode-response (prompt)
  "Get a response from the LLM in Lisp mode and handle the user's action."
  (let* ((sys-prompt (lisp-mode-system-prompt))
         (response (get-llm-response prompt :stream nil :add-to-history t :custom-system-prompt sys-prompt)))
    (unless response
      (format t "No code generated.~%")
      (return-from handle-lisp-mode-response nil))
    (let ((code (extract-lisp-from-response response)))
      (multiple-value-bind (valid-p err-msg) (validate-lisp-syntax code)
        (unless valid-p
          (format t "Syntax Error: ~A~%" err-msg)
          (return-from handle-lisp-mode-response nil))
        (display-lisp-code code)
        (loop
          (let ((choice (prompt-eval-or-reject)))
            (cond
              ((eq choice :eval)
               (multiple-value-bind (res err) (eval-lisp-safely code)
                 (if err
                     (progn
                       (format t "Execution Error: ~A~%" err)
                       (return nil))
                     (return res))))
              ((eq choice :copy)
               (copy-to-clipboard code))
              ((eq choice :view)
               (display-lisp-code code))
              ((eq choice :reject)
               (return nil)))))))))

(defun lisp-mode-intercept (prompt)
  "Intercept user prompt in interactive loop if Lisp response mode is enabled."
  (if (or *lisp-mode-enabled* *lisp-response-mode-enabled*)
      (progn
        (handle-lisp-mode-response prompt)
        t)
      nil))

(defun response-mode-system-prompt (format)
  "Generate the system prompt based on the active response mode."
  (let ((fmt-name (string-downcase (symbol-name format))))
    (format nil
            "You are Hactar in ~A Response Mode.
You must ONLY respond with valid ~A.
Wrap your response in a ```~A fenced code block, or provide it as plain text.
No conversational text outside the response.

Project Context:
- Repository Root: ~A
- Language: ~A
- Tech Stack: ~{~A~^, ~}
- Files in context (~D): ~{~A~^, ~}

Respond ONLY with valid ~A."
            (string-capitalize fmt-name)
            (string-upcase fmt-name)
            (case format
              (:org-mode "org")
              (t fmt-name))
            (or *repo-root* "")
            (or *language* "")
            (or *stack* '())
            (length *files*)
            (mapcar #'namestring *files*)
            (string-upcase fmt-name))))

(defun extract-response-content (response format)
  "Extract response content for the specified format."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) response))
         (lang-str (case format
                     (:lisp "(?:lisp|common-lisp|cl)")
                     (:json "json")
                     (:xml "xml")
                     (:yaml "yaml")
                     (:markdown "markdown")
                     (:org-mode "org")
                     (t (string-downcase (symbol-name format))))))
    (or
     (cl-ppcre:register-groups-bind (code)
         ((format nil "(?s)```~A?\\s*(.*?)\\s*```" lang-str) response)
       code)
     trimmed)))

(defun validate-response-syntax (string format)
  "Validate the syntax of the response based on the active format."
  (case format
    (:lisp (validate-lisp-syntax string))
    (:json
     (handler-case
         (values t (shasht:read-json string))
       (serious-condition (c)
         (values nil (format nil "~A" c)))))
    (:xml
     (handler-case
         (values t (cxml:parse string (cxml-dom:make-dom-builder)))
       (serious-condition (c)
         (values nil (format nil "~A" c)))))
    (:yaml
     (handler-case
         (values t (cl-yaml:parse string))
       ((or serious-condition yaml.error:parsing-error) (c)
         (values nil (format nil "~A" c)))))
    (t (if (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) string)) 0)
           (values t string)
           (values nil "Empty response")))))

(defun display-response-content (content format)
  "Display the generated content with visual separators."
  (let ((fmt-name (string-capitalize (string-downcase (symbol-name format)))))
    (format t "~&━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
    (format t "Generated ~A Content:~%" fmt-name)
    (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
    (format t "~A~%" content)
    (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")))

(defun prompt-response-action (format)
  "Prompt the user to accept, reject, copy or view the generated response."
  (if (eq format :lisp)
      (prompt-eval-or-reject)
      (progn
        (format t "~&[A]ccept, [R]eject, [C]opy to clipboard, [V]iew content? [a/r/c/v]: ")
        (force-output)
        (let ((input (string-trim '(#\Space #\Tab #\Newline #\Return) (or (read-line *standard-input* nil "") ""))))
          (cond
            ((string-equal input "a") :accept)
            ((string-equal input "c") :copy)
            ((string-equal input "v") :view)
            (t :reject))))))

(defun handle-response-mode-response (prompt format)
  "Get a response from the LLM in the specified response mode and handle user actions."
  (let* ((sys-prompt (if (eq format :lisp)
                         (lisp-mode-system-prompt)
                         (response-mode-system-prompt format)))
         (response (get-llm-response prompt :stream nil :add-to-history t :custom-system-prompt sys-prompt)))
    (unless response
      (format t "No content generated.~%")
      (return-from handle-response-mode-response nil))
    (let ((content (extract-response-content response format)))
      (multiple-value-bind (valid-p err-msg) (validate-response-syntax content format)
        (unless valid-p
          (format t "Syntax Error: ~A~%" err-msg)
          (return-from handle-response-mode-response nil))
        (display-response-content content format)
        (loop
          (let ((choice (prompt-response-action format)))
            (cond
              ((eq choice :eval)
               (multiple-value-bind (res err) (eval-lisp-safely content)
                 (if err
                     (progn
                       (format t "Execution Error: ~A~%" err)
                       (return nil))
                     (return res))))
              ((eq choice :accept)
               (return content))
              ((eq choice :copy)
               (copy-to-clipboard content))
              ((eq choice :view)
               (display-response-content content format))
              ((eq choice :reject)
               (return nil)))))))))

(defun response-mode-intercept (prompt)
  "Intercept user prompt in interactive loop if any response mode is enabled."
  (let ((mode (cond
                ((or *lisp-mode-enabled* *lisp-response-mode-enabled*) :lisp)
                (*json-response-mode-enabled* :json)
                (*response-mode* *response-mode*)
                (t nil))))
    (if mode
        (progn
          (handle-response-mode-response prompt mode)
          t)
        nil)))

;;* commands
(define-command lisp-mode (args)
  "Toggle Lisp response mode."
  (if (null args)
      (format t "Lisp mode is ~A~%" (if (or *lisp-mode-enabled* *lisp-response-mode-enabled*) "ENABLED" "DISABLED"))
      (let ((val (string-downcase (first args))))
        (cond
          ((string= val "on")
           (setf *lisp-mode-enabled* t)
           (setf *lisp-response-mode-enabled* t)
           (setf *response-mode* :lisp)
           (format t "Lisp response mode enabled.~%"))
          ((string= val "off")
           (setf *lisp-mode-enabled* nil)
           (setf *lisp-response-mode-enabled* nil)
           (when (eq *response-mode* :lisp)
             (setf *response-mode* nil))
           (format t "Lisp response mode disabled.~%"))
          (t (format t "Usage: /lisp-mode [on|off]~%"))))))

(define-command lisp-response-mode (args)
  "Toggle Lisp response mode."
  (if (null args)
      (format t "Lisp response mode is ~A~%" (if *lisp-response-mode-enabled* "ENABLED" "DISABLED"))
      (let ((val (string-downcase (first args))))
        (cond
          ((string= val "on")
           (setf *lisp-mode-enabled* t)
           (setf *lisp-response-mode-enabled* t)
           (setf *response-mode* :lisp)
           (format t "Lisp response mode enabled.~%"))
          ((string= val "off")
           (setf *lisp-mode-enabled* nil)
           (setf *lisp-response-mode-enabled* nil)
           (when (eq *response-mode* :lisp)
             (setf *response-mode* nil))
           (format t "Lisp response mode disabled.~%"))
          (t (format t "Usage: /lisp-response-mode [on|off]~%"))))))

(define-command json-response-mode (args)
  "Toggle JSON response mode."
  (if (null args)
      (format t "JSON response mode is ~A~%" (if *json-response-mode-enabled* "ENABLED" "DISABLED"))
      (let ((val (string-downcase (first args))))
        (cond
          ((string= val "on")
           (setf *json-response-mode-enabled* t)
           (setf *response-mode* :json)
           (format t "JSON response mode enabled.~%"))
          ((string= val "off")
           (setf *json-response-mode-enabled* nil)
           (when (eq *response-mode* :json)
             (setf *response-mode* nil))
           (format t "JSON response mode disabled.~%"))
          (t (format t "Usage: /json-response-mode [on|off]~%"))))))

(define-command response-mode (args)
  "Set or check the LLM response conformance mode."
  (if (null args)
      (format t "Response mode is ~A~%" (if *response-mode* (symbol-name *response-mode*) "NONE"))
      (let ((val (string-downcase (first args))))
        (cond
          ((string= val "off")
           (setf *response-mode* nil)
           (setf *lisp-mode-enabled* nil)
           (setf *lisp-response-mode-enabled* nil)
           (setf *json-response-mode-enabled* nil)
           (format t "Response mode disabled.~%"))
          ((member val '("xml" "json" "yaml" "markdown" "org-mode" "lisp") :test #'string=)
           (let ((mode (intern (string-upcase val) :keyword)))
             (setf *response-mode* mode)
             ;; Sync specific booleans
             (setf *lisp-mode-enabled* (eq mode :lisp))
             (setf *lisp-response-mode-enabled* (eq mode :lisp))
             (setf *json-response-mode-enabled* (eq mode :json))
             (format t "Response conformance mode set to: ~A~%" (symbol-name mode))))
          (t (format t "Usage: /response-mode [xml|json|yaml|markdown|org-mode|lisp|off]~%"))))))

(define-command lisp (args)
  "Execute a single prompt in Lisp response mode."
  (if (null args)
      (format t "Usage: /lisp <prompt>~%")
      (let ((prompt (format nil "~{~A~^ ~}" args)))
        (handle-lisp-mode-response prompt))))

(define-command lisp-api (args)
  "Display the Lisp API surface reference."
  (declare (ignore args))
  (format t "~A~%" (get-lisp-mode-api-surface)))
