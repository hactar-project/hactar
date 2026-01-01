;; handles creating new projects with a starter kit
(in-package :hactar)

(defun create-project (starter user-prompt)
  "Create a new project using a starter and a user prompt/query.
STARTER can be a known starter name (e.g., \"react\", \"sinatra\"), or a file path.
USER-PROMPT is the natural language query/instructions for the project."
  (unless *current-model*
    (format t "No LLM model selected. Use /model to select a model.~%")
    (return-from create-project nil))
  (let* ((starter-content
           (cond
             ;; Known starter names
             ((and starter (stringp starter)
                   (member (string-downcase starter) '("react" "sinatra") :test #'string=))
              (read-file-content (resolve-starter-path (string-downcase starter))))
             ;; File path to starter content (supports string or pathname)
             ((and starter (probe-file starter))
              (read-file-content starter))
             ;; Fallback to resolving via resolve-starter-path (only when STARTER is a string)
             ((and starter (stringp starter))
              (read-file-content (resolve-starter-path starter)))
             ;; Default if nothing provided
             (t
              (read-file-content (resolve-starter-path "react")))))
         (system-prompt-template (uiop:read-file-string (get-prompt-path "system.create.org")))
         (context (generate-context))
         (system-prompt (mustache:render* system-prompt-template
                                          `((:language . ,(or *language* "unknown"))
                                            (:rules . "")
                                            (:guide . ,starter-content)
                                            (:context . ,context))))
         (final-user-prompt (or user-prompt (uiop:read-file-string (get-prompt-path "user.create.org")))))
    (if (and starter-content (> (length starter-content) 0))
        (progn
          (format t "Creating project from starter '~A'...~%" (or starter "react"))
          (get-llm-response final-user-prompt :custom-system-prompt system-prompt :add-to-history t))
        (format t "Error: Could not load starter content for '~A'.~%" starter))))

(define-slash-command create (args)
  "Create a new project from a starter. Usage: /create <starter> <prompt...>"
  (if (< (length args) 2)
      (format t "Usage: /create <starter> <prompt...>~%")
      (let* ((starter (first args))
             (user-prompt (format nil "~{~A~^ ~}" (rest args))))
        (create-project starter user-prompt))))

(define-sub-command create (args)
  "Create a new project from a starter. Usage: hactar create --path <dir> --starter <starter> <prompt...>"
  (let* ((path (getf args :path))
         (starter (or (getf args :starter) "react"))
         (subcommand (getf args :subcommand))
         (free-args (getf args :args))
         (user-prompt (when free-args (format nil "~{~A~^ ~}" (uiop:ensure-list free-args)))))
    (unless path
      (format t "Error: --path is required for the create subcommand. ~%")
      (format t "Usage: hactar create --path <dir> [--starter <starter>] <prompt...>~%")
      (uiop:quit 1))
    (let ((resolved-path (uiop:ensure-directory-pathname (expand-path path))))
      (ensure-directories-exist resolved-path)
      (setf *repo-root* resolved-path)

      (unless (git-repo-present-p *repo-root*)
        (format t "No git repository found in: ~A~%Initializing...~%" (namestring *repo-root*))
        (unless (git-init *repo-root*)
          (format *error-output* "~&Error: Failed to initialize git repository in: ~A~%" (namestring *repo-root*))
          (uiop:quit 1)))

      (format t "Creating project in:  ~A~%" (namestring *repo-root*))
      (create-project starter user-prompt)))
  :cli-options ((:long "path" :short "p" :description "Path to create the project in (required)")
                (:long "starter" :short "s" :description "Starter template to use (default: react)")))

(define-command create.agent (args)
  "Create a new Agent project using the AgentStarter guide. Usage: /create.agent [prompt...]"
  (let ((user-prompt (when args (format nil "~{~A~^ ~}" args))))
    (create-project *hactar-starters-agent* user-prompt)))
