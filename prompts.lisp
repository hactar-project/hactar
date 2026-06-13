;;* Prompt registry
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defprompt get-prompt render-prompt prompt prompt-p
            prompt-name prompt-type prompt-description prompt-template
            prompt-file prompt-system prompt-vars prompt-stream
            list-prompts find-prompt)))

(defstruct prompt
  name
  type
  description
  template
  file
  system
  vars
  (stream t))

(defun normalize-prompt-name (name)
  (intern (string-upcase (string name)) :keyword))

(defmacro defprompt (name &rest args)
  "Register a prompt template.
   Signature: (defprompt name [type] &body body)
   Or via keywords in body: :type keyword, :template string, :file path, :system string, :vars list, :stream bool."
  (let* ((has-type-arg (and (first args)
                            (symbolp (first args))
                            (not (keywordp (first args)))))
         (type (cond (has-type-arg (first args))
                     (t (getf args :type))))
         (body (if has-type-arg (rest args) args))
         (description (when (stringp (first body)) (first body)))
         (body-opts (if (stringp (first body)) (rest body) body))
         (tmpl (getf body-opts :template))
         (file (getf body-opts :file))
         (system (getf body-opts :system))
         (vars (getf body-opts :vars))
         (stream (getf body-opts :stream t)))
    `(progn
       (setf (gethash (normalize-prompt-name ',name) *prompts*)
             (make-prompt :name (normalize-prompt-name ',name)
                          :type ',type
                          :description ,description
                          :template ,tmpl
                          :file ,file
                          :system ,system
                          :vars ',vars
                          :stream ,stream))
       (normalize-prompt-name ',name))))

(defun find-prompt (name)
  "Find the prompt struct registered for NAME."
  (gethash (normalize-prompt-name name) *prompts*))

(defun get-prompt (name &optional file-fallback)
  "Return the template string registered for NAME.
   If absent and FILE-FALLBACK is given, load and memoize it from a prompt file."
  (let* ((norm-name (normalize-prompt-name name))
         (p (gethash norm-name *prompts*)))
    (cond
      ((null p)
       (when file-fallback
         (let ((tmpl (handler-case (uiop:read-file-string (get-prompt-path file-fallback))
                       (error () nil))))
           (when tmpl
             (setf (gethash norm-name *prompts*) tmpl))
           tmpl)))
      ((stringp p) p)
      ((prompt-p p)
       (let ((res (or (prompt-template p)
                      (and (prompt-file p)
                           (handler-case (uiop:read-file-string (get-prompt-path (prompt-file p)))
                             (error () nil))))))
         (if (and res (string/= res ""))
             res
             nil)))
      (t nil))))

(defun render-prompt (name alist &key file-fallback)
  "Render the prompt NAME via mustache using ALIST bindings."
  (let ((tmpl (get-prompt name file-fallback))
        (mustache:*escape-tokens* nil))
    (when tmpl
      (mustache:render* tmpl alist))))

(defun list-prompts ()
  "Return a list of all registered prompt structs sorted by name."
  (let (res)
    (maphash (lambda (k v)
               (if (prompt-p v)
                   (push v res)
                   (push (make-prompt :name k :template v) res)))
             *prompts*)
    (sort res #'string< :key (lambda (p) (string (prompt-name p))))))

;;* Standard Prompts Registration
(defprompt system.default system
  "The default system prompt template."
  :file "system.default.org")

(defprompt system.assistant system
  "The system prompt template for assistant mode."
  :file "system.assistant.org")

(defprompt system.litmode system
  "The system prompt template for literate mode."
  :file "system.litmode.org")

(defprompt system.dot-command system
  "The system prompt template for dot commands."
  :file "system.dot-command.org")

(defprompt system.create system
  "The system prompt template for creating files."
  :file "system.create.org")

(defprompt user.create user
  "The user prompt template for creating files."
  :file "user.create.org")

(defprompt git-commit template
  "The prompt template for generating git commit messages."
  :file "git-commit.mustache")

(defprompt complete-text template
  "The prompt template for text completion."
  :file "complete-text.mustache")

(defprompt ai-comment-analyzer template
  "The prompt template for analyzing AI comments."
  :file "ai-comment-analyzer.mustache")

(defprompt lint-fix template
  "The prompt template for fixing lint/build errors."
  :file "lint-fix.mustache")

(defprompt typecheck-fix template
  "The prompt template for fixing typecheck/build errors."
  :file "typecheck-fix.mustache")

(defprompt test-fix template
  "The prompt template for fixing test failures."
  :file "test-fix.mustache")

(defprompt summary template
  "The prompt template for generating task summaries."
  :file "summary.org")

(defprompt context template
  "The base context prompt template."
  :file "context.org")

(defprompt docs-ask-url template
  "The prompt template for asking documentation URLs."
  :file "docs-ask-url.mustache")

(defprompt generate-guide template
  "The prompt template for generating guides."
  :file "generate-guide.mustache")

(defprompt guide-author-system system
  "The system prompt template for the guide author."
  :file "guide-author-system.org")

(defprompt github-search-query-system system
  "The system prompt template for github search queries."
  :file "github-search-query-system.org")

(defprompt github-extract-snippets-system system
  "The system prompt template for github extract snippets."
  :file "github-extract-snippets-system.org")

(defprompt searchquery template
  "The prompt template for searching github."
  :file "searchquery.mustache")

(defprompt extractsnippets template
  "The prompt template for extracting code snippets."
  :file "extractcodesnippets.mustache")

(defprompt generate-shell-command template
  "The prompt template for generating shell commands."
  :file "generate-shell-command.mustache")

(defprompt redwood-to-lisp template
  "The prompt template for converting Redwood to Lisp."
  :file "redwood-to-lisp.org")

(defprompt html-to-markdown template
  "The prompt template for converting HTML to Markdown."
  :file "html-to-markdown.mustache")

(defprompt html-to-org template
  "The prompt template for converting HTML to Org mode."
  :file "html-to-org.mustache")

(defprompt api-docs.gen template
  "The prompt template for generating API docs."
  :file "api-docs.gen.org")

(defprompt code-gen-planner-system system
  "The system prompt template for code generation planning."
  :file "code-gen-planner-system.org")

(defprompt codetolisp template
  "The prompt template for converting code to Lisp."
  :file "codetolisp.org")

(defprompt docs-find-package template
  "The prompt template for finding documentation packages."
  :file "docs-find-package.mustache")

(defprompt generate-rule template
  "The prompt template for generating rules."
  :file "generate-rule.mustache")

(defprompt project-analyzer-system system
  "The system prompt template for project analysis."
  :file "project-analyzer-system.org")

(defprompt ruhe-compile template
  "The prompt template for compiling in Ruhe."
  :file "ruhe-compile.mustache")

(defprompt ruhe-generate-output template
  "The prompt template for generating output in Ruhe."
  :file "ruhe-generate-output.mustache")

(defprompt ruhe-parse-intent template
  "The prompt template for parsing intent in Ruhe."
  :file "ruhe-parse-intent.mustache")

(defprompt ruhe-summarize template
  "The prompt template for summarizing in Ruhe."
  :file "ruhe-summarize.mustache")

(defprompt system.apidocs.update system
  "System prompt for the hactar.apidocs.update command."
  :template "You are a technical documentation writer. You will be provided with an existing Org-mode API reference document and a YAML spec of the latest functions in the repository (including their signatures and docstrings). Your task is to update the Org-mode API reference document to incorporate any new functions, update changed signatures or docstrings, and remove deprecated functions if necessary, ensuring that the updated document is returned in full as a valid Org-mode document. Do not include any chat formatting, conversational text, or markdown code fences; respond ONLY with the raw Org-mode document content.")
