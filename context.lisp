(in-package :hactar)

;;* Context Handling for Hactar
;;; Stuff like adding documentation, generating the system prompt, removing and adding files etc

;; Hooks for context file add/drop events
(nhooks:define-hook-type context-file-added (function (string) t)
  "Hook run when a file is added to the context. Handler is called with the file path (string).")
(defvar *context-file-added-hook* (make-instance 'hook-context-file-added))

(nhooks:define-hook-type context-file-dropped (function (string) t)
  "Hook run when a file is removed from the context. Handler is called with the file path (string).")
(defvar *context-file-dropped-hook* (make-instance 'hook-context-file-dropped))

;; Hook for project variable changes
(nhooks:define-hook-type context-variable-changed (function (keyword t) t)
  "Hook run when a project variable changes. Handler is called with var name (keyword) and new value.")
(defvar *context-variable-changed-hook* (make-instance 'hook-context-variable-changed))

(defun get-active-guide-content ()
  "Reads the content of the active guide file, performing size checks."
  (when (and *active-guide-file* (probe-file *active-guide-file*))
    (handler-case
        (let* ((content (uiop:read-file-string *active-guide-file*))
               (content-len (length content)))
          (cond
           ((> content-len *guide-max-chars*)
            (format t "~&Error: Active guide file '~A' exceeds maximum size (~A > ~A chars). Not including in prompt.~%"
                    (uiop:native-namestring *active-guide-file*) content-len *guide-max-chars*)
            nil)
           ((> content-len *guide-warn-chars*)
            (format t "~&Warning: Active guide file '~A' is large (~A > ~A chars). Including in prompt.~%"
                    (uiop:native-namestring *active-guide-file*) content-len *guide-warn-chars*)
	    ;; Filter content before returning
            (org-mode:filter-headlines content *guide-exclude-tags*))
           (t
	    ;; Filter content before returning
            (org-mode:filter-headlines content *guide-exclude-tags*))))
      (error (e)
             (format t "~&Error reading or processing active guide file '~A': ~A~%" (uiop:native-namestring *active-guide-file*) e)
	     nil))))

(defun dot-system-prompt ()
  "Generate the system prompt for dot commands."
  (let* ((mustache:*escape-tokens* nil)
	 (template-string (handler-case (uiop:read-file-string (get-prompt-path "system.dot-command.org"))
                            (error (e)
				   (format t "Error reading dot command system prompt: ~A~%" e)
				   "Error: Could not load dot command system prompt. Commands: {{commands}}. Rules: {{rules}}. Context: {{context}}.")))
         (context-string (generate-context))
         (rules-string (with-output-to-string (s)
					      (maphash (lambda (key value)
							 (declare (ignore key))
							 (format s "~A~%~%" value))
						       *active-rules*)))
         (commands-string (with-output-to-string (s)
						 (maphash (lambda (cmd-name cmd-info)
							    (format s "command: ~A~% ~A~%" cmd-name (second cmd-info)))
							  *dot-commands*))))
    (mustache:render* template-string
                      `((:commands . ,commands-string)
                        (:rules . ,(if (string= rules-string "") "(No active rules)" rules-string))
                        (:context . ,context-string)))))

(defun default-system-prompt ()
  "Generate the system prompt by combining templates, rules, context, and the active guide."
  (let* ((mustache:*escape-tokens* nil)
         (base-prompt-name (if (and (fboundp 'litmode-active-p)
                                    (funcall 'litmode-active-p))
                               "system.litmode.org"
                               "system.default.org"))
	 (base-prompt (uiop:read-file-string (get-prompt-path base-prompt-name)))
         (context (generate-context))
         (guide-content (get-active-guide-content))
         (rules-text (with-output-to-string (s)
					    (maphash (lambda (key value)
						       (declare (ignore key))
						       (format s "~A~%~%" value))
						     *active-rules*)))
         (tools-xml (get-tools-section))
         (tools-enabled (and *tool-use-enabled*
                             *tools-in-system-prompt*
                             (> (hash-table-count *defined-tools*) 0))))
    (mustache:render* base-prompt
                      `((:language . ,(language))
                        (:rules . ,rules-text)
                        (:mold . ,(mold-context-string))
                        (:guide . ,(or guide-content ""))
                        (:context . ,context)
                        (:tools . ,(or tools-xml ""))
                        (:tools_enabled . ,tools-enabled)))))

(defun assistant-mode-system-prompt ()
  "Generate the system prompt for assistant mode."
  (handler-case
      (let ((template-string (uiop:read-file-string (get-prompt-path "system.assistant.org"))))
        ;; Currently, system.assistant.org has no placeholders,
        ;; but we use mustache for consistency and future-proofing.
        (mustache:render* template-string '())) ; Empty alist for no variables
    (error (e)
      (format t "Error reading assistant system prompt: ~A~%" e)
      "Error: Could not load assistant system prompt.")))

(defun system-prompt ()
  "Returns the appropriate system prompt based on the current mode."
  (cond (*assistant-mode-active*
         (assistant-mode-system-prompt))
        (*lisp-rpc-mode*
         (lisp-rpc-system-prompt))
        (t
         (default-system-prompt))))

(defun generate-commit-message ()
  "Asks the LLM to generate a commit message based on the staged git diff."
  (unless *cheap-model*
    (format t "No LLM model selected. Cannot generate commit message.~%")
    (return-from generate-commit-message nil))
  (let ((model-config (find-model-by-name *cheap-model*)))
    (unless model-config
      (format t "Model '~A' not found.~%" *cheap-model*)
      (return-from generate-commit-message nil))
    (let* ((prompt-template (uiop:read-file-string (get-prompt-path "git-commit.mustache")))
           (diff (run-git-command '("diff" "--staged") :ignore-error nil))
           (prompt (mustache:render* prompt-template `((:diff . ,diff))))
           (messages `(((:role . "user") (:content . ,prompt))))
           (provider (intern (string-upcase (model-config-provider model-config)) :keyword)))
      (handler-case
          (multiple-value-bind (response _)
			       (llm:complete provider messages
					     :model (model-config-model-name model-config)
					     :max-tokens (model-config-max-output-tokens model-config)
					     :system-prompt "You generate commit messages."
					     :stream nil)
			       (declare (ignore _))
			       (when response
				 (let ((first-line (first (str:lines response))))
				   (string-trim '(#\Space #\Tab #\Newline #\Return) first-line))))
        (error (e)
               (format t "Error generating commit message: ~A~%" e)
               nil)))))

(defun language ()
  "Return the current language"
  *language*)

(defun images-context-string ()
  "Generate context string describing added images."
  (if *images*
    (with-output-to-string (s)
      (format s "Images in Context:~%")
      (dolist (img *images*)
        (format s "  - ~A~@[ (Description: ~A)~]~%"
                (uiop:native-namestring (getf img :path))
                (getf img :text))))
    "(No images added to context)"))

(defun files-context (&optional (max-tokens nil))
  "Generate context string from *text* files, respecting token limits.
   Iterates files from newest to oldest, adding content until max-tokens is approached.
   Returns the context string and a list of skipped files."
  (let ((files-content-stream (make-string-output-stream))
        (current-tokens 0)
        (skipped-files '())
        (included-files '())
        ;; Process files in reverse order (newest first)
        (files-to-process (reverse *files*)))
    (loop for file in files-to-process
          do (let* ((content (get-file-content file))
                    (file-tokens (when content (estimate-tokens content)))
                    (relative-path (uiop:native-namestring (uiop:enough-pathname file *repo-root*)))
                    (extension (pathname-type file))
                    (lang-hint (get-language-hint-from-extension extension))
                    (formatted-block (format nil "~%```~A ~A~%~A~%```~%"
                                             (or lang-hint "")
                                             relative-path
                                             content))
                    (block-tokens (estimate-tokens formatted-block)))
               (if (and content file-tokens max-tokens (> (+ current-tokens block-tokens) max-tokens))
                 (progn
                   (debug-log "Skipping file due to token limit:" relative-path)
                   (push relative-path skipped-files))
                 (when content
                   (debug-log "Including file:" relative-path "Tokens:" block-tokens)
                   (write-string formatted-block files-content-stream)
                   (incf current-tokens block-tokens)
                   (push relative-path included-files)))))
    (values (get-output-stream-string files-content-stream) (nreverse skipped-files) (nreverse included-files))))

(defun generate-context-without-files ()
  "Generates the context string excluding the file content part."
  (let ((mustache:*escape-tokens* nil))
    (mustache:render*
     (uiop:read-file-string (get-prompt-path "context.org"))
     `((:repo-map . ,*repo-map*)
       (:files . "")
       (:stack . ,(format nil "~{~A~^, ~}" *stack*))
       (:shell . ,*shell*)
       (:language . ,(language))
       (:name . ,*name*)
       (:author . ,*author*)
       (:docs-context . ,(docs-context-string))
       (:errors-context . ,(errors-context-string))
       (:images-context . ,(images-context-string))
       (:mold . ,(mold-context-string))))))

(defun read-context-from-file ()
  "Read the entire context from the exposed context file. Returns the file content as string."
  (when (and *exposed-context-file* (probe-file *exposed-context-file*))
    (handler-case
        (uiop:read-file-string *exposed-context-file*)
      (error (e)
        (format t "~&Error reading context file: ~A~%" e)
        nil))))

(defun generate-context (&optional (raw-context nil))
  "Generate full context string by reading from the context file if it exists, otherwise build traditionally."
  (if (and *exposed-context-file* (probe-file *exposed-context-file*) (not raw-context))
      ;; Use context file as source of truth
      (or (read-context-from-file) "")
      ;; Fallback to traditional context generation
      (let* ((mustache:*escape-tokens* nil)
	     (base-context-template (uiop:read-file-string (get-prompt-path "context.org")))
             (docs-string (docs-context-string))
             (errors-string (errors-context-string))
             (repo-map-string (if *repo-map* *repo-map* ""))
             (stack-string (format nil "~{~A~^, ~}" *stack*))
             ;; Estimate tokens used by non-file parts
             (mold-string (mold-context-string))
             (base-tokens (estimate-tokens
                           (mustache:render* base-context-template
                                             `((:repo-map . ,repo-map-string)
                                               (:files . "")
                                               (:stack . ,stack-string)
                                               (:shell . ,*shell*)
                                               (:language . ,(language))
                                               (:name . ,*name*)
                                               (:author . ,*author*)
                                               (:docs-context . ,docs-string)
                                               (:errors-context . ,errors-string)
                                               (:mold . ,mold-string)))))
             (max-input-tokens (if *current-model* (model-config-max-input-tokens *current-model*) 32000))
             (remaining-tokens-for-files (- max-input-tokens base-tokens))
             (files-context-string "")
             (skipped-files '())
             (included-files '()))

        (when (> remaining-tokens-for-files 0)
          (multiple-value-bind (fcs skipped included)
                               (files-context remaining-tokens-for-files)
            (setf files-context-string fcs)
            (setf skipped-files skipped)
            (setf included-files included)))

        (when skipped-files
          (format t "~&Warning: The following files were not included in the context due to token limits: ~{~A~^, ~}~%" skipped-files))

        (mustache:render* base-context-template
                          `((:repo-map . ,repo-map-string)
                            (:files . ,files-context-string)
                            (:stack . ,stack-string)
                            (:shell . ,*shell*)
                            (:language . ,(language))
                            (:name . ,*name*)
                            (:author . ,*author*)
                            (:docs-context . ,docs-string)
                            (:errors-context . ,errors-string)
                            (:mold . ,mold-string))))))

(defun add-file-to-context (file-path)
  "Add a file to the context.

When litmode is active, also upsert the file as an org src block under
* Context -> ** Files in .hactar.org."
  (let ((was-present (member file-path *files* :test #'string=)))
    (pushnew file-path *files* :test #'string=)
    (unless was-present
      ;; Keep exposed context file in sync (non-litmode flow).
      (context-expose-upsert-files-section)
      (nhooks:run-hook *context-file-added-hook* file-path)))

  ;; In litmode, reflect /add into .hactar.org under ** Files.
  (when (and (fboundp 'litmode-active-p)
             (funcall 'litmode-active-p)
             (boundp '*litmode-parsed*)
             (boundp '*litmode-path*))
    (handler-case
        (progn
          ;; Ensure litmode document is loaded
          (when (or (null (symbol-value '*litmode-parsed*))
                    (null (symbol-value '*litmode-path*)))
            (when (fboundp 'load-litmode-file) (funcall 'load-litmode-file t)))

          (let* ((doc (symbol-value '*litmode-parsed*))
                 (base-dir (uiop:pathname-directory-pathname (symbol-value '*litmode-path*)))
                 (abs-path (pathname file-path))
                 ;; Store relative path when possible (more portable in repo).
                 (rel-path (handler-case
                               (uiop:enough-pathname abs-path base-dir)
                             (error () abs-path)))
                 (rel-str (uiop:native-namestring rel-path))
                 (content (get-file-content abs-path))
                 (ext (pathname-type abs-path))
                 (lang (or (ignore-errors (get-language-hint-from-extension ext)) ""))
                 (files-heading (and doc
                                     (when (fboundp 'find-heading-by-custom-id)
                                       (funcall 'find-heading-by-custom-id doc "ctx-files")))))
            (when (and files-heading content)
              ;; Remove any existing paragraph that references this file path
              ;; to avoid duplicates on repeated /add.
              (let ((children-to-remove '()))
                (dolist (child (org-mode-parser:node-children files-heading))
                  (when (typep child 'org-mode-parser:org-paragraph)
                    (let ((c (org-mode-parser:paragraph-content child)))
                      (when (and c (search rel-str c))
                        (push child children-to-remove)))))
                (dolist (child children-to-remove)
                  (org-mode-parser:remove-node child)))
              ;; Add the src block with :tangle keyword
              (org-mode-parser:add-child
               files-heading
               (make-instance 'org-mode-parser:org-paragraph
                              :type :paragraph
                              :content
                              (format nil "#+begin_src ~A :tangle ~A~%~A~%#+end_src~%~%"
                                      lang
                                      rel-str
                                      (org-mode:escape-for-org-s content))))
              (when (fboundp 'save-litmode-file)
                (funcall 'save-litmode-file doc)))))
      (error (e)
        (debug-log "Litmode: failed to upsert added file into .hactar.org:" e))))

  ;; Check potential token usage after adding
  (when *current-model*
    (let* ((file-content (get-file-content file-path))
           (file-tokens (when file-content (estimate-tokens file-content)))
           (current-history-tokens (get-current-token-count))
           ;; Estimate tokens from other context parts (docs, repo map, etc.) - rough estimate
           (other-context-tokens (estimate-tokens (generate-context-without-files)))
           (projected-total (+ current-history-tokens other-context-tokens file-tokens))
           (max-input (model-config-max-input-tokens *current-model*)))
      (when (> projected-total max-input)
        (cond
          (*acp-mode*
           (acp-log "Warning: Adding file '~A' (~A tokens) might exceed the model's input limit (~A tokens) when combined with current history and context."
                    (uiop:enough-pathname file-path *repo-root*) file-tokens max-input))
          (*lisp-rpc-mode*
           (rpc-warning (format nil "Adding file '~A' (~A tokens) might exceed the model's input limit (~A tokens)."
                                (uiop:enough-pathname file-path *repo-root*) file-tokens max-input)
                        :file (uiop:native-namestring (uiop:enough-pathname file-path *repo-root*))
                        :file-tokens file-tokens
                        :max-input-tokens max-input))
          (t
           (format t "~&Warning: Adding file '~A' (~A tokens) might exceed the model's input limit (~A tokens) when combined with current history and context.~%"
                   (uiop:enough-pathname file-path *repo-root*) file-tokens max-input)))))))
(defun add-image-to-context (image-path &optional text)
  "Add an image to the context window."
  (check-image-size image-path) ; Warn if too large
  (pushnew `(:path ,image-path :text ,text) *images* :test #'equal :key (lambda (img) (getf img :path)))
  (format t "Added image to context: ~A~@[ with description: ~A~]~%" (uiop:native-namestring image-path) text))

(defun drop-file-from-context (file-path)
  "Remove a text file from the context by updating both the in-memory list and the context file.
When litmode is active, also removes the file's src block from .hactar.org."
  (let ((was-present (member file-path *files* :test #'string=)))
    (setf *files* (remove file-path *files* :test #'string=))
    (when was-present
      (context-expose-upsert-files-section)
      (nhooks:run-hook *context-file-dropped-hook* file-path)

      ;; In litmode, remove the file from .hactar.org under ** Files
      (when (and (fboundp 'litmode-active-p)
                 (funcall 'litmode-active-p)
                 (boundp '*litmode-parsed*)
                 (boundp '*litmode-path*))
        (handler-case
            (progn
              (when (or (null (symbol-value '*litmode-parsed*))
                        (null (symbol-value '*litmode-path*)))
                (when (fboundp 'load-litmode-file) (funcall 'load-litmode-file t)))

              (let* ((doc (symbol-value '*litmode-parsed*))
                     (base-dir (uiop:pathname-directory-pathname (symbol-value '*litmode-path*)))
                     (abs-path (pathname file-path))
                     (rel-path (handler-case
                                   (uiop:enough-pathname abs-path base-dir)
                                 (error () abs-path)))
                     (rel-str (uiop:native-namestring rel-path))
                     (files-heading (and doc
                                         (when (fboundp 'find-heading-by-custom-id)
                                           (funcall 'find-heading-by-custom-id doc "ctx-files")))))
                (when files-heading
                  ;; Remove paragraph children whose content references this file path
                  (let ((children-to-remove '()))
                    (dolist (child (org-mode-parser:node-children files-heading))
                      (when (typep child 'org-mode-parser:org-paragraph)
                        (let ((content (org-mode-parser:paragraph-content child)))
                          (when (and content (search rel-str content))
                            (push child children-to-remove)))))
                    (dolist (child children-to-remove)
                      (org-mode-parser:remove-node child)))
                  (when (fboundp 'save-litmode-file)
                    (funcall 'save-litmode-file doc)))))
          (error (e)
            (debug-log "Litmode: failed to remove dropped file from .hactar.org:" e)))))))

(defun drop-image-from-context (image-path)
  "Remove an image from the context window."
  (setf *images* (remove image-path *images* :test #'string= :key (lambda (img) (getf img :path)))))

(defun list-context-files ()
  "List all files in the context."
  (if *files*
    (dolist (file *files*)
      (format t "  ~A~%" file))
    (format t "  (No files in context)~%")))

(defun docs-context-string ()
  "Generate context string from added documentation."
  (if *docs-context*
    (with-output-to-string (s)
      (dolist (doc *docs-context*)
        (format s "Title: ~A~%Source: ~A~%Covers: ~A~%Tags: ~A~%~A"
                (cdr (assoc :title doc))
                (cdr (assoc :source doc))
                (format nil "~A" (cdr (assoc :covers doc)))
                (format nil "~A" (cdr (assoc :tags doc)))
		(format nil "content: ~%~A"
			(cdr (assoc :content doc))))))
    "(No documentation added to context)"))

(defun errors-context-string ()
  "Generate context string from added errors."
  (if *errors-context*
    (with-output-to-string (s)
      (dolist (err *errors-context*)
        (format s "Title: ~A~%Code: ~A~%Message: ~A~%Cause: ~A~%Solution: ~A~%~%"
                (cdr (assoc :title err))
                (cdr (assoc :code err))
                (cdr (assoc :message err))
                (cdr (assoc :cause err))
                (cdr (assoc :solution err)))))
    "(No errors added to context)"))

(defun add-doc-to-context (doc-plist)
  "Adds a documentation plist to the *docs-context* if not already present (by ID).
Also updates the exposed context docs section if active."
  (unless (member (cdr (assoc :id doc-plist)) *docs-context* :key (lambda (d) (cdr (assoc :id d))))
    (push doc-plist *docs-context*)
    (format t "Added doc to context: ~A~%" (cdr (assoc :title doc-plist)))
    (when *exposed-context-file*
      (context-expose-upsert-docs-section))))

(defun remove-doc-from-context (doc-id)
  "Removes a documentation plist from *docs-context* by ID.
Also updates the exposed context docs section if active."
  (setf *docs-context* (remove doc-id *docs-context* :key (lambda (d) (cdr (assoc :id d)))))
  (when *exposed-context-file*
    (context-expose-upsert-docs-section)))

(defun add-error-to-context (error-plist)
  "Add an error plist to the context if not already present."
  (unless (find (cdr (assoc :code error-plist)) *errors-context* :key (lambda (e) (cdr (assoc :code e))) :test #'string=)
    (push error-plist *errors-context*)
    (unless *silent* (format t "Added error to context: ~A~%" (cdr (assoc :title error-plist))))
    (when *exposed-context-file*
      (context-expose-upsert-errors-section))))

(defun remove-error-from-context (error-code)
  "Remove an error from context by code."
  (setf *errors-context* (remove error-code *errors-context* :key (lambda (e) (cdr (assoc :code e))) :test #'string=))
  (when *exposed-context-file*
    (context-expose-upsert-errors-section)))

(defun parse-add-args (args)
  "Parse arguments for /add command. Returns (values files descriptions)."
  (let ((descriptions '())
        (files '()))
    (dolist (arg args)
      (if (str:starts-with? "-descriptions=" arg)
          (let ((val (subseq arg (length "-descriptions="))))
            (setf descriptions (uiop:split-string val :separator ",")))
          (push arg files)))
    (values (nreverse files) descriptions)))

(defun expand-file-pattern (pattern &optional (root *repo-root*))
  "Expand a file pattern (glob) into a list of absolute native pathnames.
   Handles patterns like 'src/*' or 'src/**/*.lisp' using the glob function."
  (let* ((pattern-str (if (pathnamep pattern) (namestring pattern) pattern))
         (has-wildcard (or (position #\* pattern-str)
                           (position #\? pattern-str)
                           (position #\[ pattern-str))))
    (if has-wildcard
        ;; Use glob for patterns with wildcards
        (let ((matches (glob pattern-str root)))
          (loop for match in matches
                unless (uiop:directory-pathname-p match)
                collect (uiop:native-namestring (merge-pathnames match root))))
        ;; No wildcards - exact file path
        (let* ((path (pathname pattern-str))
               (full-path (if (uiop:absolute-pathname-p path)
                              path
                              (merge-pathnames path root))))
          (if (probe-file full-path)
              (list (uiop:native-namestring full-path))
              nil)))))

(defun process-add-request (args)
  "Process the add request with raw arguments."
  (multiple-value-bind (file-patterns descriptions) (parse-add-args args)
    (dolist (pattern file-patterns)
      (let ((expanded-files (expand-file-pattern pattern)))
        (if expanded-files
            (dolist (native-path expanded-files)
              (if (is-image-file? native-path)
                  (let ((desc (pop descriptions)))
                    (add-image-to-context native-path desc))
                  (add-file-to-context native-path)))
            (format t "~&File or pattern not found: ~A~%" pattern))))))

(define-command add (args)
                "Add files or images to the chat. If no arguments are given, uses fzf to select a file.
Can provide image descriptions via -descriptions=\"desc1,desc2\""
                (if args
                    (process-add-request args)
                  ;; FZF selection (only for files, not images for now)
                  (let* ((tracked-files (list-git-tracked-files *repo-root*))
                         ;; Filter out likely image files from fzf selection for now
                         (non-image-files (remove-if #'is-image-file? tracked-files :key (lambda (f) (merge-pathnames f *repo-root*))))
                         (selected-relative-path (when non-image-files
                                                   (select-with-fzf non-image-files))))
                    (if selected-relative-path
			(let ((selected-full-path (uiop:native-namestring
                                                   (merge-pathnames selected-relative-path *repo-root*))))
                          ;; Double check it's not an image before adding as text file
                          (if (is-image-file? selected-full-path)
                              (format t "Selected file appears to be an image. Use '/add <image_path>' to add it.~%")
                            (add-file-to-context selected-full-path)))
                      (format t "No file selected or no non-image tracked files found.~%"))))
                :completions (lambda (text args)
                               (declare (ignore args))
                               (let* ((tracked (ignore-errors (list-git-tracked-files *repo-root*)))
                                      (already (mapcar (lambda (f)
                                                         (uiop:native-namestring
                                                          (uiop:enough-pathname f *repo-root*)))
                                                       *files*)))
                                 (remove-if (lambda (f) (member f already :test #'string=))
                                            (if (string= text "")
                                                tracked
                                                (remove-if-not
                                                 (lambda (f)
                                                   (str:starts-with-p text f :ignore-case t))
                                                 tracked)))))
                :acp (lambda (cmd-args)
                       (if cmd-args
                           (progn
                             (process-add-request cmd-args)
                             (let ((added-files
                                     (loop for pattern in cmd-args
                                           append (expand-file-pattern pattern))))
                               `(("text" . ,(format nil "Added ~A file(s) to context." (length added-files)))
                                 ("data" . ,(coerce
                                             (mapcar (lambda (f)
                                                       `(("path" . ,f)
                                                         ("type" . ,(if (is-image-file? f) "image" "file"))))
                                                     added-files)
                                             'vector)))))
                           `(("text" . "No files specified. Provide file paths or patterns.")))))

(define-command drop (args)
                "Remove files or images from the chat session."
                (dolist (file-arg args)
                  (let* ((path (pathname file-arg))
                         (full-path (if (uiop:absolute-pathname-p path)
					path
                                      (merge-pathnames path *repo-root*)))
                         (native-path (uiop:native-namestring full-path)))
                    ;; Try dropping as both file and image
                    (drop-file-from-context native-path)
                    (drop-image-from-context native-path)))
                (format t "Dropped files/images from context: ~{~A~^, ~}~%" args)
                :completions (lambda (text args)
                               (declare (ignore args))
                               (let ((in-context (mapcar (lambda (f)
                                                           (uiop:native-namestring
                                                            (uiop:enough-pathname f *repo-root*)))
                                                         *files*)))
                                 (if (string= text "")
                                     in-context
                                     (remove-if-not
                                      (lambda (f)
                                        (str:starts-with-p text f :ignore-case t))
                                      in-context))))
                :acp (lambda (cmd-args)
                       (let ((dropped '()))
                         (dolist (file-arg cmd-args)
                           (let* ((path (pathname file-arg))
                                  (full-path (if (uiop:absolute-pathname-p path)
                                                 path
                                                 (merge-pathnames path *repo-root*)))
                                  (native-path (uiop:native-namestring full-path)))
                             (drop-file-from-context native-path)
                             (drop-image-from-context native-path)
                             (push native-path dropped)))
                         `(("text" . ,(format nil "Dropped ~A file(s) from context." (length dropped)))
                           ("data" . ,(coerce
                                       (mapcar (lambda (f) `(("path" . ,f))) (nreverse dropped))
                                       'vector))))))

(define-command editor (args)
                "Open an editor to write a prompt."
                (declare (ignore args))
                (let ((prompt (get-multiline-input)))
                  (when (and prompt (not (string= (string-trim '(#\Space #\Tab #\Newline) prompt) "")))
                    (get-llm-response prompt))))

(define-command ls (args)
                "List all known files and indicate which are included in the chat session.
Usage: /ls [--format=json]"
                (let ((format-opt (getf args :format)))
                  (if (string-equal format-opt "json")
                      (let* ((files (mapcar (lambda (f) `((:path . ,f) (:type . "file"))) *files*))
                             (images (mapcar (lambda (img)
                                               `((:path . ,(uiop:native-namestring (getf img :path)))
                                                 (:description . ,(getf img :text))
                                                 (:type . "image")))
                                             *images*))
                             (json-str (format nil "#+begin_src json :type ls-output~%~A~%#+end_src~%"
                                               (to-json (coerce (append files images) 'vector)))))
                        (editor-output json-str :type "json" :success "true"))
                      (progn
                        (format t "Files in context:~%")
                        (list-context-files)
                        (format t "~%Images in context:~%")
                        (if *images*
                            (dolist (img *images*)
                              (format t "  ~A~@[ (Description: ~A)~]~%"
                                      (uiop:native-namestring (getf img :path))
                                      (getf img :text)))
                            (format t "  (No images in context)~%")))))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (let* ((files (mapcar (lambda (f)
                                              `(("path" . ,f) ("type" . "file")))
                                            *files*))
                              (images (mapcar (lambda (img)
                                               `(("path" . ,(uiop:native-namestring (getf img :path)))
                                                 ("description" . ,(or (getf img :text) ""))
                                                 ("type" . "image")))
                                             *images*))
                              (all-items (append files images)))
                         `(("text" . ,(format nil "~A file(s) and ~A image(s) in context."
                                              (length *files*) (length *images*)))
                           ("data" . ,(coerce all-items 'vector)))))
                :cli-options ((:long "format" :description "Output format (json)")))

(define-command context (args)
                "List files in the current context."
                (declare (ignore args))
                (format t "~A" (generate-context))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       `(("text" . ,(generate-context)))))

(define-slash-command dump-context (args)
		    "Dump the raw context that the LLM sees."
		    (declare (ignore args))
		    (format t "~A" (generate-context t)))

;; Register dump-context as ACP-compatible
(setf (gethash "/dump-context" *acp-commands*)
      (lambda (cmd-args)
        (declare (ignore cmd-args))
        `(("text" . ,(generate-context t)))))

(define-command images (args)
                "List images currently in the context."
                (declare (ignore args))
                (if *images*
                    (progn
                      (format t "Images in context:~%")
                      (dolist (img *images*)
			(format t "  - ~A~@[ (Description: ~A)~]~%"
				(uiop:native-namestring (getf img :path))
				(getf img :text))))
                  (format t "No images in context.~%"))
                :acp (lambda (cmd-args)
                       (declare (ignore cmd-args))
                       (let ((imgs (mapcar (lambda (img)
                                            `(("path" . ,(uiop:native-namestring (getf img :path)))
                                              ("description" . ,(or (getf img :text) ""))))
                                          *images*)))
                         `(("text" . ,(format nil "~A image(s) in context." (length *images*)))
                           ("data" . ,(coerce imgs 'vector))))))

(define-command drop-image (args)
                "Remove an image from the context by its path."
                (if args
                  (let* ((image-arg (first args))
                         (path (pathname image-arg))
                         (full-path (if (uiop:absolute-pathname-p path)
                                      path
                                      (merge-pathnames path *repo-root*)))
                         (native-path (uiop:native-namestring full-path)))
                    (drop-image-from-context native-path)
                    (format t "Dropped image: ~A~%" native-path))
                  (format t "Please specify the image path to drop.~%"))
                :acp (lambda (cmd-args)
                       (if cmd-args
                           (let* ((image-arg (first cmd-args))
                                  (path (pathname image-arg))
                                  (full-path (if (uiop:absolute-pathname-p path)
                                                 path
                                                 (merge-pathnames path *repo-root*)))
                                  (native-path (uiop:native-namestring full-path)))
                             (drop-image-from-context native-path)
                             `(("text" . ,(format nil "Dropped image: ~A" native-path))
                               ("data" . (("path" . ,native-path)))))
                           `(("text" . "Please specify the image path to drop.")))))

;; --- Context Expose Feature ---

(defun context-expose-file-path ()
  "Compute the pathname for the exposed context file hactar.{pid}.context.org under repo root."
  (merge-pathnames (format nil ".hactar.~A.context.org" (current-pid)) *repo-root*))

(defun context-expose-generate-files-section ()
  "Generate src blocks for all current context files (without the Files headline)."
  (with-output-to-string (s)
    (dolist (file *files*)
      (let* ((content (get-file-content file))
             (extension (pathname-type (pathname file)))
             (lang (or (ignore-errors (get-language-hint-from-extension extension)) ""))
             (rel (uiop:native-namestring (uiop:enough-pathname file *repo-root*))))
        (when content
          (format s "#+begin_src ~A ~A~%~A~%#+end_src~%~%" lang rel (org-mode:escape-for-org-s content)))))))

(defun context-expose-generate-project-details ()
  "Generate the content for the Project Details section."
  (format nil "- Name: ~A~%- Author: ~A~%- Language: ~A~%- Shell: ~A~%- Stack: ~{~A~^, ~}~%"
          (or *name* "Unknown")
          (or *author* "Unknown")
          (or *language* "Unknown")
          (or *shell* "Unknown")
          (or *stack* '())))

(defun context-expose-upsert-project-details ()
  "Create or update the '* Project Details' section in the exposed context file."
  (when *exposed-context-file*
    (let* ((details (context-expose-generate-project-details))
           (file *exposed-context-file*)
           (existing (when (probe-file file)
                       (uiop:read-file-string file)))
           (updated (org-mode:upsert-files-section (or existing "") details 
                                                    :heading-title "Project Details" 
                                                    :level 1)))
      (ensure-directories-exist file)
      (write-file-content file updated))))

(defun context-expose-upsert-files-section ()
  "Create or update the '* Files' section in the exposed context file."
  (when *exposed-context-file*
    (let* ((blocks (context-expose-generate-files-section))
           (file *exposed-context-file*)
           (existing (when (probe-file file)
                       (uiop:read-file-string file)))
           (updated (org-mode:upsert-files-section (or existing "") blocks)))
      (ensure-directories-exist file)
      (write-file-content file updated))))

(defun context-expose-upsert-docs-section ()
  "Create or update the '* Documentation' section in the exposed context file."
  (when *exposed-context-file*
    (let* ((file *exposed-context-file*)
           (existing (when (probe-file file)
                       (uiop:read-file-string file)))
           (updated (org-mode:upsert-docs-section (or existing "") *docs-context*)))
      (ensure-directories-exist file)
      (write-file-content file updated))))

(defun context-expose-upsert-errors-section ()
  "Create or update the '* Errors' section in the exposed context file."
  (when *exposed-context-file*
    (let* ((file *exposed-context-file*)
           (existing (when (probe-file file)
                       (uiop:read-file-string file)))
           (updated (org-mode:upsert-errors-section (or existing "") *errors-context*)))
      (ensure-directories-exist file)
      (write-file-content file updated))))

(defun context-expose-on-file-added (file-path)
  "Hook handler: when a file is added to context, refresh Files section."
  (declare (ignore file-path))
  (when *exposed-context-file*
    (context-expose-upsert-files-section)))

(defun context-expose-on-file-dropped (file-path)
  "Hook handler: when a file is removed from context, refresh Files section."
  (declare (ignore file-path))
  (when *exposed-context-file*
    (context-expose-upsert-files-section)))

(defun context-expose-on-variable-changed (var-name new-value)
  "Hook handler: when a project variable changes, refresh Project Details section."
  (declare (ignore var-name new-value))
  (when *exposed-context-file*
    (context-expose-upsert-project-details)))

(defun context-expose-on-fs-event (pathname event-type)
  "Hook handler: when a tracked file changes on disk, refresh Files section."
  (declare (ignore event-type))
  (when *exposed-context-file*
    (let ((native (uiop:native-namestring pathname)))
      (when (member native *files* :test #'string=)
        (context-expose-upsert-files-section)))))

(defun context-expose-install-hooks-if-needed ()
  "Install expose-related hooks once."
  (unless *context-expose-hooks-installed*
    (nhooks:add-hook *context-file-added-hook*
                     (make-instance 'nhooks:handler
                                    :fn #'context-expose-on-file-added
                                    :name 'context-expose-on-file-added))
    (nhooks:add-hook *context-file-dropped-hook*
                     (make-instance 'nhooks:handler
                                    :fn #'context-expose-on-file-dropped
                                    :name 'context-expose-on-file-dropped))
    (nhooks:add-hook *file-event-hook*
                     (make-instance 'nhooks:handler
                                    :fn #'context-expose-on-fs-event
                                    :name 'context-expose-on-fs-event))
    (nhooks:add-hook *context-variable-changed-hook*
                     (make-instance 'nhooks:handler
                                    :fn #'context-expose-on-variable-changed
                                    :name 'context-expose-on-variable-changed))
    (setf *context-expose-hooks-installed* t)))

(defun set-context-variable (var-name new-value)
  "Set a project variable and trigger sync to context file if exposed."
  (ecase var-name
    (:name (setf *name* new-value))
    (:author (setf *author* new-value))
    (:language (setf *language* new-value))
    (:shell (setf *shell* new-value))
    (:stack (setf *stack* new-value)))
  (nhooks:run-hook *context-variable-changed-hook* var-name new-value))

(define-command "context.expose" (args)
  "Expose the current context to hactar.{pid}.context.org and keep it synchronized:
   - Adds/removes src blocks under the 'Files' headline when files are added/dropped.
   - Automatically updates block contents when files change on disk.
   - Syncs project variables to 'Project Details' section."
  (declare (ignore args))
  (let ((path (context-expose-file-path)))
    (setf *exposed-context-file* path)
    (context-expose-install-hooks-if-needed)
    (context-expose-upsert-project-details)
    (context-expose-upsert-docs-section)
    (context-expose-upsert-errors-section)
    (context-expose-upsert-files-section)
    (format t "Context exposed to: ~A~%" (uiop:native-namestring path))))
