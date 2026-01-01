;;;; to-org.lisp — Transform a codebase into a literate .hactar.org file
;;;;
;;;; Collects project files (via git), organizes them into a structured
;;;; org-mode file compatible with Hactar's litmode.

(in-package :hactar)

;;* Configuration

(defvar *to-org-default-ignore-patterns*
  '("*.min.js" "*.min.css" "*.map"
    "*.lock" "package-lock.json" "yarn.lock" "pnpm-lock.yaml" "Cargo.lock"
    "*.wasm" "*.pyc" "*.pyo" "*.o" "*.so" "*.dylib" "*.a" "*.obj" "*.exe"
    "*.png" "*.jpg" "*.jpeg" "*.gif" "*.ico" "*.svg" "*.webp" "*.bmp" "*.tiff"
    "*.ttf" "*.woff" "*.woff2" "*.eot" "*.otf"
    "*.pdf" "*.zip" "*.tar" "*.gz" "*.bz2" "*.xz" "*.7z" "*.rar"
    "*.sqlite" "*.db" "*.sqlite3"
    ".git/*" "node_modules/*" "dist/*" "build/*" ".next/*" ".nuxt/*"
    "__pycache__/*" ".tox/*" ".venv/*" "venv/*" ".mypy_cache/*"
    "vendor/*" "straight/*" ".bundle/*"
    ".hactar.org" ".hactar.litmode.db" ".hactar.org.lock"
    ".hactar.*.log" ".hactar.*.context.org"
    ".tags" "TAGS" "tags" ".DS_Store" "Thumbs.db")
  "Default list of glob patterns to ignore when collecting files for to-org.")

(defvar *to-org-extra-ignore-patterns* '()
  "User-extensible list of additional glob patterns to ignore in to-org.")

(defvar *to-org-max-tokens* 10000
  "Maximum estimated token count for a single file. Files exceeding this are skipped.")

;;* File Collection

(defun to-org-ignore-patterns ()
  "Return the combined list of ignore patterns."
  (append *to-org-default-ignore-patterns* *to-org-extra-ignore-patterns*))

(defun to-org-file-ignored-p (relative-path patterns)
  "Check if RELATIVE-PATH matches any of the ignore PATTERNS.
   Matches against the full relative path and also just the filename.
   Patterns ending in /* are treated as matching anything under that directory."
  (let ((basename (file-namestring (pathname relative-path))))
    (dolist (pattern patterns)
      (when (or
             ;; Direct glob match against full path
             (glob-matches pattern relative-path)
             ;; Match pattern against just the filename (for patterns like *.min.js)
             (and (not (position #\/ pattern))
                  (glob-matches pattern basename))
             ;; For dir/* patterns, also match dir/**/anything (recursive)
             (and (str:ends-with-p "/*" pattern)
                  (let ((dir-prefix (subseq pattern 0 (- (length pattern) 1))))
                    (str:starts-with-p dir-prefix relative-path))))
        (return-from to-org-file-ignored-p t))))
  nil)

(defun to-org-file-binary-p (filepath)
  "Heuristic check if a file is binary by reading the first 512 bytes."
  (handler-case
      (with-open-file (s filepath :element-type '(unsigned-byte 8)
                                   :if-does-not-exist nil)
        (when s
          (let ((buf (make-array (min 512 (file-length s))
                                 :element-type '(unsigned-byte 8))))
            (read-sequence buf s)
            ;; If more than 10% null bytes, consider binary
            (let ((null-count (count 0 buf)))
              (> null-count (max 1 (floor (length buf) 10)))))))
    (error () t)))

(defun collect-project-files (&key (max-tokens *to-org-max-tokens*)
                                    (extra-ignore-patterns nil)
                                    (include-patterns nil))
  "Collect project files suitable for to-org conversion.
   Returns a list of plists: (:path :relative-path :content :tokens :language :extension).
   Files are sorted by relative path."
  (unless *repo-root*
    (error "No repository root set. Run from within a git repository."))
  (let* ((tracked-files (list-git-tracked-files *repo-root*))
         (ignore-pats (append (to-org-ignore-patterns) extra-ignore-patterns))
         (results '())
         (skipped-count 0)
         (binary-count 0)
         (too-large-count 0))
    (unless tracked-files
      (error "No git-tracked files found in ~A" *repo-root*))
    (dolist (rel-path tracked-files)
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) rel-path)))
        (when (and (> (length trimmed) 0)
                   ;; Include filter: if specified, file must match at least one
                   (or (null include-patterns)
                       (some (lambda (pat)
                               (or (glob-matches pat trimmed)
                                   ;; Also match against just the filename for simple patterns
                                   (and (not (position #\/ pat))
                                        (glob-matches pat (file-namestring (pathname trimmed))))))
                             include-patterns))
                   ;; Ignore filter
                   (not (to-org-file-ignored-p trimmed ignore-pats)))
          (let ((full-path (merge-pathnames trimmed *repo-root*)))
            (cond
              ;; Skip directories
              ((uiop:directory-exists-p full-path)
               (incf skipped-count))
              ;; Skip binary files
              ((to-org-file-binary-p full-path)
               (incf binary-count))
              (t
               (handler-case
                   (let* ((content (uiop:read-file-string full-path :external-format :utf-8))
                          (tokens (hactar::estimate-tokens-str content))
                          (ext (pathname-type full-path))
                          (lang (get-language-hint-from-extension ext)))
                     (if (and max-tokens (> tokens max-tokens))
                         (incf too-large-count)
                         (push (list :path (namestring full-path)
                                     :relative-path trimmed
                                     :content content
                                     :tokens tokens
                                     :language (or lang ext "text")
                                     :extension ext)
                               results)))
                 (error ()
                   (incf skipped-count)))))))))
    (unless *silent*
      (when (> binary-count 0)
        (format t "  Skipped ~A binary file~:P~%" binary-count))
      (when (> too-large-count 0)
        (format t "  Skipped ~A file~:P exceeding ~A token limit~%" too-large-count max-tokens))
      (when (> skipped-count 0)
        (format t "  Skipped ~A other file~:P~%" skipped-count)))
    ;; Sort by relative path
    (sort (nreverse results) #'string< :key (lambda (f) (getf f :relative-path)))))

;;* File Grouping

(defun to-org-group-name (dir-name)
  "Map a directory name to a human-readable group name."
  (cond
    ((null dir-name) "Root")
    ((string= dir-name "") "Root")
    ((member dir-name '("src" "source" "sources") :test #'string-equal) "Source")
    ((member dir-name '("lib" "libs" "library" "libraries") :test #'string-equal) "Libraries")
    ((member dir-name '("test" "tests" "spec" "specs") :test #'string-equal) "Tests")
    ((member dir-name '("doc" "docs" "documentation") :test #'string-equal) "Documentation")
    ((member dir-name '("bin" "scripts" "script") :test #'string-equal) "Scripts")
    ((member dir-name '("config" "conf" "cfg" "settings") :test #'string-equal) "Configuration")
    ((member dir-name '("cmd" "commands") :test #'string-equal) "Commands")
    ((member dir-name '("pkg" "packages") :test #'string-equal) "Packages")
    ((member dir-name '("internal") :test #'string-equal) "Internal")
    ((member dir-name '("api") :test #'string-equal) "API")
    ((member dir-name '("web" "www" "public" "static" "assets") :test #'string-equal) "Web Assets")
    ((member dir-name '("migrations" "db") :test #'string-equal) "Database")
    ((member dir-name '("prompts") :test #'string-equal) "Prompts")
    ((member dir-name '("data") :test #'string-equal) "Data")
    (t (string-capitalize dir-name))))

(defun to-org-top-directory (relative-path)
  "Extract the top-level directory from a relative path, or nil for root files."
  (let ((slash-pos (position #\/ relative-path)))
    (if slash-pos
        (subseq relative-path 0 slash-pos)
        nil)))

(defun group-files-by-directory (files)
  "Group a list of file plists by their top-level directory.
   Returns an alist of (group-name . files-list), sorted by group name
   with 'Root' first."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (file files)
      (let* ((rel-path (getf file :relative-path))
             (top-dir (to-org-top-directory rel-path))
             (group-name (to-org-group-name top-dir)))
        (push file (gethash group-name groups))))
    ;; Convert to sorted alist
    (let ((result '()))
      (maphash (lambda (name files)
                 (push (cons name (nreverse files)) result))
               groups)
      ;; Sort: Root first, then alphabetical
      (sort result (lambda (a b)
                     (cond
                       ((string= a "Root") t)
                       ((string= b "Root") nil)
                       (t (string< a b))))
            :key #'car))))

;;* ID Sanitization

(defun sanitize-id (path)
  "Convert a file path to a valid org-mode ID.
   Replaces non-alphanumeric characters with hyphens, collapses runs."
  (let* ((cleaned (cl-ppcre:regex-replace-all "[^a-zA-Z0-9]" path "-"))
         (collapsed (cl-ppcre:regex-replace-all "-+" cleaned "-"))
         (trimmed (string-trim "-" collapsed)))
    (format nil "code-~A" (string-downcase trimmed))))

;;* Org Generation (Default — No LLM)

(defun generate-org-header (&key title)
  "Generate the org file preamble."
  (with-output-to-string (s)
    (format s "#+TITLE: ~A~%" (or title *name* "Project"))
    (when *current-model*
      (format s "#+HACTAR_MODEL: ~A~%" (model-config-name *current-model*)))
    (when (and *stack* (> (length *stack*) 0))
      (format s "#+HACTAR_STACK: ~{~A~^, ~}~%" *stack*))
    (when (and *language* (not (string= *language* "unknown")))
      (format s "#+HACTAR_LANGUAGE: ~A~%" *language*))
    (when (and *author* (not (string= *author* "")))
      (format s "#+HACTAR_AUTHOR: ~A~%" *author*))))

(defun generate-meta-zone (&key scope-name)
  "Generate the Meta zone."
  (with-output-to-string (s)
    (format s "* Meta                                                              :meta:noctx:~%")

    ;; Headline Index
    (format s "** Headline Index~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: headline-index~%")
    (format s ":HACTAR_AUTO_GENERATED: t~%")
    (format s ":END:~%~%")

    ;; Agent Locks
    (format s "** Agent Locks~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: agent-locks~%")
    (format s ":END:~%~%")

    ;; Scope Profiles
    (format s "** Scope Profiles~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: scope-profiles~%")
    (format s ":END:~%")
    (when scope-name
      (format s "~%*** ~A~%" scope-name)
      (format s ":PROPERTIES:~%")
      (format s ":ID: scope-~A~%" (string-downcase (cl-ppcre:regex-replace-all "[^a-zA-Z0-9]" scope-name "-")))
      (format s ":HACTAR_SCOPE_INCLUDE_TAGS: ctx,code~%")
      (format s ":HACTAR_SCOPE_EXCLUDE_TAGS: noctx~%")
      (format s ":HACTAR_SCOPE_MAX_TOKENS: 50000~%")
      (format s ":END:~%"))
    (format s "~%")

    ;; Tooling
    (format s "** Tooling~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: tooling~%")
    (format s ":END:~%")))

(defun generate-context-zone (files file-groups)
  "Generate the Context zone with project details and files listing."
  (with-output-to-string (s)
    (format s "* Context                                                           :ctx:~%")

    ;; Project Details
    (format s "** Project Details~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: project-details~%")
    (format s ":END:~%")
    (format s "Project: ~A~%" (or *name* "Unknown"))
    (when (and *language* (not (string= *language* "unknown")))
      (format s "Language: ~A~%" *language*))
    (when (and *stack* (> (length *stack*) 0))
      (format s "Stack: ~{~A~^, ~}~%" *stack*))
    (format s "Total files: ~A~%" (length files))
    (format s "Total estimated tokens: ~A~%"
            (reduce #'+ files :key (lambda (f) (getf f :tokens))))
    (format s "~%File structure:~%")
    (dolist (group file-groups)
      (format s "- ~A (~A file~:P)~%" (car group) (length (cdr group))))
    (format s "~%")

    ;; Architecture (placeholder)
    (format s "** Architecture~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: arch-overview~%")
    (format s ":END:~%")
    (format s "Architecture overview not yet generated. Use =/to-org --smart= to auto-generate.~%~%")

    ;; Files section
    (format s "** Files                                                            :files:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: ctx-files~%")
    (format s ":END:~%~%")

    ;; Documentation
    (format s "** Documentation                                                    :docs:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: ctx-docs~%")
    (format s ":END:~%~%")

    ;; Errors
    (format s "** Errors                                                           :errors:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: ctx-errors~%")
    (format s ":END:~%")))

(defun generate-code-zone (file-groups)
  "Generate the Code zone with tangle blocks organized by directory group."
  (with-output-to-string (s)
    (format s "* Code                                                              :code:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: code-zone~%")
    (format s ":END:~%")

    (dolist (group file-groups)
      (let ((group-name (car group))
            (group-files (cdr group)))
        (format s "~%** ~A~%" group-name)
        (dolist (file group-files)
          (let* ((rel-path (getf file :relative-path))
                 (content (getf file :content))
                 (lang (getf file :language))
                 (tokens (getf file :tokens))
                 (file-id (sanitize-id rel-path)))
            (format s "*** ~A~%" rel-path)
            (format s ":PROPERTIES:~%")
            (format s ":ID: ~A~%" file-id)
            (format s ":HACTAR_TANGLE: ~A~%" rel-path)
            (when lang
              (format s ":HACTAR_LANGUAGE: ~A~%" lang))
            (format s ":HACTAR_TOKENS: ~A~%" tokens)
            (format s ":END:~%")
            (format s "#+begin_src ~A :tangle ~A~%" (or lang "text") rel-path)
            (write-string (org-mode:escape-for-org-s content) s)
            (unless (and (> (length content) 0)
                         (char= (char content (1- (length content))) #\Newline))
              (terpri s))
            (format s "#+end_src~%")))))))

(defun generate-tasks-zone ()
  "Generate the Tasks zone."
  (with-output-to-string (s)
    (format s "* Tasks                                                             :tasks:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: tasks-zone~%")
    (format s ":END:~%")))

(defun generate-scratch-zone ()
  "Generate the Scratch zone."
  (with-output-to-string (s)
    (format s "* Scratch                                                           :scratch:noctx:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: scratch-zone~%")
    (format s ":END:~%")))

;;* Smart Mode (LLM-enhanced)

(defun to-org-smart-architecture (files)
  "Use the cheap model to generate an architecture overview from file paths and snippets."
  (let* ((file-overview
           (with-output-to-string (s)
             (format s "Project: ~A~%" (or *name* "Unknown"))
             (when *stack*
               (format s "Stack: ~{~A~^, ~}~%" *stack*))
             (when *language*
               (format s "Language: ~A~%~%" *language*))
             (format s "Files in project:~%")
             (dolist (f files)
               (let* ((rel (getf f :relative-path))
                      (content (getf f :content))
                      (preview-lines (subseq content 0
                                             (min (length content)
                                                  (or (position #\Newline content
                                                                :start (min 500 (length content)))
                                                      (min 500 (length content)))))))
                 (format s "~%--- ~A ---~%~A~%"
                         rel
                         (subseq preview-lines 0 (min 500 (length preview-lines))))))))
         (prompt "You are analyzing a software project. Based on the file structure and code snippets below, write a concise architecture overview in plain text (no markdown, no org-mode markup). Cover: main components, how they relate, data flow, and key design decisions. Keep it under 500 words.

Project information and files:
"))
    (handler-case
        (let ((response (get-llm-response
                         (concatenate 'string prompt file-overview)
                         :model *cheap-model*
                         :stream nil
                         :add-to-history nil)))
          (or response "Architecture overview generation failed."))
      (error (e)
        (format nil "Error generating architecture overview: ~A" e)))))

(defun to-org-smart-summary (file-plist)
  "Use the cheap model to generate a one-line summary for a file."
  (let* ((rel-path (getf file-plist :relative-path))
         (content (getf file-plist :content))
         ;; Truncate content to avoid overwhelming the model
         (truncated (subseq content 0 (min (length content) 3000)))
         (prompt (format nil "Summarize the purpose of the file '~A' in ONE sentence (max 80 characters). Output ONLY the summary, nothing else.

File content:
~A" rel-path truncated)))
    (handler-case
        (let ((response (get-llm-response prompt
                                          :model *cheap-model*
                                          :stream nil
                                          :add-to-history nil)))
          (when (and response (> (length response) 0))
            (string-trim '(#\Space #\Tab #\Newline #\Return #\") response)))
      (error () nil))))

(defun generate-code-zone-smart (file-groups)
  "Generate Code zone with LLM summaries for each file."
  (with-output-to-string (s)
    (format s "* Code                                                              :code:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: code-zone~%")
    (format s ":END:~%")

    (dolist (group file-groups)
      (let ((group-name (car group))
            (group-files (cdr group)))
        (format s "~%** ~A~%" group-name)
        (dolist (file group-files)
          (let* ((rel-path (getf file :relative-path))
                 (content (getf file :content))
                 (lang (getf file :language))
                 (tokens (getf file :tokens))
                 (file-id (sanitize-id rel-path))
                 (summary (progn
                            (unless *silent*
                              (format t "  Summarizing ~A...~%" rel-path))
                            (to-org-smart-summary file))))
            (format s "*** ~A~%" rel-path)
            (format s ":PROPERTIES:~%")
            (format s ":ID: ~A~%" file-id)
            (format s ":HACTAR_TANGLE: ~A~%" rel-path)
            (when lang
              (format s ":HACTAR_LANGUAGE: ~A~%" lang))
            (format s ":HACTAR_TOKENS: ~A~%" tokens)
            (when (and summary (not (string= summary "")))
              (format s ":HACTAR_SUMMARY: ~A~%" summary))
            (format s ":END:~%")
            (format s "#+begin_src ~A :tangle ~A~%" (or lang "text") rel-path)
            (write-string (org-mode:escape-for-org-s content) s)
            (unless (and (> (length content) 0)
                         (char= (char content (1- (length content))) #\Newline))
              (terpri s))
            (format s "#+end_src~%")))))))

(defun generate-context-zone-smart (files file-groups architecture-text)
  "Generate Context zone with LLM-enhanced architecture section."
  (with-output-to-string (s)
    (format s "* Context                                                           :ctx:~%")

    ;; Project Details
    (format s "** Project Details~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: project-details~%")
    (format s ":END:~%")
    (format s "Project: ~A~%" (or *name* "Unknown"))
    (when (and *language* (not (string= *language* "unknown")))
      (format s "Language: ~A~%" *language*))
    (when (and *stack* (> (length *stack*) 0))
      (format s "Stack: ~{~A~^, ~}~%" *stack*))
    (format s "Total files: ~A~%" (length files))
    (format s "Total estimated tokens: ~A~%"
            (reduce #'+ files :key (lambda (f) (getf f :tokens))))
    (format s "~%File structure:~%")
    (dolist (group file-groups)
      (format s "- ~A (~A file~:P)~%" (car group) (length (cdr group))))
    (format s "~%")

    ;; Architecture (LLM-generated)
    (format s "** Architecture~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: arch-overview~%")
    (format s ":END:~%")
    (format s "~A~%~%" (or architecture-text ""))

    ;; Files section
    (format s "** Files                                                            :files:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: ctx-files~%")
    (format s ":END:~%~%")

    ;; Documentation
    (format s "** Documentation                                                    :docs:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: ctx-docs~%")
    (format s ":END:~%~%")

    ;; Errors
    (format s "** Errors                                                           :errors:~%")
    (format s ":PROPERTIES:~%")
    (format s ":ID: ctx-errors~%")
    (format s ":END:~%")))

;;* Main Orchestrator

(defun to-org (&key (output nil)
                    (smart nil)
                    (max-tokens *to-org-max-tokens*)
                    (extra-ignore nil)
                    (include nil)
                    (dry-run nil)
                    (no-code nil)
                    (scope nil)
                    (force nil))
  "Transform the current codebase into a literate .hactar.org file.
   OUTPUT: output file path (default: .hactar.org in repo root)
   SMART: use LLM for summaries and architecture
   MAX-TOKENS: skip files exceeding this token count
   EXTRA-IGNORE: additional glob patterns to ignore
   INCLUDE: only include files matching these patterns
   DRY-RUN: print file list without generating
   NO-CODE: omit the Code zone
   SCOPE: name for a default scope profile
   FORCE: overwrite existing file without prompting"
  (unless *repo-root*
    (format t "~&Error: No repository root found. Run from within a git repository.~%")
    (return-from to-org nil))

  (let* ((output-path (or (when output (merge-pathnames output *repo-root*))
                          (merge-pathnames ".hactar.org" *repo-root*)))
         (extra-pats (when extra-ignore
                       (if (listp extra-ignore)
                           extra-ignore
                           (mapcar (lambda (s) (string-trim '(#\Space) s))
                                   (str:split #\, extra-ignore)))))
         (include-pats (when include
                         (if (listp include)
                             include
                             (mapcar (lambda (s) (string-trim '(#\Space) s))
                                     (str:split #\, include))))))

    ;; Check for existing file
    (when (and (probe-file output-path) (not force) (not dry-run))
      (unless (confirm-action (format nil "~A already exists. Overwrite?" (uiop:native-namestring output-path)))
        (format t "~&Aborted.~%")
        (return-from to-org nil)))

    ;; Collect files
    (unless *silent*
      (format t "~&Collecting files from ~A...~%" (uiop:native-namestring *repo-root*)))
    (let ((files (collect-project-files :max-tokens max-tokens
                                        :extra-ignore-patterns extra-pats
                                        :include-patterns include-pats)))
      (unless *silent*
        (format t "  Found ~A file~:P (~A estimated tokens total)~%"
                (length files)
                (reduce #'+ files :key (lambda (f) (getf f :tokens)) :initial-value 0)))

      (when (null files)
        (format t "~&No files to process.~%")
        (return-from to-org nil))

      ;; Dry run: just print file list
      (when dry-run
        (format t "~&Files that would be included:~%")
        (dolist (f files)
          (format t "  ~A (~A tokens, ~A)~%"
                  (getf f :relative-path)
                  (getf f :tokens)
                  (getf f :language)))
        (return-from to-org files))

      ;; Group files
      (let ((file-groups (group-files-by-directory files)))

        ;; Generate org content
        (unless *silent*
          (format t "~&Generating org file...~%"))

        (let ((org-content
                (with-output-to-string (s)
                  ;; Header
                  (write-string (generate-org-header) s)
                  (terpri s)

                  ;; Meta zone
                  (write-string (generate-meta-zone :scope-name scope) s)
                  (terpri s)

                  ;; Context zone
                  (if smart
                      (progn
                        (unless *silent*
                          (format t "  Generating architecture overview with LLM...~%"))
                        (let ((arch-text (to-org-smart-architecture files)))
                          (write-string (generate-context-zone-smart files file-groups arch-text) s)))
                      (write-string (generate-context-zone files file-groups) s))
                  (terpri s)

                  ;; Tasks zone
                  (write-string (generate-tasks-zone) s)
                  (terpri s)

                  ;; Code zone
                  (unless no-code
                    (if smart
                        (progn
                          (unless *silent*
                            (format t "  Generating file summaries with LLM...~%"))
                          (write-string (generate-code-zone-smart file-groups) s))
                        (write-string (generate-code-zone file-groups) s))
                    (terpri s))

                  ;; Scratch zone
                  (write-string (generate-scratch-zone) s))))

          ;; Write output
          (with-open-file (out output-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
            (write-string org-content out))

          (unless *silent*
            (format t "~&✓ Generated ~A~%" (uiop:native-namestring output-path))
            (format t "  Files: ~A | Tokens: ~A | Groups: ~A~%"
                    (length files)
                    (reduce #'+ files :key (lambda (f) (getf f :tokens)) :initial-value 0)
                    (length file-groups))
            (format t "  Run /litmode-init to activate literate mode.~%"))

          output-path)))))

;;* Commands

(define-command to-org (args)
  "Transform the codebase into a literate .hactar.org file.
Usage: /to-org [options]
  --output PATH     Output file path (default: .hactar.org)
  --smart           Use LLM for summaries and architecture
  --max-tokens N    Skip files exceeding N tokens (default: 10000)
  --ignore PATS     Additional ignore patterns (comma-separated)
  --include PATS    Only include files matching patterns (comma-separated)
  --dry-run         Print file list without generating
  --no-code         Omit the Code zone
  --scope NAME      Generate a default scope profile
  --force           Overwrite existing file without prompting"
  (let ((output nil)
        (smart nil)
        (max-tokens *to-org-max-tokens*)
        (ignore nil)
        (include nil)
        (dry-run nil)
        (no-code nil)
        (scope nil)
        (force nil))
    ;; Parse args
    (loop with remaining = args
          while remaining
          do (let ((arg (pop remaining)))
               (cond
                 ((string= arg "--output")
                  (setf output (pop remaining)))
                 ((string= arg "--smart")
                  (setf smart t))
                 ((string= arg "--max-tokens")
                  (let ((val (pop remaining)))
                    (when val
                      (setf max-tokens (parse-integer val :junk-allowed t)))))
                 ((string= arg "--ignore")
                  (setf ignore (pop remaining)))
                 ((string= arg "--include")
                  (setf include (pop remaining)))
                 ((string= arg "--dry-run")
                  (setf dry-run t))
                 ((string= arg "--no-code")
                  (setf no-code t))
                 ((string= arg "--scope")
                  (setf scope (pop remaining)))
                 ((string= arg "--force")
                  (setf force t)))))
    (to-org :output output
            :smart smart
            :max-tokens max-tokens
            :extra-ignore ignore
            :include include
            :dry-run dry-run
            :no-code no-code
            :scope scope
            :force force)))

(define-sub-command to-org (args)
  "Transform the codebase into a literate .hactar.org file.
Usage: hactar to-org [options]
  --output PATH     Output file path (default: .hactar.org)
  --smart           Use LLM for summaries and architecture
  --max-tokens N    Skip files exceeding N tokens (default: 10000)
  --ignore PATS     Additional ignore patterns (comma-separated)
  --include PATS    Only include files matching patterns (comma-separated)
  --dry-run         Print file list without generating
  --no-code         Omit the Code zone
  --scope NAME      Generate a default scope profile
  --force           Overwrite existing file without prompting"
  (let ((output nil)
        (smart nil)
        (max-tokens *to-org-max-tokens*)
        (ignore nil)
        (include nil)
        (dry-run nil)
        (no-code nil)
        (scope nil)
        (force nil))
    ;; Parse args
    (loop with remaining = (copy-list args)
          while remaining
          do (let ((arg (pop remaining)))
               (cond
                 ((string= arg "--output")
                  (setf output (pop remaining)))
                 ((string= arg "--smart")
                  (setf smart t))
                 ((string= arg "--max-tokens")
                  (let ((val (pop remaining)))
                    (when val
                      (setf max-tokens (parse-integer val :junk-allowed t)))))
                 ((string= arg "--ignore")
                  (setf ignore (pop remaining)))
                 ((string= arg "--include")
                  (setf include (pop remaining)))
                 ((string= arg "--dry-run")
                  (setf dry-run t))
                 ((string= arg "--no-code")
                  (setf no-code t))
                 ((string= arg "--scope")
                  (setf scope (pop remaining)))
                 ((string= arg "--force")
                  (setf force t)))))
    (to-org :output output
            :smart smart
            :max-tokens max-tokens
            :extra-ignore ignore
            :include include
            :dry-run dry-run
            :no-code no-code
            :scope scope
            :force force)))
