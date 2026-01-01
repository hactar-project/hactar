;;;; litmode.lisp — Single-File Literate Mode for Hactar

(in-package :hactar)

;;* state
(defvar *litmode-active* nil
  "T when literate single-file mode is active.")

(defvar *litmode-path* nil
  "Pathname to the .hactar.org file.")

(defvar *litmode-content* nil
  "Cached string content of the .hactar.org file.")

(defvar *litmode-parsed* nil
  "Cached parsed org-document of the .hactar.org file.")

(defvar *active-scope* nil
  "Name of the currently active scope profile (string or NIL).")

(defvar *expanded-ids* '()
  "List of headline IDs explicitly expanded into context.")

(defvar *focused-id* nil
  "If non-nil, a single headline ID to focus on (narrowing context).")

(defvar *headline-index-cache* nil
  "Cached headline index as a list of plists.")

(defvar *agent-id* nil
  "Unique ID for this Hactar instance (for locking).")

(defvar *lock-duration-seconds* 1800
  "Default lock duration in seconds (30 minutes).")

(defvar *litmode-db-path* nil
  "Path to the litmode SQLite database.")

(defvar *named-blocks* (make-hash-table :test 'equal)
  "Registry of named src blocks. Key: name, Value: plist with :content :language :eval :dir :tool-desc.")

;;* hooks
(nhooks:define-hook-type litmode-scope-changed (function (string string) t)
  "Hook run when active scope changes. Args: new-scope, old-scope.")
(defvar *scope-changed-hook* (make-instance 'hook-litmode-scope-changed))

(nhooks:define-hook-type litmode-tangle-completed (function (list) t)
  "Hook run after tangle completes. Args: list of written file paths.")
(defvar *tangle-completed-hook* (make-instance 'hook-litmode-tangle-completed))

(nhooks:define-hook-type litmode-task-transitioned (function (string string string) t)
  "Hook run when task state changes. Args: task-id, old-state, new-state.")
(defvar *task-transitioned-hook* (make-instance 'hook-litmode-task-transitioned))

(nhooks:define-hook-type litmode-drift-detected (function (list) t)
  "Hook run when code drift is detected. Args: list of drift plists.")
(defvar *drift-detected-hook* (make-instance 'hook-litmode-drift-detected))

(defun litmode-active-p ()
  "Return T if literate mode is currently active."
  *litmode-active*)

(defun get-litmode-path ()
  "Return the path to the .hactar.org file."
  *litmode-path*)

(defun estimate-tokens-str (text)
  "Rough token estimate: ~4 chars per token."
  (if (and text (> (length text) 0))
      (ceiling (length text) 4)
      0))

(defun current-timestamp ()
  "Return current universal time as integer."
  (get-universal-time))

(defun format-org-ts (universal-time)
  "Format a universal time as an org-mode timestamp."
  (multiple-value-bind (sec min hour day month year dow)
      (decode-universal-time universal-time)
    (let ((day-names #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
      (format nil "[~4,'0D-~2,'0D-~2,'0D ~A ~2,'0D:~2,'0D]"
              year month day (aref day-names dow) hour min))))

(defun generate-agent-id ()
  "Generate a unique agent ID for this Hactar instance."
  (format nil "hactar-agent-~A" (subseq (format nil "~A" (uuid:make-v4-uuid)) 0 8)))

(defun sxhash-string (s)
  "Compute a hex string checksum via sxhash."
  (format nil "~X" (sxhash s)))

;;* parsing helpers

(defun find-zone-heading (doc zone-tag)
  (org-mode-parser:find-zone-heading doc zone-tag))

(defun heading-has-tag-p (heading tag)
  (org-mode-parser:heading-has-tag-p heading tag))

(defun heading-id (heading)
  (org-mode-parser:heading-id heading))

(defun heading-property (heading prop-key)
  (org-mode-parser:heading-property heading prop-key))

(defun set-heading-property (heading prop-key value)
  (org-mode-parser:set-heading-property heading prop-key value))

(defun collect-all-headings (doc)
  (org-mode-parser:collect-all-headings doc))

(defun find-heading-by-custom-id (doc id)
  (org-mode-parser:find-heading-by-custom-id doc id))

(defun heading-content-string (heading)
  (org-mode-parser:heading-content-string heading))

(defun heading-all-tags (heading)
  (org-mode-parser:heading-all-tags heading))

(defun heading-depth (heading)
  (org-mode-parser:heading-depth heading))

(defun get-doc-setting (doc key)
  (org-mode-parser:get-doc-setting doc key))

(defun set-doc-setting (doc key value)
  (org-mode-parser:set-doc-setting doc key value))

(defun litmode-lock-path ()
  "Return the lock file path for the .hactar.org file."
  (when *litmode-path*
    (merge-pathnames ".hactar.org.lock"
                     (uiop:pathname-directory-pathname *litmode-path*))))

(defmacro with-org-file-lock ((&key (timeout 5)) &body body)
  "Execute BODY with an advisory lock on the .hactar.org file.
   Uses a simple lock file strategy."
  `(let ((lock-path (litmode-lock-path)))
     (when lock-path
       (let ((acquired nil)
             (start (get-universal-time)))
         (unwind-protect
              (progn
                ;; Try to acquire lock
                (loop
                  (handler-case
                      (progn
                        (with-open-file (s lock-path
                                          :direction :output
                                          :if-exists :error
                                          :if-does-not-exist :create)
                          (format s "~A ~A ~A"
                                  (or *agent-id* "unknown")
                                  #+sbcl (sb-posix:getpid)
                                  #-sbcl 0
                                  (get-universal-time)))
                        (setf acquired t)
                        (return))
                    (file-error ()
                      ;; Lock file exists, check timeout
                      (when (> (- (get-universal-time) start) ,timeout)
                        ;; Force acquire by deleting stale lock
                        (ignore-errors (delete-file lock-path))
                        ;; Try one more time
                        (handler-case
                            (progn
                              (with-open-file (s lock-path
                                                :direction :output
                                                :if-exists :error
                                                :if-does-not-exist :create)
                                (format s "~A ~A ~A"
                                        (or *agent-id* "unknown")
                                        #+sbcl (sb-posix:getpid)
                                        #-sbcl 0
                                        (get-universal-time)))
                              (setf acquired t)
                              (return))
                          (file-error ()
                            (error "Could not acquire lock on ~A within ~A seconds"
                                   *litmode-path* ,timeout))))
                      (sleep 0.1))))
                ;; Lock acquired, run body
                ,@body)
           ;; Release lock
           (when acquired
             (ignore-errors (delete-file lock-path))))))))

(defun load-litmode-file (&optional force)
  "Load and parse the .hactar.org file. Uses cache unless FORCE is T."
  (unless *litmode-path*
    (error "Literate mode not initialized. Run /litmode-init first."))
  (when (or force (null *litmode-content*))
    (let ((content (uiop:read-file-string *litmode-path*)))
      (setf *litmode-content* content)
      (setf *litmode-parsed* (org-mode-parser:parse-org-string content))
      ;; Extract scope from document settings
      (let ((scope (get-doc-setting *litmode-parsed* :HACTAR_SCOPE)))
        (when (and scope (not (string= scope "")))
          (setf *active-scope* scope)))))
  *litmode-parsed*)

(defun save-litmode-file (&optional doc)
  "Serialize and write the current parsed document back to the .hactar.org file."
  (let ((doc (or doc *litmode-parsed*)))
    (unless doc
      (error "No parsed document to save."))
    (with-org-file-lock (:timeout 5)
      (let ((content (string-right-trim '(#\Newline)
                                        (org-mode-parser:org-to-string doc))))
        (with-open-file (s *litmode-path*
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :external-format :utf-8)
          (write-string content s)
          (terpri s))
        (setf *litmode-content* content)
        (setf *litmode-parsed* doc)))))

;;* database

(defun init-litmode-db ()
  "Initialize the litmode SQLite database."
  (when *litmode-db-path*
    (ensure-directories-exist *litmode-db-path*)
    (sqlite:with-open-database (db *litmode-db-path*)
      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS headlines (
           id TEXT PRIMARY KEY,
           file_path TEXT NOT NULL,
           title TEXT NOT NULL,
           tags TEXT,
           depth INTEGER,
           tokens INTEGER,
           summary TEXT,
           checksum TEXT,
           parent_id TEXT,
           line_start INTEGER,
           line_end INTEGER,
           last_indexed INTEGER
         )")

      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS agent_locks (
           id INTEGER PRIMARY KEY AUTOINCREMENT,
           lock_owner TEXT NOT NULL,
           lock_pid INTEGER,
           headline_ids TEXT NOT NULL,
           scope_name TEXT,
           acquired_at INTEGER NOT NULL,
           expires_at INTEGER NOT NULL
         )")

      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS context_moves (
           id INTEGER PRIMARY KEY AUTOINCREMENT,
           agent_id TEXT,
           move_type TEXT NOT NULL,
           headline_ids TEXT NOT NULL,
           reason TEXT,
           timestamp INTEGER DEFAULT (strftime('%s', 'now')),
           task_id TEXT
         )")

      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS tangle_history (
           id INTEGER PRIMARY KEY AUTOINCREMENT,
           block_id TEXT,
           target_path TEXT NOT NULL,
           checksum TEXT,
           tangled_at INTEGER DEFAULT (strftime('%s', 'now'))
         )")

      (sqlite:execute-non-query db
        "CREATE INDEX IF NOT EXISTS idx_headlines_tags ON headlines(tags)")
      (sqlite:execute-non-query db
        "CREATE INDEX IF NOT EXISTS idx_headlines_parent ON headlines(parent_id)")
      (sqlite:execute-non-query db
        "CREATE INDEX IF NOT EXISTS idx_locks_owner ON agent_locks(lock_owner)")
      (sqlite:execute-non-query db
        "CREATE INDEX IF NOT EXISTS idx_locks_expires ON agent_locks(expires_at)"))))
;;* init
(defun init-litmode (&optional path)
  "Initialize literate single-file mode.
   PATH: Directory or explicit .hactar.org path. Defaults to *repo-root*."
  (let* ((base-dir (or path *repo-root* (uiop:getcwd)))
         (org-path (if (and (stringp (namestring base-dir))
                            (str:ends-with-p ".org" (namestring base-dir)))
                       (pathname base-dir)
                       (merge-pathnames ".hactar.org"
                                        (uiop:ensure-directory-pathname base-dir)))))

    (setf *litmode-path* org-path)
    (setf *litmode-db-path*
          (merge-pathnames ".hactar.litmode.db"
                           (uiop:pathname-directory-pathname org-path)))
    (setf *agent-id* (generate-agent-id))

    (unless (probe-file org-path)
      (create-default-litmode-file org-path))

    (init-litmode-db)
    (load-litmode-file t)
    (build-headline-index)
    (register-org-tools)
    (setf *litmode-active* t)

    (unless *silent*
      (format t "~&Literate mode initialized: ~A~%" (uiop:native-namestring org-path))
      (format t "  Agent ID: ~A~%" *agent-id*)
      (format t "  Database: ~A~%" (uiop:native-namestring *litmode-db-path*)))

    org-path))

(defun create-default-litmode-file (path)
  "Create a default .hactar.org file at PATH."
  (ensure-directories-exist path)
  (let ((project-name (or *name*
                          (car (last (pathname-directory
                                      (uiop:pathname-directory-pathname path))))
                          "my-project"))
        (model-name (if *current-model*
                        (model-config-name *current-model*)
                        "ollama/qwen3:14b"))
        (stack-str (if *stack*
                       (format nil "~{~A~^, ~}" *stack*)
                       ""))
        (lang *language*)
        (author (or *author* "")))
    (with-open-file (s path :direction :output
                       :if-does-not-exist :create
                       :external-format :utf-8)
      (format s "#+TITLE: ~A~%" project-name)
      (format s "#+HACTAR_MODEL: ~A~%" model-name)
      (when (and stack-str (not (string= stack-str "")))
        (format s "#+HACTAR_STACK: ~A~%" stack-str))
      (when (and lang (not (string= lang "unknown")))
        (format s "#+HACTAR_LANGUAGE: ~A~%" lang))
      (when (and author (not (string= author "")))
        (format s "#+HACTAR_AUTHOR: ~A~%" author))
      (format s "~%")
      (format s "* Meta                                                              :meta:noctx:~%")
      (format s "** Headline Index~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: headline-index~%")
      (format s ":HACTAR_AUTO_GENERATED: t~%")
      (format s ":END:~%~%")
      (format s "| ID | Title | Tags | Depth | Tokens | Summary |~%")
      (format s "|----+-------+------+-------+--------+---------|~%")
      (format s "~%")

      (format s "** Agent Locks~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: agent-locks~%")
      (format s ":END:~%~%")

      (format s "** Scope Profiles~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: scope-profiles~%")
      (format s ":END:~%~%")

      (format s "** Tooling~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: tooling~%")
      (format s ":END:~%~%")

      (format s "* Context                                                           :ctx:~%")
      (format s "** Project Details~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: project-details~%")
      (format s ":END:~%")
      (format s "Project: ~A~%" project-name)
      (when (and stack-str (not (string= stack-str "")))
        (format s "Stack: ~A~%" stack-str))
      (when (and lang (not (string= lang "unknown")))
        (format s "Language: ~A~%" lang))
      (format s "~%")

      (format s "** Architecture~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: arch-overview~%")
      (format s ":END:~%~%")

      (format s "** Files                                                            :files:~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: ctx-files~%")
      (format s ":END:~%~%")

      (format s "** Documentation                                                    :docs:~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: ctx-docs~%")
      (format s ":END:~%~%")

      (format s "** Errors                                                           :errors:~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: ctx-errors~%")
      (format s ":END:~%~%")

      (format s "* Tasks                                                             :tasks:~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: tasks-zone~%")
      (format s ":END:~%~%")

      (format s "* Code                                                              :code:~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: code-zone~%")
      (format s ":END:~%~%")

      (format s "* Scratch                                                           :scratch:noctx:~%")
      (format s ":PROPERTIES:~%")
      (format s ":ID: scratch-zone~%")
      (format s ":END:~%"))))

;;* indexing
(defun build-headline-index ()
  "Build the headline index from the parsed document. Updates DB and cache."
  (unless *litmode-parsed*
    (load-litmode-file t))
  (let ((headings (collect-all-headings *litmode-parsed*))
        (index '()))
    (dolist (h headings)
      (let* ((id (or (heading-id h) ""))
             (title (org-mode-parser:heading-title h))
             (tags (org-mode-parser:heading-tags h))
             (depth (org-mode-parser:heading-level h))
             (content-str (heading-content-string h))
             (tokens (estimate-tokens-str content-str))
             (checksum (sxhash-string content-str))
             (summary (or (heading-property h :HACTAR_SUMMARY) ""))
             (parent-node (org-mode-parser:node-parent h))
             (parent-id (when (typep parent-node 'org-mode-parser:org-heading)
                          (or (heading-id parent-node) ""))))
        (when (and id (not (string= id "")))
          (push (list :id id
                      :title title
                      :tags tags
                      :depth depth
                      :tokens tokens
                      :checksum checksum
                      :summary summary
                      :parent-id (or parent-id ""))
                index)
          ;; Store in DB
          (handler-case
              (sqlite:with-open-database (db *litmode-db-path*)
                (sqlite:execute-non-query db
                  "INSERT OR REPLACE INTO headlines
                   (id, file_path, title, tags, depth, tokens, summary, checksum, parent_id, last_indexed)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                  id (namestring *litmode-path*) title
                  (cl-json:encode-json-to-string (or tags '()))
                  depth tokens summary checksum (or parent-id "")
                  (current-timestamp)))
            (error (e)
              (:debug-log "Error indexing headline" id ":" e))))))
    (setf *headline-index-cache* (nreverse index))

    ;; Update the Headline Index table in the org file
    (update-index-headline-in-file)

    *headline-index-cache*))

(defun update-index-headline-in-file ()
  "Update the Headline Index table in the Meta zone of the org file."
  (when (and *litmode-parsed* *headline-index-cache*)
    (let ((index-heading (find-heading-by-custom-id *litmode-parsed* "headline-index")))
      (when index-heading
        (let ((drawer nil))
          (dolist (child (copy-list (org-mode-parser:node-children index-heading)))
            (if (typep child 'org-mode-parser:org-properties-drawer)
                (setf drawer child)
                (org-mode-parser:remove-node child)))
          (let ((table-content
                  (with-output-to-string (s)
                    (format s "| ID | Title | Tags | Depth | Tokens | Summary |~%")
                    (format s "|----+-------+------+-------+--------+---------|~%")
                    (dolist (entry *headline-index-cache*)
                      (let ((id (getf entry :id))
                            (title (getf entry :title))
                            (tags (getf entry :tags))
                            (depth (getf entry :depth))
                            (tokens (getf entry :tokens))
                            (summary (getf entry :summary)))
                        (format s "| ~A | ~A | ~{~A~^,~} | ~A | ~A | ~A |~%"
                                id
                                (subseq title 0 (min 30 (length title)))
                                (or tags '())
                                depth tokens
                                (subseq (or summary "") 0
                                        (min 40 (length (or summary ""))))))))))
            (set-heading-property index-heading :HACTAR_UPDATED
                                  (format-org-ts (current-timestamp)))
            (org-mode-parser:add-child
             index-heading
             (make-instance 'org-mode-parser:org-paragraph
                            :type :paragraph
                            :content table-content))))))))

(defun get-headline-index ()
  "Return the cached headline index."
  (or *headline-index-cache*
      (build-headline-index)))

(defun get-headline-by-id (id)
  "Get a headline index entry by ID."
  (find id (get-headline-index)
        :key (lambda (e) (getf e :id))
        :test #'string=))
;;* scopes
(defun parse-scope-profile (heading)
  "Parse a scope profile heading into a plist of scope rules."
  (let ((include-ids (heading-property heading :HACTAR_SCOPE_INCLUDE_IDS))
        (include-tags (heading-property heading :HACTAR_SCOPE_INCLUDE_TAGS))
        (exclude-tags (heading-property heading :HACTAR_SCOPE_EXCLUDE_TAGS))
        (max-depth (heading-property heading :HACTAR_SCOPE_MAX_DEPTH))
        (max-tokens (heading-property heading :HACTAR_SCOPE_MAX_TOKENS)))
    (list :name (org-mode-parser:heading-title heading)
          :id (heading-id heading)
          :include-ids (when include-ids
                         (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                 (str:split #\, include-ids)))
          :include-tags (when include-tags
                          (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                  (str:split #\, include-tags)))
          :exclude-tags (when exclude-tags
                          (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                  (str:split #\, exclude-tags)))
          :max-depth (when max-depth (parse-integer max-depth :junk-allowed t))
          :max-tokens (when max-tokens (parse-integer max-tokens :junk-allowed t)))))

(defun find-scope-profile (doc scope-name)
  "Find and parse a scope profile by name from the Meta zone."
  (let ((profiles-heading (find-heading-by-custom-id doc "scope-profiles")))
    (when profiles-heading
      (dolist (child (org-mode-parser:node-children profiles-heading))
        (when (and (typep child 'org-mode-parser:org-heading)
                   (string-equal (org-mode-parser:heading-title child) scope-name))
          (return (parse-scope-profile child)))))))

(defun list-scope-profiles ()
  "List all available scope profiles."
  (unless *litmode-parsed* (load-litmode-file))
  (let ((profiles-heading (find-heading-by-custom-id *litmode-parsed* "scope-profiles"))
        (profiles '()))
    (when profiles-heading
      (dolist (child (org-mode-parser:node-children profiles-heading))
        (when (typep child 'org-mode-parser:org-heading)
          (push (parse-scope-profile child) profiles))))
    (nreverse profiles)))

(defun resolve-scope (scope-name)
  "Resolve a scope profile to a list of heading IDs that should be in context.
   Returns a list of heading ID strings."
  (unless *litmode-parsed* (load-litmode-file))
  (let* ((profile (find-scope-profile *litmode-parsed* scope-name))
         (all-headings (collect-all-headings *litmode-parsed*))
         (included-ids '())
         (exclude-tags (getf profile :exclude-tags))
         (include-ids (getf profile :include-ids))
         (include-tags (getf profile :include-tags))
         (max-depth (getf profile :max-depth))
         (max-tokens (or (getf profile :max-tokens) 100000)))

    (dolist (h all-headings)
      (when (heading-has-tag-p h "pin")
        (let ((id (heading-id h)))
          (when id (pushnew id included-ids :test #'string=)))))

    (dolist (id include-ids)
      (when (find-heading-by-custom-id *litmode-parsed* id)
        (pushnew id included-ids :test #'string=)))

    (when include-tags
      (dolist (h all-headings)
        (let ((h-tags (mapcar #'string-downcase
                              (or (org-mode-parser:heading-tags h) '()))))
          (when (some (lambda (tag)
                        (member (string-downcase tag) h-tags :test #'string=))
                      include-tags)
            (let ((id (heading-id h)))
              (when id (pushnew id included-ids :test #'string=)))))))

    (dolist (id *expanded-ids*)
      (pushnew id included-ids :test #'string=))

    (when exclude-tags
      (setf included-ids
            (remove-if
             (lambda (id)
               (let ((h (find-heading-by-custom-id *litmode-parsed* id)))
                 (when h
                   (let ((h-tags (mapcar #'string-downcase
                                         (or (org-mode-parser:heading-tags h) '()))))
                     (some (lambda (tag)
                             (member (string-downcase tag) h-tags :test #'string=))
                           exclude-tags)))))
             included-ids)))

    (when max-depth
      (setf included-ids
            (remove-if
             (lambda (id)
               (let ((h (find-heading-by-custom-id *litmode-parsed* id)))
                 (when h
                   (> (org-mode-parser:heading-level h) max-depth))))
             included-ids)))

    (let ((total-tokens 0)
          (budgeted-ids '()))
      ;; Sort by priority (HACTAR_PRIORITY property, default 5; lower = higher priority)
      (let ((id-priorities
              (mapcar (lambda (id)
                        (let* ((h (find-heading-by-custom-id *litmode-parsed* id))
                               (priority-str (when h (heading-property h :HACTAR_PRIORITY)))
                               (priority (if priority-str
                                             (or (parse-integer priority-str :junk-allowed t) 5)
                                             5)))
                          (cons id priority)))
                      included-ids)))
        (setf id-priorities (sort id-priorities #'< :key #'cdr))
        (dolist (pair id-priorities)
          (let* ((id (car pair))
                 (entry (get-headline-by-id id))
                 (tokens (or (getf entry :tokens) 0)))
            (if (> (+ total-tokens tokens) max-tokens)
                ;; Over budget, try using summary instead
                (let ((summary (getf entry :summary)))
                  (when (and summary (not (string= summary "")))
                    (let ((summary-tokens (estimate-tokens-str summary)))
                      (when (<= (+ total-tokens summary-tokens) max-tokens)
                        (incf total-tokens summary-tokens)
                        (push id budgeted-ids)))))
                (progn
                  (incf total-tokens tokens)
                  (push id budgeted-ids)))))
        (nreverse budgeted-ids)))))

(defun set-active-scope (scope-name)
  "Set the active scope profile."
  (let ((old-scope *active-scope*))
    (setf *active-scope* scope-name)
    (setf *expanded-ids* '())
    (setf *focused-id* nil)
    (when *litmode-parsed*
      (set-doc-setting *litmode-parsed* :HACTAR_SCOPE (or scope-name ""))
      (save-litmode-file))
    (nhooks:run-hook *scope-changed-hook* (or scope-name "") (or old-scope ""))
    (fire-eval-trigger "on-scope-change")
    scope-name))

(defun get-active-scope ()
  "Return the currently active scope name."
  *active-scope*)

;;* context generation

(defun generate-litmode-context ()
  "Generate the context string from the .hactar.org file based on current scope.
   This replaces the normal context generation when litmode is active."
  (unless *litmode-parsed* (load-litmode-file))

  (let ((result-parts '()))

    (cond
      (*focused-id*
       (let ((focused-heading (find-heading-by-custom-id *litmode-parsed* *focused-id*)))
         (let ((index-heading (find-heading-by-custom-id *litmode-parsed* "headline-index")))
           (when index-heading
             (push (heading-content-string index-heading) result-parts)))
         (dolist (h (collect-all-headings *litmode-parsed*))
           (when (and (heading-has-tag-p h "pin")
                      (not (string= (or (heading-id h) "") "headline-index")))
             (push (heading-content-string h) result-parts)))
         (when focused-heading
           (push (heading-content-string focused-heading) result-parts))))

      (*active-scope*
       (let ((included-ids (resolve-scope *active-scope*)))
         (pushnew "headline-index" included-ids :test #'string=)
         (dolist (id included-ids)
           (let ((h (find-heading-by-custom-id *litmode-parsed* id)))
             (when h
               (let* ((entry (get-headline-by-id id))
                      (summary (getf entry :summary))
                      (tokens (or (getf entry :tokens) 0)))
                 (if (and summary (not (string= summary ""))
                          (> tokens 2000))
                     (push (format nil "~A ~A~%:PROPERTIES:~%:ID: ~A~%:END:~%~A~%"
                                   (make-string (org-mode-parser:heading-level h)
                                                :initial-element #\*)
                                   (org-mode-parser:heading-title h)
                                   id summary)
                           result-parts)
                     (push (heading-content-string h) result-parts))))))))

      (t
       (dolist (h (collect-all-headings *litmode-parsed*))
         (unless (or (heading-has-tag-p h "noctx")
                     ;; Check parent chain for noctx
                     (let ((parent (org-mode-parser:node-parent h)))
                       (loop while (typep parent 'org-mode-parser:org-heading)
                             thereis (heading-has-tag-p parent "noctx")
                             do (setf parent (org-mode-parser:node-parent parent)))))
           (push (heading-content-string h) result-parts)))))

    (format nil "~{~A~^~%~}" (nreverse result-parts))))

(defun expand-context (headline-ids &optional reason)
  "Expand context to include additional headline IDs."
  (dolist (id (if (listp headline-ids) headline-ids (list headline-ids)))
    (when (find-heading-by-custom-id *litmode-parsed* id)
      (pushnew id *expanded-ids* :test #'string=)))
  (when *litmode-db-path*
    (handler-case
        (sqlite:with-open-database (db *litmode-db-path*)
          (sqlite:execute-non-query db
            "INSERT INTO context_moves (agent_id, move_type, headline_ids, reason)
             VALUES (?, 'expand', ?, ?)"
            *agent-id*
            (cl-json:encode-json-to-string
             (if (listp headline-ids) headline-ids (list headline-ids)))
            (or reason "")))
      (error (e) (:debug-log "Error logging context move:" e))))
  *expanded-ids*)

(defun focus-context (headline-id &optional reason)
  "Focus context on a single headline (plus pins and index)."
  (when (find-heading-by-custom-id *litmode-parsed* headline-id)
    (setf *focused-id* headline-id)
    (when *litmode-db-path*
      (handler-case
          (sqlite:with-open-database (db *litmode-db-path*)
            (sqlite:execute-non-query db
              "INSERT INTO context_moves (agent_id, move_type, headline_ids, reason)
               VALUES (?, 'focus', ?, ?)"
              *agent-id*
              (cl-json:encode-json-to-string (list headline-id))
              (or reason "")))
        (error (e) (:debug-log "Error logging context move:" e))))
    *focused-id*))

;;* tangling
(defun extract-tangle-blocks (doc)
  "Extract all code blocks with :tangle targets from the Code zone.
   Returns a list of plists with :id :tangle-target :language :content :checksum :heading-id."
  (let ((code-zone (find-zone-heading doc "code"))
        (blocks '()))
    (when code-zone
      (org-mode-parser:walk-tree
       code-zone
       (lambda (node)
         (when (typep node 'org-mode-parser:org-src-block)
           ;; Check parent heading for :HACTAR_TANGLE: property
           (let* ((parent (org-mode-parser:node-parent node))
                  (tangle-target (when (typep parent 'org-mode-parser:org-heading)
                                   (heading-property parent :HACTAR_TANGLE)))
                  (h-id (when (typep parent 'org-mode-parser:org-heading)
                          (heading-id parent)))
                  (content (org-mode-parser:src-block-content node))
                  (language (org-mode-parser:src-block-language node)))
             (when tangle-target
               (push (list :id (or h-id (format nil "block-~A" (length blocks)))
                           :tangle-target tangle-target
                           :language language
                           :content content
                           :checksum (sxhash-string content)
                           :heading-id h-id)
                     blocks)))))))
    (nreverse blocks)))

(defun tangle-code-zone (&key dry-run)
  "Tangle all code blocks from the Code zone to their target files.
   Returns a list of written file paths."
  (unless *litmode-parsed* (load-litmode-file))
  (let ((blocks (extract-tangle-blocks *litmode-parsed*))
        (written-files '())
        (base-dir (uiop:pathname-directory-pathname *litmode-path*)))

    (dolist (block blocks)
      (let* ((target (getf block :tangle-target))
             (content (getf block :content))
             (checksum (getf block :checksum))
             (block-id (getf block :id))
             (full-path (if (uiop:absolute-pathname-p (pathname target))
                            (pathname target)
                            (merge-pathnames target base-dir))))
        (if dry-run
            (progn
              (format t "  Would write: ~A~%" full-path)
              (push (namestring full-path) written-files))
            (handler-case
                (progn
                  (ensure-directories-exist full-path)
                  (with-open-file (s full-path
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
                    (write-string content s))
                  (push (namestring full-path) written-files)

                  ;; Update checksum in org file
                  (let ((heading (find-heading-by-custom-id *litmode-parsed* block-id)))
                    (when heading
                      (set-heading-property heading :HACTAR_CHECKSUM checksum)))

                  ;; Record tangle history
                  (handler-case
                      (sqlite:with-open-database (db *litmode-db-path*)
                        (sqlite:execute-non-query db
                          "INSERT INTO tangle_history (block_id, target_path, checksum)
                           VALUES (?, ?, ?)"
                          block-id (namestring full-path) checksum))
                    (error (e) (:debug-log "Error recording tangle:" e)))

                  (unless *silent*
                    (format t "  ✓ ~A~%" (uiop:native-namestring full-path))))
              (error (e)
                (format t "  ✗ ~A: ~A~%" target e))))))

    ;; Save updated checksums
    (unless dry-run
      (save-litmode-file)
      ;; Fire tangle hook
      (nhooks:run-hook *tangle-completed-hook* written-files)
      ;; Fire eval triggers
      (fire-eval-trigger "on-tangle"))

    written-files))

(defun tangle-block (heading-id &key dry-run)
  "Tangle a single code block by its heading ID."
  (unless *litmode-parsed* (load-litmode-file))
  (let ((heading (find-heading-by-custom-id *litmode-parsed* heading-id)))
    (unless heading
      (error "Heading not found: ~A" heading-id))
    (let ((tangle-target (heading-property heading :HACTAR_TANGLE)))
      (unless tangle-target
        (error "Heading ~A has no :HACTAR_TANGLE: property" heading-id))
      ;; Find src block child
      (let ((src-block (find-if (lambda (c) (typep c 'org-mode-parser:org-src-block))
                                (org-mode-parser:node-children heading))))
        (unless src-block
          (error "Heading ~A has no source code block" heading-id))
        (let* ((content (org-mode-parser:src-block-content src-block))
               (base-dir (uiop:pathname-directory-pathname *litmode-path*))
               (full-path (if (uiop:absolute-pathname-p (pathname tangle-target))
                              (pathname tangle-target)
                              (merge-pathnames tangle-target base-dir))))
          (if dry-run
              (format t "Would write: ~A~%" full-path)
              (progn
                (ensure-directories-exist full-path)
                (with-open-file (s full-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
                  (write-string content s))
                (set-heading-property heading :HACTAR_CHECKSUM (sxhash-string content))
                (save-litmode-file)
                (unless *silent*
                  (format t "✓ ~A~%" (uiop:native-namestring full-path)))))
          (namestring full-path))))))

;;* untangling

(defun untangle-file (file-path)
  "Pull changes from a tangled file back into the .hactar.org source.
   FILE-PATH is relative to the project root or absolute."
  (unless *litmode-parsed* (load-litmode-file))
  (let* ((base-dir (uiop:pathname-directory-pathname *litmode-path*))
         (full-path (if (uiop:absolute-pathname-p (pathname file-path))
                        (pathname file-path)
                        (merge-pathnames file-path base-dir)))
         (target-str (namestring (uiop:enough-pathname full-path base-dir))))

    (unless (probe-file full-path)
      (error "File not found: ~A" full-path))

    (let ((target-heading nil))
      (dolist (h (collect-all-headings *litmode-parsed*))
        (let ((tangle (heading-property h :HACTAR_TANGLE)))
          (when (and tangle (string= tangle target-str))
            (setf target-heading h)
            (return))))

      (unless target-heading
        (dolist (h (collect-all-headings *litmode-parsed*))
          (let ((tangle (heading-property h :HACTAR_TANGLE)))
            (when (and tangle (string= tangle (if (stringp file-path)
                                                   file-path
                                                   (namestring file-path))))
              (setf target-heading h)
              (return)))))

      (unless target-heading
        (error "No heading found with :HACTAR_TANGLE: ~A" file-path))

      (let ((new-content (uiop:read-file-string full-path :external-format :utf-8))
            (org-checksum (heading-property target-heading :HACTAR_CHECKSUM))
            (file-checksum (sxhash-string (uiop:read-file-string full-path))))

        (when (and org-checksum (string= org-checksum file-checksum))
          (unless *silent*
            (format t "No changes detected in ~A~%" file-path))
          (return-from untangle-file nil))

        (let ((src-block (find-if (lambda (c) (typep c 'org-mode-parser:org-src-block))
                                  (org-mode-parser:node-children target-heading))))
          (if src-block
              (setf (org-mode-parser:src-block-content src-block) new-content)
              (let ((lang (or (heading-property target-heading :HACTAR_LANGUAGE) "")))
                (org-mode-parser:add-child
                 target-heading
                 (make-instance 'org-mode-parser:org-src-block
                                :type :src-block
                                :language lang
                                :content new-content)))))

        (set-heading-property target-heading :HACTAR_CHECKSUM
                              (sxhash-string new-content))

        (save-litmode-file)
        (unless *silent*
          (format t "Untangled ~A → ~A~%"
                  file-path (or (heading-id target-heading) "unknown")))
        t))))

;;* drift detection
(defun detect-litmode-drift ()
  "Detect drift between org source blocks and tangled files.
   Returns a list of drift plists."
  (unless *litmode-parsed* (load-litmode-file))
  (let ((blocks (extract-tangle-blocks *litmode-parsed*))
        (drifts '())
        (base-dir (uiop:pathname-directory-pathname *litmode-path*)))

    (dolist (block blocks)
      (let* ((target (getf block :tangle-target))
             (org-checksum (getf block :checksum))
             (full-path (if (uiop:absolute-pathname-p (pathname target))
                            (pathname target)
                            (merge-pathnames target base-dir))))

        (cond
          ((not (probe-file full-path))
           (push (list :file (namestring full-path)
                       :heading-id (getf block :heading-id)
                       :drift-type :not-tangled
                       :org-checksum org-checksum
                       :file-checksum nil)
                 drifts))

          (t
           (let* ((file-content (uiop:read-file-string full-path))
                  (file-checksum (sxhash-string file-content)))
             (unless (string= org-checksum file-checksum)
               (push (list :file (namestring full-path)
                           :heading-id (getf block :heading-id)
                           :drift-type :modified
                           :org-checksum org-checksum
                           :file-checksum file-checksum)
                     drifts)))))))

    (when drifts
      (nhooks:run-hook *drift-detected-hook* drifts))
    (nreverse drifts)))

;;* locks

(defun acquire-lock (headline-ids &key scope-name)
  "Acquire locks on the given headline IDs for this agent.
   Returns T on success, NIL on conflict."
  (let ((ids (if (listp headline-ids) headline-ids (list headline-ids)))
        (now (current-timestamp)))

    (sqlite:with-open-database (db *litmode-db-path*)
      (dolist (id ids)
        (let ((conflicts (sqlite:execute-to-list db
                           "SELECT lock_owner, headline_ids, expires_at FROM agent_locks
                            WHERE headline_ids LIKE ? AND expires_at > ?"
                           (format nil "%~A%" id) now)))
          (dolist (conflict conflicts)
            (let ((owner (first conflict)))
              (unless (string= owner *agent-id*)
                (unless *silent*
                  (format t "~&Lock conflict: ~A is locked by ~A~%" id owner))
                (return-from acquire-lock nil))))))

      (sqlite:execute-non-query db
        "INSERT INTO agent_locks (lock_owner, lock_pid, headline_ids, scope_name, acquired_at, expires_at)
         VALUES (?, ?, ?, ?, ?, ?)"
        *agent-id*
        #+sbcl (sb-posix:getpid) #-sbcl 0
        (cl-json:encode-json-to-string ids)
        (or scope-name "")
        now
        (+ now *lock-duration-seconds*))

      (update-locks-in-org)
      t)))

(defun release-lock (headline-ids)
  "Release locks on the given headline IDs for this agent."
  (let ((ids (if (listp headline-ids) headline-ids (list headline-ids))))
    (sqlite:with-open-database (db *litmode-db-path*)
      (dolist (id ids)
        (sqlite:execute-non-query db
          "DELETE FROM agent_locks WHERE lock_owner = ? AND headline_ids LIKE ?"
          *agent-id* (format nil "%~A%" id))))
    (update-locks-in-org)
    t))

(defun heartbeat-locks ()
  "Extend the expiry of all locks owned by this agent."
  (let ((now (current-timestamp)))
    (sqlite:with-open-database (db *litmode-db-path*)
      (sqlite:execute-non-query db
        "UPDATE agent_locks SET expires_at = ? WHERE lock_owner = ?"
        (+ now *lock-duration-seconds*) *agent-id*))
    (update-locks-in-org)))

(defun get-active-locks ()
  "Get all non-expired locks."
  (let ((now (current-timestamp)))
    (sqlite:with-open-database (db *litmode-db-path*)
      (sqlite:execute-to-list db
        "SELECT lock_owner, headline_ids, scope_name, acquired_at, expires_at
         FROM agent_locks WHERE expires_at > ?"
        now))))

(defun cleanup-expired-locks ()
  "Remove expired locks from the database."
  (let ((now (current-timestamp)))
    (sqlite:with-open-database (db *litmode-db-path*)
      (sqlite:execute-non-query db
        "DELETE FROM agent_locks WHERE expires_at <= ?" now))
    (update-locks-in-org)))

(defun update-locks-in-org ()
  "Update the Agent Locks section in the org file to mirror DB state."
  (when *litmode-parsed*
    (let ((locks-heading (find-heading-by-custom-id *litmode-parsed* "agent-locks")))
      (when locks-heading
        (dolist (child (copy-list (org-mode-parser:node-children locks-heading)))
          (unless (typep child 'org-mode-parser:org-properties-drawer)
            (org-mode-parser:remove-node child)))

        (let ((active-locks (get-active-locks)))
          (dolist (lock active-locks)
            (let* ((owner (first lock))
                   (ids-json (second lock))
                   (scope (third lock))
                   (acquired (fourth lock))
                   (expires (fifth lock))
                   (lock-heading (make-instance 'org-mode-parser:org-heading
                                                :type :heading
                                                :level 3
                                                :title (format nil "Lock: ~A" owner)))
                   (drawer (make-instance 'org-mode-parser:org-properties-drawer
                                          :type :properties-drawer
                                          :properties
                                          `((:HACTAR_LOCK_OWNER . ,owner)
                                            (:HACTAR_LOCK_HEADLINES . ,ids-json)
                                            (:HACTAR_LOCK_SCOPE . ,(or scope ""))
                                            (:HACTAR_LOCK_ACQUIRED . ,(format-org-ts acquired))
                                            (:HACTAR_LOCK_EXPIRES . ,(format-org-ts expires))))))
              (org-mode-parser:add-child lock-heading drawer)
              (org-mode-parser:add-child locks-heading lock-heading))))

        (save-litmode-file)))))

;;* task management

(defun get-tasks (&key state)
  "Get tasks from the Tasks zone. Optionally filter by STATE (TODO, DOING, DONE, BLOCKED)."
  (unless *litmode-parsed* (load-litmode-file))
  (let ((tasks-zone (find-zone-heading *litmode-parsed* "tasks"))
        (tasks '()))
    (when tasks-zone
      (dolist (child (org-mode-parser:node-children tasks-zone))
        (when (typep child 'org-mode-parser:org-heading)
          (let ((todo (org-mode-parser:heading-todo-keyword child)))
            (when (or (null state)
                      (and todo (string-equal todo state)))
              (push (list :id (heading-id child)
                          :title (org-mode-parser:heading-title child)
                          :state (or todo "")
                          :tags (org-mode-parser:heading-tags child)
                          :scope (heading-property child :HACTAR_SCOPE))
                    tasks))))))
    (nreverse tasks)))

(defun transition-task (task-id new-state)
  "Transition a task to a new state (TODO, DOING, DONE, BLOCKED)."
  (unless *litmode-parsed* (load-litmode-file))
  (let ((heading (find-heading-by-custom-id *litmode-parsed* task-id)))
    (unless heading
      (error "Task not found: ~A" task-id))
    (let ((old-state (or (org-mode-parser:heading-todo-keyword heading) "")))
      (setf (org-mode-parser:heading-todo-keyword heading)
            (string-upcase new-state))

      (cond
        ((string-equal new-state "DOING")
         (let ((task-scope (heading-property heading :HACTAR_SCOPE)))
           (when task-scope
             (set-active-scope task-scope)))
         (let ((lock-headlines (heading-property heading :HACTAR_LOCK_HEADLINES)))
           (when lock-headlines
             (let ((ids (mapcar (lambda (s) (string-trim '(#\Space) s))
                                (str:split #\, lock-headlines))))
               (acquire-lock ids :scope-name (heading-property heading :HACTAR_SCOPE))))))

        ((string-equal new-state "DONE")
         (let ((lock-headlines (heading-property heading :HACTAR_LOCK_HEADLINES)))
           (when lock-headlines
             (let ((ids (mapcar (lambda (s) (string-trim '(#\Space) s))
                                (str:split #\, lock-headlines))))
               (release-lock ids))))
         (fire-eval-trigger "on-task-done")))

      (save-litmode-file)

      (nhooks:run-hook *task-transitioned-hook*
                       (or task-id "") old-state (string-upcase new-state))

      (unless *silent*
        (format t "~&Task ~A: ~A → ~A~%" task-id old-state new-state)))))

;;* named block registry & scripting

(defun register-org-tools ()
  "Scan the Tooling section and register named src blocks."
  (unless *litmode-parsed* (load-litmode-file))
  (clrhash *named-blocks*)

  (let ((tooling-heading (find-heading-by-custom-id *litmode-parsed* "tooling")))
    (when tooling-heading
      (org-mode-parser:walk-tree
       tooling-heading
       (lambda (node)
         (when (typep node 'org-mode-parser:org-src-block)
           (let* ((parent (org-mode-parser:node-parent node))
                  (language (org-mode-parser:src-block-language node))
                  (content (org-mode-parser:src-block-content node))
                  (name (when (typep parent 'org-mode-parser:org-heading)
                          (or (heading-property parent :NAME)
                              (string-downcase
                               (:kebab-case
                                (org-mode-parser:heading-title parent))))))
                  (eval-trigger (when (typep parent 'org-mode-parser:org-heading)
                                  (heading-property parent :EVAL)))
                  (tool-desc (when (typep parent 'org-mode-parser:org-heading)
                               (heading-property parent :TOOL_DESC)))
                  (dir (when (typep parent 'org-mode-parser:org-heading)
                         (heading-property parent :DIR))))
             (when name
               (setf (gethash name *named-blocks*)
                     (list :name name
                           :language language
                           :content content
                           :eval (or eval-trigger "never")
                           :dir dir
                           :tool-desc tool-desc))
               (when (and (typep parent 'org-mode-parser:org-heading)
                          (heading-has-tag-p parent "tool")
                          tool-desc)
                 (register-block-as-tool name language content tool-desc)))))))))

  (let ((count (hash-table-count *named-blocks*)))
    (unless (or *silent* (zerop count))
      (format t "~&Registered ~A named block~:P from Tooling section.~%" count))))

(defun register-block-as-tool (name language content tool-desc)
  "Register an org src block as a Hactar tool."
  (let ((tool-name (substitute #\_ #\- name)))
    (setf (gethash tool-name *defined-tools*)
          (:make-tool-definition
           :name tool-name
           :description tool-desc
           :parameters (list (:make-tool-parameter
                              :name "input"
                              :type :string
                              :description "Input to the tool"
                              :required nil))
           :permissions :confirm
           :function (lambda (args)
                       (declare (ignore args))
                       (run-named-block name))))))

(defun run-named-block (name)
  "Execute a named src block."
  (let ((block-info (gethash name *named-blocks*)))
    (unless block-info
      (error "Named block not found: ~A" name))
    (let ((language (getf block-info :language))
          (content (getf block-info :content))
          (dir (getf block-info :dir)))
      (cond
        ((or (string-equal language "shell")
             (string-equal language "sh")
             (string-equal language "bash"))
         (let ((cwd (or (when dir
                          (let ((d (merge-pathnames dir
                                     (uiop:pathname-directory-pathname *litmode-path*))))
                            (namestring d)))
                        (when *repo-root*
                          (namestring *repo-root*)))))
           (multiple-value-bind (output error-output exit-code)
               (uiop:run-program (list *shell* "-c" content)
                                 :output :string
                                 :error-output :string
                                 :ignore-error-status t
                                 :directory cwd)
             (format nil "~A~@[~%stderr: ~A~]~@[~%(exit: ~A)~]"
                     output
                     (when (> (length error-output) 0) error-output)
                     (unless (zerop exit-code) exit-code)))))

        ((string-equal language "hactar-tool")
         (handler-case
             (let ((result (eval (read-from-string content))))
               (format nil "~A" result))
           (error (e)
             (format nil "Error: ~A" e))))

        ((or (string-equal language "lisp")
             (string-equal language "common-lisp"))
         (handler-case
             (let ((result (eval (read-from-string content))))
               (format nil "~A" result))
           (error (e)
             (format nil "Error: ~A" e))))

        (t
         (format nil "Cannot execute ~A blocks directly." language))))))

(defun fire-eval-trigger (trigger-name)
  "Fire all named blocks matching the given :eval trigger."
  (maphash (lambda (name info)
             (when (string-equal (getf info :eval) trigger-name)
               (unless *silent*
                 (format t "~&Firing ~A: ~A~%" trigger-name name))
               (handler-case
                   (run-named-block name)
                 (error (e)
                   (format t "~&Error in ~A (~A): ~A~%" name trigger-name e)))))
           *named-blocks*))

;;* auto-summarization

(defun auto-summarize-headlines ()
  "Generate summaries for headlines that don't have one, using the cheap model."
  (unless *litmode-parsed* (load-litmode-file))
  (let ((count 0))
    (dolist (h (collect-all-headings *litmode-parsed*))
      (let ((id (heading-id h))
            (summary (heading-property h :HACTAR_SUMMARY)))
        (when (and id (or (null summary) (string= summary "")))
          (let* ((content (heading-content-string h))
                 (truncated (subseq content 0 (min 2000 (length content)))))
            (handler-case
                (let ((generated-summary
                        (:get-llm-response
                         (format nil "Summarize this org-mode section in one sentence (max 80 chars). Output ONLY the summary, nothing else:~%~%~A" truncated)
                         :model *cheap-model*
                         :stream nil
                         :add-to-history nil)))
                  (when (and generated-summary (> (length generated-summary) 0))
                    (let ((clean (string-trim '(#\Space #\Tab #\Newline #\Return #\")
                                              generated-summary)))
                      (set-heading-property h :HACTAR_SUMMARY clean)
                      (incf count))))
              (error (e)
                (:debug-log "Error summarizing" id ":" e)))))))
    (when (> count 0)
      (save-litmode-file)
      (unless *silent*
        (format t "~&Generated ~A summary(ies).~%" count)))
    count))

;;* litmode commands

(define-command litmode-init (args)
  "Initialize literate single-file mode.
Usage: /litmode-init [path]"
  (let ((path (first args)))
    (init-litmode (when path (pathname path)))))

(define-command scope (args)
  "Set or show the active scope profile.
Usage: /scope [name]
       /scope --list"
  (cond
    ((member "--list" args :test #'string=)
     (let ((profiles (list-scope-profiles)))
       (if profiles
           (progn
             (format t "~&Available scope profiles:~%")
             (dolist (p profiles)
               (format t "  ~A~%" (getf p :name))
               (when (getf p :include-tags)
                 (format t "    Tags: ~{~A~^, ~}~%" (getf p :include-tags)))
               (when (getf p :max-tokens)
                 (format t "    Max tokens: ~A~%" (getf p :max-tokens)))))
           (format t "~&No scope profiles defined.~%"))))

    ((first args)
     (set-active-scope (first args))
     (format t "~&Scope set to: ~A~%" (first args)))

    (t
     (let ((current (get-active-scope)))
       (if current
           (format t "~&Active scope: ~A~%" current)
           (format t "~&No active scope. Use /scope <name> or /scope --list~%"))))))

(define-command expand (args)
  "Expand context to include additional headlines.
Usage: /expand <id1> [id2 ...]"
  (if args
      (progn
        (expand-context args "User requested expansion")
        (format t "~&Expanded context with: ~{~A~^, ~}~%" args))
      (format t "~&Usage: /expand <headline-id> [more-ids...]~%")))

(define-command focus (args)
  "Focus context on a single headline.
Usage: /focus <id>
       /focus --clear"
  (cond
    ((member "--clear" args :test #'string=)
     (setf *focused-id* nil)
     (format t "~&Focus cleared.~%"))
    ((first args)
     (focus-context (first args) "User requested focus")
     (format t "~&Focused on: ~A~%" (first args)))
    (t
     (if *focused-id*
         (format t "~&Focused on: ~A~%" *focused-id*)
         (format t "~&No focus set. Use /focus <headline-id>~%")))))

(define-command litmode-index (args)
  "Rebuild the headline index.
Usage: /litmode-index"
  (declare (ignore args))
  (if (not (litmode-active-p))
    (format t "~&Literate mode not active. Run /litmode-init first.~%")
    (progn
      (build-headline-index)
      (format t "~&Headline index rebuilt. ~A entries.~%"
              (length *headline-index-cache*)))))

(define-command litmode-tangle (args)
  "Tangle code from .hactar.org to files.
Usage: /litmode-tangle [--dry-run]
       /litmode-tangle <heading-id>"
  (if (not (litmode-active-p))
    (format t "~&Literate mode not active.~%")
    (let ((dry-run (member "--dry-run" args :test #'string=))
          (heading-id (first (remove "--dry-run" args :test #'string=))))
      (if heading-id
          (tangle-block heading-id :dry-run dry-run)
          (let ((files (tangle-code-zone :dry-run dry-run)))
            (format t "~&Tangled ~A file~:P.~%" (length files)))))))

(define-command litmode-untangle (args)
  "Pull changes from tangled files back into .hactar.org.
Usage: /litmode-untangle <file-path>"
  (if (not (litmode-active-p))
    (format t "~&Literate mode not active.~%")
    (if (first args)
        (untangle-file (first args))
        (format t "~&Usage: /litmode-untangle <file-path>~%"))))

(define-command litmode-drift (args)
  "Detect drift between org source and tangled files.
Usage: /litmode-drift"
  (declare (ignore args))
  (if (not (litmode-active-p))
    (format t "~&Literate mode not active.~%")
    (let ((drifts (detect-litmode-drift)))
      (if drifts
          (progn
            (format t "~&Detected ~A drifted file~:P:~%" (length drifts))
            (dolist (d drifts)
              (format t "  ~A (~A)~%" (getf d :file) (getf d :drift-type))))
          (format t "~&No drift detected.~%")))))

(define-command litmode-tasks (args)
  "Show tasks from .hactar.org.
Usage: /litmode-tasks [--state <TODO|DOING|DONE|BLOCKED>]"
  (if (not (litmode-active-p))
    (format t "~&Literate mode not active.~%")
    (let* ((state-pos (position "--state" args :test #'string=))
           (state (when state-pos (nth (1+ state-pos) args)))
           (tasks (get-tasks :state state)))
      (if tasks
          (progn
            (format t "~&Tasks:~%")
            (dolist (task tasks)
              (format t "  [~A] ~A~@[ :~{~A~^:~}:~]~@[ (scope: ~A)~]~%"
                      (getf task :state)
                      (getf task :title)
                      (getf task :tags)
                      (getf task :scope))))
          (format t "~&No tasks~@[ with state ~A~].~%" state)))))

(define-command litmode-task-do (args)
  "Transition a task to a new state.
Usage: /litmode-task-do <task-id> <state>"
  (if (not (litmode-active-p))
    (format t "~&Literate mode not active.~%")
    (if (and (first args) (second args))
        (transition-task (first args) (second args))
        (format t "~&Usage: /litmode-task-do <task-id> <TODO|DOING|DONE|BLOCKED>~%"))))

(define-command litmode-locks (args)
  "Show active agent locks.
Usage: /litmode-locks [--cleanup]"
  (if (not (litmode-active-p))
    (format t "~&Literate mode not active.~%")
    (progn
      (when (member "--cleanup" args :test #'string=)
        (cleanup-expired-locks)
        (format t "~&Expired locks cleaned up.~%"))
      (let ((locks (get-active-locks)))
        (if locks
            (progn
              (format t "~&Active locks:~%")
              (dolist (lock locks)
                (format t "  Owner: ~A, Headlines: ~A, Scope: ~A~%"
                        (first lock) (second lock) (third lock))))
            (format t "~&No active locks.~%"))))))

(define-command run (args)
  "Run a named src block from the Tooling section.
Usage: /run <name>"
  (if (not (litmode-active-p))
    (format t "~&Literate mode not active.~%")
    (if (first args)
        (let ((result (run-named-block (first args))))
          (when result
            (format t "~A~%" result)))
        (progn
          (format t "~&Available named blocks:~%")
          (maphash (lambda (name info)
                     (format t "  ~A (~A, eval: ~A)~%"
                             name
                             (getf info :language)
                             (getf info :eval)))
                   *named-blocks*)))))

(define-command litmode-summarize (args)
  "Auto-generate summaries for headlines without one.
Usage: /litmode-summarize"
  (declare (ignore args))
  (if (not (litmode-active-p))
    (format t "~&Literate mode not active.~%")
    (auto-summarize-headlines)))

(define-command litmode-status (args)
  "Show literate mode status.
Usage: /litmode-status"
  (declare (ignore args))
  (if (litmode-active-p)
      (progn
        (format t "~&Literate Mode Status~%")
        (format t "====================~%")
        (format t "File: ~A~%" (uiop:native-namestring *litmode-path*))
        (format t "Agent ID: ~A~%" *agent-id*)
        (format t "Active scope: ~A~%"
                (or (get-active-scope) "(none)"))
        (format t "Focused on: ~A~%"
                (or *focused-id* "(none)"))
        (format t "Expanded IDs: ~{~A~^, ~}~%"
                (or *expanded-ids* '("(none)")))
        (format t "Headlines indexed: ~A~%"
                (length (get-headline-index)))
        (format t "Named blocks: ~A~%"
                (hash-table-count *named-blocks*))
        (format t "Active locks: ~A~%"
                (length (get-active-locks))))
      (format t "~&Literate mode not active. Run /litmode-init to start.~%")))

;;* tools for LLM

(deftool expand-context
    ((headline-ids :string
      :description "Comma-separated list of headline IDs to add to context"
      :required t)
     (reason :string
      :description "Why these headlines are needed"
      :required nil))
  :description "Expand the current context to include additional headlines from the .hactar.org file. Use the Headline Index to discover available headline IDs."
  :permissions :auto
  (let* ((ids-str (getf args :headline-ids))
         (reason (getf args :reason))
         (ids (mapcar (lambda (s) (string-trim '(#\Space) s))
                      (str:split #\, ids-str))))
    (if (litmode-active-p)
        (progn
          (expand-context ids reason)
          (format nil "Expanded context with: ~{~A~^, ~}" ids))
        "Literate mode is not active.")))

(deftool focus-context
    ((headline-id :string
      :description "The headline ID to focus on"
      :required t)
     (reason :string
      :description "Why focusing on this headline"
      :required nil))
  :description "Focus the context on a single headline (plus pinned headlines and the index). This narrows the context window for more targeted work."
  :permissions :auto
  (let ((id (getf args :headline-id))
        (reason (getf args :reason)))
    (if (litmode-active-p)
        (progn
          (focus-context id reason)
          (format nil "Focused context on: ~A" id))
        "Literate mode is not active.")))

;;* CLI subcommands

(define-sub-command litmode.init (args)
  "Initialize literate single-file mode.
Usage: hactar litmode.init [path]"
  (let ((path (first args)))
    (init-litmode (when path (pathname path)))))

(define-sub-command litmode.tangle (args)
  "Tangle code from .hactar.org to files.
Usage: hactar litmode.tangle [--dry-run]"
  (let ((dry-run (member "--dry-run" args :test #'string=)))
    (init-litmode)
    (let ((files (tangle-code-zone :dry-run dry-run)))
      (format t "Tangled ~A file~:P.~%" (length files)))))

(define-sub-command litmode.drift (args)
  "Detect drift between org and tangled code.
Usage: hactar litmode.drift"
  (declare (ignore args))
  (init-litmode)
  (let ((drifts (detect-litmode-drift)))
    (if drifts
        (progn
          (dolist (d drifts)
            (format t "DRIFT: ~A (~A)~%" (getf d :file) (getf d :drift-type)))
          (uiop:quit 1))
        (progn
          (format t "No drift detected.~%")
          (uiop:quit 0)))))

(define-sub-command litmode.index (args)
  "Build the headline index.
Usage: hactar litmode.index"
  (declare (ignore args))
  (init-litmode)
  (build-headline-index)
  (format t "Index built: ~A entries.~%"
          (length *headline-index-cache*)))

;;* system prompt integration

(defun litmode-system-prompt-section ()
  "Generate the litmode section for the system prompt."
  (when *litmode-active*
    (with-output-to-string (s)
      (format s "~%## Literate Mode~%~%")
      (format s "You are operating in literate single-file mode. Context is generated from a .hactar.org file.~%~%")

      (when *active-scope*
        (format s "Active scope: ~A~%" *active-scope*))

      (when *focused-id*
        (format s "Context is focused on headline: ~A~%" *focused-id*))

      (format s "~%### Available Actions~%")
      (format s "- Use `expand_context` tool to request additional headlines into context~%")
      (format s "- Use `focus_context` tool to narrow context to a specific headline~%")
      (format s "- The Headline Index shows all available headlines you can request~%")
      (format s "- Headlines tagged :pin: are always in context~%")
      (format s "- Headlines tagged :noctx: are never in context~%")
      (format s "- Code blocks with :HACTAR_TANGLE: properties map to real files~%")

      ;; Show quick index summary
      (when *headline-index-cache*
        (format s "~%### Quick Index (~A headlines)~%" (length *headline-index-cache*))
        (dolist (entry (subseq *headline-index-cache* 0
                               (min 20 (length *headline-index-cache*))))
          (format s "- ~A: ~A (~A tokens)~@[ — ~A~]~%"
                  (getf entry :id)
                  (getf entry :title)
                  (getf entry :tokens)
                  (let ((sum (getf entry :summary)))
                    (when (and sum (not (string= sum ""))) sum))))
        (when (> (length *headline-index-cache*) 20)
          (format s "  ... and ~A more. Use expand_context to see them.~%"
                  (- (length *headline-index-cache*) 20)))))))

;;* file watcher analyzer

(def-analyzer litmode-file-watcher ((*file-event-hook*)) nil (pathname event-type)
  "Watch for changes to the .hactar.org file and reload when modified."
  (when (and (litmode-active-p)
             *litmode-path*
             (equal (truename pathname)
                    (ignore-errors (truename *litmode-path*)))
             (member event-type '(:file-changed)))
    (debug-log "Litmode file changed, reloading...")
    (handler-case
        (progn
          (load-litmode-file t)
          (register-org-tools)
          ;; Fire on-save triggers
          (fire-eval-trigger "on-save"))
      (error (e)
        (debug-log "Error reloading litmode file:" e)))))
