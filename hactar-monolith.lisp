;;* Hactar Monolith - Org-Centric Literate Programming Mode
(in-package :hactar)

(defpackage #:hactar-monolith
  (:use #:cl #:hactar)
  (:export
   ;; Core structures
   #:org-concept
   #:org-project
   #:org-journal-entry
   #:org-edge
   #:org-code-block
   ;; Monolith operations
   #:init-monolith
   #:index-monolith
   #:get-monolith-status
   ;; Concept operations
   #:create-concept
   #:find-concepts
   #:get-concept
   #:update-concept-maturity
   ;; Tangling
   #:tangle-project
   #:tangle-file
   #:detect-drift
   #:untangle-changes
   ;; Graph operations
   #:query-graph
   #:get-related-concepts
   #:get-concept-evolution
   ;; Sync
   #:sync-push
   #:sync-pull
   #:get-sync-status))

(in-package #:hactar-monolith)

;;** Configuration

(defvar *monolith-path* nil
  "Path to the knowledge monolith root directory.")

(defvar *monolith-db-path* nil
  "Path to the SQLite database for the knowledge graph.")

(defvar *monolith-structure*
  '((:concepts . "Concepts/")
    (:projects . "Projects/")
    (:journal . "Journal/")
    (:principles . "Principles/"))
  "Default directory structure for the monolith.")

(defvar *default-maturity* :seedling
  "Default maturity level for new concepts.")

(defvar *sync-conflict-resolution* :prefer-org
  "Default conflict resolution strategy: :prefer-org, :prefer-repo, :manual")

;;** Data Structures

(defstruct org-concept
  "A concept node in the knowledge graph."
  (id nil :type (or null string))
  (title nil :type (or null string))
  (path nil :type (or null pathname))
  (maturity :seedling :type keyword)
  (languages nil :type list)
  (content nil :type (or null string))
  (embedding nil :type (or null vector))
  (extracted-to nil :type list)
  (sources nil :type list)
  (last-modified nil :type (or null integer)))

(defstruct org-project
  "A project node in the knowledge graph."
  (id nil :type (or null string))
  (name nil :type (or null string))
  (path nil :type (or null pathname))
  (sources nil :type list)
  (repos nil :type list)
  (sync-config nil))

(defstruct org-journal-entry
  "A journal entry with extracted insights."
  (id nil :type (or null string))
  (date nil :type (or null string))
  (path nil :type (or null pathname))
  (mentions nil :type list)
  (insights nil :type list)
  (embedding nil :type (or null vector)))

(defstruct org-edge
  "A relationship between nodes."
  (id nil :type (or null integer))
  (from-id nil :type (or null string))
  (to-id nil :type (or null string))
  (relation nil :type keyword)
  (metadata nil))

(defstruct org-code-block
  "A code block within an org file."
  (id nil :type (or null string))
  (concept-id nil :type (or null string))
  (language nil :type (or null string))
  (content nil :type (or null string))
  (tangle-target nil :type (or null string))
  (project nil :type (or null string))
  (checksum nil :type (or null string))
  (line-start nil :type (or null integer))
  (line-end nil :type (or null integer)))

(defstruct tangle-result
  "Result of a tangle operation."
  (success-p nil :type boolean)
  (files-written nil :type list)
  (errors nil :type list)
  (checksums nil :type list))

(defstruct drift-info
  "Information about code drift."
  (file nil :type (or null string))
  (org-source nil :type (or null string))
  (tangled-content nil :type (or null string))
  (org-checksum nil :type (or null string))
  (file-checksum nil :type (or null string))
  (drift-type nil :type keyword))  ; :modified, :deleted, :new

;;** Hooks

(nhooks:define-hook-type org-heading-parsed (function (list) t)
  "Hook for parsed org headings.")
(defvar *org-heading-parsed-hook* (make-instance 'hook-org-heading-parsed))

(nhooks:define-hook-type concept-created (function (org-concept) t)
  "Hook for newly created concepts.")
(defvar *concept-created-hook* (make-instance 'hook-concept-created))

(nhooks:define-hook-type concept-matured (function (org-concept keyword keyword) t)
  "Hook for concept maturity changes. Args: concept, old-maturity, new-maturity")
(defvar *concept-matured-hook* (make-instance 'hook-concept-matured))

(nhooks:define-hook-type tangle-completed (function (tangle-result) t)
  "Hook for completed tangle operations.")
(defvar *tangle-completed-hook* (make-instance 'hook-tangle-completed))

(nhooks:define-hook-type drift-detected (function (list) t)
  "Hook for detected code drift. Args: list of drift-info.")
(defvar *drift-detected-hook* (make-instance 'hook-drift-detected))

(nhooks:define-hook-type link-resolved (function (string string) t)
  "Hook for resolved org links. Args: link-text, resolved-path")
(defvar *link-resolved-hook* (make-instance 'hook-link-resolved))

;;** Org Parsing

(defun parse-org-properties (content)
  "Parse :PROPERTIES: drawer from org content. Returns alist."
  (org-mode:parse-org-properties-from-content content))

(defun parse-org-heading (line)
  "Parse an org heading line. Returns (level . title) or nil."
  (org-mode:parse-org-heading-line line))

(defun parse-org-links (content)
  "Extract org-mode links from content. Returns list of (type . target)."
  (org-mode:parse-org-links content))

(defun parse-org-code-blocks (content &optional file-path)
  "Extract code blocks from org content. Returns list of org-code-block structs."
  (let ((raw-blocks (org-mode:parse-org-code-blocks-from-content content file-path)))
    (mapcar (lambda (plist)
              (make-org-code-block
               :id (getf plist :id)
               :language (getf plist :language)
               :content (getf plist :content)
               :tangle-target (getf plist :tangle-target)
               :project (getf plist :project)
               :checksum (getf plist :checksum)
               :line-start (getf plist :line-start)
               :line-end (getf plist :line-end)))
            raw-blocks)))

(defun parse-org-file (file-path)
  "Parse an org file completely. Returns plist with all extracted data."
  (let ((result (org-mode:parse-org-file-full file-path)))
    ;; Convert code block plists to org-code-block structs
    (setf (getf result :code-blocks)
          (mapcar (lambda (plist)
                    (make-org-code-block
                     :id (getf plist :id)
                     :language (getf plist :language)
                     :content (getf plist :content)
                     :tangle-target (getf plist :tangle-target)
                     :project (getf plist :project)
                     :checksum (getf plist :checksum)
                     :line-start (getf plist :line-start)
                     :line-end (getf plist :line-end)))
                  (getf result :code-blocks)))
    ;; Run hook
    (nhooks:run-hook *org-heading-parsed-hook* result)
    result))

(defun compute-checksum (content)
  "Compute a checksum for content string."
  (org-mode:compute-content-checksum content))

;;** Monolith Initialization

(defun init-monolith (path)
  "Initialize a new knowledge monolith at PATH."
  (let ((root (uiop:ensure-directory-pathname path)))
    (setf *monolith-path* root)
    (setf *monolith-db-path* (merge-pathnames ".hactar-monolith.db" root))
    
    (dolist (dir-spec *monolith-structure*)
      (let ((dir-path (merge-pathnames (cdr dir-spec) root)))
        (ensure-directories-exist dir-path)
        (format t "Created: ~A~%" (uiop:native-namestring dir-path))))
    
    (let ((gitignore (merge-pathnames ".gitignore" root)))
      (unless (probe-file gitignore)
        (with-open-file (s gitignore :direction :output :if-does-not-exist :create)
          (format s ".hactar-monolith.db~%"))))
    
    (init-monolith-db)
    
    (format t "~&Monolith initialized at: ~A~%" (uiop:native-namestring root))
    root))

(defun init-monolith-db ()
  "Initialize the SQLite database for the knowledge graph."
  (when *monolith-db-path*
    (sqlite:with-open-database (db *monolith-db-path*)
      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS concepts (
           id TEXT PRIMARY KEY,
           title TEXT NOT NULL,
           path TEXT NOT NULL,
           maturity TEXT DEFAULT 'seedling',
           languages TEXT,
           content TEXT,
           embedding BLOB,
           last_modified INTEGER,
           last_extracted INTEGER
         )")
      
      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS projects (
           id TEXT PRIMARY KEY,
           name TEXT NOT NULL,
           path TEXT NOT NULL,
           repos TEXT,
           sync_config TEXT
         )")
      
      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS journal_entries (
           id TEXT PRIMARY KEY,
           date TEXT NOT NULL,
           path TEXT NOT NULL,
           insights TEXT,
           embedding BLOB
         )")
      
      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS edges (
           id INTEGER PRIMARY KEY AUTOINCREMENT,
           from_id TEXT NOT NULL,
           to_id TEXT NOT NULL,
           relation TEXT NOT NULL,
           metadata TEXT,
           created_at INTEGER DEFAULT (strftime('%s', 'now'))
         )")
      
      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS code_blocks (
           id TEXT PRIMARY KEY,
           concept_id TEXT,
           language TEXT NOT NULL,
           content TEXT NOT NULL,
           tangle_target TEXT,
           project TEXT,
           checksum TEXT,
           line_start INTEGER,
           line_end INTEGER
         )")
      
      (sqlite:execute-non-query db
        "CREATE TABLE IF NOT EXISTS tangle_history (
           id INTEGER PRIMARY KEY AUTOINCREMENT,
           code_block_id TEXT,
           target_path TEXT NOT NULL,
           tangled_at INTEGER DEFAULT (strftime('%s', 'now')),
           checksum TEXT
         )")
      
      (sqlite:execute-non-query db 
        "CREATE INDEX IF NOT EXISTS idx_concepts_maturity ON concepts(maturity)")
      (sqlite:execute-non-query db 
        "CREATE INDEX IF NOT EXISTS idx_concepts_path ON concepts(path)")
      (sqlite:execute-non-query db 
        "CREATE INDEX IF NOT EXISTS idx_edges_from ON edges(from_id)")
      (sqlite:execute-non-query db 
        "CREATE INDEX IF NOT EXISTS idx_edges_to ON edges(to_id)")
      (sqlite:execute-non-query db 
        "CREATE INDEX IF NOT EXISTS idx_edges_relation ON edges(relation)")
      (sqlite:execute-non-query db 
        "CREATE INDEX IF NOT EXISTS idx_code_blocks_concept ON code_blocks(concept_id)")
      (sqlite:execute-non-query db 
        "CREATE INDEX IF NOT EXISTS idx_code_blocks_language ON code_blocks(language)"))))

;;** Indexing

(defun index-monolith (&key force)
  "Index the entire knowledge monolith."
  (unless *monolith-path*
    (error "Monolith not initialized. Run /monolith-init first."))
  
  (format t "~&Indexing monolith at ~A...~%" (uiop:native-namestring *monolith-path*))
  
  (let ((org-files (directory (merge-pathnames "**/*.org" *monolith-path*)))
        (concepts-indexed 0)
        (projects-indexed 0)
        (journal-indexed 0)
        (code-blocks-indexed 0))
    
    (dolist (file org-files)
      (let* ((relative-path (uiop:enough-pathname file *monolith-path*))
             (dir-name (second (pathname-directory relative-path)))
             (parsed (parse-org-file file)))
        
        (cond
          ;; Concepts
          ((string-equal dir-name "Concepts")
           (index-concept-file parsed)
           (incf concepts-indexed))
          
          ;; Projects
          ((string-equal dir-name "Projects")
           (index-project-file parsed)
           (incf projects-indexed))
          
          ;; Journal
          ((string-equal dir-name "Journal")
           (index-journal-file parsed)
           (incf journal-indexed)))
        
        ;; Index code blocks from all files
        (dolist (block (getf parsed :code-blocks))
          (index-code-block block (getf parsed :path))
          (incf code-blocks-indexed))))
    
    (format t "~&Indexed: ~A concepts, ~A projects, ~A journal entries, ~A code blocks~%"
            concepts-indexed projects-indexed journal-indexed code-blocks-indexed)))

(defun index-concept-file (parsed)
  "Index a concept file into the database."
  (let* ((props (getf parsed :properties))
         (path (getf parsed :path))
         (headings (getf parsed :headings))
         (first-heading (first headings))
         (id (or (cdr (assoc :ID props))
                 (format nil "~A" (uuid:make-v4-uuid))))
         (title (or (getf first-heading :title)
                    (pathname-name path)))
         (maturity (or (cdr (assoc :MATURITY props)) "seedling"))
         (languages-str (cdr (assoc :LANGUAGES props)))
         (languages (when languages-str
                      (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                              (str:split #\, languages-str))))
         (content (getf first-heading :content)))
    
    (sqlite:with-open-database (db *monolith-db-path*)
      (sqlite:execute-non-query db
        "INSERT OR REPLACE INTO concepts 
         (id, title, path, maturity, languages, content, last_modified)
         VALUES (?, ?, ?, ?, ?, ?, ?)"
        id title (namestring path) maturity 
        (when languages (cl-json:encode-json-to-string languages))
        content (get-universal-time)))
    
    (dolist (link (getf parsed :links))
      (index-edge id (cdr link) (car link)))))

(defun index-project-file (parsed)
  "Index a project file into the database."
  (let* ((props (getf parsed :properties))
         (path (getf parsed :path))
         (headings (getf parsed :headings))
         (first-heading (first headings))
         (id (or (cdr (assoc :ID props))
                 (format nil "~A" (uuid:make-v4-uuid))))
         (name (or (getf first-heading :title)
                   (pathname-name path)))
         (repos-str (cdr (assoc :REPOS props)))
         (repos (when repos-str
                  (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                          (str:split #\, repos-str)))))
    
    (sqlite:with-open-database (db *monolith-db-path*)
      (sqlite:execute-non-query db
        "INSERT OR REPLACE INTO projects (id, name, path, repos)
         VALUES (?, ?, ?, ?)"
        id name (namestring path)
        (when repos (cl-json:encode-json-to-string repos))))
    
    (dolist (link (getf parsed :links))
      (when (eq (car link) :semantic)
        (index-edge id (cdr link) :implements)))))

(defun index-journal-file (parsed)
  "Index a journal entry into the database."
  (let* ((props (getf parsed :properties))
         (path (getf parsed :path))
         (id (or (cdr (assoc :ID props))
                 (format nil "~A" (uuid:make-v4-uuid))))
         (date (or (cdr (assoc :DATE props))
                   (pathname-name path))))
    
    (sqlite:with-open-database (db *monolith-db-path*)
      (sqlite:execute-non-query db
        "INSERT OR REPLACE INTO journal_entries (id, date, path)
         VALUES (?, ?, ?)"
        id date (namestring path)))
    
    (dolist (link (getf parsed :links))
      (index-edge id (cdr link) :mentions))))

(defun index-code-block (block source-path)
  "Index a code block into the database."
  (sqlite:with-open-database (db *monolith-db-path*)
    (sqlite:execute-non-query db
      "INSERT OR REPLACE INTO code_blocks 
       (id, language, content, tangle_target, project, checksum, line_start, line_end)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      (org-code-block-id block)
      (org-code-block-language block)
      (org-code-block-content block)
      (org-code-block-tangle-target block)
      (org-code-block-project block)
      (org-code-block-checksum block)
      (org-code-block-line-start block)
      (org-code-block-line-end block))))

(defun index-edge (from-id to-id relation)
  "Index an edge between nodes."
  (sqlite:with-open-database (db *monolith-db-path*)
    (let ((existing (sqlite:execute-to-list db
                      "SELECT id FROM edges WHERE from_id = ? AND to_id = ? AND relation = ?"
                      from-id to-id (string relation))))
      (unless existing
        (sqlite:execute-non-query db
          "INSERT INTO edges (from_id, to_id, relation) VALUES (?, ?, ?)"
          from-id to-id (string relation))))))

;;** Tangling

(defun tangle-project (project-name &key dry-run lang)
  "Tangle all code blocks for a project."
  (unless *monolith-path*
    (error "Monolith not initialized."))
  
  (format t "~&Tangling project: ~A~%" project-name)
  
  (sqlite:with-open-database (db *monolith-db-path*)
    (let* ((query (if lang
                      "SELECT * FROM code_blocks WHERE project = ? AND language = ? AND tangle_target IS NOT NULL"
                      "SELECT * FROM code_blocks WHERE project = ? AND tangle_target IS NOT NULL"))
           (rows (if lang
                     (sqlite:execute-to-list db query project-name lang)
                     (sqlite:execute-to-list db query project-name)))
           (result (make-tangle-result :success-p t)))
      
      (dolist (row rows)
        (let* ((block-id (first row))
               (content (fourth row))
               (tangle-target (fifth row))
               (checksum (seventh row))
               (target-path (resolve-tangle-target tangle-target project-name)))
          
          (if dry-run
              (format t "  Would write: ~A~%" target-path)
              (handler-case
                  (progn
                    (ensure-directories-exist target-path)
                    (with-open-file (s target-path 
                                       :direction :output 
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                      (write-string content s))
                    (push target-path (tangle-result-files-written result))
                    (push (cons target-path checksum) (tangle-result-checksums result))
                    
                    ;; Record in history
                    (sqlite:execute-non-query db
                      "INSERT INTO tangle_history (code_block_id, target_path, checksum)
                       VALUES (?, ?, ?)"
                      block-id (namestring target-path) checksum)
                    
                    (format t "  ✓ ~A~%" target-path))
                (error (e)
                  (push (format nil "~A: ~A" target-path e) 
                        (tangle-result-errors result))
                  (setf (tangle-result-success-p result) nil))))))
      
      (nhooks:run-hook *tangle-completed-hook* result)
      result)))

(defun tangle-file (file-path &key dry-run)
  "Tangle code blocks from a single org file."
  (let* ((parsed (parse-org-file file-path))
         (blocks (getf parsed :code-blocks))
         (result (make-tangle-result :success-p t)))
    
    (dolist (block blocks)
      (when (org-code-block-tangle-target block)
        (let* ((target (resolve-tangle-target 
                        (org-code-block-tangle-target block)
                        (org-code-block-project block)))
               (content (org-code-block-content block)))
          
          (if dry-run
              (format t "  Would write: ~A~%" target)
              (handler-case
                  (progn
                    (ensure-directories-exist target)
                    (with-open-file (s target 
                                       :direction :output 
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                      (write-string content s))
                    (push target (tangle-result-files-written result))
                    (format t "  ✓ ~A~%" target))
                (error (e)
                  (push (format nil "~A: ~A" target e)
                        (tangle-result-errors result))
                  (setf (tangle-result-success-p result) nil)))))))
    
    (nhooks:run-hook *tangle-completed-hook* result)
    result))

(defun resolve-tangle-target (target project)
  "Resolve a tangle target to an absolute path."
  (let ((base-path (if project
                       (merge-pathnames (format nil "Projects/~A/" project) *monolith-path*)
                       *monolith-path*)))
    (if (uiop:absolute-pathname-p target)
        (pathname target)
        (merge-pathnames target base-path))))

;;** Drift Detection

(defun detect-drift (&key project all)
  "Detect drift between org source and tangled files."
  (unless *monolith-path*
    (error "Monolith not initialized."))
  
  (let ((drifts nil))
    (sqlite:with-open-database (db *monolith-db-path*)
      (let* ((query (cond
                      (project "SELECT cb.*, th.target_path, th.checksum as tangled_checksum
                                FROM code_blocks cb
                                JOIN tangle_history th ON cb.id = th.code_block_id
                                WHERE cb.project = ?
                                GROUP BY th.target_path
                                HAVING th.tangled_at = MAX(th.tangled_at)")
                      (all "SELECT cb.*, th.target_path, th.checksum as tangled_checksum
                            FROM code_blocks cb
                            JOIN tangle_history th ON cb.id = th.code_block_id
                            GROUP BY th.target_path
                            HAVING th.tangled_at = MAX(th.tangled_at)")))
             (rows (if project
                       (sqlite:execute-to-list db query project)
                       (sqlite:execute-to-list db query))))
        
        (dolist (row rows)
          (let* ((org-checksum (seventh row))       ; cb.checksum
                 (target-path (tenth row))           ; th.target_path
                 (tangled-checksum (nth 10 row)))    ; th.checksum (tangled_checksum)
            
            (when (probe-file target-path)
              (let* ((file-content (uiop:read-file-string target-path))
                     (current-checksum (compute-checksum file-content)))
                (unless (string= current-checksum tangled-checksum)
                  (push (make-drift-info
                         :file target-path
                         :org-checksum org-checksum
                         :file-checksum current-checksum
                         :drift-type :modified)
                        drifts))))))))
    
    (when drifts
      (nhooks:run-hook *drift-detected-hook* drifts))
    
    drifts))

;;** Concept Operations

(defun find-concepts (query &key maturity language limit)
  "Find concepts matching a query."
  (unless *monolith-db-path*
    (error "Monolith not initialized."))
  
  (sqlite:with-open-database (db *monolith-db-path*)
    (let* ((where-clauses (list "1=1"))
           (params nil))
      
      (when query
        (push "title LIKE ?" where-clauses)
        (push (format nil "%~A%" query) params))
      
      (when maturity
        (push "maturity = ?" where-clauses)
        (push (string maturity) params))
      
      (when language
        (push "languages LIKE ?" where-clauses)
        (push (format nil "%\"~A\"%" language) params))
      
      (let ((sql (format nil "SELECT id, title, path, maturity, languages 
                              FROM concepts 
                              WHERE ~{~A~^ AND ~}
                              ~@[LIMIT ~A~]"
                         (nreverse where-clauses)
                         limit)))
        (apply #'sqlite:execute-to-list db sql (nreverse params))))))

(defun get-concept (id)
  "Get a concept by ID."
  (sqlite:with-open-database (db *monolith-db-path*)
    (first (sqlite:execute-to-list db
             "SELECT * FROM concepts WHERE id = ?" id))))

(defun create-concept (title &key maturity languages content path)
  "Create a new concept."
  (let* ((id (format nil "~A" (uuid:make-v4-uuid)))
         (maturity (or maturity *default-maturity*))
         (path (or path 
                   (merge-pathnames 
                    (format nil "Concepts/~A.org" (hactar::kebab-case title))
                    *monolith-path*)))
         (concept (make-org-concept
                   :id id
                   :title title
                   :path path
                   :maturity maturity
                   :languages languages
                   :content content
                   :last-modified (get-universal-time))))
    
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output 
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format s "#+TITLE: ~A~%" title)
      (format s "~%:PROPERTIES:~%")
      (format s ":ID: ~A~%" id)
      (format s ":MATURITY: ~A~%" maturity)
      (when languages
        (format s ":LANGUAGES: ~{~A~^, ~}~%" languages))
      (format s ":END:~%~%")
      (when content
        (format s "~A~%" content)))
    
    (sqlite:with-open-database (db *monolith-db-path*)
      (sqlite:execute-non-query db
        "INSERT INTO concepts (id, title, path, maturity, languages, content, last_modified)
         VALUES (?, ?, ?, ?, ?, ?, ?)"
        id title (namestring path) (string maturity)
        (when languages (cl-json:encode-json-to-string languages))
        content (get-universal-time)))
    
    (nhooks:run-hook *concept-created-hook* concept)
    concept))

(defun update-concept-maturity (id new-maturity)
  "Update a concept's maturity level."
  (sqlite:with-open-database (db *monolith-db-path*)
    (let* ((concept-row (first (sqlite:execute-to-list db
                                 "SELECT maturity, path FROM concepts WHERE id = ?" id)))
           (old-maturity (first concept-row))
           (path (second concept-row)))
      
      (when concept-row
        (sqlite:execute-non-query db
          "UPDATE concepts SET maturity = ?, last_modified = ? WHERE id = ?"
          (string new-maturity) (get-universal-time) id)
        
        (when (probe-file path)
          (let* ((content (uiop:read-file-string path))
                 (new-content (cl-ppcre:regex-replace 
                               ":MATURITY: \\w+"
                               content
                               (format nil ":MATURITY: ~A" new-maturity))))
            (with-open-file (s path :direction :output :if-exists :supersede)
              (write-string new-content s))))
        
        (let ((concept (make-org-concept
                        :id id
                        :maturity new-maturity
                        :path (pathname path))))
          (nhooks:run-hook *concept-matured-hook* 
                           concept 
                           (intern (string-upcase old-maturity) :keyword)
                           new-maturity))))))

;;** Graph Queries

(defun get-related-concepts (id &key relation depth)
  "Get concepts related to the given ID."
  (let ((depth (or depth 1))
        (visited (make-hash-table :test 'equal))
        (results nil))
    
    (labels ((traverse (current-id current-depth)
               (when (and (> current-depth 0)
                          (not (gethash current-id visited)))
                 (setf (gethash current-id visited) t)
                 (sqlite:with-open-database (db *monolith-db-path*)
                   (let* ((query (if relation
                                     "SELECT to_id, relation FROM edges WHERE from_id = ? AND relation = ?"
                                     "SELECT to_id, relation FROM edges WHERE from_id = ?"))
                          (rows (if relation
                                    (sqlite:execute-to-list db query current-id (string relation))
                                    (sqlite:execute-to-list db query current-id))))
                     (dolist (row rows)
                       (let ((to-id (first row))
                             (rel (second row)))
                         (push (list :from current-id :to to-id :relation rel) results)
                         (traverse to-id (1- current-depth)))))))))
      (traverse id depth))
    
    (nreverse results)))

(defun query-graph (query-string)
  "Execute a natural language query against the knowledge graph."
  ;; Parse common query patterns
  (cond
    ;; "concepts never implemented"
    ((search "never implemented" query-string)
     (sqlite:with-open-database (db *monolith-db-path*)
       (sqlite:execute-to-list db
         "SELECT c.id, c.title, c.maturity 
          FROM concepts c
          WHERE c.id NOT IN (
            SELECT from_id FROM edges WHERE relation = 'IMPLEMENTS'
          )")))
    
    ((search "sharing concepts" query-string)
     (sqlite:with-open-database (db *monolith-db-path*)
       (sqlite:execute-to-list db
         "SELECT p1.name, p2.name, COUNT(*) as shared
          FROM edges e1
          JOIN edges e2 ON e1.to_id = e2.to_id AND e1.from_id != e2.from_id
          JOIN projects p1 ON e1.from_id = p1.id
          JOIN projects p2 ON e2.from_id = p2.id
          WHERE e1.relation = 'IMPLEMENTS' AND e2.relation = 'IMPLEMENTS'
          GROUP BY p1.id, p2.id
          ORDER BY shared DESC")))
    
    (t
     (find-concepts query-string))))

;;** Commands
(in-package :hactar)

;;*** Monolith Management

(define-command monolith-init (args)
  "Initialize a new knowledge monolith.
Usage: /monolith-init [path]"
  (let ((path (or (first args) (uiop:getcwd))))
    (hactar-monolith::init-monolith path)))

(define-command monolith-status (args)
  "Show status of the knowledge monolith."
  (declare (ignore args))
  (if (not hactar-monolith::*monolith-path*)
    (format t "~&Monolith not initialized. Run /monolith-init first.~%")
    (progn
      (format t "~&Monolith Status~%")
      (format t "===============~%")
      (format t "Path: ~A~%" (uiop:native-namestring hactar-monolith::*monolith-path*))
      
      (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
        (let ((concept-count (caar (sqlite:execute-to-list db 
                                     "SELECT COUNT(*) FROM concepts")))
              (project-count (caar (sqlite:execute-to-list db
                                     "SELECT COUNT(*) FROM projects")))
              (journal-count (caar (sqlite:execute-to-list db
                                     "SELECT COUNT(*) FROM journal_entries")))
              (block-count (caar (sqlite:execute-to-list db
                                   "SELECT COUNT(*) FROM code_blocks")))
              (edge-count (caar (sqlite:execute-to-list db
                                  "SELECT COUNT(*) FROM edges"))))
          
          (format t "~%Counts:~%")
          (format t "  Concepts: ~A~%" concept-count)
          (format t "  Projects: ~A~%" project-count)
          (format t "  Journal Entries: ~A~%" journal-count)
          (format t "  Code Blocks: ~A~%" block-count)
          (format t "  Graph Edges: ~A~%" edge-count)
          
          ;; Maturity distribution
          (format t "~%Concept Maturity:~%")
          (let ((maturity-dist (sqlite:execute-to-list db
                                 "SELECT maturity, COUNT(*) FROM concepts GROUP BY maturity")))
            (dolist (row maturity-dist)
              (format t "  ~A: ~A~%" (first row) (second row)))))))))

(define-command monolith-index (args)
  "Index the knowledge monolith.
Usage: /monolith-index [--force]"
  (let ((force (member "--force" args :test #'string=)))
    (hactar-monolith::index-monolith :force force)))

;;*** Concept Commands

(define-command concept-new (args)
  "Create a new concept.
Usage: /concept-new <title> [--maturity <level>] [--lang <languages>]"
  (let* ((title (first args))
         (maturity (let ((pos (position "--maturity" args :test #'string=)))
                     (when pos (intern (string-upcase (nth (1+ pos) args)) :keyword))))
         (languages (let ((pos (position "--lang" args :test #'string=)))
                      (when pos 
                        (str:split #\, (nth (1+ pos) args))))))
    (if title
        (let ((concept (hactar-monolith::create-concept title 
                                                        :maturity maturity
                                                        :languages languages)))
          (format t "~&Created concept: ~A~%" (hactar-monolith::org-concept-title concept))
          (format t "  ID: ~A~%" (hactar-monolith::org-concept-id concept))
          (format t "  Path: ~A~%" (hactar-monolith::org-concept-path concept)))
        (format t "~&Usage: /concept-new <title> [--maturity <level>] [--lang <languages>]~%"))))

(define-command concept-find (args)
  "Find concepts.
Usage: /concept-find <query> [--maturity <level>] [--lang <language>]"
  (let* ((query (first args))
         (maturity (let ((pos (position "--maturity" args :test #'string=)))
                     (when pos (intern (string-upcase (nth (1+ pos) args)) :keyword))))
         (language (let ((pos (position "--lang" args :test #'string=)))
                     (when pos (nth (1+ pos) args))))
         (results (hactar-monolith::find-concepts query 
                                                  :maturity maturity 
                                                  :language language
                                                  :limit 20)))
    (if results
        (progn
          (format t "~&Found ~A concept(s):~%" (length results))
          (dolist (row results)
            (format t "  [~A] ~A (~A)~%" 
                    (fourth row)  ; maturity
                    (second row)  ; title
                    (third row)))) ; path
        (format t "~&No concepts found.~%"))))

(define-command concept-mature (args)
  "Promote a concept to a higher maturity level.
Usage: /concept-mature <id-or-title> --to <maturity>"
  (let* ((id-or-title (first args))
         (new-maturity (let ((pos (position "--to" args :test #'string=)))
                         (when pos (intern (string-upcase (nth (1+ pos) args)) :keyword)))))
    (if (and id-or-title new-maturity)
        (progn
          (hactar-monolith::update-concept-maturity id-or-title new-maturity)
          (format t "~&Updated maturity to: ~A~%" new-maturity))
        (format t "~&Usage: /concept-mature <id-or-title> --to <maturity>~%"))))

;;*** Tangling Commands

(define-command tangle (args)
  "Extract code from org files.
Usage: /tangle --project <name> [--lang <language>] [--dry-run]
       /tangle --file <path> [--dry-run]"
  (let* ((project (let ((pos (position "--project" args :test #'string=)))
                    (when pos (nth (1+ pos) args))))
         (file (let ((pos (position "--file" args :test #'string=)))
                 (when pos (nth (1+ pos) args))))
         (lang (let ((pos (position "--lang" args :test #'string=)))
                 (when pos (nth (1+ pos) args))))
         (dry-run (member "--dry-run" args :test #'string=)))
    (cond
      (project
       (let ((result (hactar-monolith::tangle-project project :dry-run dry-run :lang lang)))
         (if (hactar-monolith::tangle-result-success-p result)
             (format t "~&Tangling complete. ~A file(s) written.~%"
                     (length (hactar-monolith::tangle-result-files-written result)))
             (format t "~&Tangling had errors: ~{~A~^, ~}~%"
                     (hactar-monolith::tangle-result-errors result)))))
      (file
       (let ((result (hactar-monolith::tangle-file file :dry-run dry-run)))
         (if (hactar-monolith::tangle-result-success-p result)
             (format t "~&Tangling complete.~%")
             (format t "~&Tangling had errors.~%"))))
      (t
       (format t "~&Usage: /tangle --project <name> | --file <path> [options]~%")))))

(define-command drift (args)
  "Detect drift between org and tangled code.
Usage: /drift --project <name>
       /drift --all"
  (let* ((project (let ((pos (position "--project" args :test #'string=)))
                    (when pos (nth (1+ pos) args))))
         (all (member "--all" args :test #'string=))
         (drifts (hactar-monolith::detect-drift :project project :all all)))
    (if drifts
        (progn
          (format t "~&Detected ~A file(s) with drift:~%" (length drifts))
          (dolist (drift drifts)
            (format t "  ~A (~A)~%"
                    (hactar-monolith::drift-info-file drift)
                    (hactar-monolith::drift-info-drift-type drift))))
        (format t "~&No drift detected.~%"))))

;;*** Graph Commands

(define-command graph-query (args)
  "Query the knowledge graph.
Usage: /graph-query <natural language query>"
  (let ((query (format nil "~{~A~^ ~}" args)))
    (if (string= query "")
        (format t "~&Usage: /graph-query <query>~%")
        (let ((results (hactar-monolith::query-graph query)))
          (if results
              (progn
                (format t "~&Results:~%")
                (dolist (row results)
                  (format t "  ~A~%" row)))
              (format t "~&No results.~%"))))))

;;*** Quick Capture

(define-command remember (args)
  "Quick capture of ideas with auto-linking.
Usage: /remember <thought>
       /remember --link-to <concept> <thought>"
  (let* ((link-to (let ((pos (position "--link-to" args :test #'string=)))
                    (when pos (nth (1+ pos) args))))
         (thought-args (if link-to
                           (subseq args (+ 2 (position "--link-to" args :test #'string=)))
                           args))
         (thought (format nil "~{~A~^ ~}" thought-args)))
    
    (if (not hactar-monolith::*monolith-path*)
      (format t "~&Monolith not initialized.~%")
    
    (let* ((today (multiple-value-bind (sec min hour day month year)
                      (decode-universal-time (get-universal-time))
                    (declare (ignore sec min hour))
                    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))
           (journal-path (merge-pathnames 
                          (format nil "Journal/~A.org" today)
                          hactar-monolith::*monolith-path*)))
      
      (ensure-directories-exist journal-path)
      
      (with-open-file (s journal-path 
                         :direction :output 
                         :if-exists :append
                         :if-does-not-exist :create)
        (when (zerop (file-length s))
          (format s "#+TITLE: Journal ~A~%~%" today))
        (format s "~%* ~A~%" 
                (multiple-value-bind (sec min hour)
                    (decode-universal-time (get-universal-time))
                  (format nil "~2,'0D:~2,'0D" hour min)))
        (format s "~A~%" thought)
        (when link-to
          (format s "~%Related: [[~A]]~%" link-to)))
      
      (format t "~&Captured to Journal/~A.org~%" today)
      (when link-to
        (format t "  Linked to: ~A~%" link-to))))))

;;*** Synthesis

(define-command synthesize (args)
  "Generate new code by composing existing concepts.
Usage: /synthesize <description> --draw-from <glob patterns>"
  (let* ((draw-from-pos (position "--draw-from" args :test #'string=))
         (description (format nil "~{~A~^ ~}" 
                              (if draw-from-pos
                                  (subseq args 0 draw-from-pos)
                                  args)))
         (draw-from (when draw-from-pos
                      (subseq args (1+ draw-from-pos)))))
    
    (if (not hactar-monolith::*monolith-path*)
      (format t "~&Monolith not initialized.~%")
    
    (let ((relevant-concepts nil))
      (dolist (pattern draw-from)
        (let ((files (directory (merge-pathnames pattern hactar-monolith::*monolith-path*))))
          (dolist (file files)
            (push (hactar-monolith::parse-org-file file) relevant-concepts))))
      
      (let ((context (with-output-to-string (s)
                       (format s "Drawing from these concepts in the knowledge base:~%~%")
                       (dolist (concept relevant-concepts)
                         (format s "=== ~A ===~%" (getf concept :path))
                         (dolist (heading (getf concept :headings))
                           (format s "~A~%" (getf heading :content)))
                         (format s "~%")))))
        
        (format t "~&Synthesizing from ~A concept(s)...~%~%" (length relevant-concepts))
        (get-llm-response (format nil "~A~%~%Task: ~A~%~%Generate code that draws from and composes the patterns and implementations shown in these concepts."
                                  context description)))))))

;;** Analyzers
(in-package :hactar)
(def-analyzer org-file-changed ((*file-event-hook*)) nil (pathname event-type)
  "Reindex org files when they change in the monolith."
  (when (and hactar-monolith::*monolith-path*
             (str:ends-with? ".org" (namestring pathname))
             (uiop:subpathp pathname hactar-monolith::*monolith-path*)
             (member event-type '(:file-added :file-changed)))
    (debug-log "Reindexing org file:" pathname)
    (let ((parsed (hactar-monolith::parse-org-file pathname)))
      (let ((relative (uiop:enough-pathname pathname hactar-monolith::*monolith-path*)))
        (cond
          ((str:starts-with? "Concepts" (namestring relative))
           (hactar-monolith::index-concept-file parsed))
          ((str:starts-with? "Projects" (namestring relative))
           (hactar-monolith::index-project-file parsed))
          ((str:starts-with? "Journal" (namestring relative))
           (hactar-monolith::index-journal-file parsed)))))))

;;** Rules

(defrule monolith-context-rule (*stack-changed-hook*) (stack)
  "Add monolith-aware instructions when org-mode knowledge base is detected."
  (when (and hactar-monolith::*monolith-path*
             (or (member "org-mode" stack :test #'string-equal)
                 (member "literate" stack :test #'string-equal)))
    "When working with this codebase:
- Code originates from org-mode files in the knowledge monolith
- Check for related concepts before implementing new features
- Prefer patterns already established in Concepts/
- Use /concept-find to discover reusable implementations
- Consider maturity levels: seedling < growing < evergreen < production
- Link new code back to concepts with [[id:...]] references"))

;;** CLI Sub-commands

(define-sub-command monolith.init (args)
  "Initialize a knowledge monolith.
Usage: hactar monolith.init [path]"
  (let ((path (or (first args) (uiop:getcwd))))
    (hactar-monolith::init-monolith path)
    (hactar-monolith::index-monolith)))

(define-sub-command monolith.index (args)
  "Index the knowledge monolith.
Usage: hactar monolith.index [--force]"
  (let ((force (member "--force" args :test #'string=)))
    (hactar-monolith::index-monolith :force force)))

(define-sub-command monolith.tangle (args)
  "Tangle code from org files.
Usage: hactar monolith.tangle --project <name> [--lang <language>] [--dry-run]"
  (let* ((project (let ((pos (position "--project" args :test #'string=)))
                    (when pos (nth (1+ pos) args))))
         (lang (let ((pos (position "--lang" args :test #'string=)))
                 (when pos (nth (1+ pos) args))))
         (dry-run (member "--dry-run" args :test #'string=)))
    (if project
        (let ((result (hactar-monolith::tangle-project project :dry-run dry-run :lang lang)))
          (if (hactar-monolith::tangle-result-success-p result)
              (progn
                (format t "Tangled ~A file(s)~%" 
                        (length (hactar-monolith::tangle-result-files-written result)))
                (uiop:quit 0))
              (progn
                (format t "Errors: ~{~A~^~%~}~%"
                        (hactar-monolith::tangle-result-errors result))
                (uiop:quit 1))))
        (progn
          (format t "Usage: hactar monolith.tangle --project <name>~%")
          (uiop:quit 1)))))

(define-sub-command monolith.drift (args)
  "Detect drift between org and tangled code.
Usage: hactar monolith.drift [--project <name>] [--all]"
  (let* ((project (let ((pos (position "--project" args :test #'string=)))
                    (when pos (nth (1+ pos) args))))
         (all (member "--all" args :test #'string=))
         (drifts (hactar-monolith::detect-drift :project project :all all)))
    (if drifts
        (progn
          (dolist (drift drifts)
            (format t "DRIFT: ~A (~A)~%"
                    (hactar-monolith::drift-info-file drift)
                    (hactar-monolith::drift-info-drift-type drift)))
          (uiop:quit 1))
        (progn
          (format t "No drift detected.~%")
          (uiop:quit 0)))))

(define-sub-command monolith.concept (args)
  "Manage concepts.
Usage: hactar monolith.concept new <title> [--maturity <level>]
       hactar monolith.concept find <query>
       hactar monolith.concept list"
  (let ((subcommand (first args))
        (rest-args (rest args)))
    (cond
      ((string= subcommand "new")
       (let* ((title (first rest-args))
              (maturity (let ((pos (position "--maturity" rest-args :test #'string=)))
                          (when pos (intern (string-upcase (nth (1+ pos) rest-args)) :keyword)))))
         (if title
             (let ((concept (hactar-monolith::create-concept title :maturity maturity)))
               (format t "Created: ~A~%" (hactar-monolith::org-concept-path concept)))
             (format t "Usage: hactar monolith.concept new <title>~%"))))
      
      ((string= subcommand "find")
       (let* ((query (format nil "~{~A~^ ~}" rest-args))
              (results (hactar-monolith::find-concepts query :limit 20)))
         (dolist (row results)
           (format t "~A [~A] ~A~%" (second row) (fourth row) (third row)))))
      
      ((string= subcommand "list")
       (let ((results (hactar-monolith::find-concepts nil :limit 100)))
         (dolist (row results)
           (format t "~A [~A]~%" (second row) (fourth row)))))
      
      (t
       (format t "Usage: hactar monolith.concept <new|find|list> [args]~%")))))

(define-sub-command monolith.synthesize (args)
  "Synthesize code from concepts.
Usage: hactar monolith.synthesize <description> --draw-from <patterns...>"
  (let* ((draw-from-pos (position "--draw-from" args :test #'string=))
         (description (format nil "~{~A~^ ~}"
                              (if draw-from-pos
                                  (subseq args 0 draw-from-pos)
                                  args)))
         (draw-from (when draw-from-pos
                      (subseq args (1+ draw-from-pos)))))
    
    (unless hactar-monolith::*monolith-path*
      (format t "Monolith not initialized.~%")
      (uiop:quit 1))
    
    ;; Find and gather concepts
    (let ((relevant-concepts nil))
      (dolist (pattern draw-from)
        (let ((files (directory (merge-pathnames pattern hactar-monolith::*monolith-path*))))
          (dolist (file files)
            (push (hactar-monolith::parse-org-file file) relevant-concepts))))
      
      (let ((context (with-output-to-string (s)
                       (dolist (concept relevant-concepts)
                         (format s "~%=== ~A ===~%" (getf concept :path))
                         (dolist (heading (getf concept :headings))
                           (format s "~A~%" (getf heading :content)))))))
        
        (let ((response (get-llm-response 
                         (format nil "~A~%~%Task: ~A" context description)
                         :stream nil)))
          (format t "~A~%" response))))))

;;** Agents

(defvar *monolith-agents*
  '((:librarian . "Organizes, tags, and connects notes")
    (:architect . "Designs systems by composing concepts")
    (:implementer . "Writes code using established patterns")
    (:critic . "Reviews code against principles"))
  "Available agents for the monolith.")

(define-command agent (args)
  "Interact with a monolith agent.
Usage: /agent <name> <task>
Available agents: librarian, architect, implementer, critic"
  (let* ((agent-name (first args))
         (task (format nil "~{~A~^ ~}" (rest args)))
         (agent-info (assoc (intern (string-upcase agent-name) :keyword) 
                            *monolith-agents*)))
    (if agent-info
        (progn
          (format t "~&[~A Agent] ~A~%~%" 
                  (string-capitalize agent-name)
                  (cdr agent-info))
          ;; Build agent context and prompt
          (let* ((system-prompt 
                   (format nil "You are the ~A agent for a knowledge monolith system.
Your role: ~A

The knowledge base is organized as:
- Concepts/ - Reusable ideas and implementations
- Projects/ - Concrete project implementations
- Journal/ - Temporal insights and learnings
- Principles/ - Guiding rules and constraints

Maturity levels: seedling < growing < evergreen < production

Help the user with their task while respecting these organizational principles."
                           agent-name (cdr agent-info))))
            (get-llm-response task :custom-system-prompt system-prompt)))
        (progn
          (format t "~&Unknown agent: ~A~%" agent-name)
          (format t "Available agents:~%")
          (dolist (a *monolith-agents*)
            (format t "  ~A - ~A~%" (car a) (cdr a)))))))


(format t "~&Loaded hactar-monolith mode.~%")
(format t "  Use /monolith-init to initialize a knowledge base.~%")
(format t "  Use /help for available commands.~%")
