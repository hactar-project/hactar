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

;;** In-Memory Monolith Registries & Macros
(defvar *concepts* (make-hash-table :test 'equal))
(defvar *projects* (make-hash-table :test 'equal))
(defvar *journal-entries* (make-hash-table :test 'equal))
(defvar *edges* nil)
(defvar *code-blocks* (make-hash-table :test 'equal))
(defvar *tangle-history* nil)

(defmacro defconcept (id &rest plist)
  `(setf (gethash ,id *concepts*)
         (make-org-concept
          :id ,id
          :title ,(getf plist :title)
          :path ,(let ((p (getf plist :path))) (when p `(pathname ,p)))
          :maturity ,(getf plist :maturity :seedling)
          :languages ',(getf plist :languages)
          :content ,(getf plist :content)
          :last-modified ,(getf plist :last-modified)
          :last-modified ,(getf plist :last-modified))))

(defmacro defproject (id &rest plist)
  `(setf (gethash ,id *projects*)
         (make-org-project
          :id ,id
          :name ,(getf plist :name)
          :path ,(let ((p (getf plist :path))) (when p `(pathname ,p)))
          :repos ',(getf plist :repos)
          :sync-config ',(getf plist :sync-config))))

(defmacro defjournal-entry (id &rest plist)
  `(setf (gethash ,id *journal-entries*)
         (make-org-journal-entry
          :id ,id
          :date ,(getf plist :date)
          :path ,(let ((p (getf plist :path))) (when p `(pathname ,p)))
          :insights ',(getf plist :insights))))

(defmacro defedge (from-id to-id relation &rest plist)
  `(push (make-org-edge
          :from-id ,from-id
          :to-id ,to-id
          :relation ,relation
          :metadata ',(getf plist :metadata))
         *edges*))

(defmacro defcode-block (id &rest plist)
  `(setf (gethash ,id *code-blocks*)
         (make-org-code-block
          :id ,id
          :concept-id ,(getf plist :concept-id)
          :language ,(getf plist :language)
          :content ,(getf plist :content)
          :tangle-target ,(getf plist :tangle-target)
          :project ,(getf plist :project)
          :checksum ,(getf plist :checksum)
          :line-start ,(getf plist :line-start)
          :line-end ,(getf plist :line-end))))

(defmacro deftangle-history (code-block-id target-path tangled-at checksum)
  `(push (list :code-block-id ,code-block-id
               :target-path ,target-path
               :tangled-at ,tangled-at
               :checksum ,checksum)
         *tangle-history*))

(defun persist-monolith ()
  "Serialize the in-memory monolith state into the .hactar.monolith.lisp file."
  (let ((lisp-path (merge-pathnames ".hactar.monolith.lisp" *monolith-path*)))
    (with-open-file (s lisp-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s ";;; Hactar Monolith State (generated automatically)~%~%")
      ;; Write concepts
      (maphash (lambda (id c)
                 (format s "(hactar-monolith::defconcept ~S~%  :title ~S~%  :path ~S~%  :maturity ~S~%  :languages '~S~%  :content ~S~%  :last-modified ~S)~%~%"
                         id
                         (org-concept-title c)
                         (when (org-concept-path c) (namestring (org-concept-path c)))
                         (org-concept-maturity c)
                         (org-concept-languages c)
                         (org-concept-content c)
                         (org-concept-last-modified c)))
               *concepts*)
      ;; Write projects
      (maphash (lambda (id p)
                 (format s "(hactar-monolith::defproject ~S~%  :name ~S~%  :path ~S~%  :repos '~S~%  :sync-config '~S)~%~%"
                         id
                         (org-project-name p)
                         (when (org-project-path p) (namestring (org-project-path p)))
                         (org-project-repos p)
                         (org-project-sync-config p)))
               *projects*)
      ;; Write journal entries
      (maphash (lambda (id j)
                 (format s "(hactar-monolith::defjournal-entry ~S~%  :date ~S~%  :path ~S~%  :insights '~S)~%~%"
                         id
                         (org-journal-entry-date j)
                         (when (org-journal-entry-path j) (namestring (org-journal-entry-path j)))
                         (org-journal-entry-insights j)))
               *journal-entries*)
      ;; Write edges
      (dolist (e *edges*)
        (format s "(hactar-monolith::defedge ~S ~S ~S~%  :metadata '~S)~%~%"
                (org-edge-from-id e)
                (org-edge-to-id e)
                (org-edge-relation e)
                (org-edge-metadata e)))
      ;; Write code blocks
      (maphash (lambda (id cb)
                 (format s "(hactar-monolith::defcode-block ~S~%  :concept-id ~S~%  :language ~S~%  :content ~S~%  :tangle-target ~S~%  :project ~S~%  :checksum ~S~%  :line-start ~S~%  :line-end ~S)~%~%"
                         id
                         (org-code-block-concept-id cb)
                         (org-code-block-language cb)
                         (org-code-block-content cb)
                         (org-code-block-tangle-target cb)
                         (org-code-block-project cb)
                         (org-code-block-checksum cb)
                         (org-code-block-line-start cb)
                         (org-code-block-line-end cb)))
               *code-blocks*)
      ;; Write tangle history
      (dolist (h *tangle-history*)
        (format s "(hactar-monolith::deftangle-history ~S ~S ~S ~S)~%~%"
                (getf h :code-block-id)
                (getf h :target-path)
                (getf h :tangled-at)
                (getf h :checksum))))))

;;** Monolith Initialization

(defun init-monolith (path)
  "Initialize a new knowledge monolith at PATH."
  (let ((root (uiop:ensure-directory-pathname path)))
    (setf *monolith-path* root)
    (setf *monolith-db-path* (merge-pathnames ".hactar.monolith.lisp" root))

    (dolist (dir-spec *monolith-structure*)
      (let ((dir-path (merge-pathnames (cdr dir-spec) root)))
        (ensure-directories-exist dir-path)
        (format t "Created: ~A~%" (uiop:native-namestring dir-path))))

    (let ((gitignore (merge-pathnames ".gitignore" root)))
      (unless (probe-file gitignore)
        (with-open-file (s gitignore :direction :output :if-does-not-exist :create)
          (format s ".hactar.monolith.lisp~%"))))

    (init-monolith-db)
    (persist-monolith)

    (format t "~&Monolith initialized at: ~A~%" (uiop:native-namestring root))
    root))

(defun init-monolith-db ()
  "Initialize the in-memory monolith registries and load persisted state."
  (clrhash *concepts*)
  (clrhash *projects*)
  (clrhash *journal-entries*)
  (setf *edges* nil)
  (clrhash *code-blocks*)
  (setf *tangle-history* nil)
  (let ((lisp-path (merge-pathnames ".hactar.monolith.lisp" *monolith-path*)))
    (when (and lisp-path (probe-file lisp-path))
      (load lisp-path))))

;;** Indexing

(defun index-monolith (&key force &allow-other-keys)
  "Index the entire knowledge monolith."
  (declare (ignore force))
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

    (persist-monolith)
    (format t "~&Indexed: ~A concepts, ~A projects, ~A journal entries, ~A code blocks~%"
            concepts-indexed projects-indexed journal-indexed code-blocks-indexed)))

(defun index-concept-file (parsed)
  "Index a concept file structurally."
  (let* ((props (getf parsed :properties))
         (path (getf parsed :path))
         (headings (getf parsed :headings))
         (first-heading (first headings))
         (id (or (cdr (assoc :ID props))
                 (format nil "~A" (uuid:make-v4-uuid))))
         (title (or (getf first-heading :title)
                    (pathname-name path)))
         (maturity-str (or (cdr (assoc :MATURITY props)) "seedling"))
         (maturity (intern (string-upcase maturity-str) :keyword))
         (languages-str (cdr (assoc :LANGUAGES props)))
         (languages (when languages-str
                      (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                              (str:split #\, languages-str))))
         (content (getf first-heading :content)))

    (setf (gethash id *concepts*)
          (make-org-concept
           :id id
           :title title
           :path path
           :maturity maturity
           :languages languages
           :content content
           :last-modified (get-universal-time)))

    (dolist (link (getf parsed :links))
      (index-edge id (cdr link) (car link)))))

(defun index-project-file (parsed)
  "Index a project file structurally."
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

    (setf (gethash id *projects*)
          (make-org-project
           :id id
           :name name
           :path path
           :repos repos))

    (dolist (link (getf parsed :links))
      (when (eq (car link) :semantic)
        (index-edge id (cdr link) :implements)))))

(defun index-journal-file (parsed)
  "Index a journal entry structurally."
  (let* ((props (getf parsed :properties))
         (path (getf parsed :path))
         (id (or (cdr (assoc :ID props))
                 (format nil "~A" (uuid:make-v4-uuid))))
         (date (or (cdr (assoc :DATE props))
                   (pathname-name path))))

    (setf (gethash id *journal-entries*)
          (make-org-journal-entry
           :id id
           :date date
           :path path))

    (dolist (link (getf parsed :links))
      (index-edge id (cdr link) :mentions))))

(defun index-code-block (block source-path)
  "Index a code block structurally."
  (declare (ignore source-path))
  (setf (gethash (org-code-block-id block) *code-blocks*) block))

(defun index-edge (from-id to-id relation)
  "Index an edge between nodes."
  (let ((rel-keyword (intern (string-upcase (string relation)) :keyword)))
    (unless (some (lambda (e)
                    (and (string= (org-edge-from-id e) from-id)
                         (string= (org-edge-to-id e) to-id)
                         (eq (org-edge-relation e) rel-keyword)))
                  *edges*)
      (push (make-org-edge :from-id from-id :to-id to-id :relation rel-keyword)
            *edges*))))

;;** Tangling

(defun tangle-project (project-name &key dry-run lang)
  "Tangle all code blocks for a project."
  (unless *monolith-path*
    (error "Monolith not initialized."))

  (format t "~&Tangling project: ~A~%" project-name)

  (let ((result (make-tangle-result :success-p t)))
    (maphash (lambda (block-id block)
               (when (and (string= (org-code-block-project block) project-name)
                          (org-code-block-tangle-target block)
                          (or (null lang) (string= (org-code-block-language block) lang)))
                 (let* ((content (org-code-block-content block))
                        (tangle-target (org-code-block-tangle-target block))
                        (checksum (org-code-block-checksum block))
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
                             (push (list :code-block-id block-id
                                         :target-path (namestring target-path)
                                         :tangled-at (get-universal-time)
                                         :checksum checksum)
                                   *tangle-history*)

                             (format t "  ✓ ~A~%" target-path))
                         (error (e)
                           (push (format nil "~A: ~A" target-path e)
                                 (tangle-result-errors result))
                           (setf (tangle-result-success-p result) nil)))))))
             *code-blocks*)
    (unless dry-run
      (persist-monolith))
    (nhooks:run-hook *tangle-completed-hook* result)
    result))

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
  (let ((drifts nil)
        (latest-tangles (make-hash-table :test 'equal)))
    ;; Group tangle history by target path and keep the one with max tangled-at
    (dolist (th *tangle-history*)
      (let* ((target-path (getf th :target-path))
             (existing (gethash target-path latest-tangles)))
        (if existing
            (when (> (getf th :tangled-at) (getf existing :tangled-at))
              (setf (gethash target-path latest-tangles) th))
            (setf (gethash target-path latest-tangles) th))))

    (maphash (lambda (target-path th)
               (let* ((cb-id (getf th :code-block-id))
                      (cb (gethash cb-id *code-blocks*)))
                 (when cb
                   (when (or all (and project (string= (org-code-block-project cb) project)))
                     (let* ((org-checksum (org-code-block-checksum cb))
                            (tangled-checksum (getf th :checksum)))
                       (when (probe-file target-path)
                         (let* ((file-content (uiop:read-file-string target-path))
                                (current-checksum (compute-checksum file-content)))
                           (unless (string= current-checksum tangled-checksum)
                             (push (make-drift-info
                                    :file target-path
                                    :org-checksum org-checksum
                                    :file-checksum current-checksum
                                    :drift-type :modified)
                                   drifts)))))))))
             latest-tangles)
    (when drifts
      (nhooks:run-hook *drift-detected-hook* drifts))
    drifts))

;;** Concept Operations

(defun find-concepts (query &key maturity language limit)
  "Find concepts matching a query."
  (let ((results nil))
    (maphash (lambda (id c)
               (let ((title (org-concept-title c))
                     (content (org-concept-content c))
                     (mat (org-concept-maturity c))
                     (langs (org-concept-languages c)))
                 (when (and (or (null query)
                                (search query title :test #'char-equal)
                                (and content (search query content :test #'char-equal)))
                            (or (null maturity)
                                (eq (intern (string-upcase (string maturity)) :keyword)
                                    (intern (string-upcase (string mat)) :keyword)))
                            (or (null language)
                                (member language langs :test #'string-equal)))
                   (push (list id
                               title
                               (when (org-concept-path c) (namestring (org-concept-path c)))
                               (string-downcase (string mat))
                               (when langs (cl-json:encode-json-to-string langs)))
                         results))))
             *concepts*)
    (if limit
        (subseq results 0 (min limit (length results)))
        results)))

(defun get-concept (id)
  "Get a concept by ID."
  (let ((c (gethash id *concepts*)))
    (when c
      (list (org-concept-id c)
            (org-concept-title c)
            (when (org-concept-path c) (namestring (org-concept-path c)))
            (string-downcase (string (org-concept-maturity c)))
            (when (org-concept-languages c) (cl-json:encode-json-to-string (org-concept-languages c)))
            (org-concept-content c)
            nil ; embedding
            (org-concept-last-modified c)
            (org-concept-last-modified c)))))

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
      (format s ":MATURITY: ~A~%" (string-downcase (string maturity)))
      (when languages
        (format s ":LANGUAGES: ~{~A~^, ~}~%" languages))
      (format s ":END:~%~%")
      (when content
        (format s "~A~%" content)))

    (setf (gethash id *concepts*) concept)
    (persist-monolith)

    (nhooks:run-hook *concept-created-hook* concept)
    concept))

(defun update-concept-maturity (id new-maturity)
  "Update a concept's maturity level."
  (let* ((c (gethash id *concepts*)))
    (when c
      (let ((old-maturity (org-concept-maturity c))
            (path (org-concept-path c)))
        (setf (org-concept-maturity c) new-maturity)
        (setf (org-concept-last-modified c) (get-universal-time))

        (when (and path (probe-file path))
          (let* ((content (uiop:read-file-string path))
                 (new-content (cl-ppcre:regex-replace
                               ":MATURITY: \\w+"
                               content
                               (format nil ":MATURITY: ~A" (string-downcase (string new-maturity))))))
            (with-open-file (s path :direction :output :if-exists :supersede)
              (write-string new-content s))))

        (persist-monolith)

        (nhooks:run-hook *concept-matured-hook*
                         c
                         (intern (string-upcase (string old-maturity)) :keyword)
                         new-maturity)))))

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
                 (dolist (e *edges*)
                   (when (and (string= (org-edge-from-id e) current-id)
                              (or (null relation)
                                  (string-equal (string (org-edge-relation e)) (string relation))))
                     (let ((to-id (org-edge-to-id e))
                           (rel (string (org-edge-relation e))))
                       (push (list :from current-id :to to-id :relation rel) results)
                       (traverse to-id (1- current-depth))))))))
      (traverse id depth))

    (nreverse results)))

(defun query-graph (query-string)
  "Execute a natural language query against the knowledge graph."
  (cond
    ;; "concepts never implemented"
    ((search "never implemented" query-string)
     (let ((results nil))
       (maphash (lambda (id c)
                  (unless (some (lambda (e)
                                  (and (eq (org-edge-relation e) :implements)
                                       (string= (org-edge-from-id e) id)))
                                *edges*)
                    (push (list id (org-concept-title c) (string-downcase (string (org-concept-maturity c))))
                          results)))
                *concepts*)
       results))

    ((search "sharing concepts" query-string)
     (let ((project-shares (make-hash-table :test 'equal))
           (results nil))
       (dolist (e1 *edges*)
         (when (eq (org-edge-relation e1) :implements)
           (dolist (e2 *edges*)
             (when (and (eq (org-edge-relation e2) :implements)
                        (string= (org-edge-to-id e1) (org-edge-to-id e2))
                        (not (string= (org-edge-from-id e1) (org-edge-from-id e2))))
               (let* ((p1 (gethash (org-edge-from-id e1) *projects*))
                      (p2 (gethash (org-edge-from-id e2) *projects*)))
                 (when (and p1 p2)
                   (let* ((name1 (org-project-name p1))
                          (name2 (org-project-name p2))
                          (pair (if (string< name1 name2) (cons name1 name2) (cons name2 name1))))
                     (incf (gethash pair project-shares 0)))))))))
       (maphash (lambda (pair count)
                  (push (list (car pair) (cdr pair) (/ count 2)) results))
                project-shares)
       (sort results #'> :key #'third)))

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
  (if (not hactar-monolith::*monolith-path*)
    (format t "~&Monolith not initialized. Run /monolith-init first.~%")
    (progn
      (format t "~&Monolith Status~%")
      (format t "===============~%")
      (format t "Path: ~A~%" (uiop:native-namestring hactar-monolith::*monolith-path*))

      (let ((concept-count (hash-table-count hactar-monolith::*concepts*))
            (project-count (hash-table-count hactar-monolith::*projects*))
            (journal-count (hash-table-count hactar-monolith::*journal-entries*))
            (block-count (hash-table-count hactar-monolith::*code-blocks*))
            (edge-count (length hactar-monolith::*edges*)))

          (format t "~%Counts:~%")
          (format t "  Concepts: ~A~%" concept-count)
          (format t "  Projects: ~A~%" project-count)
          (format t "  Journal Entries: ~A~%" journal-count)
          (format t "  Code Blocks: ~A~%" block-count)
          (format t "  Graph Edges: ~A~%" edge-count)

          ;; Maturity distribution
          (format t "~%Concept Maturity:~%")
          (let ((maturity-counts (make-hash-table :test 'equal)))
            (maphash (lambda (id c)
                       (declare (ignore id))
                       (let ((m (string-downcase (string (org-concept-maturity c)))))
                         (incf (gethash m maturity-counts 0))))
                     hactar-monolith::*concepts*)
            (maphash (lambda (m count)
                       (format t "  ~A: ~A~%" m count))
                     maturity-counts))))))

(define-command monolith-index (args)
  "Index the knowledge monolith.
Usage: /monolith-index [--force]"
  (let ((force (member "--force" args :test #'string=)))
    (hactar-monolith::index-monolith :force force)))



;;*** Tangling Commands

(define-command lit.tangle (args)
  "Extract code from org files.
Usage: /lit.tangle --project <name> [--lang <language>] [--dry-run]
       /lit.tangle --file <path> [--dry-run]"
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
       (format t "~&Usage: /lit.tangle --project <name> | --file <path> [options]~%")))))

;;*** Synthesis

(define-command "code.synthesize" (args)
  "Generate new code by composing existing concepts.
Usage: /code.synthesize <description> --draw-from <glob patterns>"
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
