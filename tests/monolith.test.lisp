(in-package :hactar-tests)

;;* Monolith Tests

(def-suite monolith-tests
  :description "Tests for Hactar monolith (org-centric literate programming mode)")

(in-suite monolith-tests)

;;* Test Helpers

(defvar *test-monolith-root* nil
  "Temporary directory for monolith tests.")

(defun make-test-monolith-root ()
  "Create a fresh temp directory for monolith tests."
  (let ((dir (merge-pathnames
              (format nil "hactar-monolith-test-~A/" (uuid:make-v4-uuid))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun cleanup-test-monolith ()
  "Remove the test monolith directory."
  (when (and *test-monolith-root* (uiop:directory-exists-p *test-monolith-root*))
    (uiop:delete-directory-tree *test-monolith-root* :validate t :if-does-not-exist :ignore))
  (setf *test-monolith-root* nil)
  (setf hactar-monolith::*monolith-path* nil)
  (setf hactar-monolith::*monolith-db-path* nil))

(defmacro with-test-monolith (&body body)
  "Execute BODY with a fresh test monolith initialized."
  `(let ((*test-monolith-root* (make-test-monolith-root)))
     (unwind-protect
          (progn
            (hactar-monolith::init-monolith *test-monolith-root*)
            ,@body)
       (cleanup-test-monolith))))

(defun write-test-org-file (relative-path content)
  "Write an org file into the test monolith."
  (let ((full-path (merge-pathnames relative-path *test-monolith-root*)))
    (ensure-directories-exist full-path)
    (with-open-file (s full-path :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :external-format :utf-8)
      (write-string content s))
    full-path))

;;* Data Structure Tests

(test monolith-struct-org-concept
  "Test org-concept struct creation and accessors."
  (let ((concept (hactar-monolith::make-org-concept
                  :id "test-id"
                  :title "Test Concept"
                  :maturity :growing
                  :languages '("lisp" "python")
                  :content "Some content")))
    (is (string= "test-id" (hactar-monolith::org-concept-id concept)))
    (is (string= "Test Concept" (hactar-monolith::org-concept-title concept)))
    (is (eq :growing (hactar-monolith::org-concept-maturity concept)))
    (is (equal '("lisp" "python") (hactar-monolith::org-concept-languages concept)))
    (is (string= "Some content" (hactar-monolith::org-concept-content concept)))
    (is (null (hactar-monolith::org-concept-embedding concept)))
    (is (null (hactar-monolith::org-concept-extracted-to concept)))
    (is (null (hactar-monolith::org-concept-sources concept)))))

(test monolith-struct-org-project
  "Test org-project struct creation."
  (let ((proj (hactar-monolith::make-org-project
               :id "proj-1"
               :name "My Project"
               :repos '("git@github.com:user/repo.git"))))
    (is (string= "proj-1" (hactar-monolith::org-project-id proj)))
    (is (string= "My Project" (hactar-monolith::org-project-name proj)))
    (is (equal '("git@github.com:user/repo.git") (hactar-monolith::org-project-repos proj)))))

(test monolith-struct-org-journal-entry
  "Test org-journal-entry struct creation."
  (let ((entry (hactar-monolith::make-org-journal-entry
                :id "j-1"
                :date "2025-01-15"
                :mentions '("concept-a" "concept-b"))))
    (is (string= "j-1" (hactar-monolith::org-journal-entry-id entry)))
    (is (string= "2025-01-15" (hactar-monolith::org-journal-entry-date entry)))
    (is (equal '("concept-a" "concept-b") (hactar-monolith::org-journal-entry-mentions entry)))))

(test monolith-struct-org-edge
  "Test org-edge struct creation."
  (let ((edge (hactar-monolith::make-org-edge
               :from-id "a"
               :to-id "b"
               :relation :implements)))
    (is (string= "a" (hactar-monolith::org-edge-from-id edge)))
    (is (string= "b" (hactar-monolith::org-edge-to-id edge)))
    (is (eq :implements (hactar-monolith::org-edge-relation edge)))))

(test monolith-struct-org-code-block
  "Test org-code-block struct creation."
  (let ((block (hactar-monolith::make-org-code-block
                :id "block-1"
                :concept-id "concept-1"
                :language "lisp"
                :content "(defun hello () (print \"hi\"))"
                :tangle-target "src/hello.lisp"
                :project "my-project"
                :checksum "ABC123"
                :line-start 10
                :line-end 15)))
    (is (string= "block-1" (hactar-monolith::org-code-block-id block)))
    (is (string= "concept-1" (hactar-monolith::org-code-block-concept-id block)))
    (is (string= "lisp" (hactar-monolith::org-code-block-language block)))
    (is (string= "src/hello.lisp" (hactar-monolith::org-code-block-tangle-target block)))
    (is (string= "my-project" (hactar-monolith::org-code-block-project block)))
    (is (= 10 (hactar-monolith::org-code-block-line-start block)))
    (is (= 15 (hactar-monolith::org-code-block-line-end block)))))

(test monolith-struct-tangle-result
  "Test tangle-result struct creation."
  (let ((result (hactar-monolith::make-tangle-result
                 :success-p t
                 :files-written '("/path/a.lisp" "/path/b.lisp")
                 :errors nil
                 :checksums '(("/path/a.lisp" . "abc") ("/path/b.lisp" . "def")))))
    (is-true (hactar-monolith::tangle-result-success-p result))
    (is (= 2 (length (hactar-monolith::tangle-result-files-written result))))
    (is (null (hactar-monolith::tangle-result-errors result)))))

(test monolith-struct-drift-info
  "Test drift-info struct creation."
  (let ((drift (hactar-monolith::make-drift-info
                :file "src/main.lisp"
                :org-checksum "aaa"
                :file-checksum "bbb"
                :drift-type :modified)))
    (is (string= "src/main.lisp" (hactar-monolith::drift-info-file drift)))
    (is (eq :modified (hactar-monolith::drift-info-drift-type drift)))))

;;* Org Parsing Tests

(test monolith-parse-org-properties
  "Test parsing :PROPERTIES: drawer from org content."
  (let ((content ":PROPERTIES:
:ID: my-concept-id
:MATURITY: evergreen
:LANGUAGES: lisp, python
:END:

Some body content."))
    (let ((props (hactar-monolith::parse-org-properties content)))
      (is (string= "my-concept-id" (cdr (assoc :ID props))))
      (is (string= "evergreen" (cdr (assoc :MATURITY props))))
      (is (string= "lisp, python" (cdr (assoc :LANGUAGES props)))))))

(test monolith-parse-org-properties-empty
  "Test parsing org content with no properties drawer."
  (let ((props (hactar-monolith::parse-org-properties "Just some text.")))
    (is (null props))))

(test monolith-parse-org-heading
  "Test parsing org heading lines."
  (let ((h1 (hactar-monolith::parse-org-heading "* Top Level"))
        (h2 (hactar-monolith::parse-org-heading "** Second Level"))
        (h3 (hactar-monolith::parse-org-heading "*** Third Level"))
        (not-heading (hactar-monolith::parse-org-heading "Not a heading")))
    (is (equal '(1 . "Top Level") h1))
    (is (equal '(2 . "Second Level") h2))
    (is (equal '(3 . "Third Level") h3))
    (is (null not-heading))))

(test monolith-parse-org-heading-empty-title
  "Test parsing a heading with just stars."
  (let ((h (hactar-monolith::parse-org-heading "*")))
    (is-true h)
    (is (= 1 (car h)))
    (is (string= "" (cdr h)))))

(test monolith-parse-org-links-id
  "Test parsing [[id:...]] links."
  (let ((links (hactar-monolith::parse-org-links "See [[id:abc-123]] for details.")))
    (is (= 1 (length links)))
    (is (eq :id (car (first links))))
    (is (string= "abc-123" (cdr (first links))))))

(test monolith-parse-org-links-file
  "Test parsing [[file:...]] links."
  (let ((links (hactar-monolith::parse-org-links "Refer to [[file:src/main.lisp]].")))
    (is (= 1 (length links)))
    (is (eq :file (car (first links))))
    (is (string= "src/main.lisp" (cdr (first links))))))

(test monolith-parse-org-links-semantic
  "Test parsing semantic links like [[Concepts/Auth/JWT]]."
  (let ((links (hactar-monolith::parse-org-links "Uses [[Concepts/Auth/JWT]] pattern.")))
    (is (= 1 (length links)))
    (is (eq :semantic (car (first links))))
    (is (string= "Concepts/Auth/JWT" (cdr (first links))))))

(test monolith-parse-org-links-mixed
  "Test parsing multiple link types."
  (let ((links (hactar-monolith::parse-org-links
                "See [[id:abc]] and [[file:foo.org]] and [[Concepts/Bar]].")))
    (is (= 3 (length links)))))

(test monolith-parse-org-links-ignores-http
  "Test that http links are not parsed as semantic links."
  (let ((links (hactar-monolith::parse-org-links "Visit [[https://example.com]].")))
    ;; https links should not be captured as semantic links
    (is (= 0 (length links)))))

(test monolith-parse-org-code-blocks-basic
  "Test parsing basic code blocks."
  (let* ((content "* My Heading
Some text.
#+BEGIN_SRC lisp
(defun hello ()
  (print \"hello\"))
#+END_SRC
More text.")
         (blocks (hactar-monolith::parse-org-code-blocks content "test.org")))
    (is (= 1 (length blocks)))
    (let ((block (first blocks)))
      (is (string= "lisp" (hactar-monolith::org-code-block-language block)))
      (is (search "defun hello" (hactar-monolith::org-code-block-content block)))
      (is-true (hactar-monolith::org-code-block-checksum block))
      (is (null (hactar-monolith::org-code-block-tangle-target block))))))

(test monolith-parse-org-code-blocks-with-tangle
  "Test parsing code blocks with :tangle header arg."
  (let* ((content "#+BEGIN_SRC python :tangle src/main.py :project my-project
print('hello')
#+END_SRC")
         (blocks (hactar-monolith::parse-org-code-blocks content)))
    (is (= 1 (length blocks)))
    (let ((block (first blocks)))
      (is (string= "python" (hactar-monolith::org-code-block-language block)))
      (is (string= "src/main.py" (hactar-monolith::org-code-block-tangle-target block)))
      (is (string= "my-project" (hactar-monolith::org-code-block-project block))))))

(test monolith-parse-org-code-blocks-tangle-no
  "Test that :tangle no is ignored."
  (let* ((content "#+BEGIN_SRC lisp :tangle no
(+ 1 2)
#+END_SRC")
         (blocks (hactar-monolith::parse-org-code-blocks content)))
    (is (= 1 (length blocks)))
    (is (null (hactar-monolith::org-code-block-tangle-target (first blocks))))))

(test monolith-parse-org-code-blocks-multiple
  "Test parsing multiple code blocks."
  (let* ((content "#+BEGIN_SRC lisp
(first-block)
#+END_SRC
Some text.
#+BEGIN_SRC python
second_block()
#+END_SRC")
         (blocks (hactar-monolith::parse-org-code-blocks content)))
    (is (= 2 (length blocks)))
    (is (string= "lisp" (hactar-monolith::org-code-block-language (first blocks))))
    (is (string= "python" (hactar-monolith::org-code-block-language (second blocks))))))

(test monolith-parse-org-code-blocks-line-numbers
  "Test that line numbers are tracked for code blocks."
  (let* ((content "line 1
line 2
#+BEGIN_SRC lisp
(hello)
#+END_SRC
line 6")
         (blocks (hactar-monolith::parse-org-code-blocks content)))
    (is (= 1 (length blocks)))
    (let ((block (first blocks)))
      (is (= 3 (hactar-monolith::org-code-block-line-start block)))
      (is (= 5 (hactar-monolith::org-code-block-line-end block))))))

(test monolith-parse-org-file-full
  "Test full org file parsing."
  (with-test-monolith
    (let* ((path (write-test-org-file
                  "Concepts/Auth.org"
                  "#+TITLE: Authentication
:PROPERTIES:
:ID: auth-concept
:MATURITY: growing
:LANGUAGES: lisp, python
:END:

* Authentication

This concept covers auth patterns.

See [[id:jwt-concept]] and [[Concepts/Crypto]].

#+BEGIN_SRC lisp :tangle src/auth.lisp :project my-proj
(defun authenticate (user pass)
  (check-credentials user pass))
#+END_SRC
"))
           (parsed (hactar-monolith::parse-org-file path)))
      ;; Properties
      (let ((props (getf parsed :properties)))
        (is (string= "auth-concept" (cdr (assoc :ID props))))
        (is (string= "growing" (cdr (assoc :MATURITY props)))))
      ;; Links
      (let ((links (getf parsed :links)))
        (is (>= (length links) 2)))
      ;; Code blocks
      (let ((blocks (getf parsed :code-blocks)))
        (is (= 1 (length blocks)))
        (is (string= "src/auth.lisp"
                      (hactar-monolith::org-code-block-tangle-target (first blocks)))))
      ;; Headings
      (let ((headings (getf parsed :headings)))
        (is (>= (length headings) 1))))))

(test monolith-compute-checksum
  "Test checksum computation."
  (let ((cs1 (hactar-monolith::compute-checksum "hello"))
        (cs2 (hactar-monolith::compute-checksum "hello"))
        (cs3 (hactar-monolith::compute-checksum "world")))
    (is (string= cs1 cs2))
    (is (not (string= cs1 cs3)))))

;;* Initialization Tests

(test monolith-init-creates-directories
  "Test that init-monolith creates the directory structure."
  (with-test-monolith
    (is-true (uiop:directory-exists-p
              (merge-pathnames "Concepts/" *test-monolith-root*)))
    (is-true (uiop:directory-exists-p
              (merge-pathnames "Projects/" *test-monolith-root*)))
    (is-true (uiop:directory-exists-p
              (merge-pathnames "Journal/" *test-monolith-root*)))
    (is-true (uiop:directory-exists-p
              (merge-pathnames "Principles/" *test-monolith-root*)))))

(test monolith-init-creates-db
  "Test that init-monolith creates the SQLite database."
  (with-test-monolith
    (is-true (probe-file hactar-monolith::*monolith-db-path*))))

(test monolith-init-creates-gitignore
  "Test that init-monolith creates a .gitignore."
  (with-test-monolith
    (let ((gitignore (merge-pathnames ".gitignore" *test-monolith-root*)))
      (is-true (probe-file gitignore))
      (let ((content (uiop:read-file-string gitignore)))
        (is (search ".hactar-monolith.db" content))))))

(test monolith-init-sets-paths
  "Test that init-monolith sets global paths."
  (with-test-monolith
    (is-true hactar-monolith::*monolith-path*)
    (is-true hactar-monolith::*monolith-db-path*)))

(test monolith-init-db-tables-exist
  "Test that the database has the expected tables."
  (with-test-monolith
    (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
      ;; Check that tables exist by querying them
      (is (listp (sqlite:execute-to-list db "SELECT * FROM concepts LIMIT 0")))
      (is (listp (sqlite:execute-to-list db "SELECT * FROM projects LIMIT 0")))
      (is (listp (sqlite:execute-to-list db "SELECT * FROM journal_entries LIMIT 0")))
      (is (listp (sqlite:execute-to-list db "SELECT * FROM edges LIMIT 0")))
      (is (listp (sqlite:execute-to-list db "SELECT * FROM code_blocks LIMIT 0")))
      (is (listp (sqlite:execute-to-list db "SELECT * FROM tangle_history LIMIT 0"))))))

;;* Concept Operations Tests

(test monolith-create-concept-basic
  "Test creating a basic concept."
  (with-test-monolith
    (let ((concept (hactar-monolith::create-concept "Rate Limiting")))
      (is-true (hactar-monolith::org-concept-id concept))
      (is (string= "Rate Limiting" (hactar-monolith::org-concept-title concept)))
      (is (eq :seedling (hactar-monolith::org-concept-maturity concept)))
      (is-true (probe-file (hactar-monolith::org-concept-path concept))))))

(test monolith-create-concept-with-options
  "Test creating a concept with maturity and languages."
  (with-test-monolith
    (let ((concept (hactar-monolith::create-concept "JWT Auth"
                     :maturity :evergreen
                     :languages '("lisp" "javascript")
                     :content "JSON Web Token authentication pattern.")))
      (is (eq :evergreen (hactar-monolith::org-concept-maturity concept)))
      (is (equal '("lisp" "javascript") (hactar-monolith::org-concept-languages concept)))
      ;; Verify the org file content
      (let ((file-content (uiop:read-file-string (hactar-monolith::org-concept-path concept))))
        (is (search "JWT Auth" file-content))
        (is (search ":MATURITY: EVERGREEN" file-content))
        (is (search "JSON Web Token" file-content))))))

(test monolith-create-concept-stored-in-db
  "Test that created concepts are stored in the database."
  (with-test-monolith
    (let ((concept (hactar-monolith::create-concept "Database Pooling")))
      (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
        (let ((rows (sqlite:execute-to-list db
                      "SELECT id, title, maturity FROM concepts WHERE id = ?"
                      (hactar-monolith::org-concept-id concept))))
          (is (= 1 (length rows)))
          (is (string= "Database Pooling" (second (first rows)))))))))

(test monolith-find-concepts-by-query
  "Test finding concepts by title query."
  (with-test-monolith
    (hactar-monolith::create-concept "Authentication")
    (hactar-monolith::create-concept "Authorization")
    (hactar-monolith::create-concept "Caching")
    (let ((results (hactar-monolith::find-concepts "Auth")))
      (is (= 2 (length results))))))

(test monolith-find-concepts-by-maturity
  "Test finding concepts filtered by maturity."
  (with-test-monolith
    (hactar-monolith::create-concept "Concept A" :maturity :seedling)
    (hactar-monolith::create-concept "Concept B" :maturity :evergreen)
    (hactar-monolith::create-concept "Concept C" :maturity :seedling)
    (let ((seedlings (hactar-monolith::find-concepts nil :maturity :seedling)))
      (is (= 2 (length seedlings))))
    (let ((evergreens (hactar-monolith::find-concepts nil :maturity :evergreen)))
      (is (= 1 (length evergreens))))))

(test monolith-find-concepts-with-limit
  "Test finding concepts with a limit."
  (with-test-monolith
    (dotimes (i 5)
      (hactar-monolith::create-concept (format nil "Concept ~A" i)))
    (let ((results (hactar-monolith::find-concepts nil :limit 3)))
      (is (= 3 (length results))))))

(test monolith-get-concept
  "Test getting a concept by ID."
  (with-test-monolith
    (let* ((concept (hactar-monolith::create-concept "Test Get"))
           (id (hactar-monolith::org-concept-id concept))
           (retrieved (hactar-monolith::get-concept id)))
      (is-true retrieved)
      (is (string= "Test Get" (second retrieved))))))

(test monolith-get-concept-not-found
  "Test getting a nonexistent concept."
  (with-test-monolith
    (let ((result (hactar-monolith::get-concept "nonexistent-id")))
      (is (null result)))))

(test monolith-update-concept-maturity
  "Test updating concept maturity."
  (with-test-monolith
    (let* ((concept (hactar-monolith::create-concept "Evolving Concept" :maturity :seedling))
           (id (hactar-monolith::org-concept-id concept)))
      (hactar-monolith::update-concept-maturity id :evergreen)
      ;; Check database
      (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
        (let ((rows (sqlite:execute-to-list db
                      "SELECT maturity FROM concepts WHERE id = ?" id)))
          (is (string= "EVERGREEN" (first (first rows))))))
      ;; Check org file
      (let ((file-content (uiop:read-file-string
                           (hactar-monolith::org-concept-path concept))))
        (is (search ":MATURITY: EVERGREEN" file-content))))))

;;* Indexing Tests

(test monolith-index-concept-file
  "Test indexing a concept file."
  (with-test-monolith
    (write-test-org-file
     "Concepts/Caching.org"
     "#+TITLE: Caching
:PROPERTIES:
:ID: caching-concept
:MATURITY: growing
:LANGUAGES: lisp
:END:

* Caching

In-memory caching patterns.

See [[id:redis-concept]].
")
    (hactar-monolith::index-monolith)
    ;; Verify concept is in database
    (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT title, maturity FROM concepts WHERE id = 'caching-concept'")))
        (is (= 1 (length rows)))
        (is (string= "Caching" (first (first rows))))
        (is (string= "growing" (second (first rows)))))
      ;; Verify edge was created for the link
      (let ((edges (sqlite:execute-to-list db
                     "SELECT to_id FROM edges WHERE from_id = 'caching-concept'")))
        (is (>= (length edges) 1))))))

(test monolith-index-project-file
  "Test indexing a project file."
  (with-test-monolith
    (write-test-org-file
     "Projects/MyApp.org"
     "#+TITLE: MyApp
:PROPERTIES:
:ID: myapp-project
:REPOS: git@github.com:user/myapp.git
:END:

* MyApp

Web application.

Uses [[Concepts/Auth]].
")
    (hactar-monolith::index-monolith)
    (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT name FROM projects WHERE id = 'myapp-project'")))
        (is (= 1 (length rows)))
        (is (string= "MyApp" (first (first rows))))))))

(test monolith-index-journal-file
  "Test indexing a journal file."
  (with-test-monolith
    (write-test-org-file
     "Journal/2025-01-15.org"
     "#+TITLE: Journal 2025-01-15
:PROPERTIES:
:ID: journal-2025-01-15
:DATE: 2025-01-15
:END:

* 10:00

Thought about [[Concepts/Caching]] today.
")
    (hactar-monolith::index-monolith)
    (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT date FROM journal_entries WHERE id = 'journal-2025-01-15'")))
        (is (= 1 (length rows)))
        (is (string= "2025-01-15" (first (first rows))))))))

(test monolith-index-code-blocks
  "Test that code blocks are indexed."
  (with-test-monolith
    (write-test-org-file
     "Concepts/Hello.org"
     "#+TITLE: Hello
:PROPERTIES:
:ID: hello-concept
:END:

* Hello

#+BEGIN_SRC lisp :tangle src/hello.lisp :project hello-proj
(defun hello () (print \"hello\"))
#+END_SRC

#+BEGIN_SRC python :tangle src/hello.py :project hello-proj
print('hello')
#+END_SRC
")
    (hactar-monolith::index-monolith)
    (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT language, tangle_target FROM code_blocks WHERE project = 'hello-proj'")))
        (is (= 2 (length rows)))))))

(test monolith-index-edges
  "Test that edges are indexed for links."
  (with-test-monolith
    (write-test-org-file
     "Concepts/A.org"
     ":PROPERTIES:
:ID: concept-a
:END:
* A
Links to [[id:concept-b]] and [[Concepts/C]].
")
    (hactar-monolith::index-monolith)
    (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
      (let ((edges (sqlite:execute-to-list db
                     "SELECT to_id, relation FROM edges WHERE from_id = 'concept-a'")))
        (is (>= (length edges) 1))))))

;;* Tangling Tests

(test monolith-tangle-project
  "Test tangling code blocks for a project."
  (with-test-monolith
    (write-test-org-file
     "Concepts/Tangle.org"
     "#+TITLE: Tangle Test
:PROPERTIES:
:ID: tangle-test
:END:

* Tangle

#+BEGIN_SRC lisp :tangle src/tangled.lisp :project tangle-proj
(defun tangled-fn ()
  42)
#+END_SRC
")
    (hactar-monolith::index-monolith)
    (let ((result (hactar-monolith::tangle-project "tangle-proj")))
      (is-true (hactar-monolith::tangle-result-success-p result))
      (is (= 1 (length (hactar-monolith::tangle-result-files-written result))))
      ;; Verify the tangled file exists and has correct content
      (let ((tangled-path (first (hactar-monolith::tangle-result-files-written result))))
        (is-true (probe-file tangled-path))
        (let ((content (uiop:read-file-string tangled-path)))
          (is (search "tangled-fn" content)))))))

(test monolith-tangle-project-dry-run
  "Test dry-run tangling doesn't write files."
  (with-test-monolith
    (write-test-org-file
     "Concepts/DryRun.org"
     "#+TITLE: Dry Run
:PROPERTIES:
:ID: dry-run
:END:

#+BEGIN_SRC lisp :tangle src/dry.lisp :project dry-proj
(+ 1 2)
#+END_SRC
")
    (hactar-monolith::index-monolith)
    (let ((result (hactar-monolith::tangle-project "dry-proj" :dry-run t)))
      ;; dry-run doesn't populate files-written
      (is (null (hactar-monolith::tangle-result-files-written result))))))

(test monolith-tangle-file
  "Test tangling code blocks from a single file."
  (with-test-monolith
    (let ((org-path (write-test-org-file
                     "Concepts/SingleFile.org"
                     "#+TITLE: Single File
:PROPERTIES:
:ID: single-file
:END:

#+BEGIN_SRC python :tangle output.py
print('tangled!')
#+END_SRC
")))
      (let ((result (hactar-monolith::tangle-file org-path)))
        (is-true (hactar-monolith::tangle-result-success-p result))
        (is (= 1 (length (hactar-monolith::tangle-result-files-written result))))))))

(test monolith-tangle-records-history
  "Test that tangling records history in the database."
  (with-test-monolith
    (write-test-org-file
     "Concepts/History.org"
     "#+TITLE: History
:PROPERTIES:
:ID: history-test
:END:

#+BEGIN_SRC lisp :tangle src/history.lisp :project hist-proj
(print :hello)
#+END_SRC
")
    (hactar-monolith::index-monolith)
    (hactar-monolith::tangle-project "hist-proj")
    (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT target_path, checksum FROM tangle_history")))
        (is (>= (length rows) 1))
        (is (search "history.lisp" (first (first rows))))))))

(test monolith-resolve-tangle-target-relative
  "Test resolving relative tangle targets."
  (with-test-monolith
    (let ((resolved (hactar-monolith::resolve-tangle-target "src/foo.lisp" "my-project")))
      (is (search "Projects/my-project/src/foo.lisp" (namestring resolved))))))

(test monolith-resolve-tangle-target-absolute
  "Test resolving absolute tangle targets."
  (with-test-monolith
    (let ((resolved (hactar-monolith::resolve-tangle-target "/tmp/absolute.lisp" nil)))
      (is (string= "/tmp/absolute.lisp" (namestring resolved))))))

;;* Drift Detection Tests

(test monolith-detect-drift-no-drift
  "Test drift detection when no files have changed."
  (with-test-monolith
    (write-test-org-file
     "Concepts/NoDrift.org"
     "#+TITLE: No Drift
:PROPERTIES:
:ID: no-drift
:END:

#+BEGIN_SRC lisp :tangle src/stable.lisp :project stable-proj
(defun stable () :ok)
#+END_SRC
")
    (hactar-monolith::index-monolith)
    (hactar-monolith::tangle-project "stable-proj")
    (let ((drifts (hactar-monolith::detect-drift :project "stable-proj")))
      (is (null drifts)))))

(test monolith-detect-drift-modified
  "Test drift detection when a tangled file has been modified."
  (with-test-monolith
    (write-test-org-file
     "Concepts/Drifted.org"
     "#+TITLE: Drifted
:PROPERTIES:
:ID: drifted
:END:

#+BEGIN_SRC lisp :tangle src/drifted.lisp :project drift-proj
(defun original () :original)
#+END_SRC
")
    (hactar-monolith::index-monolith)
    (hactar-monolith::tangle-project "drift-proj")
    ;; Now modify the tangled file
    (let ((tangled-path (merge-pathnames "Projects/drift-proj/src/drifted.lisp"
                                         *test-monolith-root*)))
      (with-open-file (s tangled-path :direction :output :if-exists :supersede)
        (write-string "(defun modified () :modified)" s)))
    (let ((drifts (hactar-monolith::detect-drift :project "drift-proj")))
      (is (= 1 (length drifts)))
      (is (eq :modified (hactar-monolith::drift-info-drift-type (first drifts)))))))

;;* Graph Query Tests

(test monolith-get-related-concepts
  "Test getting related concepts via edges."
  (with-test-monolith
    ;; Create concepts with links between them
    (write-test-org-file
     "Concepts/Parent.org"
     ":PROPERTIES:
:ID: parent-concept
:END:
* Parent
Links to [[id:child-concept-1]] and [[id:child-concept-2]].
")
    (write-test-org-file
     "Concepts/Child1.org"
     ":PROPERTIES:
:ID: child-concept-1
:END:
* Child 1
")
    (write-test-org-file
     "Concepts/Child2.org"
     ":PROPERTIES:
:ID: child-concept-2
:END:
* Child 2
")
    (hactar-monolith::index-monolith)
    (let ((related (hactar-monolith::get-related-concepts "parent-concept")))
      (is (>= (length related) 2)))))

(test monolith-get-related-concepts-empty
  "Test getting related concepts when there are none."
  (with-test-monolith
    (let ((concept (hactar-monolith::create-concept "Isolated")))
      (let ((related (hactar-monolith::get-related-concepts
                      (hactar-monolith::org-concept-id concept))))
        (is (= 0 (length related)))))))

(test monolith-query-graph-never-implemented
  "Test the 'never implemented' query pattern."
  (with-test-monolith
    (hactar-monolith::create-concept "Unimplemented Feature")
    (let ((results (hactar-monolith::query-graph "concepts never implemented")))
      (is (>= (length results) 1)))))

(test monolith-query-graph-default-search
  "Test that unknown queries fall back to concept search."
  (with-test-monolith
    (hactar-monolith::create-concept "Fancy Pattern")
    (let ((results (hactar-monolith::query-graph "Fancy")))
      (is (>= (length results) 1)))))

;;* Edge Indexing Tests

(test monolith-index-edge-no-duplicates
  "Test that duplicate edges are not created."
  (with-test-monolith
    (hactar-monolith::index-edge "a" "b" :implements)
    (hactar-monolith::index-edge "a" "b" :implements)
    (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT COUNT(*) FROM edges WHERE from_id = 'a' AND to_id = 'b'")))
        (is (= 1 (first (first rows))))))))

(test monolith-index-edge-different-relations
  "Test that same nodes can have different relation edges."
  (with-test-monolith
    (hactar-monolith::index-edge "a" "b" :implements)
    (hactar-monolith::index-edge "a" "b" :mentions)
    (sqlite:with-open-database (db hactar-monolith::*monolith-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT COUNT(*) FROM edges WHERE from_id = 'a' AND to_id = 'b'")))
        (is (= 2 (first (first rows))))))))

;;* Hook Tests

(test monolith-concept-created-hook-fires
  "Test that the concept-created hook fires."
  (with-test-monolith
    (let ((hook-fired nil))
      (nhooks:add-hook hactar-monolith::*concept-created-hook*
                       (make-instance 'nhooks:handler
                                      :fn (lambda (concept)
                                            (setf hook-fired
                                                  (hactar-monolith::org-concept-title concept)))
                                      :name 'test-concept-created))
      (unwind-protect
           (progn
             (hactar-monolith::create-concept "Hookable")
             (is (string= "Hookable" hook-fired)))
        (nhooks:remove-hook hactar-monolith::*concept-created-hook*
                            'test-concept-created)))))

(test monolith-tangle-completed-hook-fires
  "Test that the tangle-completed hook fires."
  (with-test-monolith
    (let ((hook-result nil))
      (nhooks:add-hook hactar-monolith::*tangle-completed-hook*
                       (make-instance 'nhooks:handler
                                      :fn (lambda (result)
                                            (setf hook-result result))
                                      :name 'test-tangle-completed))
      (unwind-protect
           (progn
             (write-test-org-file
              "Concepts/Hooked.org"
              "#+TITLE: Hooked
:PROPERTIES:
:ID: hooked
:END:

#+BEGIN_SRC lisp :tangle src/hooked.lisp :project hook-proj
:ok
#+END_SRC
")
             (hactar-monolith::index-monolith)
             (hactar-monolith::tangle-project "hook-proj")
             (is-true hook-result)
             (is-true (hactar-monolith::tangle-result-success-p hook-result)))
        (nhooks:remove-hook hactar-monolith::*tangle-completed-hook*
                            'test-tangle-completed)))))

;;* Integration / End-to-End Tests

(test monolith-full-workflow
  "Test a complete workflow: create, index, tangle, detect drift."
  (with-test-monolith
    (write-test-org-file
     "Concepts/Workflow.org"
     "#+TITLE: Workflow Test
:PROPERTIES:
:ID: workflow-concept
:MATURITY: seedling
:LANGUAGES: lisp
:END:

* Workflow

A complete workflow test.

#+BEGIN_SRC lisp :tangle src/workflow.lisp :project workflow-proj
(defun workflow ()
  (format t \"Running workflow~%\"))
#+END_SRC
")
    (hactar-monolith::index-monolith)

    (let ((found (hactar-monolith::find-concepts "Workflow")))
      (is (= 1 (length found))))

    (let ((result (hactar-monolith::tangle-project "workflow-proj")))
      (is-true (hactar-monolith::tangle-result-success-p result))
      (is (= 1 (length (hactar-monolith::tangle-result-files-written result)))))

    (let ((drifts (hactar-monolith::detect-drift :project "workflow-proj")))
      (is (null drifts)))

    (let ((tangled-path (merge-pathnames "Projects/workflow-proj/src/workflow.lisp"
                                         *test-monolith-root*)))
      (with-open-file (s tangled-path :direction :output :if-exists :supersede)
        (write-string "(defun workflow () (format t \"Modified~%\"))" s)))

    (let ((drifts (hactar-monolith::detect-drift :project "workflow-proj")))
      (is (= 1 (length drifts)))
      (is (eq :modified (hactar-monolith::drift-info-drift-type (first drifts)))))

    (hactar-monolith::update-concept-maturity "workflow-concept" :growing)
    (let ((concept (hactar-monolith::get-concept "workflow-concept")))
      (is (string= "GROWING" (fourth concept))))))
