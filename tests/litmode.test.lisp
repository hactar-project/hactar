;;* Litmode Tests
(in-package :hactar-tests)

(def-suite litmode-tests
  :description "Tests for Hactar literate single-file mode (litmode)")

(in-suite litmode-tests)

;;** Test Helpers

(defvar *test-litmode-root* nil
  "Temporary directory for litmode tests.")

(defun make-test-litmode-root ()
  "Create a fresh temp directory for litmode tests."
  (let ((dir (merge-pathnames
              (format nil "hactar-litmode-test-~A/" (uuid:make-v4-uuid))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun cleanup-test-litmode ()
  "Remove the test litmode directory and reset litmode state."
  (when (and *test-litmode-root* (uiop:directory-exists-p *test-litmode-root*))
    (uiop:delete-directory-tree *test-litmode-root* :validate t :if-does-not-exist :ignore))
  (setf *test-litmode-root* nil)
  (setf hactar::*litmode-active* nil)
  (setf hactar::*litmode-path* nil)
  (setf hactar::*litmode-content* nil)
  (setf hactar::*litmode-parsed* nil)
  (setf hactar::*litmode-db-path* nil)
  (setf hactar::*active-scope* nil)
  (setf hactar::*expanded-ids* '())
  (setf hactar::*focused-id* nil)
  (setf hactar::*headline-index-cache* nil)
  (setf hactar::*agent-id* nil)
  (clrhash hactar::*named-blocks*))

(defmacro with-test-litmode (&body body)
  "Execute BODY with a fresh test litmode initialized."
  `(let ((*test-litmode-root* (make-test-litmode-root))
         (hactar::*silent* t)
         (hactar::*repo-root* nil))
     (unwind-protect
          (progn
            (setf hactar::*repo-root* *test-litmode-root*)
            (hactar::init-litmode *test-litmode-root*)
            ,@body)
       (cleanup-test-litmode))))

(defun write-test-litmode-file (content)
  "Overwrite the .hactar.org file with CONTENT and reload."
  (let ((path hactar::*litmode-path*))
    (with-open-file (s path :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :external-format :utf-8)
      (write-string content s))
    (hactar::load-litmode-file t)))

;;** Initialization Tests

(test litmode-init-creates-file
  "Test that init-litmode creates the .hactar.org file."
  (with-test-litmode
    (is-true (probe-file hactar::*litmode-path*))
    (is (str:ends-with-p ".hactar.org" (namestring hactar::*litmode-path*)))))

(test litmode-init-creates-db
  "Test that init-litmode creates the litmode database."
  (with-test-litmode
    (is-true (probe-file hactar::*litmode-db-path*))
    (is (str:ends-with-p ".hactar.litmode.db" (namestring hactar::*litmode-db-path*)))))

(test litmode-init-sets-active
  "Test that init-litmode sets the active flag."
  (with-test-litmode
    (is-true hactar::*litmode-active*)
    (is-true (hactar::litmode-active-p))))

(test litmode-init-sets-agent-id
  "Test that init-litmode generates an agent ID."
  (with-test-litmode
    (is-true hactar::*agent-id*)
    (is (str:starts-with-p "hactar-agent-" hactar::*agent-id*))))

(test litmode-init-sets-paths
  "Test that init-litmode sets global paths."
  (with-test-litmode
    (is-true hactar::*litmode-path*)
    (is-true hactar::*litmode-db-path*)))

(test litmode-init-default-structure
  "Test that the default .hactar.org contains expected zones."
  (with-test-litmode
    (let ((content (uiop:read-file-string hactar::*litmode-path*)))
      (is (search "* Meta" content))
      (is (search "* Context" content))
      (is (search "* Tasks" content))
      (is (search "* Code" content))
      (is (search "* Scratch" content)))))

(test litmode-init-db-tables-exist
  "Test that the database has the expected tables."
  (with-test-litmode
    (sqlite:with-open-database (db hactar::*litmode-db-path*)
      (is (listp (sqlite:execute-to-list db "SELECT * FROM headlines LIMIT 0")))
      (is (listp (sqlite:execute-to-list db "SELECT * FROM agent_locks LIMIT 0")))
      (is (listp (sqlite:execute-to-list db "SELECT * FROM context_moves LIMIT 0")))
      (is (listp (sqlite:execute-to-list db "SELECT * FROM tangle_history LIMIT 0"))))))

(test litmode-init-idempotent
  "Test that calling init-litmode twice doesn't error."
  (with-test-litmode
    (hactar::init-litmode *test-litmode-root*)
    (is-true hactar::*litmode-active*)))

(test litmode-cli-flag-enables-at-startup
  "Test that litmode can be enabled at startup via CLI flag logic."
  (with-test-litmode
    (setf hactar::*litmode-active* nil)
    (hactar::init-litmode *test-litmode-root*)
    (is-true hactar::*litmode-active*)))

;;** File I/O Tests

(test litmode-load-file
  "Test that load-litmode-file parses the org file."
  (with-test-litmode
    (is-true hactar::*litmode-content*)
    (is-true hactar::*litmode-parsed*)
    (is (typep hactar::*litmode-parsed* 'org-mode-parser:org-document))))

(test litmode-load-file-force-reload
  "Test that force reload refreshes content."
  (with-test-litmode
    (let ((old-content hactar::*litmode-content*))
      (hactar::load-litmode-file t)
      (is-true hactar::*litmode-content*))))

(test litmode-save-file
  "Test that save-litmode-file writes back to disk."
  (with-test-litmode
    (hactar::save-litmode-file)
    (let ((content (uiop:read-file-string hactar::*litmode-path*)))
      (is (> (length content) 0))
      (is (search "Meta" content)))))

;;** Headline Index Tests

(test litmode-build-headline-index
  "Test building the headline index."
  (with-test-litmode
    (let ((index (hactar::build-headline-index)))
      (is-true index)
      (is (> (length index) 0))
      (is-true (find "headline-index" index
                     :key (lambda (e) (getf e :id))
                     :test #'string=)))))

(test litmode-get-headline-index-cached
  "Test that get-headline-index returns cached data."
  (with-test-litmode
    (hactar::build-headline-index)
    (let ((cached hactar::*headline-index-cache*))
      (is (eq cached (hactar::get-headline-index))))))

(test litmode-get-headline-by-id
  "Test getting a headline by ID."
  (with-test-litmode
    (hactar::build-headline-index)
    (let ((entry (hactar::get-headline-by-id "headline-index")))
      (is-true entry)
      (is (string= "headline-index" (getf entry :id))))))

(test litmode-get-headline-by-id-not-found
  "Test getting a nonexistent headline by ID."
  (with-test-litmode
    (hactar::build-headline-index)
    (is (null (hactar::get-headline-by-id "nonexistent-id-xyz")))))

(test litmode-headline-index-has-tokens
  "Test that headline index entries have token estimates."
  (with-test-litmode
    (hactar::build-headline-index)
    (let ((entry (hactar::get-headline-by-id "headline-index")))
      (is-true entry)
      (is (numberp (getf entry :tokens)))
      (is (> (getf entry :tokens) 0)))))

(test litmode-headline-index-stored-in-db
  "Test that headline index is stored in the database."
  (with-test-litmode
    (hactar::build-headline-index)
    (sqlite:with-open-database (db hactar::*litmode-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT id, title FROM headlines WHERE id = 'headline-index'")))
        (is (= 1 (length rows)))))))

;;** Utility Helper Tests

(test litmode-estimate-tokens
  "Test token estimation."
  (is (= 0 (hactar::estimate-tokens-str nil)))
  (is (= 0 (hactar::estimate-tokens-str "")))
  (is (= 1 (hactar::estimate-tokens-str "abcd")))  ;; 4 chars / 4 = 1
  (is (= 3 (hactar::estimate-tokens-str "hello world!"))))  ;; 12 chars / 4 = 3

(test litmode-format-org-ts
  "Test org timestamp formatting."
  ;; Use decode in UTC to match the encode
  (let* ((ut (encode-universal-time 0 30 14 15 1 2025 0))
         (ts (hactar::format-org-ts ut)))
    ;; format-org-ts uses local timezone, so just verify structure
    (is (char= #\[ (char ts 0)))
    (is (char= #\] (char ts (1- (length ts)))))
    ;; Should contain a date-like pattern YYYY-MM-DD
    (is (cl-ppcre:scan "\\d{4}-\\d{2}-\\d{2}" ts))
    ;; Should contain a time-like pattern HH:MM
    (is (cl-ppcre:scan "\\d{2}:\\d{2}" ts))))

(test litmode-generate-agent-id
  "Test agent ID generation."
  (let ((id1 (hactar::generate-agent-id))
        (id2 (hactar::generate-agent-id)))
    (is (str:starts-with-p "hactar-agent-" id1))
    (is (str:starts-with-p "hactar-agent-" id2))
    (is (not (string= id1 id2)))))

(test litmode-sxhash-string
  "Test sxhash-string checksum."
  (let ((h1 (hactar::sxhash-string "hello"))
        (h2 (hactar::sxhash-string "hello"))
        (h3 (hactar::sxhash-string "world")))
    (is (string= h1 h2))
    (is (not (string= h1 h3)))))

;;** Context Generation Tests

(test litmode-generate-context-no-scope
  "Test context generation without a scope (includes everything except noctx)."
  (with-test-litmode
    (let ((context (hactar::generate-litmode-context)))
      (is-true context)
      (is (> (length context) 0))
      (is (search "Project Details" context))
      (is (null (search "Headline Index" context))))))

(test litmode-generate-context-with-focus
  "Test context generation with focus mode."
  (with-test-litmode
    (hactar::focus-context "project-details")
    (let ((context (hactar::generate-litmode-context)))
      (is-true context)
      ;; Focused headline should be present
      (is (search "Project Details" context)))))

(test litmode-focus-context
  "Test focusing context on a headline."
  (with-test-litmode
    (hactar::focus-context "project-details" "test reason")
    (is (string= "project-details" hactar::*focused-id*))))

(test litmode-focus-context-invalid-id
  "Test focusing on nonexistent headline."
  (with-test-litmode
    (hactar::focus-context "nonexistent-id-xyz")
    (is (null hactar::*focused-id*))))

(test litmode-expand-context
  "Test expanding context with additional IDs."
  (with-test-litmode
    (hactar::expand-context '("project-details" "arch-overview") "test reason")
    (is (member "project-details" hactar::*expanded-ids* :test #'string=))
    (is (member "arch-overview" hactar::*expanded-ids* :test #'string=))))

(test litmode-expand-context-logs-move
  "Test that expand-context logs a context move in the database."
  (with-test-litmode
    (hactar::expand-context '("project-details") "testing logging")
    (sqlite:with-open-database (db hactar::*litmode-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT move_type, reason FROM context_moves WHERE move_type = 'expand'")))
        (is (>= (length rows) 1))))))

;;** Scope Resolution Tests

(test litmode-set-active-scope
  "Test setting the active scope."
  (with-test-litmode
    (hactar::set-active-scope "my-scope")
    (is (string= "my-scope" hactar::*active-scope*))
    (is (string= "my-scope" (hactar::get-active-scope)))))

(test litmode-set-scope-clears-focus
  "Test that setting scope clears focus and expanded IDs."
  (with-test-litmode
    (setf hactar::*focused-id* "some-id")
    (setf hactar::*expanded-ids* '("a" "b"))
    (hactar::set-active-scope "new-scope")
    (is (null hactar::*focused-id*))
    (is (null hactar::*expanded-ids*))))

(test litmode-list-scope-profiles-empty
  "Test listing scope profiles when none defined."
  (with-test-litmode
    (let ((profiles (hactar::list-scope-profiles)))
      (is (null profiles)))))

(test litmode-scope-profile-with-definitions
  "Test scope resolution with a defined profile."
  (with-test-litmode
    ;; Write a file with a scope profile under scope-profiles
    (write-test-litmode-file
     "#+TITLE: Test Project
#+HACTAR_SCOPE: dev

* Meta                                                              :meta:noctx:
** Headline Index
:PROPERTIES:
:ID: headline-index
:END:

** Scope Profiles
:PROPERTIES:
:ID: scope-profiles
:END:

*** dev
:PROPERTIES:
:ID: scope-dev
:HACTAR_SCOPE_INCLUDE_TAGS: ctx
:HACTAR_SCOPE_EXCLUDE_TAGS: noctx
:HACTAR_SCOPE_MAX_TOKENS: 50000
:END:

* Context                                                           :ctx:
** Project Details
:PROPERTIES:
:ID: project-details
:END:
Some project info.
")
    (hactar::build-headline-index)
    (let ((profiles (hactar::list-scope-profiles)))
      (is (= 1 (length profiles)))
      (is (string= "dev" (getf (first profiles) :name))))))

;;** Tangling Tests

(test litmode-extract-tangle-blocks-empty
  "Test extracting tangle blocks from code zone with no blocks."
  (with-test-litmode
    (let ((blocks (hactar::extract-tangle-blocks hactar::*litmode-parsed*)))
      (is (null blocks)))))

(test litmode-tangle-code-zone-empty
  "Test tangling when there are no tangle targets."
  (with-test-litmode
    (let ((files (hactar::tangle-code-zone)))
      (is (null files)))))

(test litmode-tangle-with-code-blocks
  "Test tangling code blocks to files."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Tangle Test

* Code                                                              :code:
:PROPERTIES:
:ID: code-zone
:END:

** Hello Module
:PROPERTIES:
:ID: hello-module
:HACTAR_TANGLE: src/hello.lisp
:END:
#+begin_src lisp
(defun hello () (print \"hello\"))
#+end_src
"))
    (hactar::build-headline-index)
    (let ((files (hactar::tangle-code-zone)))
      (is (= 1 (length files)))
      (let ((tangled-path (first files)))
        (is-true (probe-file tangled-path))
        (let ((content (uiop:read-file-string tangled-path)))
          (is (search "defun hello" content)))))))

(test litmode-tangle-dry-run
  "Test tangling in dry-run mode doesn't write files."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Dry Run Test

* Code                                                              :code:
:PROPERTIES:
:ID: code-zone
:END:

** Dry Module
:PROPERTIES:
:ID: dry-module
:HACTAR_TANGLE: src/dry.lisp
:END:
#+begin_src lisp
(+ 1 2)
#+end_src
"))
    (hactar::build-headline-index)
    (let ((files (hactar::tangle-code-zone :dry-run t)))
      (is (= 1 (length files)))
      (let ((expected-path (merge-pathnames "src/dry.lisp"
                             (uiop:pathname-directory-pathname hactar::*litmode-path*))))
        (is (null (probe-file expected-path)))))))

(test litmode-tangle-block-single
  "Test tangling a single block by heading ID."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Single Block Test

* Code                                                              :code:
:PROPERTIES:
:ID: code-zone
:END:

** Single Block
:PROPERTIES:
:ID: single-block
:HACTAR_TANGLE: src/single.lisp
:END:
#+begin_src lisp
(defun single () :ok)
#+end_src
"))
    (hactar::build-headline-index)
    (let ((result (hactar::tangle-block "single-block")))
      (is-true result)
      (is-true (probe-file result))
      (let ((content (uiop:read-file-string result)))
        (is (search "defun single" content))))))

(test litmode-tangle-records-history
  "Test that tangling records history in the database."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: History Test

* Code                                                              :code:
:PROPERTIES:
:ID: code-zone
:END:

** History Block
:PROPERTIES:
:ID: history-block
:HACTAR_TANGLE: src/history.lisp
:END:
#+begin_src lisp
(print :history)
#+end_src
"))
    (hactar::build-headline-index)
    (hactar::tangle-code-zone)
    (sqlite:with-open-database (db hactar::*litmode-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT target_path FROM tangle_history")))
        (is (>= (length rows) 1))))))

;;** Untangling Tests

(test litmode-untangle-file
  "Test pulling changes from a tangled file back into org."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Untangle Test

* Code                                                              :code:
:PROPERTIES:
:ID: code-zone
:END:

** Untangle Block
:PROPERTIES:
:ID: untangle-block
:HACTAR_TANGLE: src/untangle.lisp
:END:
#+begin_src lisp
(defun original () :original)
#+end_src
"))
    (hactar::build-headline-index)
    (hactar::tangle-code-zone)

    (let ((tangled-path (merge-pathnames "src/untangle.lisp"
                          (uiop:pathname-directory-pathname hactar::*litmode-path*))))
      (with-open-file (s tangled-path :direction :output :if-exists :supersede)
        (write-string "(defun modified () :modified)" s))

      (is-true (hactar::untangle-file "src/untangle.lisp"))

      (hactar::load-litmode-file t)
      (let ((content hactar::*litmode-content*))
        (is (search "modified" content))))))

(test litmode-untangle-file-not-found
  "Test untangling a nonexistent file."
  (with-test-litmode
    (signals error
      (hactar::untangle-file "nonexistent/file.lisp"))))

;;** Drift Detection Tests

(test litmode-detect-drift-none
  "Test drift detection when no files have changed."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: No Drift Test

* Code                                                              :code:
:PROPERTIES:
:ID: code-zone
:END:

** Stable Block
:PROPERTIES:
:ID: stable-block
:HACTAR_TANGLE: src/stable.lisp
:END:
#+begin_src lisp
(defun stable () :ok)
#+end_src
"))
    (hactar::build-headline-index)
    (hactar::tangle-code-zone)
    (let ((drifts (hactar::detect-litmode-drift)))
      (is (null drifts)))))

(test litmode-detect-drift-modified
  "Test drift detection when a tangled file has been modified."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Drift Test

* Code                                                              :code:
:PROPERTIES:
:ID: code-zone
:END:

** Drifted Block
:PROPERTIES:
:ID: drifted-block
:HACTAR_TANGLE: src/drifted.lisp
:END:
#+begin_src lisp
(defun original () :original)
#+end_src
"))
    (hactar::build-headline-index)
    (hactar::tangle-code-zone)

    (let ((tangled-path (merge-pathnames "src/drifted.lisp"
                          (uiop:pathname-directory-pathname hactar::*litmode-path*))))
      (with-open-file (s tangled-path :direction :output :if-exists :supersede)
        (write-string "(defun modified () :modified)" s)))

    (let ((drifts (hactar::detect-litmode-drift)))
      (is (= 1 (length drifts)))
      (is (eq :modified (getf (first drifts) :drift-type))))))

(test litmode-detect-drift-not-tangled
  "Test drift detection when a file hasn't been tangled yet."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Not Tangled Test

* Code                                                              :code:
:PROPERTIES:
:ID: code-zone
:END:

** Not Tangled Block
:PROPERTIES:
:ID: not-tangled-block
:HACTAR_TANGLE: src/not-tangled.lisp
:END:
#+begin_src lisp
(defun not-tangled () :nope)
#+end_src
"))
    (hactar::build-headline-index)
    (let ((drifts (hactar::detect-litmode-drift)))
      (is (= 1 (length drifts)))
      (is (eq :not-tangled (getf (first drifts) :drift-type))))))

;;** Task Management Tests

(test litmode-get-tasks-empty
  "Test getting tasks when none are defined."
  (with-test-litmode
    (let ((tasks (hactar::get-tasks)))
      (is (null tasks)))))

(test litmode-get-tasks-with-tasks
  "Test getting tasks from the tasks zone."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Task Test

* Tasks                                                             :tasks:
:PROPERTIES:
:ID: tasks-zone
:END:

** TODO Implement feature A
:PROPERTIES:
:ID: task-a
:END:

** DONE Fix bug B
:PROPERTIES:
:ID: task-b
:END:

** TODO Write tests C
:PROPERTIES:
:ID: task-c
:END:
"))
    (let ((all-tasks (hactar::get-tasks)))
      (is (= 3 (length all-tasks))))
    (let ((todo-tasks (hactar::get-tasks :state "TODO")))
      (is (= 2 (length todo-tasks))))
    (let ((done-tasks (hactar::get-tasks :state "DONE")))
      (is (= 1 (length done-tasks))))))

(test litmode-transition-task
  "Test transitioning a task state."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Transition Test

* Tasks                                                             :tasks:
:PROPERTIES:
:ID: tasks-zone
:END:

** TODO My Task
:PROPERTIES:
:ID: my-task
:END:
"))
    (hactar::transition-task "my-task" "DONE")
    (hactar::load-litmode-file t)
    (let ((tasks (hactar::get-tasks :state "DONE")))
      (is (= 1 (length tasks))))))

(test litmode-transition-task-not-found
  "Test transitioning a nonexistent task."
  (with-test-litmode
    (signals error
      (hactar::transition-task "nonexistent-task" "DONE"))))

;;** Named Block / Tooling Tests

(test litmode-register-org-tools-empty
  "Test registering org tools when tooling section is empty."
  (with-test-litmode
    (hactar::register-org-tools)
    (is (= 0 (hash-table-count hactar::*named-blocks*)))))

(test litmode-register-org-tools-with-blocks
  "Test registering named blocks from tooling section."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Tooling Test

* Meta                                                              :meta:noctx:
** Tooling
:PROPERTIES:
:ID: tooling
:END:

*** Run Tests
:PROPERTIES:
:NAME: run-tests
:EVAL: on-save
:END:
#+begin_src shell
echo 'running tests'
#+end_src
"))
    (hactar::register-org-tools)
    (is (= 1 (hash-table-count hactar::*named-blocks*)))
    (let ((block (gethash "run-tests" hactar::*named-blocks*)))
      (is-true block)
      (is (string= "shell" (getf block :language)))
      (is (string= "on-save" (getf block :eval))))))

(test litmode-run-named-block-shell
  "Test running a shell named block."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Run Block Test

* Meta                                                              :meta:noctx:
** Tooling
:PROPERTIES:
:ID: tooling
:END:

*** Echo Test
:PROPERTIES:
:NAME: echo-test
:EVAL: never
:END:
#+begin_src shell
echo hello-from-block
#+end_src
"))
    (hactar::register-org-tools)
    (let ((result (hactar::run-named-block "echo-test")))
      (is-true result)
      (is (search "hello-from-block" result)))))

(test litmode-run-named-block-lisp
  "Test running a lisp named block."
  (with-test-litmode
    (write-test-litmode-file
     (format nil "#+TITLE: Lisp Block Test

* Meta                                                              :meta:noctx:
** Tooling
:PROPERTIES:
:ID: tooling
:END:

*** Lisp Calc
:PROPERTIES:
:NAME: lisp-calc
:EVAL: never
:END:
#+begin_src lisp
(+ 21 21)
#+end_src
"))
    (hactar::register-org-tools)
    (let ((result (hactar::run-named-block "lisp-calc")))
      (is (string= "42" result)))))

(test litmode-run-named-block-not-found
  "Test running a nonexistent named block."
  (with-test-litmode
    (signals error
      (hactar::run-named-block "nonexistent-block"))))

;;** /add integration (litmode)

(test litmode-add-inserts-src-block-under-ctx-files
  "When litmode is active, /add (add-file-to-context) should insert a src block under ** Files (ID: ctx-files) in .hactar.org."
  (with-test-litmode
    ;; Create a file in the test project
    (let* ((base (uiop:pathname-directory-pathname hactar::*litmode-path*))
           (p (merge-pathnames "src/foo.lisp" base)))
      (ensure-directories-exist p)
      (with-open-file (s p :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
        (write-string "(defun foo () :ok)\n" s))

      (hactar::add-file-to-context (uiop:native-namestring p))

      (let ((org (uiop:read-file-string hactar::*litmode-path* :external-format :utf-8)))
        (is (search ":ID: ctx-files" org))
        (is (search "#+begin_src lisp" org))
        (is (search "src/foo.lisp" org))
        (is (search "defun foo" org))))))

;;** /drop integration (litmode)

(test litmode-drop-removes-src-block-from-ctx-files
  "When litmode is active, /drop (drop-file-from-context) should remove the file's src block from ** Files (ID: ctx-files) in .hactar.org."
  (with-test-litmode
    ;; Create a file in the test project
    (let* ((base (uiop:pathname-directory-pathname hactar::*litmode-path*))
           (p (merge-pathnames "src/bar.lisp" base)))
      (ensure-directories-exist p)
      (with-open-file (s p :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
        (write-string "(defun bar () :bar)\n" s))

      (hactar::add-file-to-context (uiop:native-namestring p))

      (let ((org-before (uiop:read-file-string hactar::*litmode-path* :external-format :utf-8)))
        (is (search "src/bar.lisp" org-before))
        (is (search "defun bar" org-before)))

      (hactar::drop-file-from-context (uiop:native-namestring p))

      (let ((org-after (uiop:read-file-string hactar::*litmode-path* :external-format :utf-8)))
        (is (null (search "src/bar.lisp" org-after)))
        (is (null (search "defun bar" org-after)))
        (is (search ":ID: ctx-files" org-after))))))

(test litmode-drop-removes-only-target-file-from-ctx-files
  "When litmode is active and multiple files are in context, /drop should only remove the dropped file's src block."
  (with-test-litmode
    (let* ((base (uiop:pathname-directory-pathname hactar::*litmode-path*))
           (p1 (merge-pathnames "src/keep.lisp" base))
           (p2 (merge-pathnames "src/remove.lisp" base)))
      (ensure-directories-exist p1)
      (with-open-file (s p1 :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
        (write-string "(defun keep-me () :keep)\n" s))
      (ensure-directories-exist p2)
      (with-open-file (s p2 :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
        (write-string "(defun remove-me () :remove)\n" s))

      (hactar::add-file-to-context (uiop:native-namestring p1))
      (hactar::add-file-to-context (uiop:native-namestring p2))

      (let ((org-mid (uiop:read-file-string hactar::*litmode-path* :external-format :utf-8)))
        (is (search "src/keep.lisp" org-mid))
        (is (search "src/remove.lisp" org-mid)))

      (hactar::drop-file-from-context (uiop:native-namestring p2))

      (let ((org-after (uiop:read-file-string hactar::*litmode-path* :external-format :utf-8)))
        (is (search "src/keep.lisp" org-after))
        (is (search "defun keep-me" org-after))
        (is (null (search "src/remove.lisp" org-after)))
        (is (null (search "defun remove-me" org-after)))))))

(test litmode-drop-file-not-in-context-is-noop
  "When litmode is active, dropping a file that isn't in context should not error or modify .hactar.org."
  (with-test-litmode
    (let* ((base (uiop:pathname-directory-pathname hactar::*litmode-path*))
           (org-before (uiop:read-file-string hactar::*litmode-path* :external-format :utf-8))
           (fake-path (uiop:native-namestring (merge-pathnames "src/nonexistent.lisp" base))))
      (hactar::drop-file-from-context fake-path)
      (let ((org-after (uiop:read-file-string hactar::*litmode-path* :external-format :utf-8)))
        (is (string= org-before org-after))))))

;;** Agent Lock Tests

(test litmode-acquire-lock
  "Test acquiring a lock on headline IDs."
  (with-test-litmode
    (is-true (hactar::acquire-lock '("project-details")))))

(test litmode-acquire-lock-and-release
  "Test acquiring and releasing a lock."
  (with-test-litmode
    (hactar::acquire-lock '("project-details"))
    (let ((locks-before (hactar::get-active-locks)))
      (is (>= (length locks-before) 1)))
    (hactar::release-lock '("project-details"))
    (let ((locks-after (hactar::get-active-locks)))
      (is (= 0 (length locks-after))))))

(test litmode-get-active-locks-empty
  "Test getting active locks when none exist."
  (with-test-litmode
    (let ((locks (hactar::get-active-locks)))
      (is (null locks)))))

(test litmode-cleanup-expired-locks
  "Test cleaning up expired locks."
  (with-test-litmode
    (sqlite:with-open-database (db hactar::*litmode-db-path*)
      (sqlite:execute-non-query db
        "INSERT INTO agent_locks (lock_owner, lock_pid, headline_ids, scope_name, acquired_at, expires_at)
         VALUES ('old-agent', 0, '[\"test-id\"]', '', 1000, 1001)"))
    (hactar::cleanup-expired-locks)
    (sqlite:with-open-database (db hactar::*litmode-db-path*)
      (let ((rows (sqlite:execute-to-list db
                    "SELECT * FROM agent_locks WHERE lock_owner = 'old-agent'")))
        (is (= 0 (length rows)))))))

(test litmode-heartbeat-locks
  "Test heartbeat extending lock expiry."
  (with-test-litmode
    (hactar::acquire-lock '("project-details"))
    (let ((locks-before (hactar::get-active-locks)))
      (is (= 1 (length locks-before)))
      (let ((expires-before (fifth (first locks-before))))
        (sleep 1)
        (hactar::heartbeat-locks)
        (let* ((locks-after (hactar::get-active-locks))
               (expires-after (fifth (first locks-after))))
          (is (> expires-after expires-before)))))))

;;** System Prompt Integration Tests

(test litmode-system-prompt-section-when-active
  "Test system prompt section generation when litmode is active."
  (with-test-litmode
    (let ((section (hactar::litmode-system-prompt-section)))
      (is-true section)
      (is (search "Literate Mode" section))
      (is (search "expand_context" section))
      (is (search "focus_context" section)))))

(test litmode-system-prompt-section-when-inactive
  "Test system prompt section when litmode is inactive."
  (let ((hactar::*litmode-active* nil))
    (is (null (hactar::litmode-system-prompt-section)))))

(test litmode-system-prompt-shows-scope
  "Test that system prompt shows the active scope."
  (with-test-litmode
    (hactar::set-active-scope "testing")
    (let ((section (hactar::litmode-system-prompt-section)))
      (is (search "testing" section)))))

(test litmode-system-prompt-shows-focus
  "Test that system prompt shows the focused headline."
  (with-test-litmode
    (hactar::focus-context "project-details")
    (let ((section (hactar::litmode-system-prompt-section)))
      (is (search "project-details" section)))))

;;** Lock File Tests

(test litmode-lock-path
  "Test lock path computation."
  (with-test-litmode
    (let ((lock-path (hactar::litmode-lock-path)))
      (is-true lock-path)
      (is (search ".hactar.org.lock" (namestring lock-path))))))

;;** Integration Tests

(test litmode-full-workflow
  "Test a complete workflow: init, write code, tangle, modify, detect drift, untangle."
  (with-test-litmode
    ;; 1. Write org file with code
    (write-test-litmode-file
     (format nil "#+TITLE: Full Workflow

* Context                                                           :ctx:
** Project Details
:PROPERTIES:
:ID: project-details
:END:
A full workflow test.

* Tasks                                                             :tasks:
:PROPERTIES:
:ID: tasks-zone
:END:

** TODO Build feature
:PROPERTIES:
:ID: build-feature
:END:

* Code                                                              :code:
:PROPERTIES:
:ID: code-zone
:END:

** Main Module
:PROPERTIES:
:ID: main-module
:HACTAR_TANGLE: src/main.lisp
:END:
#+begin_src lisp
(defun main () (format t \"Hello World~%\"))
#+end_src
"))

    (hactar::build-headline-index)
    (is (>= (length hactar::*headline-index-cache*) 3))

    (let ((files (hactar::tangle-code-zone)))
      (is (= 1 (length files))))

    (is (null (hactar::detect-litmode-drift)))

    (let ((tangled-path (merge-pathnames "src/main.lisp"
                          (uiop:pathname-directory-pathname hactar::*litmode-path*))))
      (with-open-file (s tangled-path :direction :output :if-exists :supersede)
        (write-string "(defun main () (format t \"Modified~%\"))" s)))

    (let ((drifts (hactar::detect-litmode-drift)))
      (is (= 1 (length drifts)))
      (is (eq :modified (getf (first drifts) :drift-type))))

    (is-true (hactar::untangle-file "src/main.lisp"))

    (hactar::load-litmode-file t)
    (is (search "Modified" hactar::*litmode-content*))

    (hactar::transition-task "build-feature" "DONE")
    (hactar::load-litmode-file t)
    (let ((done-tasks (hactar::get-tasks :state "DONE")))
      (is (= 1 (length done-tasks))))))
