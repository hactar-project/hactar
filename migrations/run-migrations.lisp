(defpackage #:hactar-migrations
  (:use #:cl)
  (:export #:run-migrations))

(in-package :hactar-migrations)

;; CFFI definition for enabling SQLite extension loading
(cffi:defcfun "sqlite3_enable_load_extension" :int
  (db :pointer)
  (onoff :int))

(defun load-extension (connection extension-path)
  "Enables extension loading, loads an extension, and disables it again."
  (let ((handle (sqlite::handle connection)))
    (unwind-protect
         (progn
           (sqlite3-enable-load-extension handle 1)
           (sqlite:execute-non-query connection "SELECT load_extension(?)" extension-path))
      (sqlite3-enable-load-extension handle 0))))

;;; Database Configuration - Reads from environment variables
(defun get-db-path (&key test)
  "Returns the SQLite database path based on environment variables.
   If :test is true, prefers HACTAR_TEST_DB_PATH or an in-memory database."
  (if test
      (or (uiop:getenv "HACTAR_TEST_DB_PATH") ":memory:")
      (or (uiop:getenv "HACTAR_DB_PATH")
          (uiop:subpathname (uiop:xdg-data-home) "hactar/hactar.db"))))

(defun get-migration-files ()
  "Get a sorted list of SQL migration files relative to the system."
  (sort
   (remove-if-not
    (lambda (path)
      (and (string= (pathname-type path) "sql")
           (cl-ppcre:scan "^\\d{3}_" (pathname-name path))))
    (uiop:directory-files (asdf:system-relative-pathname :hactar-migrations "migrations/")))
   #'string<
   :key #'pathname-name))

(defun execute-sql-file (conn file-path)
  "Executes a SQL file, splitting statements on semicolons and ignoring comments."
  (let ((content (uiop:read-file-string file-path)))
    ;; Remove single-line SQL comments
    (setf content (cl-ppcre:regex-replace-all "--.*" content ""))
    ;; Simple split on semicolon; might fail for complex SQL with semicolons in strings.
    (let ((statements (remove "" (cl-ppcre:split ";" content) :test #'string=)))
      (dolist (stmt statements)
        (let ((trimmed-stmt (string-trim '(#\Space #\Tab #\Newline #\Return) stmt)))
          (when (plusp (length trimmed-stmt))
            (sqlite:execute-non-query conn trimmed-stmt)))))))

(defun migration-already-applied-p (conn migration-name)
  "Check if a migration has already been applied."
  (handler-case
      (sqlite:execute-single conn "SELECT 1 FROM schema_migrations WHERE name = ?" migration-name)
    (error ()
      ;; If table doesn't exist yet, no migrations have been applied
      nil)))

(defun create-migrations-table (conn &key silent)
  "Create the schema_migrations table if it doesn't exist."
  (unless silent (format t "~&Creating schema_migrations table...~%"))
  (sqlite:execute-non-query conn "CREATE TABLE IF NOT EXISTS schema_migrations (
                   name TEXT PRIMARY KEY,
                   applied_at TEXT DEFAULT (strftime('%Y-%m-%d %H:%M:%f', 'now'))
                 )"))

(defun mark-migration-applied (conn migration-name)
  "Mark a migration as applied in the schema_migrations table."
  (sqlite:execute-non-query conn "INSERT INTO schema_migrations (name) VALUES (?)" migration-name))

(defun run-migrations (&key test connection)
  "Run all pending migrations. If :test is true, use test database configuration.
   If CONNECTION is provided, use it instead of opening a new one."
  (let* ((hactar-pkg (find-package "HACTAR"))
         (silent-var (when hactar-pkg (find-symbol "*SILENT*" hactar-pkg)))
         (silent (and silent-var (boundp silent-var) (symbol-value silent-var)))
	 (sqlite-vec-path-var (when hactar-pkg (find-symbol "*SQLITE-VEC-PATH*" hactar-pkg)))
         (sqlite-vec-path (and sqlite-vec-path-var (boundp sqlite-vec-path-var) (symbol-value sqlite-vec-path-var)))
         (db-path (get-db-path :test test)))
    (unless silent
      (format t "~&Running database migrations for ~A database...~%" (if test "TEST" "MAIN"))
      (unless connection (format t "Connecting to ~A~%" db-path)))

    (flet ((do-migrations (conn)
             (handler-case (sqlite:execute-non-query conn "SELECT vec_version()")
               (error (e)
                 (declare (ignore e))
                 (unless silent (format t "~&vec extension not found, attempting to load...~%"))
                 (let ((vec-path sqlite-vec-path))
                   (load-extension conn vec-path))))
             (create-migrations-table conn :silent silent)
             (let ((migration-files (get-migration-files)))
               (if migration-files
                   (sqlite:with-transaction conn
                     (dolist (migration-file migration-files)
                       (let ((migration-name (pathname-name migration-file)))
                         (unless silent (format t "~&Checking migration: ~A~%" migration-name))
                         (unless (migration-already-applied-p conn migration-name)
                           (unless silent (format t "~&Applying migration: ~A~%" migration-name))
                           (handler-case
                               (progn
                                 (unless silent (format t "~&Executing SQL file: ~A~%" migration-file))
                                 (execute-sql-file conn migration-file)
                                 (mark-migration-applied conn migration-name)
                                 (unless silent (format t "~&Successfully applied migration: ~A~%" migration-name)))
                             (error (e)
                               (format t "~&Error applying migration ~A: ~A~%" migration-name e)
                               ;; with-transaction will rollback on non-local exit
                               (return-from run-migrations nil)))))))
                   (unless silent (format t "~&No migration files found.~%"))))
             (unless silent (format t "~&Migrations complete.~%"))
             t))

      (handler-case
          (if connection
              (do-migrations connection)
              (sqlite:with-open-database (conn db-path)
                (do-migrations conn)))
        (error (e)
          (format *error-output* "~&Failed to connect or run migrations: ~A~%" e)
          nil)))))

;; When run as a script from the command line
;; Allows `sbcl --load run-migrations.lisp --eval '(hactar-migrations:run-migrations)' --quit`
;; Or `sbcl --load run-migrations.lisp --eval '(hactar-migrations:run-migrations :test t)' --quit`
;; Also handles simple script execution like `sbcl --script run-migrations.lisp [--test]`
(let ((args (uiop:command-line-arguments)))
  ;; Check if the script is being run directly with arguments like --test
  (when (and uiop:*command-line-arguments*
             (string= (pathname-name (first uiop:*command-line-arguments*))
                      (pathname-name *load-pathname*)))
    (let ((test-mode (member "--test" args :test #'string=)))
      (if (run-migrations :test test-mode)
          (uiop:quit 0)
          (uiop:quit 1)))))
