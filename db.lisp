(in-package :hactar)
;;* database handling
;; The database is a sqlite database backed by the sqlite-vec extension
;; Performance should be good up to a 50,000 API docs + starters and few million words.
;; Which is fine for individual dev usage. Kinda amazing how more scaleable easy tech like sqlite is when your requirements aren't google scal
(defvar *connection* nil "The global database connection.")

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

(defun connect-db ()
  "Establish a connection to the SQLite database."
  (unless *connection*
    (debug-log "Connecting to database:" *db-path*)
    (ensure-directories-exist *db-path*)
    (setf *connection* (sqlite:connect *db-path*))
    ;; Load the vec extension
    (handler-case (sqlite:execute-non-query *connection* "SELECT vec_version()")
      (error (e)
        (debug-log "vec extension not found, attempting to load: " e)
        (let ((vec-path *sqlite-vec-path*))
          (unless vec-path
            (error "sqlite-vec not found. Set SQLITE_VEC_PATH or install sqlite-vec."))
          (load-extension *connection* vec-path)))))
    *connection*)

(defun disconnect-db ()
  "Disconnect the database connection."
  (when *connection*
    (sqlite:disconnect *connection*)
    (setf *connection* nil)))

(defmacro with-db-connection (&body body)
  "Ensures a database connection is available for the execution of BODY."
  `(let ((*connection* (or *connection* (connect-db))))
     (unless *connection*
       (error "Failed to establish database connection."))
     ,@body))

;;** db utils
(defun format-vector-for-sqlite-vec (vector)
  "Formats a Lisp vector (list of numbers) into a JSON string for sqlite-vec."
  (cl-json:encode-json-to-string vector))

(defun format-array-for-sqlite (lisp-list)
  "Formats a Lisp list of strings into a JSON array string for SQLite."
  (if lisp-list
      (cl-json:encode-json-to-string lisp-list)
      "[]"))

(defun docs-clear-database ()
  "Deletes all documents from the database after confirmation."
  (if (confirm-action "Are you sure you want to delete ALL documents from the database?")
    (with-db-connection
      (handler-case
          (progn
            (sqlite:execute-non-query *connection* "DELETE FROM documents")
            (sqlite:execute-non-query *connection* "DELETE FROM vec_documents")
            (format t "Successfully deleted all documents.~%"))
        (error (e)
          (format t "Error deleting documents: ~A~%" e)))
      (setf *docs-context* nil)
      (format t "Cleared in-memory document context.~%"))
    (format t "Database clear cancelled.~%")))

(defun errors-clear-database ()
  "Deletes all errors from the database after confirmation."
  (if (confirm-action "Are you sure you want to delete ALL errors from the database?")
    (with-db-connection
      (handler-case
          (progn
            (sqlite:execute-non-query *connection* "DELETE FROM errors")
            (sqlite:execute-non-query *connection* "DELETE FROM vec_errors")
            (format t "Successfully deleted all errors.~%"))
        (error (e)
          (format t "Error deleting errors: ~A~%" e)))
      (setf *errors-context* nil)
      (format t "Cleared in-memory error context.~%"))
    (format t "Database clear cancelled.~%")))
