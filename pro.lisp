;; handling for pro features
(in-package :hactar)
(defun pro--repo-url ()
  "Resolve the Hactar Pro repository URL from env or default."
  (or (uiop:getenv "HACTAR_PRO_REPO_URL")
      "git@github.com:hactar-project/pro.git"))

(defun pro--git-repo-p (dir)
  "Return T if DIR looks like a git repository (checks for .git/)."
  (let* ((dir-path (uiop:ensure-directory-pathname dir))
         (git-dir (merge-pathnames #P".git/" dir-path)))
    (probe-file git-dir)))

(defun pro--ensure-cloned ()
  "Ensure the Pro repository exists at *hactar-pro-path*. Clone if missing. Returns T on success."
  (let ((target (uiop:ensure-directory-pathname *hactar-pro-path*)))
    (ensure-directories-exist target)
    (if (pro--git-repo-p target)
        t
        (progn
          (format t "Cloning Hactar Pro repository to ~A~%" (uiop:native-namestring target))
          (multiple-value-bind (out err code)
              (uiop:run-program (list "git" "clone" (pro--repo-url) (uiop:native-namestring target))
                                :output :string :error-output :string :ignore-error-status t)
            (declare (ignore out))
            (if (zerop code)
                (progn (format t "Clone completed.~%") t)
                (progn (format t "Error cloning Hactar Pro repository: ~A~%" err) nil)))))))

(defun pro--db-source-path (content)
  "Build the source DB pathname inside the Pro repo for the given CONTENT key."
  (merge-pathnames
    (make-pathname :directory '(:relative "data") :name content :type "db")
    (uiop:ensure-directory-pathname *hactar-pro-path*)))

(defun pro-copy-starters ()
  (let* ((starters-src (merge-pathnames #P"starters/" (uiop:ensure-directory-pathname *hactar-pro-path*)))
         (starters-dst (merge-pathnames #P"starters/" (uiop:ensure-directory-pathname *hactar-data-path*))))
    (if (uiop:directory-exists-p starters-src)
        (progn
          (ensure-directories-exist starters-dst)
          (multiple-value-bind (out err code)
              (uiop:run-program
               (list "cp" "-a" "-f"
                     (concatenate 'string (uiop:native-namestring starters-src) ".")
                     (uiop:native-namestring starters-dst))
               :output :string :error-output :string :ignore-error-status t)
            (declare (ignore out))
            (if (zerop code)
                (format t "Copied starters from ~A to ~A~%"
                        (uiop:native-namestring starters-src)
                        (uiop:native-namestring starters-dst))
                (format t "Failed to copy starters: ~A~%" err))))
        (format t "No starters directory found at ~A~%"
                (uiop:native-namestring starters-src)))))

(defun pro-copy-modes ()
  (let* ((src (merge-pathnames #P"modes/" (uiop:ensure-directory-pathname *hactar-pro-path*)))
         (dst (merge-pathnames #P"modes/" (uiop:ensure-directory-pathname *hactar-data-path*))))
    (if (uiop:directory-exists-p src)
        (progn
          (ensure-directories-exist dst)
          (multiple-value-bind (out err code)
              (uiop:run-program
               (list "cp" "-a" "-f"
                     (concatenate 'string (uiop:native-namestring src) ".")
                     (uiop:native-namestring dst))
               :output :string :error-output :string :ignore-error-status t)
            (declare (ignore out))
            (if (zerop code)
                (format t "Copied modes from ~A to ~A~%"
                        (uiop:native-namestring src)
                        (uiop:native-namestring dst))
                (format t "Failed to copy modes: ~A~%" err))))
        (format t "No modes directory found at ~A~%"
                (uiop:native-namestring src)))))

(defun pro-copy-docsets ()
  (let* ((src (merge-pathnames #P"docsets/" (uiop:ensure-directory-pathname *hactar-pro-path*)))
         (dst (merge-pathnames #P"docsets/" (uiop:ensure-directory-pathname *hactar-data-path*))))
    (if (uiop:directory-exists-p src)
        (progn
          (ensure-directories-exist dst)
          (multiple-value-bind (out err code)
              (uiop:run-program
               (list "cp" "-a" "-f"
                     (concatenate 'string (uiop:native-namestring src) ".")
                     (uiop:native-namestring dst))
               :output :string :error-output :string :ignore-error-status t)
            (declare (ignore out))
            (if (zerop code)
                (format t "Copied docsets from ~A to ~A~%"
                        (uiop:native-namestring src)
                        (uiop:native-namestring dst))
                (format t "Failed to copy docsets: ~A~%" err))))
        (format t "No docsets directory found at ~A~%"
                (uiop:native-namestring src)))))

(defun pro-copy-guides ()
  (let* ((src (merge-pathnames #P"guides/" (uiop:ensure-directory-pathname *hactar-pro-path*)))
         (dst (merge-pathnames #P"guides/" (uiop:ensure-directory-pathname *hactar-data-path*))))
    (if (uiop:directory-exists-p src)
        (progn
          (ensure-directories-exist dst)
          (multiple-value-bind (out err code)
              (uiop:run-program
               (list "cp" "-a" "-f"
                     (concatenate 'string (uiop:native-namestring src) ".")
                     (uiop:native-namestring dst))
               :output :string :error-output :string :ignore-error-status t)
            (declare (ignore out))
            (if (zerop code)
                (format t "Copied guides from ~A to ~A~%"
                        (uiop:native-namestring src)
                        (uiop:native-namestring dst))
                (format t "Failed to copy guides: ~A~%" err))))
        (format t "No guides directory found at ~A~%"
                (uiop:native-namestring src)))))

(defun pro-copy-db (content)
  (let* ((src (pro--db-source-path content))
         (dst (%to-pathname *db-path*)))
    (if (not (probe-file src))
        (format t "Content database not found: ~A~%" (uiop:native-namestring src))
        (progn
          (ensure-directories-exist (uiop:pathname-directory-pathname dst))
          (if (and (probe-file dst)
                   (not (confirm-action
                         (format nil "Database already exists at ~A.~%Overwrite it with '~A'?~%If you choose No, you can set the HACTAR_DB_PATH environment variable to: ~A"
                                 (uiop:native-namestring dst)
                                 (uiop:native-namestring src)
                                 (uiop:native-namestring src)))))
              (format t "Not overwriting existing database. You can export HACTAR_DB_PATH=~A to use the Pro database directly.~%"
                      (uiop:native-namestring src))
              (progn
                (multiple-value-bind (out err code)
                    (uiop:run-program (list "cp" (uiop:native-namestring src) (uiop:native-namestring dst))
                                      :output :string :error-output :string :ignore-error-status t)
                  (declare (ignore out))
                  (if (zerop code)
                      (format t "Copied Pro content DB '~A' to ~A~%"
                              (uiop:native-namestring src) (uiop:native-namestring dst))
                      (format t "Failed to copy DB: ~A~%" err)))))))))

(define-sub-command pro.install (args)
  "Clone the Pro repo (if needed) and copy the selected content DB into *db-path*."
  (let ((content (or (getf args :content) "all")))
    (if (not (pro--ensure-cloned))
        (format t "Failed to set up Hactar Pro repository at ~A~%"
                (uiop:native-namestring *hactar-pro-path*))
        (progn
          (pro-copy-starters)
          (pro-copy-modes)
          (pro-copy-docsets)
          (pro-copy-guides)
          (pro-copy-db content))))
  :cli-options ((:short "c" :long "content" :description "Select which content DB to copy (default: all)")))

(define-sub-command pro.update (args)
  "Update the Pro repository by running 'git pull'. Clones first if missing."
  (declare (ignore args))
  (if (pro--git-repo-p *hactar-pro-path*)
      (multiple-value-bind (out err code)
          (uiop:run-program (list "git" "-C" (uiop:native-namestring (uiop:ensure-directory-pathname *hactar-pro-path*)) "pull" "--ff-only")
                            :output :string :error-output :string :ignore-error-status t)
        (declare (ignore out))
        (if (zerop code)
            (format t "Hactar Pro repository updated.~%")
            (format t "Error updating Hactar Pro repository: ~A~%" err)))
      (progn
        (format t "Hactar Pro repository not found. Cloning...~%")
        (if (pro--ensure-cloned)
            (format t "Clone completed.~%")
            (format t "Failed to clone Hactar Pro repository.~%")))))

(define-sub-command pro.check (args)
		    "Run environment/setup checks.
Right now just an alias for run-all-checks-and-report but ideally we'd check other pro stuff too."
		    (declare (ignore args))
		    (run-all-checks-and-report))
