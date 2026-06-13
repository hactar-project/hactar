;; checks to catch potential hactar environment and configuration issues
(in-package :hactar)

(defun check-sbcl-installed? ()
  "Check if SBCL is installed."
  (if (find-executable "sbcl")
      (progn (log-good "SBCL is installed.") t)
      (progn (log-warning "SBCL is not installed or not in PATH.") nil)))

(defun check-git-installed? ()
  "Check if git is installed."
  (if (find-executable "git")
      (progn (log-good "git is installed.") t)
      (progn (log-warning "git is not installed or not in PATH.") nil)))

(defun check-hactar-pro-path-writable? ()
  "Check write permissions to *hactar-pro-path*."
  (let ((dir (%to-pathname *hactar-pro-path*)))
    (if (%dir-writable-p dir)
        (progn (log-good "Writable: ~A" (uiop:native-namestring (uiop:ensure-directory-pathname dir))) t)
        (progn (log-warning "Not writable: ~A" (uiop:native-namestring (uiop:ensure-directory-pathname dir))) nil))))

(defun check-hactar-config-path-writable? ()
  "Check write permissions to *hactar-config-path*."
  (let ((dir (%to-pathname *hactar-config-path*)))
    (if (%dir-writable-p dir)
        (progn (log-good "Writable: ~A" (uiop:native-namestring (uiop:ensure-directory-pathname dir))) t)
        (progn (log-warning "Not writable: ~A" (uiop:native-namestring (uiop:ensure-directory-pathname dir))) nil))))

(defun check-hactar-data-path-writable? ()
  "Check write permissions to *hactar-data-path*."
  (let ((dir (%to-pathname *hactar-data-path*)))
    (if (%dir-writable-p dir)
        (progn (log-good "Writable: ~A" (uiop:native-namestring (uiop:ensure-directory-pathname dir))) t)
        (progn (log-warning "Not writable: ~A" (uiop:native-namestring (uiop:ensure-directory-pathname dir))) nil))))

(defun check-piper-model-path-writable? ()
  "Check write permissions to the directory containing *piper-model-path*."
  (let* ((model (%to-pathname *piper-model-path*))
         (dir (uiop:pathname-directory-pathname model)))
    (if (%dir-writable-p dir)
        (progn (log-good "Piper model directory writable: ~A" (uiop:native-namestring dir)) t)
        (progn (log-warning "Piper model directory not writable: ~A" (uiop:native-namestring dir)) nil))))

(defun check-current-folder-git-project? ()
  "Check that the current folder is a git project."
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program (list "git" "-C" (namestring (uiop:getcwd)) "rev-parse" "--is-inside-work-tree")
                            :output :string :error-output :string :ignore-error-status t)
        (declare (ignore out err))
        (if (zerop code)
            (progn (log-good "Current directory is inside a git repository.") t)
            (progn (log-warning "Current directory is not a git repository.") nil)))
    (error (e)
      (log-warning "Git check failed: ~A" e)
      nil)))

(defun check-hactar-repo-cloned? ()
  "Check whether the Hactar repo has been cloned at *hactar-repo-dir*."
  (let* ((dir (%to-pathname *hactar-repo-dir*))
         (dir-path (uiop:ensure-directory-pathname dir)))
    (cond
      ((not (uiop:directory-exists-p dir-path))
       (log-warning "Hactar repo directory missing: ~A" (uiop:native-namestring dir-path))
       nil)
      (t
       (handler-case
           (multiple-value-bind (out err code)
               (uiop:run-program (list "git" "-C" (uiop:native-namestring dir-path) "rev-parse" "--is-inside-work-tree")
                                 :output :string :error-output :string :ignore-error-status t)
             (declare (ignore out))
             (if (zerop code)
                 (progn (log-good "Hactar repo present at: ~A" (uiop:native-namestring dir-path)) t)
                 (progn (log-warning "Hactar repo not a valid git repo at: ~A (~A)" (uiop:native-namestring dir-path) err) nil)))
         (error (e)
           (log-warning "Error checking Hactar repo at ~A: ~A" (uiop:native-namestring dir-path) e)
           nil))))))

(defun check-prompts-folder-exists? ()
  "Check whether the prompts folder exists at *hactar-config-path*/prompts."
  (let* ((base (%to-pathname *hactar-config-path*))
         (dir (uiop:subpathname (uiop:ensure-directory-pathname base) "prompts/"))
         (dir-path (uiop:ensure-directory-pathname dir)))
    (if (uiop:directory-exists-p dir-path)
        (progn (log-good "Prompts folder exists: ~A" (uiop:native-namestring dir-path)) t)
        (progn (log-warning "Prompts folder missing: ~A" (uiop:native-namestring dir-path)) nil))))

(defun check-models-config-exists? ()
  "Check whether the models configuration file exists at *hactar-config-path*/models.yaml."
  (let* ((base (%to-pathname *hactar-config-path*))
         (file (uiop:subpathname (uiop:ensure-directory-pathname base) "models.yaml"))
         (file-path (uiop:native-namestring file)))
    (if (probe-file file)
        (progn (log-good "Models config exists: ~A" file-path) t)
        (progn (log-warning "Models config missing: ~A" file-path) nil))))

(defun check-can-clone-hactar-pro-repo? ()
  "Check that we can access/clone the Hactar Pro repo (uses HACTAR_PRO_REPO_URL if set)."
  (let* ((url (or (uiop:getenv "HACTAR_PRO_REPO_URL")
                  "git@github.com:hactar-project/pro.git")))
    (handler-case
        (multiple-value-bind (out err code)
			     (uiop:run-program (list "git" "ls-remote" "--exit-code" url)
					       :output :string :error-output :string :ignore-error-status t)
			     (declare (ignore out))
			     (if (zerop code)
				 (progn (log-good "Able to access Hactar Pro repository: ~A" url) t)
			       (progn (log-warning "Cannot access Hactar Pro repository (~A): ~A" url err) nil)))
      (error (e)
        (log-warning "Error checking Hactar Pro repository: ~A" e)
        nil))))

(defun run-all-checks-and-report ()
  "Run all environment/setup checks and report results. Returns T if all pass."
  (let ((results (list
                  (check-sbcl-installed?)
                  (check-git-installed?)
                  (check-hactar-pro-path-writable?)
                  (check-hactar-data-path-writable?)
		  (check-hactar-config-path-writable?)
		  (check-hactar-repo-cloned?)
		  (check-prompts-folder-exists?)
		  (check-models-config-exists?)
		  (check-piper-model-path-writable?)
                  (check-current-folder-git-project?))))
    (every #'identity results)))

(define-command check (args)
  "Run environment and setup checks. Prints a Good/Warning line for each check."
  (declare (ignore args))
  (let ((ok (run-all-checks-and-report)))
    (if ok
        (log-good "All checks passed.")
        (log-warning "One or more checks failed. See messages above."))))

(define-sub-command check (args)
  "Run environment and setup checks and exit with appropriate status."
  (declare (ignore args))
  (let ((ok (run-all-checks-and-report)))
    (uiop:quit (if ok 0 1))))
