(in-package #:hactar-rebuild)

;;; Engine diagnostics, debug logging, and git helpers.

(defun debug-log (fmt &rest args)
  "Write diagnostic output to *ERROR-OUTPUT* when *DEBUG-MODE* is non-nil."
  (when *debug-mode*
    (format *error-output* "~&[hactar] ~?~%" fmt args)))

(defun git-command (args &key (root (or *repo-root* (uiop:getcwd))))
  "Run git ARGS in ROOT and return (values stdout stderr exit-code)."
  (uiop:run-program (cons "git" args)
                    :directory (uiop:ensure-directory-pathname root)
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun git-staged-diff (&optional (root (or *repo-root* (uiop:getcwd))))
  (multiple-value-bind (out err code)
      (git-command '("diff" "--staged") :root root)
    (declare (ignore err))
    (when (zerop code) out)))

(defun git-status-clean-p (&optional (root (or *repo-root* (uiop:getcwd))))
  (multiple-value-bind (out err code)
      (git-command '("status" "--porcelain") :root root)
    (declare (ignore err))
    (and (zerop code) (string= (str:trim out) ""))))

(defun generate-commit-message (&key (root (or *repo-root* (uiop:getcwd)))
                                     (model *current-model*))
  "Generate a single-line commit message from the staged git diff via HACTAR-LLM."
  (let ((diff (or (git-staged-diff root) "")))
    (when (string= (str:trim diff) "")
      (return-from generate-commit-message nil))
    (let ((resp (ask (format nil "Write a concise one-line commit message for this diff:~%~%~A" diff)
                     :model model)))
      (when resp
        (str:trim (first (str:lines resp)))))))

(defun find-executable (name)
  (multiple-value-bind (out err code)
      (uiop:run-program (list "which" name)
                        :output :string :error-output :string :ignore-error-status t)
    (declare (ignore err))
    (and (zerop code) (plusp (length (str:trim out))))))

(defun run-doctor-checks (&optional (root (or *repo-root* (find-repo-root))))
  "Run structured environment checks and return a RESULT."
  (let* ((rootd (uiop:ensure-directory-pathname root))
         (checks
           (list
            (list :name :sbcl :ok (not (null (find-executable "sbcl"))))
            (list :name :git :ok (not (null (find-executable "git"))))
            (list :name :repo :ok (not (null (probe-file (merge-pathnames ".git/" rootd)))))
            (list :name :hactar-file :ok (not (null (probe-file (hactar-file-path rootd)))))))
         (all-ok (every (lambda (c) (getf c :ok)) checks)))
    (make-result :status (if all-ok :ok :error)
                 :message (format nil "doctor checks: ~D/~D passing"
                                  (count-if (lambda (c) (getf c :ok)) checks)
                                  (length checks))
                 :data (list :checks checks))))
