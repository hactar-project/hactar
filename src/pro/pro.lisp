(in-package #:hactar-pro)

;;; Optional paid content-repo client (never required by hactar-core).

(defvar *pro-repo-path*
  (merge-pathnames ".local/share/hactar/pro/" (user-homedir-pathname)))

(defun pro-repo-url ()
  (or (uiop:getenv "HACTAR_PRO_REPO_URL")
      "git@github.com:hactar-project/pro.git"))

(defun pro-repo-present-p (&optional (dir *pro-repo-path*))
  (not (null (probe-file (merge-pathnames ".git/"
                                          (uiop:ensure-directory-pathname dir))))))

(defun pro-ensure-cloned (&optional (dir *pro-repo-path*))
  (if (pro-repo-present-p dir)
      (hactar-core:ok "Pro repo already cloned" :data (list :path (namestring dir)))
      (multiple-value-bind (out err code)
          (uiop:run-program (list "git" "clone" (pro-repo-url)
                                  (uiop:native-namestring
                                   (uiop:ensure-directory-pathname dir)))
                            :output :string :error-output :string
                            :ignore-error-status t)
        (declare (ignore out))
        (if (zerop code)
            (hactar-core:ok "Pro repo cloned" :data (list :path (namestring dir)))
            (hactar-core:err (format nil "Failed to clone Pro repo: ~A" err))))))

(defun pro-status (&optional (dir *pro-repo-path*))
  (hactar-core:ok (if (pro-repo-present-p dir) "present" "absent")
                  :data (list :present (pro-repo-present-p dir)
                              :url (pro-repo-url))))
