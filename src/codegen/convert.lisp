;; conversion helpers
(in-package #:hactar-codegen)

(defun %language-hint-from-extension (ext)
  (when ext
    (let ((e (string-downcase ext)))
      (cond
        ((member e '("ts" "tsx" "mts") :test #'string=) "typescript")
        ((member e '("js" "jsx" "mjs") :test #'string=) "javascript")
        ((string= e "css") "css")
        ((string= e "json") "json")
        ((string= e "toml") "toml")
        (t e)))))

(defun redwood-collect-source-files (project-path)
  "Collect all relevant RedwoodSDK source files from PROJECT-PATH.
   Returns a list of (relative-path . content) pairs."
  (let ((extensions '("ts" "tsx" "css" "json" "toml" "mts"))
        (skip-dirs '("node_modules" ".git" "dist" ".wrangler" ".tmp"))
        (results '()))
    (labels ((skip-dir-p (dir-name)
               (member dir-name skip-dirs :test #'string=))
             (walk (dir)
               (dolist (entry (uiop:directory* (merge-pathnames uiop:*wild-file-for-directory* dir)))
                 (let ((name (uiop:native-namestring (uiop:enough-pathname entry project-path))))
                   (if (uiop:directory-pathname-p entry)
                       (unless (skip-dir-p (car (last (pathname-directory entry))))
                         (walk entry))
                       (let ((ext (pathname-type entry)))
                         (when (and ext (member (string-downcase ext) extensions :test #'string=))
                           (handler-case
                               (push (cons name (uiop:read-file-string entry :external-format :utf-8))
                                     results)
                             (error (e)
                               (format *error-output* "Warning: could not read ~A: ~A~%" name e))))))))))
      (walk (uiop:ensure-directory-pathname project-path)))
    (nreverse results)))

(defun redwood-build-user-message (file-pairs)
  "Build the user message from FILE-PAIRS, each (relative-path . content)."
  (with-output-to-string (s)
    (format s "Please convert the following RedwoodSDK project into a single Hactar Lisp file.~%~%")
    (dolist (pair file-pairs)
      (let* ((path (car pair))
             (content (cdr pair))
             (ext (pathname-type (pathname path)))
             (lang (or (%language-hint-from-extension ext) "")))
        (format s "```~A ~A~%~A~%```~%~%" lang path content)))))

(defun convert-redwood (project-path &key ask-fn (system-prompt "Convert RedwoodSDK code into Hactar Lisp forms.")
                                          output-file (root (uiop:getcwd)))
  "Collect RedwoodSDK files under PROJECT-PATH and invoke ASK-FN with
(user-message system-prompt). When OUTPUT-FILE is given, stage/commit via HACTAR-VFS."
  (let ((dir (uiop:ensure-directory-pathname project-path)))
    (unless (probe-file dir)
      (error "Project path not found: ~A" project-path))
    (unless ask-fn
      (error "CONVERT-REDWOOD requires :ASK-FN"))
    (let* ((file-pairs (redwood-collect-source-files dir))
           (user-message (redwood-build-user-message file-pairs))
           (response (funcall ask-fn user-message system-prompt)))
      (when (and response output-file)
        (let ((vfs (hactar-vfs:make-vfs :root root)))
          (hactar-vfs:vfs-write vfs output-file response)
          (hactar-vfs:vfs-commit! vfs)))
      (values response (length file-pairs)))))

(defun convert (kind source-path &rest args)
  "Generic conversion entry point."
  (ecase kind
    (:redwood (apply #'convert-redwood source-path args))))
