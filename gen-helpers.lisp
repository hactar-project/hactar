;;* Code Generation helpers
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(scaffold-create-file
            scaffold-modify-file
            scaffold-install-pkg)))

(defun scaffold-create-file (relative-path content &key (confirm t))
  "Create a file at RELATIVE-PATH (under *repo-root*) with CONTENT.
   When CONFIRM and the file exists, ask before overwriting.
   Returns the full pathname on success, NIL otherwise."
  (let ((full-path (merge-pathnames relative-path *repo-root*)))
    (if (and confirm
             (probe-file full-path)
             (not (confirm-action (format nil "Overwrite ~A?" relative-path))))
        (progn
          (format t "~&Skipped: ~A~%" relative-path)
          nil)
        (progn
          (ensure-directories-exist full-path)
          (write-file-content (uiop:native-namestring full-path) content)
          (format t "~&Created: ~A~%" relative-path)
          full-path))))

(defun scaffold-modify-file (relative-path search-text replace-text)
  "Replace the first occurrence of SEARCH-TEXT with REPLACE-TEXT in RELATIVE-PATH.
   Returns T on success, NIL otherwise."
  (let ((full-path (merge-pathnames relative-path *repo-root*)))
    (cond
      ((not (probe-file full-path))
       (format t "~&File not found: ~A~%" relative-path)
       nil)
      (t
       (let* ((content (uiop:read-file-string full-path))
              (pos (search search-text content)))
         (if pos
             (let ((new-content (concatenate 'string
                                             (subseq content 0 pos)
                                             replace-text
                                             (subseq content (+ pos (length search-text))))))
               (write-file-content (uiop:native-namestring full-path) new-content)
               (format t "~&Modified: ~A~%" relative-path)
               t)
             (progn
               (format t "~&Search text not found in ~A~%" relative-path)
               nil)))))))

(defun scaffold-install-pkg (package)
  "Install PACKAGE using the inferred JS package manager (composition helper).
   Returns T on success. Falls back to npm when inference is unavailable."
  (let* ((pm (if (fboundp 'infer-package-manager)
                 (funcall 'infer-package-manager)
                 :npm))
         (cmd (case pm
                (:bun (format nil "bun add ~A" package))
                (:pnpm (format nil "pnpm add ~A" package))
                (:yarn (format nil "yarn add ~A" package))
                (t (format nil "npm install ~A" package)))))
    (format t "~&Installing ~A (~A)...~%" package cmd)
    (multiple-value-bind (out err code)
        (uiop:run-program cmd
                          :directory *repo-root*
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore out))
      (if (zerop code)
          (progn (format t "~&Installed: ~A~%" package) t)
          (progn (format t "~&Failed to install ~A: ~A~%" package err) nil)))))
