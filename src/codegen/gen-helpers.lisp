;;* Code Generation helpers
(in-package #:hactar-codegen)

(defun scaffold-create-file (relative-path content &key (root (uiop:getcwd)) (overwrite t))
  "Stage and commit RELATIVE-PATH under ROOT via HACTAR-VFS."
  (let* ((vfs (hactar-vfs:make-vfs :root root))
         (existing (hactar-vfs:vfs-read vfs relative-path)))
    (when (and existing (not overwrite))
      (return-from scaffold-create-file nil))
    (hactar-vfs:vfs-write vfs relative-path content)
    (first (hactar-vfs:vfs-commit! vfs))))

(defun scaffold-modify-file (relative-path search-text replace-text &key (root (uiop:getcwd)))
  "Replace the first occurrence of SEARCH-TEXT with REPLACE-TEXT in RELATIVE-PATH."
  (let* ((vfs (hactar-vfs:make-vfs :root root))
         (content (hactar-vfs:vfs-read vfs relative-path)))
    (when content
      (let ((pos (search search-text content)))
        (when pos
          (let ((new-content (concatenate 'string
                                          (subseq content 0 pos)
                                          replace-text
                                          (subseq content (+ pos (length search-text))))))
            (hactar-vfs:vfs-write vfs relative-path new-content)
            (hactar-vfs:vfs-commit! vfs)
            t))))))

(defun scaffold-install-pkg (package &key (root (uiop:getcwd)) (pm :npm))
  "Install PACKAGE using PM (:npm/:bun/:pnpm/:yarn) under ROOT."
  (let ((cmd (case pm
               (:bun (list "bun" "add" package))
               (:pnpm (list "pnpm" "add" package))
               (:yarn (list "yarn" "add" package))
               (t (list "npm" "install" package)))))
    (multiple-value-bind (out err code)
        (uiop:run-program cmd
                          :directory root
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore out err))
      (zerop code))))
