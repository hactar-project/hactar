(in-package #:hactar-vfs)

(defstruct (vfs (:constructor %make-vfs))
  root
  (writes (make-hash-table :test 'equal)))

(defun make-vfs (&key (root (uiop:getcwd)))
  (%make-vfs :root (uiop:ensure-directory-pathname root)))

(defun safe-relative-path (path)
  (let ((pathname (pathname path)))
    (when (or (wild-pathname-p pathname)
              (uiop:absolute-pathname-p pathname)
              (member :up (pathname-directory pathname)))
      (error "VFS path must stay under its root: ~A" path))
    (namestring pathname)))

(defun contained-path-p (candidate root)
  (let ((candidate-name (namestring candidate))
        (root-name (namestring root)))
    (and (<= (length root-name) (length candidate-name))
         (string= root-name candidate-name :end2 (length root-name)))))

(defun parent-directory (path)
  (let ((directory (pathname-directory
                    (uiop:ensure-directory-pathname path))))
    (make-pathname :directory (butlast directory)
                   :name nil :type nil :defaults path)))

(defun nearest-existing-path (path)
  (loop for candidate = path then (parent-directory candidate)
        for existing = (probe-file candidate)
        when existing return (truename existing)
        when (equal candidate (parent-directory candidate))
          do (error "No existing ancestor for VFS path ~A" path)))

(defun assert-contained (vfs path)
  (let* ((root (truename (vfs-root vfs)))
         (target (merge-pathnames (safe-relative-path path) root))
         (resolved (if (probe-file target)
                       (truename target)
                       (nearest-existing-path
                        (uiop:pathname-directory-pathname target)))))
    (unless (contained-path-p resolved root)
      (error "VFS path escapes root through a symbolic link: ~A" path))
    target))

(defun full-path (vfs path)
  (assert-contained vfs path))

(defun vfs-write (vfs path content)
  "Stage CONTENT at relative PATH; no disk write occurs."
  (check-type content string)
  (let ((relative (safe-relative-path path)))
    (setf (gethash relative (vfs-writes vfs)) content)
    relative))

(defun vfs-read (vfs path)
  "Read staged content first, then disk content, returning NIL if absent."
  (let ((relative (safe-relative-path path)))
    (multiple-value-bind (content staged-p)
        (gethash relative (vfs-writes vfs))
      (if staged-p
          content
          (let ((disk-path (full-path vfs relative)))
            (when (probe-file disk-path)
              (uiop:read-file-string disk-path)))))))

(defun vfs-paths (vfs)
  (sort (loop for path being the hash-keys of (vfs-writes vfs) collect path)
        #'string<))

(defun vfs-plan (vfs)
  "Return deterministic write operations as plain data."
  (loop for path in (vfs-paths vfs)
        collect (list :write path :content (gethash path (vfs-writes vfs)))))

(defun vfs-diff (vfs)
  "Return staged changes as (:PATH :BEFORE :AFTER) plists."
  (loop for operation in (vfs-plan vfs)
        for path = (second operation)
        for before = (let ((disk-path (full-path vfs path)))
                       (when (probe-file disk-path)
                         (uiop:read-file-string disk-path)))
        for after = (getf (cddr operation) :content)
        unless (equal before after)
          collect (list :path path :before before :after after)))

(defun descriptor-path (descriptor &optional child)
  (if child
      (format nil "/proc/self/fd/~D/~A" descriptor child)
      (format nil "/proc/self/fd/~D/" descriptor)))

#+(and linux sbcl)
(defun open-directory-descriptor (path)
  (sb-posix:open path
                 (logior sb-posix:o-rdonly sb-posix:o-directory
                         sb-posix:o-nofollow)))

(defun portable-replace-file (vfs path content)
  "Portable atomic replacement after containment check."
  (let* ((target (assert-contained vfs path))
         (dir (uiop:pathname-directory-pathname target))
         (temp (merge-pathnames
                (format nil ".~A-~A.tmp" (file-namestring target) (random 1000000))
                dir)))
    (ensure-directories-exist target)
    (assert-contained vfs path)
    (with-open-file (stream temp :direction :output
                                 :if-exists :error
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
      (write-string content stream))
    (unwind-protect
         (uiop:rename-file-overwriting-target temp target)
      (when (probe-file temp)
        (ignore-errors (delete-file temp))))))

(defun secure-replace-file (vfs path content)
  "Replace PATH by walking from a pinned root without following symlinks."
  #+(and linux sbcl)
  (let* ((relative (pathname (safe-relative-path path)))
         (components (remove :relative (pathname-directory relative)))
         (root-descriptor
           (open-directory-descriptor (namestring (truename (vfs-root vfs)))))
         (parent-descriptor root-descriptor)
         (name (file-namestring relative))
         (temporary-name (format nil ".~A-~A.tmp" name (random 1000000))))
    (unwind-protect
         (progn
           (dolist (component components)
             (let ((child (descriptor-path parent-descriptor component)))
               (unless (probe-file child)
                 (sb-posix:mkdir child #o755))
               (let ((next (open-directory-descriptor child)))
                 (unless (= parent-descriptor root-descriptor)
                   (sb-posix:close parent-descriptor))
                 (setf parent-descriptor next))))
           (let ((temporary (descriptor-path parent-descriptor temporary-name))
                 (destination (descriptor-path parent-descriptor name)))
             (with-open-file (stream temporary :direction :output
                                             :if-exists :error
                                             :if-does-not-exist :create
                                             :external-format :utf-8)
               (write-string content stream))
             (unwind-protect
                  (rename-file temporary destination)
               (when (probe-file temporary)
                 (delete-file temporary)))))
      (unless (= parent-descriptor root-descriptor)
        (sb-posix:close parent-descriptor))
      (sb-posix:close root-descriptor)))
  #-(and linux sbcl)
  (portable-replace-file vfs path content))

(defun vfs-commit! (vfs)
  "Commit staged writes to disk and return the written pathnames."
  (let ((written nil))
    (dolist (operation (vfs-plan vfs))
      (let* ((path (second operation))
             (target (full-path vfs path))
             (content (getf (cddr operation) :content)))
        (secure-replace-file vfs path content)
        (push target written)))
    (clrhash (vfs-writes vfs))
    (nreverse written)))

(defun clear-vfs (vfs)
  (clrhash (vfs-writes vfs))
  vfs)

(defmacro with-staged-fs ((name &key root commit) &body body)
  "Bind NAME to a VFS, optionally committing only after BODY succeeds."
  `(let ((,name (make-vfs :root ,(or root '(uiop:getcwd)))))
     (multiple-value-prog1
         (progn ,@body)
       ,(when commit `(vfs-commit! ,name)))))
