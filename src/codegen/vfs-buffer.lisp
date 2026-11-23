(in-package #:hactar-codegen)

;;; Cursor/append output buffer for compiler emitters, converting cleanly to
;;; HACTAR-VFS plans for inspection and atomic disk commits.

(defstruct emit-buffer
  (files (make-hash-table :test 'equal) :type hash-table)
  (current-file nil :type (or null string)))

(defstruct emit-file
  (path "" :type string)
  (content (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
           :type (array character (*)))
  (metadata nil :type list))

(defun make-vfs ()
  "Create a new compiler output buffer."
  (make-emit-buffer))

(defun vfs-current-file (vfs)
  (emit-buffer-current-file vfs))

(defun vfs-select-file (vfs path)
  "Select PATH as the active output file, creating it if absent."
  (unless (gethash path (emit-buffer-files vfs))
    (setf (gethash path (emit-buffer-files vfs))
          (make-emit-file :path path)))
  (setf (emit-buffer-current-file vfs) path)
  path)

(defun %current-emit-file (vfs)
  (let ((path (emit-buffer-current-file vfs)))
    (unless path
      (error "No file selected in codegen buffer. Call VFS-SELECT-FILE first."))
    (gethash path (emit-buffer-files vfs))))

(defun vfs-write (vfs string)
  (let* ((ef (%current-emit-file vfs))
         (buf (emit-file-content ef)))
    (loop for ch across string do (vector-push-extend ch buf))))

(defun vfs-prepend (vfs string)
  (let* ((ef (%current-emit-file vfs))
         (old (coerce (emit-file-content ef) 'string))
         (new-content (concatenate 'string string old))
         (new-buf (make-array (length new-content)
                              :element-type 'character
                              :fill-pointer (length new-content)
                              :adjustable t)))
    (replace new-buf new-content)
    (setf (emit-file-content ef) new-buf)))

(defun vfs-writeln (vfs string)
  (vfs-write vfs string)
  (vfs-write vfs (string #\Newline)))

(defun vfs-get-content (vfs path)
  (let ((ef (gethash path (emit-buffer-files vfs))))
    (when ef (coerce (emit-file-content ef) 'string))))

(defun vfs-all-paths (vfs)
  (let ((paths nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k paths))
             (emit-buffer-files vfs))
    (sort paths #'string<)))

(defun emit-buffer->vfs (buffer &key (root (uiop:getcwd)))
  "Convert an EMIT-BUFFER into an inspectable HACTAR-VFS:VFS plan."
  (let ((staged (hactar-vfs:make-vfs :root root)))
    (dolist (path (vfs-all-paths buffer))
      (hactar-vfs:vfs-write staged path (vfs-get-content buffer path)))
    staged))

(defun vfs-flush-to-disk (vfs output-root)
  "Commit all generated files under OUTPUT-ROOT through HACTAR-VFS."
  (let ((staged (emit-buffer->vfs vfs :root output-root)))
    (mapcar #'namestring (hactar-vfs:vfs-commit! staged))))

(defun vfs-flush-to-org (vfs &optional (stream *standard-output*) (tangle-dir "."))
  (format stream "#+PROPERTY: TANGLE_DIR ~A~%~%" tangle-dir)
  (dolist (path (vfs-all-paths vfs))
    (let* ((ext (pathname-type (pathname path)))
           (lang (cond
                   ((member ext '("ts" "tsx") :test #'string=) "typescript")
                   ((member ext '("js" "jsx") :test #'string=) "javascript")
                   ((string= ext "lisp") "lisp")
                   ((string= ext "css") "css")
                   ((string= ext "html") "html")
                   (t ext)))
           (content (vfs-get-content vfs path)))
      (format stream "* ~A~%" path)
      (format stream "#+begin_src ~A :tangle ~A~%" (or lang "") path)
      (write-string content stream)
      (unless (and (plusp (length content))
                   (char= (char content (1- (length content))) #\Newline))
        (terpri stream))
      (format stream "#+end_src~%~%"))))

(defun vfs-flush-to-lisp (vfs &optional (stream *standard-output*))
  (format stream "(let ((vfs (hactar-codegen:make-vfs)))~%")
  (dolist (path (vfs-all-paths vfs))
    (format stream "  (hactar-codegen:vfs-select-file vfs ~S)~%" path)
    (format stream "  (hactar-codegen:vfs-write vfs ~S)~%" (vfs-get-content vfs path)))
  (format stream "  vfs)~%"))

(defun vfs-flush-to-tags (vfs &optional (stream *standard-output*))
  (dolist (path (vfs-all-paths vfs))
    (format stream "<file path=~S>~A</file>~%" path (vfs-get-content vfs path))))

(defun vfs-set-metadata (vfs path key value)
  (let ((ef (gethash path (emit-buffer-files vfs))))
    (when ef (setf (getf (emit-file-metadata ef) key) value))))

(defun vfs-get-metadata (vfs path key)
  (let ((ef (gethash path (emit-buffer-files vfs))))
    (when ef (getf (emit-file-metadata ef) key))))
