(in-package :hactar)

;;; Virtual File System for compiler output

(defstruct vfs
  "A virtual file system that collects compiler output."
  (files (make-hash-table :test 'equal) :type hash-table)
  (current-file nil :type (or null string)))

(defstruct vfs-file
  "A single virtual file."
  (path "" :type string)
  (content (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t) :type (array character (*)))
  (metadata nil :type list))

(defun vfs-select-file (vfs path)
  "Set the current output file. Creates it if it doesn't exist."
  (unless (gethash path (vfs-files vfs))
    (setf (gethash path (vfs-files vfs))
          (make-vfs-file :path path)))
  (setf (vfs-current-file vfs) path)
  path)

(defun vfs-current-vfs-file (vfs)
  "Return the current vfs-file struct, or signal error if none selected."
  (let ((path (vfs-current-file vfs)))
    (unless path
      (error "No file selected in VFS. Call vfs-select-file first."))
    (gethash path (vfs-files vfs))))

(defun vfs-write (vfs string)
  "Write STRING to the current file's content buffer."
  (let* ((vf (vfs-current-vfs-file vfs))
         (buf (vfs-file-content vf)))
    (loop for ch across string
          do (vector-push-extend ch buf))))

(defun vfs-prepend (vfs string)
  "Prepend STRING to the beginning of the current file's content buffer."
  (let* ((vf (vfs-current-vfs-file vfs))
         (old (coerce (vfs-file-content vf) 'string))
         (new-content (concatenate 'string string old))
         (new-buf (make-array (length new-content)
                              :element-type 'character
                              :fill-pointer (length new-content)
                              :adjustable t)))
    (replace new-buf new-content)
    (setf (vfs-file-content vf) new-buf)))

(defun vfs-writeln (vfs string)
  "Write STRING followed by a newline."
  (vfs-write vfs string)
  (vfs-write vfs (string #\Newline)))

(defun vfs-get-content (vfs path)
  "Return the accumulated string content for PATH, or NIL."
  (let ((vf (gethash path (vfs-files vfs))))
    (when vf
      (coerce (vfs-file-content vf) 'string))))

(defun vfs-all-paths (vfs)
  "Return a sorted list of all file paths in the VFS."
  (let ((paths nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k paths))
             (vfs-files vfs))
    (sort paths #'string<)))

(defun vfs-flush-to-disk (vfs output-root)
  "Write every virtual file to disk under OUTPUT-ROOT."
  (let ((*repo-root* (uiop:ensure-directory-pathname output-root))
        (written nil))
    (maphash (lambda (path vf)
               (let ((full-path (safe-write-to-file path (coerce (vfs-file-content vf) 'string))))
                 (push (namestring full-path) written)))
             (vfs-files vfs))
    (nreverse written)))

(defun vfs-flush-to-org (vfs &optional (stream *standard-output*) (tangle-dir "."))
  "Flush VFS contents as an Org-mode document with tangled src blocks."
  (format stream "#+PROPERTY: TANGLE_DIR ~A~%~%" tangle-dir)
  (maphash (lambda (path vf)
             (let* ((ext (pathname-type (pathname path)))
                    (lang (cond
                            ((member ext '("ts" "tsx") :test #'string=) "typescript")
                            ((member ext '("js" "jsx") :test #'string=) "javascript")
                            ((string= ext "lisp") "lisp")
                            ((string= ext "css") "css")
                            ((string= ext "html") "html")
                            (t ext)))
                    (content (coerce (vfs-file-content vf) 'string)))
               (format stream "* ~A~%" path)
               (format stream "#+begin_src ~A :tangle ~A~%" (or lang "") path)
               (write-string content stream)
               (unless (and (> (length content) 0)
                            (char= (char content (1- (length content))) #\Newline))
                 (terpri stream))
               (format stream "#+end_src~%~%")))
           (vfs-files vfs)))

(defun vfs-flush-to-lisp (vfs &optional (stream *standard-output*))
  "Flush VFS contents as a Lisp program of VFS operations that can recreate the files."
  (format stream "(let ((vfs (hactar:make-vfs)))~%")
  (dolist (path (vfs-all-paths vfs))
    (let* ((vf (gethash path (vfs-files vfs)))
           (content (coerce (vfs-file-content vf) 'string)))
      (format stream "  (hactar:vfs-select-file vfs ~S)~%" path)
      (format stream "  (hactar:vfs-write vfs ~S)~%" content)))
  (format stream "  vfs)~%"))

(defun vfs-flush-to-tags (vfs &optional (stream *standard-output*))
  "Flush VFS contents as <file path=PATH>CONTENT</file> tags."
  (dolist (path (vfs-all-paths vfs))
    (let* ((vf (gethash path (vfs-files vfs)))
           (content (coerce (vfs-file-content vf) 'string)))
      (format stream "<file path=~S>~A</file>~%" path content))))

(defun vfs-set-metadata (vfs path key value)
  "Set metadata on a virtual file."
  (let ((vf (gethash path (vfs-files vfs))))
    (when vf
      (setf (getf (vfs-file-metadata vf) key) value))))

(defun vfs-get-metadata (vfs path key)
  "Get metadata value for KEY on PATH."
  (let ((vf (gethash path (vfs-files vfs))))
    (when vf
      (getf (vfs-file-metadata vf) key))))
