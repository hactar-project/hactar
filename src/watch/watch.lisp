(in-package #:hactar-watch)

(defstruct (watcher (:constructor %make-watcher))
  root
  (callbacks nil)
  (events nil)
  (snapshot (make-hash-table :test 'equal))
  active-p)

(defun canonical-entry-p (entry root)
  (let ((resolved (ignore-errors (truename entry))))
    (and resolved
         (string= (namestring resolved) (namestring entry))
         (let ((root-name (namestring root))
               (resolved-name (namestring resolved)))
           (and (<= (length root-name) (length resolved-name))
                (string= root-name resolved-name
                         :end2 (length root-name)))))))

(defun file-signature (file)
  (with-open-file (stream file :direction :input
                               :element-type '(unsigned-byte 8))
    (let ((hash 5381)
          (size 0))
      (loop for byte = (read-byte stream nil nil)
            while byte
            do (setf hash (logand most-positive-fixnum
                                  (+ (* hash 33) byte)))
               (incf size))
      (list (file-write-date file) size hash))))

(defun file-snapshot (root)
  (let ((snapshot (make-hash-table :test 'equal))
        (canonical-root (truename root)))
    (labels ((visit (directory)
               (dolist (file (uiop:directory-files directory))
                 (when (canonical-entry-p file canonical-root)
                   (setf (gethash (namestring file) snapshot)
                         (file-signature file))))
               (dolist (child (uiop:subdirectories directory))
                 (when (canonical-entry-p child canonical-root)
                   (visit child)))))
      (visit root))
    snapshot))

(defun watch (root)
  "Create an active watcher. Call POLL-WATCH from the owning event loop."
  (let ((directory (uiop:ensure-directory-pathname root)))
    (%make-watcher :root directory :snapshot (file-snapshot directory)
                   :active-p t)))

(defun unwatch (watcher)
  (setf (watcher-active-p watcher) nil)
  watcher)

(defun watching-p (watcher)
  (watcher-active-p watcher))

(defun on-change (watcher callback)
  "Register CALLBACK of one event plist and return WATCHER."
  (setf (watcher-callbacks watcher)
        (append (watcher-callbacks watcher) (list callback)))
  watcher)

(defun emit-event (watcher event)
  "Queue EVENT and synchronously notify callbacks."
  (setf (watcher-events watcher)
        (append (watcher-events watcher) (list event)))
  (dolist (callback (watcher-callbacks watcher))
    (funcall callback event))
  event)

(defun poll-watch (watcher)
  "Compare filesystem state with the last snapshot and emit change data."
  (unless (watcher-active-p watcher)
    (return-from poll-watch nil))
  (let ((previous (watcher-snapshot watcher))
        (current (file-snapshot (watcher-root watcher)))
        (events nil))
    (maphash (lambda (path signature)
               (multiple-value-bind (old present-p) (gethash path previous)
                 (let ((kind (cond ((not present-p) :created)
                                   ((not (equal old signature)) :modified))))
                   (when kind
                     (push (emit-event watcher
                                       (list :kind kind :path path
                                             :signature signature))
                           events)))))
             current)
    (maphash (lambda (path signature)
               (declare (ignore signature))
               (unless (gethash path current)
                 (push (emit-event watcher (list :kind :deleted :path path))
                       events)))
             previous)
    (setf (watcher-snapshot watcher) current)
    (nreverse events)))

(defun pending-events (watcher)
  (copy-list (watcher-events watcher)))

(defun drain-events (watcher)
  (prog1 (watcher-events watcher)
    (setf (watcher-events watcher) nil)))
