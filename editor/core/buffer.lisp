;;;; Buffer Abstraction for Hactar Editor
;;;; A buffer represents an editable text region with metadata.

(defpackage :hactar-editor/buffer
  (:use :cl :hactar-editor/gap-buffer)
  (:export #:buffer
           #:make-buffer
           #:buffer-name
           #:buffer-content
           #:buffer-point
           #:buffer-mark
           #:buffer-file-path
           #:buffer-modified-p
           #:buffer-mode
           #:buffer-local-variables
           #:buffer-undo-history
           #:buffer-ai-context
           #:buffer-overlays
           ;; Operations
           #:buffer-insert
           #:buffer-insert-string
           #:buffer-delete-forward
           #:buffer-delete-backward
           #:buffer-goto-char
           #:buffer-forward-char
           #:buffer-backward-char
           #:buffer-beginning-of-line
           #:buffer-end-of-line
           #:buffer-forward-line
           #:buffer-backward-line
           #:buffer-substring
           #:buffer-to-string
           #:buffer-line-number
           #:buffer-column
           #:buffer-length
           ;; Region operations
           #:buffer-set-mark
           #:buffer-region-active-p
           #:buffer-region-beginning
           #:buffer-region-end
           #:buffer-region-string
           #:buffer-kill-region
           ;; Undo
           #:buffer-undo
           #:buffer-redo
           ;; Buffer list
           #:*buffer-list*
           #:*current-buffer*
           #:current-buffer
           #:switch-to-buffer
           #:find-buffer
           #:kill-buffer
           #:find-file-buffer
           #:save-buffer))

(in-package :hactar-editor/buffer)

;;; Undo System
(defstruct undo-entry
  "An entry in the undo history."
  type          ; :insert, :delete, :compound
  position      ; Position where change occurred
  content       ; Content that was inserted/deleted
  point-before  ; Point before the change
  children)     ; For compound entries

(defclass buffer ()
  ((name :accessor buffer-name 
         :initarg :name
         :type string
         :documentation "The name of this buffer")
   (content :accessor buffer-content
            :initform (make-gap-buffer)
            :documentation "The gap buffer holding text content")
   (mark :accessor buffer-mark
         :initform nil
         :type (or null fixnum)
         :documentation "The mark position, or nil if not set")
   (mark-active :accessor buffer-mark-active-p
                :initform nil
                :type boolean
                :documentation "Whether the mark is active (region visible)")
   (file-path :accessor buffer-file-path
              :initarg :file-path
              :initform nil
              :documentation "Path to the file this buffer is visiting")
   (modified-p :accessor buffer-modified-p
               :initform nil
               :type boolean
               :documentation "Whether buffer has unsaved changes")
   (read-only-p :accessor buffer-read-only-p
                :initform nil
                :type boolean
                :documentation "Whether buffer is read-only")
   (mode :accessor buffer-mode
         :initarg :mode
         :initform 'fundamental-mode
         :documentation "The major mode of this buffer")
   (minor-modes :accessor buffer-minor-modes
                :initform nil
                :type list
                :documentation "List of active minor modes")
   (local-variables :accessor buffer-local-variables
                    :initform (make-hash-table :test 'eq)
                    :documentation "Buffer-local variable bindings")
   (undo-history :accessor buffer-undo-history
                 :initform nil
                 :type list
                 :documentation "Stack of undo entries")
   (redo-history :accessor buffer-redo-history
                 :initform nil
                 :type list
                 :documentation "Stack of redo entries")
   (ai-context :accessor buffer-ai-context
               :initform nil
               :documentation "AI context for this buffer")
   (overlays :accessor buffer-overlays
             :initform nil
             :type list
             :documentation "Visual overlays on the buffer"))
  (:documentation "A text buffer with editing capabilities."))

;;; Buffer List Management
(defvar *buffer-list* nil
  "List of all open buffers.")

(defvar *current-buffer* nil
  "The currently active buffer.")

(defun current-buffer ()
  "Return the current buffer."
  *current-buffer*)

(defun (setf current-buffer) (buffer)
  "Set the current buffer."
  (setf *current-buffer* buffer))

(defun make-buffer (name &key file-path mode content)
  "Create a new buffer with the given NAME."
  (let ((buffer (make-instance 'buffer
                               :name name
                               :file-path file-path
                               :mode (or mode 'fundamental-mode))))
    (when content
      (buffer-insert-string buffer content))
    (push buffer *buffer-list*)
    buffer))

(defun find-buffer (name)
  "Find a buffer by NAME."
  (find name *buffer-list* :key #'buffer-name :test #'string=))

(defun switch-to-buffer (buffer-or-name)
  "Switch to the specified buffer."
  (let ((buffer (if (stringp buffer-or-name)
                    (or (find-buffer buffer-or-name)
                        (make-buffer buffer-or-name))
                    buffer-or-name)))
    (setf *current-buffer* buffer)
    buffer))

(defun kill-buffer (buffer-or-name)
  "Kill the specified buffer."
  (let ((buffer (if (stringp buffer-or-name)
                    (find-buffer buffer-or-name)
                    buffer-or-name)))
    (when buffer
      (setf *buffer-list* (remove buffer *buffer-list*))
      (when (eq buffer *current-buffer*)
        (setf *current-buffer* (car *buffer-list*)))
      t)))

;;; Point and Mark Operations
(defun buffer-point (buffer)
  "Return the point (cursor position) in BUFFER."
  (gb-point (buffer-content buffer)))

(defun buffer-set-mark (buffer &optional pos)
  "Set the mark in BUFFER to POS (or current point if not specified)."
  (setf (buffer-mark buffer) (or pos (buffer-point buffer)))
  (setf (buffer-mark-active-p buffer) t))

(defun buffer-region-active-p (buffer)
  "Return T if BUFFER has an active region."
  (and (buffer-mark buffer)
       (buffer-mark-active-p buffer)))

(defun buffer-region-beginning (buffer)
  "Return the beginning of the region in BUFFER."
  (when (buffer-mark buffer)
    (min (buffer-point buffer) (buffer-mark buffer))))

(defun buffer-region-end (buffer)
  "Return the end of the region in BUFFER."
  (when (buffer-mark buffer)
    (max (buffer-point buffer) (buffer-mark buffer))))

;;; Basic Operations
(defun buffer-length (buffer)
  "Return the length of BUFFER's content."
  (gb-length (buffer-content buffer)))

(defun buffer-line-number (buffer)
  "Return the current line number in BUFFER."
  (gb-line-number (buffer-content buffer)))

(defun buffer-column (buffer)
  "Return the current column in BUFFER."
  (gb-column (buffer-content buffer)))

;;; Movement Operations
(defun buffer-goto-char (buffer pos)
  "Move point to position POS in BUFFER."
  (gb-move-point-to (buffer-content buffer) pos))

(defun buffer-forward-char (buffer &optional (n 1))
  "Move point forward N characters in BUFFER."
  (gb-move-point (buffer-content buffer) n))

(defun buffer-backward-char (buffer &optional (n 1))
  "Move point backward N characters in BUFFER."
  (gb-move-point (buffer-content buffer) (- n)))

(defun buffer-beginning-of-line (buffer)
  "Move point to beginning of current line in BUFFER."
  (let* ((content (buffer-content buffer))
         (line-start (gb-line-start content)))
    (gb-move-point-to content line-start)))

(defun buffer-end-of-line (buffer)
  "Move point to end of current line in BUFFER."
  (let* ((content (buffer-content buffer))
         (line-end (gb-line-end content)))
    (gb-move-point-to content line-end)))

(defun buffer-forward-line (buffer &optional (n 1))
  "Move point forward N lines in BUFFER."
  (let ((content (buffer-content buffer))
        (target-column (buffer-column buffer)))
    (dotimes (i (abs n))
      (if (plusp n)
          (progn
            (gb-move-point-to content (gb-line-end content))
            (gb-move-point content 1)) ; Skip newline
          (progn
            (gb-move-point-to content (gb-line-start content))
            (gb-move-point content -1)
            (gb-move-point-to content (gb-line-start content)))))
    ;; Try to restore column
    (let ((line-length (- (gb-line-end content) (gb-line-start content))))
      (gb-move-point content (min target-column line-length)))))

(defun buffer-backward-line (buffer &optional (n 1))
  "Move point backward N lines in BUFFER."
  (buffer-forward-line buffer (- n)))

;;; Insertion and Deletion
(defun record-undo (buffer type position content)
  "Record an undo entry in BUFFER."
  (let ((entry (make-undo-entry :type type
                                :position position
                                :content content
                                :point-before (buffer-point buffer))))
    (push entry (buffer-undo-history buffer))
    ;; Clear redo history on new change
    (setf (buffer-redo-history buffer) nil)))

(defun buffer-insert (buffer char)
  "Insert CHAR at point in BUFFER."
  (when (buffer-read-only-p buffer)
    (error "Buffer is read-only"))
  (let ((pos (buffer-point buffer)))
    (gb-insert (buffer-content buffer) char)
    (record-undo buffer :insert pos (string char))
    (setf (buffer-modified-p buffer) t)))

(defun buffer-insert-string (buffer string)
  "Insert STRING at point in BUFFER."
  (when (buffer-read-only-p buffer)
    (error "Buffer is read-only"))
  (let ((pos (buffer-point buffer)))
    (gb-insert-string (buffer-content buffer) string)
    (record-undo buffer :insert pos string)
    (setf (buffer-modified-p buffer) t)))

(defun buffer-delete-forward (buffer &optional (count 1))
  "Delete COUNT characters forward in BUFFER."
  (when (buffer-read-only-p buffer)
    (error "Buffer is read-only"))
  (let* ((pos (buffer-point buffer))
         (deleted (gb-delete-forward (buffer-content buffer) count)))
    (when deleted
      (record-undo buffer :delete pos deleted)
      (setf (buffer-modified-p buffer) t))
    deleted))

(defun buffer-delete-backward (buffer &optional (count 1))
  "Delete COUNT characters backward in BUFFER."
  (when (buffer-read-only-p buffer)
    (error "Buffer is read-only"))
  (let* ((pos (buffer-point buffer))
         (deleted (gb-delete-backward (buffer-content buffer) count)))
    (when deleted
      (record-undo buffer :delete (- pos count) deleted)
      (setf (buffer-modified-p buffer) t))
    deleted))

;;; Region Operations
(defun buffer-region-string (buffer)
  "Return the text in the active region of BUFFER."
  (when (buffer-region-active-p buffer)
    (buffer-substring buffer 
                      (buffer-region-beginning buffer)
                      (buffer-region-end buffer))))

(defun buffer-kill-region (buffer)
  "Kill (cut) the region in BUFFER. Returns the killed text."
  (when (buffer-region-active-p buffer)
    (let* ((start (buffer-region-beginning buffer))
           (end (buffer-region-end buffer))
           (text (buffer-substring buffer start end)))
      (buffer-goto-char buffer start)
      (buffer-delete-forward buffer (- end start))
      (setf (buffer-mark-active-p buffer) nil)
      text)))

;;; Substring Operations
(defun buffer-substring (buffer start &optional end)
  "Return a substring of BUFFER from START to END."
  (gb-substring (buffer-content buffer) start end))

(defun buffer-to-string (buffer)
  "Return the entire contents of BUFFER as a string."
  (gb-to-string (buffer-content buffer)))

;;; Undo/Redo
(defun buffer-undo (buffer)
  "Undo the last change in BUFFER."
  (let ((entry (pop (buffer-undo-history buffer))))
    (when entry
      (push entry (buffer-redo-history buffer))
      (let ((content (buffer-content buffer)))
        (case (undo-entry-type entry)
          (:insert
           ;; Undo insert by deleting
           (gb-move-point-to content (undo-entry-position entry))
           (gb-delete-forward content (length (undo-entry-content entry))))
          (:delete
           ;; Undo delete by inserting
           (gb-move-point-to content (undo-entry-position entry))
           (gb-insert-string content (undo-entry-content entry)))))
      ;; Restore point
      (gb-move-point-to (buffer-content buffer) 
                        (undo-entry-point-before entry))
      t)))

(defun buffer-redo (buffer)
  "Redo the last undone change in BUFFER."
  (let ((entry (pop (buffer-redo-history buffer))))
    (when entry
      (push entry (buffer-undo-history buffer))
      (let ((content (buffer-content buffer)))
        (case (undo-entry-type entry)
          (:insert
           ;; Redo insert by inserting
           (gb-move-point-to content (undo-entry-position entry))
           (gb-insert-string content (undo-entry-content entry)))
          (:delete
           ;; Redo delete by deleting
           (gb-move-point-to content (undo-entry-position entry))
           (gb-delete-forward content (length (undo-entry-content entry))))))
      t)))

;;; File Operations
(defun find-file-buffer (file-path)
  "Find or create a buffer visiting FILE-PATH."
  (or (find file-path *buffer-list* 
            :key #'buffer-file-path 
            :test #'equal)
      (let* ((name (file-namestring file-path))
             (buffer (make-buffer name :file-path file-path)))
        (when (probe-file file-path)
          (let ((content (uiop:read-file-string file-path)))
            (gb-insert-string (buffer-content buffer) content)
            (gb-move-point-to (buffer-content buffer) 0)
            (setf (buffer-modified-p buffer) nil)))
        buffer)))

(defun save-buffer (buffer)
  "Save BUFFER to its file."
  (let ((path (buffer-file-path buffer)))
    (unless path
      (error "Buffer ~A has no file path" (buffer-name buffer)))
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
      (write-string (buffer-to-string buffer) stream))
    (setf (buffer-modified-p buffer) nil)
    path))
