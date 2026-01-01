;;;; Gap Buffer Implementation for Hactar Editor
;;;; A gap buffer is an efficient data structure for text editing
;;;; with O(1) insertions/deletions at the cursor position.

(defpackage :hactar-editor/gap-buffer
  (:use :cl)
  (:export #:gap-buffer
           #:make-gap-buffer
           #:gap-buffer-p
           #:gb-length
           #:gb-point
           #:gb-char-at
           #:gb-insert
           #:gb-insert-string
           #:gb-delete-forward
           #:gb-delete-backward
           #:gb-move-point
           #:gb-move-point-to
           #:gb-substring
           #:gb-to-string
           #:gb-line-start
           #:gb-line-end
           #:gb-line-number
           #:gb-column))

(in-package :hactar-editor/gap-buffer)

(defconstant +initial-gap-size+ 2048
  "Initial size of the gap in characters.")

(defconstant +gap-growth-factor+ 2
  "Factor by which to grow the gap when it fills up.")

(defstruct (gap-buffer (:constructor %make-gap-buffer))
  "A gap buffer for efficient text editing.
   The buffer is divided into three parts:
   [before-gap content][gap][after-gap content]
   
   Slots:
   - data: The underlying character array
   - gap-start: Index where the gap begins (also the logical 'point')
   - gap-end: Index where the gap ends
   - length: Logical length of content (excluding gap)"
  (data (make-array +initial-gap-size+ 
                    :element-type 'character 
                    :adjustable t)
        :type (array character (*)))
  (gap-start 0 :type fixnum)
  (gap-end +initial-gap-size+ :type fixnum)
  (length 0 :type fixnum))

(defun make-gap-buffer (&optional (initial-content ""))
  "Create a new gap buffer, optionally initialized with INITIAL-CONTENT."
  (let* ((content-length (length initial-content))
         (total-size (+ content-length +initial-gap-size+))
         (buffer (%make-gap-buffer
                  :data (make-array total-size
                                    :element-type 'character
                                    :adjustable t)
                  :gap-start 0
                  :gap-end +initial-gap-size+
                  :length content-length)))
    ;; Copy initial content after the gap
    (when (> content-length 0)
      (replace (gap-buffer-data buffer) initial-content
               :start1 +initial-gap-size+))
    buffer))

(defun gap-size (gb)
  "Return the current size of the gap."
  (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))

(defun gb-length (gb)
  "Return the logical length of the buffer content."
  (gap-buffer-length gb))

(defun gb-point (gb)
  "Return the current point (cursor position) in the buffer."
  (gap-buffer-gap-start gb))

(defun logical-to-physical (gb logical-pos)
  "Convert a logical position to a physical position in the data array."
  (if (< logical-pos (gap-buffer-gap-start gb))
      logical-pos
      (+ logical-pos (gap-size gb))))

(defun gb-char-at (gb pos)
  "Return the character at logical position POS."
  (when (and (>= pos 0) (< pos (gap-buffer-length gb)))
    (aref (gap-buffer-data gb) (logical-to-physical gb pos))))

(defun ensure-gap-size (gb needed)
  "Ensure the gap is at least NEEDED characters large."
  (when (< (gap-size gb) needed)
    (let* ((current-size (length (gap-buffer-data gb)))
           (new-gap-size (* (max needed (gap-size gb)) +gap-growth-factor+))
           (new-size (+ current-size new-gap-size))
           (new-data (make-array new-size :element-type 'character :adjustable t))
           (after-gap-length (- current-size (gap-buffer-gap-end gb))))
      ;; Copy content before gap
      (replace new-data (gap-buffer-data gb)
               :end1 (gap-buffer-gap-start gb)
               :end2 (gap-buffer-gap-start gb))
      ;; Copy content after gap (to new position)
      (replace new-data (gap-buffer-data gb)
               :start1 (+ (gap-buffer-gap-start gb) new-gap-size)
               :start2 (gap-buffer-gap-end gb)
               :end2 current-size)
      ;; Update buffer
      (setf (gap-buffer-data gb) new-data)
      (setf (gap-buffer-gap-end gb) 
            (+ (gap-buffer-gap-start gb) new-gap-size)))))

(defun move-gap-to (gb pos)
  "Move the gap so that gap-start is at logical position POS."
  (let ((gap-start (gap-buffer-gap-start gb))
        (gap-end (gap-buffer-gap-end gb))
        (data (gap-buffer-data gb)))
    (cond
      ((= pos gap-start)
       ;; Already there
       nil)
      ((< pos gap-start)
       ;; Move gap left: shift content right into gap
       (let ((shift-count (- gap-start pos)))
         (replace data data
                  :start1 (- gap-end shift-count)
                  :end1 gap-end
                  :start2 pos
                  :end2 gap-start)
         (setf (gap-buffer-gap-start gb) pos)
         (setf (gap-buffer-gap-end gb) (- gap-end shift-count))))
      (t
       ;; Move gap right: shift content left into gap
       (let* ((physical-pos (+ pos (gap-size gb)))
              (shift-count (- pos gap-start)))
         (replace data data
                  :start1 gap-start
                  :end1 (+ gap-start shift-count)
                  :start2 gap-end
                  :end2 physical-pos)
         (setf (gap-buffer-gap-start gb) pos)
         (setf (gap-buffer-gap-end gb) physical-pos))))))

(defun gb-move-point (gb delta)
  "Move the point by DELTA characters (positive = forward, negative = backward).
   Returns the new point position."
  (let* ((current (gap-buffer-gap-start gb))
         (new-pos (max 0 (min (gap-buffer-length gb) (+ current delta)))))
    (move-gap-to gb new-pos)
    new-pos))

(defun gb-move-point-to (gb pos)
  "Move the point to absolute position POS.
   Returns the new point position."
  (let ((clamped-pos (max 0 (min (gap-buffer-length gb) pos))))
    (move-gap-to gb clamped-pos)
    clamped-pos))

(defun gb-insert (gb char)
  "Insert CHAR at point."
  (ensure-gap-size gb 1)
  (setf (aref (gap-buffer-data gb) (gap-buffer-gap-start gb)) char)
  (incf (gap-buffer-gap-start gb))
  (incf (gap-buffer-length gb)))

(defun gb-insert-string (gb string)
  "Insert STRING at point."
  (let ((len (length string)))
    (ensure-gap-size gb len)
    (replace (gap-buffer-data gb) string
             :start1 (gap-buffer-gap-start gb))
    (incf (gap-buffer-gap-start gb) len)
    (incf (gap-buffer-length gb) len)))

(defun gb-delete-forward (gb &optional (count 1))
  "Delete COUNT characters after point. Returns the deleted text."
  (let* ((available (- (gap-buffer-length gb) (gap-buffer-gap-start gb)))
         (actual-count (min count available)))
    (when (> actual-count 0)
      (let ((deleted (make-string actual-count)))
        (replace deleted (gap-buffer-data gb)
                 :start2 (gap-buffer-gap-end gb)
                 :end2 (+ (gap-buffer-gap-end gb) actual-count))
        (incf (gap-buffer-gap-end gb) actual-count)
        (decf (gap-buffer-length gb) actual-count)
        deleted))))

(defun gb-delete-backward (gb &optional (count 1))
  "Delete COUNT characters before point. Returns the deleted text."
  (let* ((available (gap-buffer-gap-start gb))
         (actual-count (min count available)))
    (when (> actual-count 0)
      (let ((deleted (make-string actual-count)))
        (replace deleted (gap-buffer-data gb)
                 :start2 (- (gap-buffer-gap-start gb) actual-count)
                 :end2 (gap-buffer-gap-start gb))
        (decf (gap-buffer-gap-start gb) actual-count)
        (decf (gap-buffer-length gb) actual-count)
        deleted))))

(defun gb-substring (gb start &optional end)
  "Return a substring of the buffer from START to END (or end of buffer)."
  (let* ((actual-end (or end (gap-buffer-length gb)))
         (result (make-string (- actual-end start)))
         (gap-start (gap-buffer-gap-start gb))
         (gap-end (gap-buffer-gap-end gb))
         (data (gap-buffer-data gb)))
    (cond
      ;; Entirely before gap
      ((<= actual-end gap-start)
       (replace result data :start2 start :end2 actual-end))
      ;; Entirely after gap
      ((>= start gap-start)
       (replace result data
                :start2 (+ start (gap-size gb))
                :end2 (+ actual-end (gap-size gb))))
      ;; Spans the gap
      (t
       (let ((before-gap-len (- gap-start start)))
         (replace result data :start2 start :end2 gap-start)
         (replace result data
                  :start1 before-gap-len
                  :start2 gap-end
                  :end2 (+ gap-end (- actual-end gap-start))))))
    result))

(defun gb-to-string (gb)
  "Convert the entire buffer contents to a string."
  (gb-substring gb 0 (gap-buffer-length gb)))

(defun gb-line-start (gb &optional (pos (gb-point gb)))
  "Return the position of the start of the line containing POS."
  (loop for i from (1- pos) downto 0
        when (char= (gb-char-at gb i) #\Newline)
          return (1+ i)
        finally (return 0)))

(defun gb-line-end (gb &optional (pos (gb-point gb)))
  "Return the position of the end of the line containing POS."
  (loop for i from pos below (gap-buffer-length gb)
        when (char= (gb-char-at gb i) #\Newline)
          return i
        finally (return (gap-buffer-length gb))))

(defun gb-line-number (gb &optional (pos (gb-point gb)))
  "Return the line number (1-based) of the line containing POS."
  (1+ (loop for i from 0 below pos
            count (char= (gb-char-at gb i) #\Newline))))

(defun gb-column (gb &optional (pos (gb-point gb)))
  "Return the column number (0-based) of POS within its line."
  (- pos (gb-line-start gb pos)))
