(in-package #:hactar-rebuild)

;;; ---------------------------------------------------------------------------
;;; .hactar.lisp path
;;; ---------------------------------------------------------------------------

(defun hactar-file-path (&optional (root (or *repo-root* (uiop:getcwd))))
  (merge-pathnames ".hactar.lisp" (uiop:ensure-directory-pathname root)))

;;; ---------------------------------------------------------------------------
;;; Reading (validate-before-eval, *read-eval* disabled)
;;; ---------------------------------------------------------------------------

(defun read-forms (path)
  "Read all forms from PATH with reader eval disabled. Honors IN-PACKAGE."
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((*read-eval* nil)
          (*package* (find-package "HACTAR-REBUILD"))
          (forms '()))
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (progn
                 (push form forms)
                 (when (and (consp form) (symbolp (car form))
                            (string-equal (symbol-name (car form)) "in-package"))
                   (let ((package (find-package (second form))))
                     (unless package
                       (error "Unknown package in IN-PACKAGE: ~A" (second form)))
                     (setf *package* package)))))
      (nreverse forms))))

(defun load-hactar-file (path)
  "Validate then load PATH into a fresh hactar-user environment."
  (let ((forms (read-forms path)))
    (multiple-value-bind (ok errors) (validate-forms forms)
      (unless ok (error "Invalid .hactar.lisp: ~{~A~^; ~}" errors)))
    (reset-registries)
    (let ((*package* (find-package "HACTAR-USER")))
      (dolist (form forms)
        (unless (and (consp form) (symbolp (car form))
                     (string-equal (symbol-name (car form)) "in-package"))
          (eval form)))))
  t)

;;; ---------------------------------------------------------------------------
;;; Writing: replace only the (generated ...) region; preserve user region
;;; ---------------------------------------------------------------------------

(defparameter +user-region-marker+
  ";; --- USER REGION (preserved across updates) ---"
  "Everything from this marker onward is user-owned and never overwritten.")

(defun now-timestamp ()
  (multiple-value-bind (s mi h d mo y dow) (get-decoded-time)
    (declare (ignore s))
    (let ((days #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
      (format nil "<~D-~2,'0D-~2,'0D ~A ~2,'0D:~2,'0D>"
              y mo d (aref days dow) h mi))))

(defun rehome-symbol (obj)
  "Re-intern OBJ into hactar-user if it is an internal hactar-rebuild symbol.
Exported DSL operators (defproject, deftool, run-test-cmd, ...) are left alone
so they still resolve to the engine's macros/functions; only local names
(tool names, lambda-list args, skill/agent names) are re-homed for readability."
  (if (and (symbolp obj) (not (keywordp obj)) (symbol-package obj)
           (eq (symbol-package obj) (find-package :hactar-rebuild)))
      (multiple-value-bind (sym status)
          (find-symbol (symbol-name obj) :hactar-rebuild)
        (declare (ignore sym))
        (if (eq status :external)
            obj
            (intern (symbol-name obj) :hactar-user)))
      obj))

(defun rehome-tree (form)
  "Recursively re-home internal engine symbols in FORM into hactar-user."
  (cond
    ((consp form) (cons (rehome-tree (car form)) (rehome-tree (cdr form))))
    (t (rehome-symbol form))))

(defun render-generated-block (generated-forms &key (updated-at (now-timestamp)))
  "Render GENERATED-FORMS as a single (generated :updated-at ... ...) block.
Printed in the hactar-user package so DSL forms read back unqualified."
  (with-output-to-string (s)
    (let ((*print-case* :downcase)
          (*print-right-margin* 100)
          (*package* (find-package :hactar-user)))
      (format s "(generated :updated-at ~S~%" updated-at)
      (dolist (form (mapcar #'rehome-tree generated-forms))
        (format s "~%  ~S" form))
      (format s ")~%"))))

(defun extract-user-region (old-content)
  "Return the user-owned region of OLD-CONTENT (from the marker on), or a
fresh empty user region if none exists."
  (if (and old-content (search +user-region-marker+ old-content))
      (subseq old-content (search +user-region-marker+ old-content))
      (format nil "~A~%" +user-region-marker+)))

(defun atomic-write (path content)
  "Write CONTENT to PATH via a temp file + rename."
  (let ((tmp (make-pathname :type "tmp.new"
                            :defaults (merge-pathnames path))))
    (with-open-file (s tmp :direction :output :if-exists :supersede
                           :if-does-not-exist :create :external-format :utf-8)
      (write-string content s))
    (rename-file tmp path)
    path))

(defun write-hactar-file (path generated-forms &key (updated-at (now-timestamp)))
  "Replace only the (generated ...) region; preserve everything else."
  (let* ((old (when (probe-file path) (uiop:read-file-string path)))
         (user-region (extract-user-region old))
         (block (render-generated-block generated-forms :updated-at updated-at)))
    (atomic-write path
      (format nil "(in-package :hactar-user)~%~%~A~%~A~%" block user-region))))
