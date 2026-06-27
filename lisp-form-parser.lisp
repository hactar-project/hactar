(in-package :hactar)

(defparameter *hactar-form-schemas*
  '((defproject   . (:required (name) :optional (:type :root :modes :scanned-at)))
    (defroutes    . (:elements (:required (:file :path) :optional (:component :method :middleware :line-start :line-end :tags :note))))
    (defmodels    . (:elements (:required (:name :file) :optional (:fields :note :line-start :line-end))))
    (defmodules   . (:elements (:required (:module :file) :optional (:exports :imports :type))))
    (defscripts   . (:elements (:required (:script :command) :optional (:description))))
    (defcomponents . (:elements (:required (:component :file) :optional (:props :state :children))))
    (defdeps      . (:elements (:required (:dep :version) :optional (:type :registry))))
    (defimports   . (:elements (:required (:file :imports) :optional ())))
    (defenvs      . (:elements (:required (:name :value) :optional (:type :file :line-start :line-end :note)))))
  "Validation schema for Hactar Lisp forms.")

(defun validate-plist (plist schema)
  "Validate a plist against a schema, returning list of error strings."
  (let ((errors '())
        (required (getf schema :required))
        (optional (getf schema :optional)))
    ;; Check all required keys are present
    (dolist (req required)
      (unless (getf plist req)
        (push (format nil "Missing required key: ~A" req) errors)))
    ;; Check that there are no unknown keys
    (loop for (k v) on plist by #'cddr
          do (unless (or (member k required) (member k optional))
               (push (format nil "Unknown key: ~A" k) errors)))
    errors))

(defun validate-hactar-form (form)
  "Validate a single LISP form against *hactar-form-schemas*.
   Returns (values valid-p errors corrections)."
  (unless (consp form)
    (return-from validate-hactar-form (values nil (list "Form is not a list") nil)))
  (let* ((op (car form))
         (schema-entry (assoc op *hactar-form-schemas*))
         (errors '())
         (corrected-form form))
    (unless schema-entry
      (return-from validate-hactar-form (values nil (list (format nil "Unknown Hactar form: ~A" op)) nil)))
    (let ((schema (cdr schema-entry)))
      (cond
        ((eq op 'defproject)
         ;; (defproject name &rest plist)
         (let ((name (second form))
               (plist (cddr form)))
           (unless (or (stringp name) (symbolp name))
             (push "defproject name must be a string or symbol" errors))
           (let ((plist-errors (validate-plist plist (list :required '() :optional (getf schema :optional)))))
             (setf errors (append errors plist-errors)))))
        (t
         ;; (defroutes &rest elements)
         (let ((elements (cdr form))
               (new-elements '()))
           (dolist (elem elements)
             (unless (listp elem)
               (push (format nil "Element is not a plist: ~S" elem) errors))
             (when (listp elem)
               (let* ((corrected-elem (copy-list elem))
                      (plist-errors (validate-plist elem (getf schema :elements))))
                 (setf errors (append errors plist-errors))
                 ;; Try heuristic corrections on elements
                 ;; e.g. normalise route path to lead with /
                 (when (and (eq op 'defroutes) (getf corrected-elem :path))
                   (let ((path (getf corrected-elem :path)))
                     (when (and (stringp path) (not (str:starts-with-p "/" path)))
                       (setf (getf corrected-elem :path) (format nil "/~A" path)))))
                 (push corrected-elem new-elements))))
           (setf corrected-form (cons op (nreverse new-elements))))))
      (values (null errors) errors corrected-form))))

(defun balance-parentheses (str)
  "Balance parentheses in a string by appending close parens if open > close."
  (let ((open-count 0)
        (close-count 0)
        (in-string nil)
        (escaped nil))
    (loop for char across str do
          (cond
            (escaped (setf escaped nil))
            ((char= char #\\) (setf escaped t))
            ((char= char #\") (setf in-string (not in-string)))
            ((not in-string)
             (cond
               ((char= char #\() (incf open-count))
               ((char= char #\)) (incf close-count))))))
    (if (> open-count close-count)
        (concatenate 'string str (make-string (- open-count close-count) :initial-element #\)))
        str)))

(defun extract-lisp-from-text (raw-text)
  "Extract Lisp forms from text, potentially inside markdown code blocks."
  (let ((cleaned '()))
    ;; Find code blocks first
    (cl-ppcre:do-register-groups (block) ("(?s)```(?:lisp|scheme|clojure)?\\s*(.*?)\\s*```" raw-text)
      (push block cleaned))
    (if cleaned
        (str:join " " (nreverse cleaned))
        ;; If no markdown code blocks, try to find text between first ( and last )
        (let ((start (position #\( raw-text))
              (end (position #\) raw-text :from-end t)))
          (if (and start end (< start end))
              (subseq raw-text start (1+ end))
              raw-text)))))

(defun safe-read-forms (text)
  "Read forms from text without raising errors. Returns list of forms."
  (let ((balanced (balance-parentheses text))
        (forms '())
        (eof (gensym)))
    (handler-case
        (with-input-from-string (stream balanced)
          (loop for form = (read stream nil eof)
                until (eq form eof)
                do (push form forms)))
      (error (e)
        (debug-log "safe-read-forms caught reader error:" e)))
    (nreverse forms)))

(defun correct-hactar-form (form errors)
  "Normalize/correct form. E.g. converting keyword string args or fixing duplicate elements."
  (declare (ignore errors))
  (multiple-value-bind (valid-p errs corrected) (validate-hactar-form form)
    (declare (ignore valid-p errs))
    corrected))

(defun parse-diffusion-output (raw-text)
  "Main parser entry point."
  (let* ((extracted (extract-lisp-from-text raw-text))
         (raw-forms (safe-read-forms extracted))
         (validated '()))
    (dolist (form raw-forms)
      (multiple-value-bind (valid-p errors corrected) (validate-hactar-form form)
        (declare (ignore valid-p errors))
        (when corrected
          (push corrected validated))))
    (nreverse validated)))

(defun read-hactar-lisp (path)
  "Read all forms from the specified .hactar.lisp path."
  (handler-case
      (uiop:read-file-forms path)
    (error () nil)))

(defun merge-hactar-forms (existing new)
  "Merge new forms with existing forms.
   - For defproject, the new one replaces the existing one.
   - For defroutes, defmodules, etc., merge their elements by unique key."
  (let ((project-form (or (find 'defproject new :key #'car) (find 'defproject existing :key #'car)))
        (merged-forms '()))
    (when project-form
      (push project-form merged-forms))
    ;; For all other form operators:
    (dolist (op '(defroutes defmodels defmodules defscripts defcomponents defdeps defimports defenvs))
      (let ((exist-form (find op existing :key #'car))
            (new-form (find op new :key #'car)))
        (cond
          ((and exist-form new-form)
           ;; Merge elements by their unique identifying key
           (let ((key-attr (case op
                             (defroutes :path)
                             (defmodels :name)
                             (defmodules :module)
                             (defscripts :script)
                             (defcomponents :component)
                             (defdeps :dep)
                             (defimports :file)
                             (defenvs :name)
                             (t nil))))
             (if key-attr
                 (let ((merged-elements '()))
                   ;; Push existing elements, then overwrite or append new ones
                   (dolist (elem (cdr exist-form))
                     (push elem merged-elements))
                   (dolist (new-elem (cdr new-form))
                     (let ((existing-match (find (getf new-elem key-attr) merged-elements
                                                 :key (lambda (el) (getf el key-attr))
                                                 :test #'equal)))
                       (if existing-match
                           ;; Replace
                           (setf merged-elements (substitute new-elem existing-match merged-elements))
                           ;; Append
                           (push new-elem merged-elements))))
                   (push (cons op (nreverse merged-elements)) merged-forms))
                 ;; No unique key, just append all new elements to existing
                 (push (cons op (append (cdr exist-form) (cdr new-form))) merged-forms))))
          (exist-form (push exist-form merged-forms))
          (new-form (push new-form merged-forms)))))
    (nreverse merged-forms)))

(defun sort-hactar-forms (forms)
  "Sort Hactar forms by operator priority."
  (let ((priority '((defproject . 0)
                    (defroutes . 1)
                    (defmodels . 2)
                    (defmodules . 3)
                    (defcomponents . 4)
                    (defscripts . 5)
                    (defdeps . 6)
                    (defimports . 7)
                    (defenvs . 8))))
    (sort (copy-list forms)
          (lambda (a b)
            (< (or (cdr (assoc (car a) priority)) 99)
               (or (cdr (assoc (car b) priority)) 99))))))

(defun write-hactar-lisp (forms path &key merge-p)
  "Write Lisp forms to path. If merge-p, merge with existing forms."
  (let* ((existing-content (when (probe-file path) (uiop:read-file-string path)))
         (existing (when (and merge-p existing-content) (read-hactar-lisp path)))
         (merged (if merge-p (merge-hactar-forms existing forms) forms))
         (sorted (sort-hactar-forms merged))
         (user-region (or (pcli-extract-user-region existing-content)
                          (pcli-default-user-region))))
    (ensure-directories-exist (directory-namestring path))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-case* :downcase))
        (format out ";;; -*- mode: lisp; package: hactar -*-~%")
        (format out ";;; .hactar.lisp — generated project data map~%")
        (format out ";;; Edit freely. BEGIN/END GENERATED regions are owned by the~%")
        (format out ";;; generator + fusions and may be rewritten.~%")
        (format out "(in-package :hactar)~%~%")
        ;; Write regions
        (let ((written-tags '()))
          (dolist (form sorted)
            (let* ((op (car form))
                   (tag (case op
                          (defproject "meta")
                          (defroutes "routes")
                          (defmodels "models")
                          (defscripts "scripts")
                          (defcomponents "components")
                          (defdeps "deps")
                          (defimports "imports")
                          (defenvs "envs")
                          (t (string-downcase (string op))))))
              (push tag written-tags)
              (cond
                ((eq op 'defproject)
                 (let ((name (second form))
                       (stack (getf (cddr form) :stack)))
                   (format out "~A~%~%" (render-defproject name stack "*repo-root*"))))
                ((eq op 'defroutes)
                 (format out "~A~%~%" (render-defroutes (mapcar #'plist->route-entry (cdr form)))))
                ((eq op 'defmodels)
                 (format out "~A~%~%" (render-defmodels (mapcar #'plist->model-entry (cdr form)))))
                (t
                 (format out "~A~%" (pcli-region-begin tag))
                 (format out "~S~%" form)
                 (format out "~A~%~%" (pcli-region-end tag))))))
          ;; Make sure the tests don't fail when looking for (defroutes) / (defmodels) regions even if they are empty
          (unless (member "routes" written-tags :test #'string=)
            (format out "~A~%~%" (render-defroutes nil)))
          (unless (member "models" written-tags :test #'string=)
            (format out "~A~%~%" (render-defmodels nil))))
        (format out "~A~%" (string-right-trim '(#\Newline) user-region))))))
