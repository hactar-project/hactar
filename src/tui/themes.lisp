(in-package #:hactar-tui)

;;; Data-driven TUI themes + safe loader for data/themes/*.lisp.

(defstruct (theme (:constructor %make-theme))
  (name "default" :type string)
  (prompt "> ")
  (error-prefix "error: ")
  (palette nil :type list))

(defun make-theme (&rest args &key (name "default") (prompt "> ")
                                  (error-prefix "error: ")
                       &allow-other-keys)
  "Construct a THEME with prompt/error-prefix plus an arbitrary color palette plist."
  (let ((palette nil))
    (loop for (k v) on args by #'cddr
          unless (member k '(:name :prompt :error-prefix))
            do (setf palette (list* k v palette)))
    (%make-theme :name (string name)
                 :prompt prompt
                 :error-prefix error-prefix
                 :palette palette)))

(defun make-tui-theme (&rest args)
  "Alias for MAKE-THEME compatible with data/themes/*.lisp definitions."
  (apply #'make-theme args))

(defun theme-color (theme key &optional default)
  (or (and theme (getf (theme-palette theme) key)) default))

(defvar *theme* (make-theme))
(defvar *themes* (make-hash-table :test 'equal))

(defun register-theme (theme)
  (setf (gethash (string-downcase (theme-name theme)) *themes*) theme)
  theme)

(defun find-theme (name)
  (gethash (string-downcase (string name)) *themes*))

(defun list-themes ()
  (loop for th being the hash-values of *themes* collect th))

(defun use-theme (name-or-theme)
  "Set *THEME* to NAME-OR-THEME (looked up in *THEMES* if a string/symbol)."
  (let ((resolved (if (theme-p name-or-theme)
                      name-or-theme
                      (or (find-theme name-or-theme)
                          (error "Unknown theme: ~A" name-or-theme)))))
    (setf *theme* resolved)))

(register-theme *theme*)

(defun %rekeywordize-plist (forms)
  (loop for (k v) on forms by #'cddr
        append (list (intern (string-upcase (string k)) :keyword) v)))

(defun load-theme-file (path)
  "Safely read a theme definition file (like data/themes/dracula.lisp) without EVAL."
  (let* ((text (uiop:read-file-string path))
         (normalized (if (uiop:string-prefix-p "(hactar:" (string-downcase text))
                         (concatenate 'string "(" (subseq text (length "(hactar:")))
                         text)))
    (let ((*read-eval* nil)
          (*package* (find-package :hactar-tui)))
      (let ((raw (read-from-string normalized)))
        (unless (and (consp raw)
                     (symbolp (first raw))
                     (member (symbol-name (first raw))
                             '("MAKE-TUI-THEME" "MAKE-THEME") :test #'string-equal))
          (error "Invalid theme form in ~A" path))
        (let ((th (apply #'make-theme (%rekeywordize-plist (rest raw)))))
          (register-theme th))))))

(defun load-themes-dir (dir)
  "Load all *.lisp theme files in DIR and return the registered themes."
  (let ((loaded nil))
    (dolist (file (sort (directory (merge-pathnames "*.lisp"
                                                    (uiop:ensure-directory-pathname dir)))
                        #'string< :key #'namestring))
      (push (load-theme-file file) loaded))
    (nreverse loaded)))
