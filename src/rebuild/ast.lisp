(in-package #:hactar-rebuild)

;;; AST-first editing primitive. Full stack backends (ts-morph / tree-sitter /
;;; sexp) live in stack modes; this core entry validates operations, snapshots
;;; the file, and dispatches to a backend. Without a backend it falls back to a
;;; recorded no-op so the tool surface stays stable and testable.

(defparameter *ast-tier1*
  '(:set-type :set-return-type :set-initializer :remove-initializer :set-value
    :set-async :set-generator :set-export :set-default-export :set-abstract
    :set-static :set-readonly :set-scope :set-optional :rename :remove
    :add-parameter :remove-parameter))

(defparameter *ast-tier2*
  '(:set-body :add-statement :insert-statement :remove-statement :add-property
    :remove-property :add-method :remove-method :add-constructor :set-extends
    :add-extends :remove-extends :add-implements :remove-implements
    :add-type-parameter :add-decorator :remove-decorator :add-jsdoc
    :remove-jsdoc :unwrap :set-structure :extract-interface))

(defparameter *ast-tier3* '(:replace :replace-in-body))

(defparameter *ast-file-level*
  '(:create-file :add-import :remove-import :add-named-import
    :remove-named-import :set-module-specifier :organize-imports
    :fix-missing-imports :fix-unused :add-function :add-class :add-interface
    :add-type-alias :add-enum :add-variable :insert-text :add-form))

(defun valid-ast-action-p (action)
  (or (member action *ast-tier1*) (member action *ast-tier2*)
      (member action *ast-tier3*) (member action *ast-file-level*)))

(defvar *ast-backends* (make-hash-table :test 'equal)
  "file-extension (string) -> function (path operations) -> result.")

(defun register-ast-backend (extension fn)
  (setf (gethash (string-downcase extension) *ast-backends*) fn))

;;; ---------------------------------------------------------------------------
;;; Common Lisp backend, powered by hactar-fern (Hactar eats its own food).
;;; ---------------------------------------------------------------------------

(defun fern-lisp-backend (path ops)
  "Apply OPS to the Lisp file at PATH via hactar-fern, with snapshot safety."
  (unless (probe-file path)
    (return-from fern-lisp-backend (err (format nil "File not found: ~A" path))))
  (let ((source (uiop:read-file-string path)))
    (handler-case
        (multiple-value-bind (new-source applied)
            (hactar-fern:edit-source source ops)
          (unless (hactar-fern:valid-lisp-p new-source)
            (return-from fern-lisp-backend
              (err "AST edit would produce unbalanced Lisp; rejected.")))
          (atomic-write path new-source)
          (ok (format nil "ast-edit: applied ~D op(s) to ~A" applied path)
              :data (list :path path :applied applied)))
      (error (e) (err e)))))

(dolist (ext '("lisp" "lsp" "asd" "cl"))
  (register-ast-backend ext #'fern-lisp-backend))

(defun normalize-ast-operations (operations)
  "Accept either a single op plist or (:operations (op...))."
  (cond
    ((and (consp operations) (eq (first operations) :operations))
     (second operations))
    ((and (consp operations) (keywordp (first operations)))
     (list operations))
    (t operations)))

(defun run-ast-edit (path &rest operations)
  "Validate and apply structural OPERATIONS to PATH atomically."
  (let ((ops (normalize-ast-operations operations)))
    (dolist (op ops)
      (let ((action (getf op :action)))
        (unless (valid-ast-action-p action)
          (return-from run-ast-edit
            (err (format nil "Invalid AST action: ~A" action))))))
    (let* ((ext (pathname-type (pathname path)))
           (backend (and ext (gethash (string-downcase ext) *ast-backends*))))
      (if backend
          (funcall backend path ops)
          (ok (format nil "[ast-edit:no-backend] ~A ops on ~A" (length ops) path)
              :data (list :path path :operations ops))))))
