(in-package #:hactar-fern)

;;; Fern: Hactar's native structural editing backend. This module implements the
;;; Common Lisp backend (Hactar eats its own food): it locates top-level
;;; definitions by kind+name, edits them via sexp transforms, and splices the
;;; printed result back at the original span so the rest of the file is
;;; untouched. Other-language grammars (the PEG engine in fern.lisp) plug into
;;; the same node/edit protocol.

(defstruct (fern-node (:conc-name fern-node-))
  kind name start end text form)

(defparameter *supported-actions*
  '(:rename :replace :remove :set-body :add-parameter :add-form :add-function)
  "AST actions the Lisp backend understands.")

(defun def-kind (operator)
  "Map a definer symbol to a node kind keyword, or NIL if not a definition."
  (let ((n (string-downcase (string operator))))
    (cond
      ((member n '("defun" "defgeneric" "defmethod") :test #'string=) :function)
      ((string= n "defmacro") :macro)
      ((member n '("defvar" "defparameter" "defconstant") :test #'string=) :variable)
      ((string= n "defclass") :class)
      ((string= n "defstruct") :struct)
      (t nil))))

(defun form-name (form)
  "Return the defined name (a string) of a top-level definition FORM, or NIL."
  (when (and (consp form) (cddr form))
    (let ((n (second form)))
      (cond ((symbolp n) (string-downcase (symbol-name n)))
            ((consp n) (string-downcase (string (car n)))) ; defstruct (name ..)
            (t nil)))))

(defun forms-with-spans (source)
  "Read every top-level form in SOURCE, returning FERN-NODEs with byte spans."
  (with-input-from-string (s source)
    (let ((*read-eval* nil)
          (*package* (find-package :hactar-fern))
          (nodes '()))
      (handler-case
          (loop
            (let ((ch (peek-char t s nil :eof)))
              (when (eq ch :eof) (return))
              (let* ((start (file-position s))
                     (form (read s nil :eof)))
                (when (eq form :eof) (return))
                (let ((end (file-position s)))
                  (push (make-fern-node
                         :kind (and (consp form) (def-kind (car form)))
                         :name (form-name form)
                         :start start :end end
                         :text (subseq source start end)
                         :form form)
                        nodes)))))
        (end-of-file () nil))
      (nreverse nodes))))

(defun parse-lisp (source) (forms-with-spans source))

(defun valid-lisp-p (source)
  "True if SOURCE reads as balanced Lisp forms."
  (handler-case (progn (forms-with-spans source) t)
    (error () nil)))

(defun find-definition (nodes name &optional kind)
  "Find a definition node by NAME (case-insensitive) and optional KIND."
  (find-if (lambda (n)
             (and (fern-node-name n)
                  (string-equal (fern-node-name n) (string name))
                  (or (null kind) (eq (fern-node-kind n) kind))))
           nodes))

(defun read-forms-from-string (str)
  (with-input-from-string (s str)
    (let ((*read-eval* nil) (*package* (find-package :hactar-fern)))
      (loop for f = (read s nil :eof) until (eq f :eof) collect f))))

(defun print-form (form)
  (let ((*print-case* :downcase) (*print-right-margin* 100)
        (*package* (find-package :hactar-fern)))
    (prin1-to-string form)))

(defun transform-form (form action op)
  "Return replacement TEXT for FORM under ACTION, or :remove, or NIL if the
action does not apply structurally (caller falls back)."
  (ecase action
    (:replace (getf op :new-code))
    (:remove :remove)
    (:rename
     (let ((new (getf op :value (getf op :new-name))))
       (print-form (list* (first form) (intern (string-upcase new) :hactar-fern)
                          (cddr form)))))
    (:set-body
     ;; (defun name lambda-list . body) -> keep sig, swap body
     (let ((new-body (read-forms-from-string (getf op :new-code))))
       (print-form (list* (first form) (second form) (third form) new-body))))
    (:add-parameter
     (let ((param (intern (string-upcase (getf op :value)) :hactar-fern)))
       (print-form (list* (first form) (second form)
                          (append (third form) (list param))
                          (cdddr form)))))))

(defun edit-source (source operations)
  "Apply OPERATIONS to SOURCE and return (values new-source applied-count).
Edits are computed against original spans and applied right-to-left so offsets
stay valid. File-level adds (:add-form/:add-function) append at end."
  (let* ((nodes (forms-with-spans source))
         (edits '())      ; (start end replacement)
         (appends '())
         (applied 0))
    (dolist (op operations)
      (let ((action (getf op :action)))
        (unless (member action *supported-actions*)
          (error "Unsupported Lisp AST action: ~A" action))
        (case action
          ((:add-form :add-function)
           (push (getf op :new-code) appends) (incf applied))
          (t
           (let ((node (find-definition nodes (getf op :name)
                                        (getf op :target))))
             (unless node
               (error "No ~@[~A ~]definition named ~A"
                      (getf op :target) (getf op :name)))
             (let ((rep (transform-form (fern-node-form node) action op)))
               (push (list (fern-node-start node) (fern-node-end node)
                           (if (eq rep :remove) "" rep))
                     edits)
               (incf applied)))))))
    ;; apply span edits right-to-left
    (let ((result source))
      (dolist (e (sort edits #'> :key #'first))
        (destructuring-bind (start end rep) e
          (setf result (concatenate 'string
                                    (subseq result 0 start) rep
                                    (subseq result end)))))
      (when appends
        (setf result (concatenate 'string result
                                  (format nil "~%~{~A~%~}" (nreverse appends)))))
      (values result applied))))
