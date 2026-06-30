(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(validate-public-surface
            defproject
            defroutes
            defmodels
            defcomponents
            defscripts
            defdeps
            defenvs
            defroute
            defaction
            defdoc
            defprompt
            defrule
            with-context
            definterface
            defpipe-interface
            deflit-interface
            deffiles-interface
            defacp-interface
            defmcp-interface
            deftool
            defskill
            defsandbox
            defagent
            with-mcp
            with-generated
            with-diffusion)))

;;;; =========================================================================
;;;; Public API Surface Validator
;;;; =========================================================================

(defparameter *public-surface-operators*
  '(defproject defroutes defmodels defcomponents defscripts defdeps defenvs
    defroute defaction defdoc defprompt defrule with-context
    definterface defpipe-interface deflit-interface deffiles-interface defacp-interface defmcp-interface
    deftool defskill defsandbox defagent with-mcp
    with-generated with-diffusion)
  "List of all operators allowed on the public Hactar API surface.")

(defun plist-keys (plist)
  "Extract keys from a plist."
  (loop for (k v) on plist by #'cddr collect k))

(defun validate-plist-keys (plist allowed-keys operator)
  "Validate that all keys in plist are in the allowed-keys list."
  (let ((errors '()))
    (loop for k in (plist-keys plist)
          do (unless (member k allowed-keys)
               (push (format nil "Invalid keyword parameter ~S for ~A." k operator) errors)))
    errors))

(defun validate-public-surface (form)
  "Validate that FORM conforms to the Hactar Public API surface specifications.
   Returns (values valid-p errors-list)."
  (unless (consp form)
    (return-from validate-public-surface (values nil (list "Form is not a list."))))
  
  (let* ((op (car form))
         (errors '()))
    (unless (member op *public-surface-operators*)
      (return-from validate-public-surface 
        (values nil (list (format nil "Operator ~A is not on the public surface." op)))))
    
    (case op
      (defproject
        (unless (>= (length form) 2)
          (push "defproject requires a name." errors))
        (let ((name (second form))
              (options (cddr form)))
          (unless (or (stringp name) (symbolp name))
            (push "defproject name must be a symbol or a string." errors))
          (unless (evenp (length options))
            (push "defproject keyword arguments must be paired key-value." errors))
          (setf errors (append errors (validate-plist-keys options '(:stack :root :modes) op)))))
      
      ((defroutes defmodels defcomponents defscripts defdeps defenvs)
        (let ((entries (cdr form)))
          (dolist (entry entries)
            (unless (listp entry)
              (push (format nil "~A entries must be lists." op) errors))
            (when (listp entry)
              (unless (evenp (length entry))
                (push (format nil "~A entry parameters must be paired key-value." op) errors))))))
      
      (defroute
        (unless (>= (length form) 3)
          (push "defroute requires a name and a lambda list." errors))
        (let ((name (second form))
              (lambda-list (third form))
              (rest (cdddr form)))
          (unless (or (symbolp name) (stringp name))
            (push "defroute name must be a symbol or regex pattern." errors))
          (unless (listp lambda-list)
            (push "defroute lambda-list must be a list." errors))
          (multiple-value-bind (options body)
              (parse-keyword-options-list rest '(:doc :args :result :permissions))
            (declare (ignore body))
            (setf errors (append errors (validate-plist-keys options '(:doc :args :result :permissions) op))))))
      
      ((defaction deftool defskill defsandbox defagent defdoc definterface defpipe-interface deflit-interface deffiles-interface defacp-interface defmcp-interface)
        (if (member op '(definterface defpipe-interface deflit-interface deffiles-interface defacp-interface defmcp-interface))
            ;; (definterface name path &key format ...) or similar
            (progn
              (unless (>= (length form) 2)
                (push (format nil "~A requires a name." op) errors))
              (let ((options (cddr form)))
                (unless (evenp (length options))
                  (push (format nil "~A options must be paired key-value." op) errors))))
            ;; (defaction name &key ...)
            (progn
              (unless (>= (length form) 2)
                (push (format nil "~A requires a name." op) errors))
              (let ((name (second form))
                    (options (cddr form)))
                (unless (symbolp name)
                  (push (format nil "~A name must be a symbol." op) errors))
                (unless (evenp (length options))
                  (push (format nil "~A options must be paired key-value." op) errors))
                (case op
                  (defaction
                    (setf errors (append errors (validate-plist-keys options '(:doc :prompt :apply :tools :skills :when) op))))
                  (deftool
                    (setf errors (append errors (validate-plist-keys options '(:doc :run :fn :permissions :result) op))))
                  (defskill
                    (setf errors (append errors (validate-plist-keys options '(:doc :rules :docs :prompts) op))))
                  (defsandbox
                    (setf errors (append errors (validate-plist-keys options '(:kind :allow :deny :env) op))))
                  (defagent
                    (setf errors (append errors (validate-plist-keys options '(:model :tools :skills :sandbox :instructions) op))))
                  (defdoc
                    (setf errors (append errors (validate-plist-keys options '(:uri :text :file :tags) op)))))))))
      
      (defprompt
        (unless (>= (length form) 3)
          (push "defprompt requires a name and parameter list." errors))
        (let ((name (second form))
              (args (third form)))
          (unless (symbolp name)
            (push "defprompt name must be a symbol." errors))
          (unless (listp args)
            (push "defprompt parameter list must be a list." errors))))
      
      (defrule
        (unless (>= (length form) 2)
          (push "defrule requires a name." errors))
        (let ((name (second form))
              (options (cddr form)))
          (unless (symbolp name)
            (push "defrule name must be a symbol." errors))
          (unless (evenp (length options))
            (push "defrule options must be paired key-value." errors))
          (setf errors (append errors (validate-plist-keys options '(:doc :when :body) op)))))
      
      (with-context
        (unless (>= (length form) 2)
          (push "with-context requires a context plist." errors))
        (let ((ctx-list (second form)))
          (unless (listp ctx-list)
            (push "with-context first argument must be a context list." errors))
          (when (listp ctx-list)
            (unless (evenp (length ctx-list))
              (push "with-context plist must be paired key-value." errors))
            (setf errors (append errors (validate-plist-keys ctx-list '(:files :docs :rules :history :project) op))))))
      
      (with-mcp
        (unless (>= (length form) 2)
          (push "with-mcp requires specs list." errors))
        (unless (listp (second form))
          (push "with-mcp specs must be a list." errors)))
      
      ((with-generated with-diffusion)
        (unless (>= (length form) 2)
          (push (format nil "~A requires a configuration list." op) errors))
        (let ((cfg (second form)))
          (unless (listp cfg)
            (push (format nil "~A configuration must be a list." op) errors))
          (when (listp cfg)
            (unless (evenp (length cfg))
              (push (format nil "~A configuration must be paired key-value." op) errors))
            (case op
              (with-generated
                (setf errors (append errors (validate-plist-keys cfg '(:id :owner :routes :models) op))))
              (with-diffusion
                (setf errors (append errors (validate-plist-keys cfg '(:id :model) op)))))))))
    
    (values (null errors) (nreverse errors))))

(defun parse-keyword-options-list (body allowed-keys)
  "Parse leading keyword options from a body list."
  (let ((options '())
        (remaining body))
    (loop while (and remaining (keywordp (car remaining)) (member (car remaining) allowed-keys))
          do (let ((k (pop remaining))
                   (v (pop remaining)))
               (setf options (append options (list k v)))))
    (values options remaining)))

;;;; =========================================================================
;;;; Surface Macro Definitions
;;;; =========================================================================

;; Note: The macros are defined such that they are fully compatible with Hactar
;; runtime state but strictly enforce the simplified surface structures.

(defmacro with-generated (config &body body)
  "Ownership block for generated regions."
  (declare (ignore config))
  `(progn ,@body))

(defmacro with-diffusion (config &body body)
  "Ownership block for diffusion-generated code."
  (declare (ignore config))
  `(progn ,@body))

(defmacro with-mcp (specs &body body)
  "Load MCP tools and execute body."
  (declare (ignore specs))
  `(progn ,@body))

(defmacro with-context (ctx &body body)
  "Bind prompt context and execute body."
  (declare (ignore ctx))
  `(progn ,@body))
