(in-package :hactar)

(deftarget :json
  :file-extension "json")

(defun json-emit-key (key)
  (compiler-emit (format nil "\"~A\"" key)))

(defun json-emit-primitive (value)
  (cond
    ((eq value :true) (compiler-emit "true"))
    ((or (eq value :false) (null value)) (compiler-emit "false"))
    ((eq value :null) (compiler-emit "null"))
    ((stringp value) (compiler-emit (format nil "\"~A\"" value)))
    ((numberp value) (compiler-emit (format nil "~A" value)))
    (t (compile-form value))))

(defun register-json-emitters (target)
  "Register JSON emission forms on TARGET."
  (let ((target-keyword target))
    (defemit target-keyword json-value (value)
      (json-emit-primitive value))

    (defemit target-keyword json-array (&rest elements)
      (compiler-emit "[")
      (loop for (element . rest) on elements
            do (if (or (eq element :true)
                       (eq element :false)
                       (eq element :null)
                       (null element)
                       (stringp element)
                       (numberp element))
                   (json-emit-primitive element)
                   (compile-form element))
            when rest do (compiler-emit ", "))
      (compiler-emit "]"))

    (defemit target-keyword json-object (&rest key-value-pairs)
      (compiler-emitln "{")
      (with-indent
        (loop for (key value . rest) on key-value-pairs by #'cddr
              do (emit-indent)
                 (json-emit-key key)
                 (compiler-emit ": ")
                 (if (or (eq value :true)
                         (eq value :false)
                         (eq value :null)
                         (null value)
                         (stringp value)
                         (numberp value))
                     (json-emit-primitive value)
                     (compile-form value))
                 (if rest
                     (compiler-emitln ",")
                     (compiler-emitln ""))))
      (emit-indent)
      (compiler-emit "}"))))
