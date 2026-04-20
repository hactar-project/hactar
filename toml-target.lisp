(in-package :hactar)

(deftarget :toml
  :file-extension "toml")

(defun toml-emit-value (value)
  (cond
    ((eq value :true) (compiler-emit "true"))
    ((or (eq value :false) (null value)) (compiler-emit "false"))
    ((stringp value) (compiler-emit (format nil "\"~A\"" value)))
    ((numberp value) (compiler-emit (format nil "~A" value)))
    ((and (listp value) (eq (first value) 'toml-array))
     (compile-form value))
    (t (compile-form value))))

(defun register-toml-emitters (target)
  "Register TOML emission forms on TARGET."
  (let ((target-keyword target))
    (defemit target-keyword toml-array (&rest elements)
      (compiler-emit "[")
      (loop for (element . rest) on elements
            do (toml-emit-value element)
            when rest do (compiler-emit ", "))
      (compiler-emit "]"))

    (defemit target-keyword toml-pairs (&rest key-value-pairs)
      (loop for (key value) on key-value-pairs by #'cddr
            do (compiler-emit key)
               (compiler-emit " = ")
               (toml-emit-value value)
               (compiler-emitln "")))

    (defemit target-keyword toml-table (name &rest key-value-pairs)
      (compiler-emitln (format nil "[~A]" name))
      (loop for (key value) on key-value-pairs by #'cddr
            do (compiler-emit key)
               (compiler-emit " = ")
               (toml-emit-value value)
               (compiler-emitln "")))))
