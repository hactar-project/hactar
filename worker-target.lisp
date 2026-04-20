(in-package :hactar)

(deftarget :worker
  :parent :typescript
  :file-extension "ts")

;;* Emitters 
(defemit :worker defworker (args &body body)
  (compiler-emitln "export default {")
  (with-indent
      (emit-indent)
    (compiler-emit "async fetch(")
    (loop for (arg . rest) on args
          do (emit-name arg)
          when rest do (compiler-emit ", "))
    (compiler-emit ") ")
    (emit-block body)
    (compiler-emitln ","))
  (compiler-emit "};"))

(defjsmacro :worker json-response (data &key (status 200))
  `(new (*Response (json-stringify ,data)
                   (create :status ,status
                           :headers (create "Content-Type" "application/json")))))

(defjsmacro :worker text-response (data &key (status 200))
  `(new (*Response ,data (create :status ,status))))

(defemit :worker fetch (url &rest opts)
  (compiler-emit "fetch(")
  (compile-form url)
  (when opts
    (compiler-emit ", { ")
    (loop for (key val . rest) on opts by #'cddr
          do (compiler-emit (kebab-to-camel (string-downcase (symbol-name key))))
             (compiler-emit ": ")
             (compile-form val)
          when rest do (compiler-emit ", "))
    (compiler-emit " }"))
  (compiler-emit ")"))
