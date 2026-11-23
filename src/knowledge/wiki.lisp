(in-package #:hactar-knowledge)

;;; Wiki-as-Lisp registry: docsets, endpoints, components, and wiki:// lookups.

(defvar *wiki-nodes* (make-hash-table :test 'equal))
(defvar *wiki-docsets* (make-hash-table :test 'equal))

(defstruct wiki-node namespace path type properties related stack-tags)
(defstruct wiki-docset name version stack-tags description)

(defun %wiki-key (namespace path)
  (format nil "~A/~A" (string-downcase (string namespace))
          (string-trim '(#\/) (string path))))

(defun register-wiki-docset (name &key version stack-tags description)
  (let ((k (string-downcase (string name))))
    (setf (gethash k *wiki-docsets*)
          (make-wiki-docset :name k :version version
                            :stack-tags stack-tags :description description))))

(defun register-wiki-node (namespace path &key (type :endpoint) properties related stack-tags)
  (let* ((ns (string-downcase (string namespace)))
         (clean-path (string-trim '(#\/) (string path)))
         (node (make-wiki-node :namespace ns :path clean-path :type type
                               :properties properties :related related
                               :stack-tags stack-tags)))
    (setf (gethash (%wiki-key ns clean-path) *wiki-nodes*) node)
    node))

(defun find-wiki-node (namespace path)
  (gethash (%wiki-key namespace path) *wiki-nodes*))

(defun find-nodes-under (namespace path-prefix)
  (let ((ns (string-downcase (string namespace)))
        (prefix (string-trim '(#\/) (string path-prefix)))
        (found nil))
    (maphash (lambda (k node)
               (declare (ignore k))
               (when (and (string= (wiki-node-namespace node) ns)
                          (or (string= prefix "")
                              (uiop:string-prefix-p prefix (wiki-node-path node))))
                 (push node found)))
             *wiki-nodes*)
    (sort found #'string< :key #'wiki-node-path)))

(defun parse-wiki-uri (uri)
  "Parse \"wiki://ns/path\" into (values namespace path)."
  (let ((rest (if (uiop:string-prefix-p "wiki://" uri)
                  (subseq uri 7)
                  uri)))
    (let ((slash (position #\/ rest)))
      (if slash
          (values (subseq rest 0 slash) (subseq rest (1+ slash)))
          (values rest "")))))

(defmacro defdocset (name &key version stack-tags description)
  `(register-wiki-docset ',name :version ,version
                                :stack-tags ',stack-tags
                                :description ,description))

(defun %split-qualified (qname)
  (let* ((s (string-downcase (string qname)))
         (slash (position #\/ s)))
    (if slash
        (values (subseq s 0 slash) (subseq s (1+ slash)))
        (values "default" s))))

(defmacro defendpoint (qualified-name &rest props)
  (multiple-value-bind (ns path) (%split-qualified qualified-name)
    `(register-wiki-node ,ns ,path :type :endpoint :properties ',props)))

(defmacro defcomponent (qualified-name &rest props)
  (multiple-value-bind (ns path) (%split-qualified qualified-name)
    `(register-wiki-node ,ns ,path :type :component :properties ',props)))
