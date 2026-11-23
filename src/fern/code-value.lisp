(in-package #:hactar-fern)

(defstruct (code-value (:constructor make-code-value
                           (&key source language nodes parent transformation)))
  "An immutable semantic wrapper around source and its parsed structure."
  (source "" :type string)
  language
  nodes
  parent
  transformation)

(defun parse-source (source &key (language "lisp"))
  "Parse SOURCE using the structural backend selected by LANGUAGE."
  (cond
    ((member (string-downcase (string language))
             '("lisp" "common-lisp" "cl") :test #'string=)
     (parse-lisp source))
    (t (error "No Fern parser registered for language ~A" language))))

(defun find-node (nodes name &key kind)
  "Find a structural node by NAME and optional KIND."
  (find-definition nodes name kind))

(defun node-kind (node)
  "Return NODE's language-independent kind."
  (fern-node-kind node))

(defun node-name (node)
  "Return NODE's language-independent name."
  (fern-node-name node))

(defun replace-node (source node replacement)
  "Replace NODE's exact source span with REPLACEMENT."
  (concatenate 'string
               (subseq source 0 (fern-node-start node))
               replacement
               (subseq source (fern-node-end node))))

(defun insert-node (source new-code &key after)
  "Insert NEW-CODE at EOF, or immediately after the structural node AFTER."
  (if after
      (concatenate 'string
                   (subseq source 0 (fern-node-end after))
                   (format nil "~%~A" new-code)
                   (subseq source (fern-node-end after)))
      (concatenate 'string source
                   (unless (or (zerop (length source))
                               (char= (char source (1- (length source)))
                                      #\Newline))
                     (string #\Newline))
                   new-code
                   (string #\Newline))))

(defun apply-edits (source operations &key (language "lisp"))
  "Apply structural OPERATIONS to SOURCE using LANGUAGE's backend."
  (unless (member (string-downcase (string language))
                  '("lisp" "common-lisp" "cl") :test #'string=)
    (error "No Fern editor registered for language ~A" language))
  (edit-source source operations))

(defun code-from-string (source &key (language "lisp"))
  "Create a CODE-VALUE and parse its structural nodes."
  (make-code-value :source source :language language
                   :nodes (parse-source source :language language)))

(defun transform-code-value (value operations &key description)
  "Return a new CODE-VALUE produced by applying OPERATIONS to VALUE."
  (multiple-value-bind (source count)
      (apply-edits (code-value-source value) operations
                   :language (code-value-language value))
    (values (make-code-value
             :source source
             :language (code-value-language value)
             :nodes (parse-source source :language (code-value-language value))
             :parent value
             :transformation description)
            count)))
