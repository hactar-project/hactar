(in-package #:hactar-fern)

(defstruct tag name file address kind (line 0))
(defstruct symbol-entry name kind start)

(defparameter *symbol-indexer* nil
  "Optional function of SOURCE and LANGUAGE returning SYMBOL-ENTRY values.")

(defun %split-tabs (line)
  (uiop:split-string line :separator (list #\Tab)))

(defun parse-tag-line (line)
  "Parse one Universal Ctags output line, returning a TAG or NIL."
  (unless (or (zerop (length line))
              (and (> (length line) 1)
                   (char= (char line 0) #\!)
                   (char= (char line 1) #\_)))
    (let ((parts (%split-tabs line)))
      (when (>= (length parts) 3)
        (let* ((address-field (third parts))
               (terminator (search ";\"" address-field))
               (fields (nthcdr 3 parts))
               (line-field (find-if
                            (lambda (field)
                              (and (>= (length field) 5)
                                   (string-equal "line:" field :end2 5)))
                            fields)))
          (make-tag :name (first parts)
                    :file (second parts)
                    :address (if terminator
                                 (subseq address-field 0 terminator)
                                 address-field)
                    :kind (first fields)
                    :line (if line-field
                              (or (parse-integer (subseq line-field 5)
                                                 :junk-allowed t)
                                  0)
                              0)))))))

(defun parse-tags (source)
  "Parse Universal Ctags SOURCE into TAG values, ignoring header lines."
  (loop for line in (uiop:split-string source :separator '(#\Newline))
        for tag = (parse-tag-line line)
        when tag collect tag))

(defun symbol-index (source &key (language "lisp"))
  "Return a structural symbol index for SOURCE.
When *SYMBOL-INDEXER* is bound, delegate to it; otherwise index Lisp forms
without invoking a subprocess."
  (if *symbol-indexer*
      (funcall *symbol-indexer* source language)
      (loop for node in (parse-source source :language language)
            when (fern-node-name node)
              collect (make-symbol-entry :name (fern-node-name node)
                                         :kind (fern-node-kind node)
                                         :start (fern-node-start node)))))
