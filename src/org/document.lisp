(in-package #:hactar-org)

;;; Tree-oriented Org support. PARSE-ORG remains the small flat parser; this
;;; layer adds round-trippable document structure without changing that API.

(defun %trim (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun %prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun %lines (source)
  (uiop:split-string source :separator '(#\Newline)))

(defun %heading-p (line)
  (and (plusp (length line)) (char= (char line 0) #\*)))

(defun %split-char (string char)
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos do (setf start (1+ pos))))

(defun %parse-heading (line)
  (let* ((level (loop for ch across line while (char= ch #\*) count t))
         (rest (%trim (subseq line level)))
         (todo nil)
         (priority nil)
         (tags nil))
    (dolist (word '("TODO" "DONE" "DOING" "NEXT" "WAITING" "BLOCKED"))
      (when (%prefix-p (concatenate 'string word " ") rest)
        (setf todo word
              rest (%trim (subseq rest (length word))))))
    (when (and (>= (length rest) 4)
               (char= (char rest 0) #\[)
               (char= (char rest 1) #\#)
               (char= (char rest 3) #\]))
      (setf priority (char rest 2)
            rest (%trim (subseq rest 4))))
    (let ((tag-start (search " :" rest :from-end t)))
      (when (and tag-start (char= (char rest (1- (length rest))) #\:))
        (setf tags (remove "" (%split-char
                               (subseq rest (+ tag-start 2)
                                       (1- (length rest)))
                               #\:)
                           :test #'string=)
              rest (%trim (subseq rest 0 tag-start)))))
    (values level rest todo priority tags)))

(defun %parse-setting (line)
  (when (%prefix-p "#+" line)
    (let ((colon (position #\: line :start 2)))
      (when colon
        (cons (intern (string-upcase (%trim (subseq line 2 colon))) :keyword)
              (%trim (subseq line (1+ colon))))))))

(defun %parse-properties (lines start)
  (let ((properties nil)
        (index (1+ start)))
    (loop while (< index (length lines))
          for line = (%trim (nth index lines))
          do (cond
               ((string-equal line ":END:")
                (return-from %parse-properties
                  (values (nreverse properties) (1+ index))))
               ((and (plusp (length line)) (char= (char line 0) #\:))
                (let ((colon (position #\: line :start 1)))
                  (when colon
                    (push (cons (intern (string-upcase (subseq line 1 colon))
                                        :keyword)
                                (%trim (subseq line (1+ colon))))
                          properties)))))
             (incf index))
    (values (nreverse properties) index)))

(defun %append-child (parent child)
  (setf (org-node-parent child) parent
        (org-node-children parent)
        (append (org-node-children parent) (list child)))
  child)

(defun parse-org-document (source)
  "Parse SOURCE into a round-trippable ORG-NODE tree rooted at :DOCUMENT."
  (let* ((lines (%lines source))
         (document (make-org-node :type :document))
         (stack (list document))
         (index 0)
         (paragraph nil))
    (labels ((flush-paragraph ()
               (when paragraph
                 (%append-child
                  (first stack)
                  (make-org-node :type :paragraph
                                 :body (format nil "~{~A~^~%~}"
                                               (nreverse paragraph))))
                 (setf paragraph nil)))
             (parent-for (level)
               (loop while (and (> (length stack) 1)
                                (>= (org-node-level (first stack)) level))
                     do (pop stack))
               (first stack)))
      (loop while (< index (length lines))
            for line = (nth index lines)
            for trimmed = (%trim line)
            do
               (cond
                 ((and (%prefix-p "#+" trimmed)
                       (not (%prefix-p "#+begin_src" trimmed)))
                  (flush-paragraph)
                  (let ((setting (%parse-setting trimmed)))
                    (when setting
                      (setf (org-node-settings document)
                            (append (org-node-settings document)
                                    (list setting)))))
                  (incf index))
                 ((%prefix-p "#+begin_src" trimmed)
                  (flush-paragraph)
                  (let* ((header (%trim (subseq trimmed
                                                (length "#+begin_src"))))
                         (separator (position-if
                                     (lambda (char)
                                       (member char '(#\Space #\Tab)))
                                     header))
                         (lang (if separator
                                   (subseq header 0 separator)
                                   header))
                         (header-args (and separator
                                           (%trim (subseq header separator))))
                        (body nil))
                    (incf index)
                    (loop while (< index (length lines))
                          for body-line = (nth index lines)
                          until (string-equal (%trim body-line) "#+end_src")
                          do (push body-line body) (incf index))
                    (when (< index (length lines)) (incf index))
                    (%append-child
                     (first stack)
                     (make-org-node :type :src :lang lang
                                    :header-args header-args
                                    :body (format nil "~{~A~^~%~}"
                                                  (nreverse body))))))
                 ((%heading-p line)
                  (flush-paragraph)
                  (multiple-value-bind (level title todo priority tags)
                      (%parse-heading line)
                    (let* ((parent (parent-for level))
                           (node (make-org-node :type :headline :level level
                                                :title title :todo todo
                                                :priority priority :tags tags)))
                      (%append-child parent node)
                      (push node stack)
                      (incf index))))
                 ((string-equal trimmed ":PROPERTIES:")
                  (flush-paragraph)
                  (multiple-value-bind (properties next)
                      (%parse-properties lines index)
                    (%append-child
                     (first stack)
                     (make-org-node :type :properties
                                    :properties properties))
                    (setf index next)))
                 ((string= trimmed "")
                  (flush-paragraph)
                  (incf index))
                 (t
                  (push line paragraph)
                  (incf index))))
      (flush-paragraph))
    document))

(defun walk-org (node function)
  "Call FUNCTION for NODE and each descendant in depth-first order."
  (funcall function node)
  (dolist (child (org-node-children node))
    (walk-org child function))
  node)

(defun find-org-node (root predicate)
  "Return the first node under ROOT satisfying PREDICATE."
  (block found
    (walk-org root (lambda (node)
                     (when (funcall predicate node)
                       (return-from found node))))
    nil))

(defun org-src-blocks (document &optional lang)
  "Return source block nodes from DOCUMENT, optionally restricted to LANG."
  (let ((blocks nil))
    (walk-org document
              (lambda (node)
                (when (and (eq (org-node-type node) :src)
                           (or (null lang)
                               (string-equal (or (org-node-lang node) "")
                                             lang)))
                  (push node blocks))))
    (nreverse blocks)))

(defun %render-node (node stream)
  (case (org-node-type node)
    (:document
     (dolist (setting (org-node-settings node))
       (format stream "#+~A: ~A~%" (symbol-name (car setting)) (cdr setting)))
     (when (org-node-settings node) (terpri stream))
     (dolist (child (org-node-children node)) (%render-node child stream)))
    (:headline
     (format stream "~A ~@[~A ~]~@[[#~A] ~]~A~@[ :~{~A~^:~}:~]~%"
             (make-string (org-node-level node) :initial-element #\*)
             (org-node-todo node) (org-node-priority node)
             (org-node-title node) (org-node-tags node))
     (dolist (child (org-node-children node)) (%render-node child stream)))
    (:properties
     (format stream ":PROPERTIES:~%")
     (dolist (property (org-node-properties node))
       (format stream ":~A: ~A~%" (symbol-name (car property)) (cdr property)))
     (format stream ":END:~%"))
    (:src
     (format stream "#+begin_src~@[ ~A~]~@[ ~A~]~%~A~%#+end_src~%"
             (and (plusp (length (or (org-node-lang node) "")))
                  (org-node-lang node))
             (and (plusp (length (or (org-node-header-args node) "")))
                  (org-node-header-args node))
             (org-node-body node)))
    (:paragraph (format stream "~A~%" (org-node-body node)))))

(defun render-org (document)
  "Serialize a document returned by PARSE-ORG-DOCUMENT."
  (with-output-to-string (stream)
    (%render-node document stream)))

(defun to-org (forms &key (title "Hactar Program") (package "hactar-user"))
  "Render Lisp FORMS as a literate Org document containing one Lisp block."
  (with-output-to-string (stream)
    (format stream "#+title: ~A~%~%* Program~%#+begin_src lisp~%" title)
    (when package (format stream "(in-package :~A)~%~%" package))
    (let ((*print-case* :downcase) (*print-pretty* t))
      (dolist (form forms)
        (pprint form stream)
        (terpri stream)))
    (format stream "#+end_src~%")))

(defun from-org (source)
  "Read and return all Lisp forms in SOURCE's Lisp blocks, with *READ-EVAL* NIL."
  (let ((forms nil)
        (eof (gensym)))
    (dolist (block (org-src-blocks (parse-org-document source) "lisp"))
      (with-input-from-string (stream (org-node-body block))
        (let ((*read-eval* nil)
              (*package* *package*))
          (loop for form = (read stream nil eof)
                until (eq form eof)
                do
                   (push form forms)
                   (when (and (consp form)
                              (symbolp (first form))
                              (string-equal "IN-PACKAGE"
                                            (symbol-name (first form)))
                              (second form))
                     (let ((package (find-package (second form))))
                       (unless package
                         (error "Org Lisp block names unknown package ~A"
                                (second form)))
                       (setf *package* package)))))))
    (nreverse forms)))
