;;; org-mode-parser.lisp — WIP feature incomplete org-mode parser
(defpackage :org-mode-parser
  (:use :cl)
  (:export #:parse-org-file
           #:parse-org-string
           #:org-node
           #:org-document
           #:org-heading
           #:org-paragraph
           #:org-properties-drawer
           #:org-src-block
           #:src-block-language
           #:src-block-content
           #:node-type
           #:node-properties
           #:node-children
           #:node-parent
           #:document-settings
           #:heading-level
           #:heading-title
           #:heading-tags
           #:heading-todo-keyword
           #:heading-priority
           #:paragraph-content
           #:walk-tree
           #:find-node
           #:find-all-nodes
           #:find-heading-by-id
           #:add-child
           #:remove-node
           #:insert-after
           #:insert-before
           #:org-to-string
           ;; Utility helpers
           #:find-zone-heading
           #:heading-has-tag-p
           #:heading-id
           #:heading-property
           #:set-heading-property
           #:collect-all-headings
           #:find-heading-by-custom-id
           #:heading-content-string
           #:heading-all-tags
           #:heading-depth
           #:get-doc-setting
           #:set-doc-setting))

(in-package :org-mode-parser)

;;* Data Structures
(defclass org-node ()
  ((type :initarg :type :accessor node-type)
   (properties :initarg :properties :initform nil :accessor node-properties)
   (parent :initarg :parent :initform nil :accessor node-parent)
   (children :initarg :children :initform nil :accessor node-children)))

(defclass org-document (org-node)
  ((settings :initarg :settings :initform nil :accessor document-settings)))

(defclass org-heading (org-node)
  ((level :initarg :level :accessor heading-level)
   (title :initarg :title :accessor heading-title)
   (tags :initarg :tags :initform nil :accessor heading-tags)
   (todo-keyword :initarg :todo-keyword :initform nil :accessor heading-todo-keyword)
   (priority :initarg :priority :initform nil :accessor heading-priority)))

(defclass org-paragraph (org-node)
  ((content :initarg :content :accessor paragraph-content)))

(defclass org-properties-drawer (org-node)
  ())

(defclass org-src-block (org-node)
  ((language :initarg :language :accessor src-block-language)
   (content :initarg :content :accessor src-block-content)))
;;* Utility Functions
(defun trim-string (string)
  "Remove leading and trailing whitespace from STRING."
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun split-lines (text)
  "Split text into lines, preserving empty lines."
  (when text
    (let ((lines '())
          (start 0))
      (loop for i from 0 below (length text)
            when (char= (char text i) #\Newline)
            do (progn
                 (push (subseq text start i) lines)
                 (setf start (1+ i)))
            finally (push (subseq text start) lines))
      (nreverse lines))))

(defun starts-with-p (string prefix)
  "Check if STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

;;* Parser State
(defstruct parser-state
  (lines nil)
  (position 0)
  (current-line ""))

(defun make-parser (text)
  "Create a new parser state from TEXT."
  (let* ((lines (split-lines text))
         (state (make-parser-state :lines lines :position 0)))
    (when lines
      (setf (parser-state-current-line state) (first lines)))
    state))

(defun advance-line (state)
  "Move to the next line in the parser STATE."
  (incf (parser-state-position state))
  (let ((pos (parser-state-position state))
        (lines (parser-state-lines state)))
    (if (< pos (length lines))
        (setf (parser-state-current-line state) (nth pos lines))
        (setf (parser-state-current-line state) nil))))

(defun peek-line (state &optional (offset 0))
  "Look ahead at a line without advancing the parser."
  (let ((pos (+ (parser-state-position state) offset))
        (lines (parser-state-lines state)))
    (if (< pos (length lines))
        (nth pos lines)
        nil)))

(defun end-of-input-p (state)
  "Check if we've reached the end of input."
  (null (parser-state-current-line state)))

(defun next-significant-line (state)
  "Peek ahead from the current position, skipping blank lines and directive lines, and return the next significant line without advancing."
  (let* ((pos (parser-state-position state))
         (lines (parser-state-lines state))
         (len (length lines)))
    (loop for idx from pos below len
          for line = (nth idx lines)
          for trimmed = (and line (trim-string line))
          do (cond
               ((or (null trimmed)
                    (string= trimmed "")
                    (starts-with-p trimmed "#+"))
                ;; Skip empty and directive lines
                )
               (t (return line)))
          finally (return nil))))

;;* Heading Parser

(defun split-string-by-char (string char)
  "Split STRING by CHAR."
  (let ((result '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) char)
          do (progn
               (push (subseq string start i) result)
               (setf start (1+ i)))
          finally (push (subseq string start) result))
    (nreverse result)))

(defun parse-tags (tag-string)
  "Parse a ':tag1:tag2:' string into a list of strings."
  (let ((trimmed (trim-string tag-string)))
    (if (and (>= (length trimmed) 2)
             (char= (char trimmed 0) #\:)
             (char= (char trimmed (1- (length trimmed))) #\:))
        (let ((inner (subseq trimmed 1 (1- (length trimmed)))))
          (if (string= inner "")
              nil
              (remove "" (split-string-by-char inner #\:) :test #'string=)))
        nil)))

(defun parse-heading-line (line)
  "Parse a heading line and return level, todo, priority, title, and tags."
  (when (and line (starts-with-p line "*"))
    (let* ((level 0)
           (i 0)
           todo-keyword
           priority
           title
           tags)
      ;; Count stars
      (loop while (and (< i (length line))
                       (char= (char line i) #\*))
            do (progn (incf level) (incf i)))

      ;; Skip whitespace
      (loop while (and (< i (length line))
                       (member (char line i) '(#\Space #\Tab)))
            do (incf i))

      (let ((rest (subseq line i)))
        ;; Check for TODO keyword
        (when (or (starts-with-p rest "TODO ")
                  (starts-with-p rest "DONE ")
                  (starts-with-p rest "DOING ")
                  (starts-with-p rest "NEXT ")
                  (starts-with-p rest "WAITING ")
                  (starts-with-p rest "BLOCKED "))
          (let ((space-pos (position #\Space rest)))
            (setf todo-keyword (subseq rest 0 space-pos))
            (setf rest (trim-string (subseq rest space-pos)))))

        ;; Check for priority [#A]
        (when (and (> (length rest) 3)
                   (char= (char rest 0) #\[)
                   (char= (char rest 1) #\#)
                   (char= (char rest 3) #\]))
          (setf priority (char rest 2))
          (setf rest (trim-string (subseq rest 4))))

        ;; Extract tags (at the end, surrounded by colons)
        (let ((tag-start (search " :" rest :from-end t)))
          (when (and tag-start
                     (char= (char rest (1- (length rest))) #\:))
            (setf tags (parse-tags (subseq rest (1+ tag-start))))
            (setf rest (trim-string (subseq rest 0 tag-start)))))

        (setf title rest))

      (values level todo-keyword priority title tags))))

(defun parse-properties-drawer (state)
  "Parse a properties drawer and return it as a node with properties alist."
  (let ((line (parser-state-current-line state)))
    (when (and line (string-equal (trim-string line) ":PROPERTIES:"))
      (advance-line state)
      (let ((props '())
            (drawer (make-instance 'org-properties-drawer :type :properties-drawer)))
        (loop while (and (not (end-of-input-p state))
                        (parser-state-current-line state))
              do (let ((current (trim-string (parser-state-current-line state))))
                   (when (string-equal current ":END:")
                     (advance-line state)
                     (return))
                   (when (and (> (length current) 0)
                             (char= (char current 0) #\:))
                     (let ((colon-pos (position #\: current :start 1)))
                       (when colon-pos
                         (let ((key (intern (string-upcase (subseq current 1 colon-pos)) :keyword))
                               (value (trim-string (subseq current (1+ colon-pos)))))
                           (push (cons key value) props)))))
                   (advance-line state)))
        (setf (node-properties drawer) (nreverse props))
        drawer))))

(defun parse-heading (state parent-level)
  "Parse a heading and its children from STATE."
  (multiple-value-bind (level todo priority title tags)
      (parse-heading-line (parser-state-current-line state))
    (when level
      (advance-line state)
      (let ((heading (make-instance 'org-heading
                                    :type :heading
                                    :level level
                                    :title title
                                    :tags tags
                                    :todo-keyword todo
                                    :priority priority))
            (children '()))

        ;; Check for properties drawer
        (when (and (not (end-of-input-p state))
                  (parser-state-current-line state)
                  (string-equal (trim-string (parser-state-current-line state)) ":PROPERTIES:"))
          (let ((drawer (parse-properties-drawer state)))
            (when drawer
              (setf (node-parent drawer) heading)
              (push drawer children))))

        ;; Parse children until we hit a heading of equal or lower level.
        ;; Important: look ahead past blank/directive lines so sibling headings
        ;; are not misclassified as children if separated by empty lines.
        (loop while (and (not (end-of-input-p state))
                        (parser-state-current-line state))
              do (let ((next-line (next-significant-line state)))
                   (multiple-value-bind (next-level)
                       (parse-heading-line next-line)
                     (when (and next-level (<= next-level level))
                       (return))
                     (let ((child (parse-element state level)))
                       (when child
                         (setf (node-parent child) heading)
                         (push child children))))))

        (setf (node-children heading) (nreverse children))
        heading))))

;;* Src Block Parser
(defun parse-src-block (state)
  "Parse a #+begin_src block."
  (let ((line (parser-state-current-line state)))
    (when (and line (starts-with-p (string-downcase line) "#+begin_src"))
      (let* ((parts (split-string-by-char (trim-string line) #\Space))
             (lang (if (> (length parts) 1) (second parts) nil))
             (content-lines '()))
        (advance-line state)
        (loop while (and (not (end-of-input-p state))
                        (parser-state-current-line state))
              do (let ((current (parser-state-current-line state)))
                   (if (starts-with-p (string-downcase current) "#+end_src")
                       (progn (advance-line state) (return))
                       (progn
                         (push current content-lines)
                         (advance-line state)))))
        (make-instance 'org-src-block
                       :type :src-block
                       :language lang
                       :content (format nil "~{~A~^~%~}" (nreverse content-lines)))))))

;;* Paragraph Parser
(defun parse-paragraph (state)
  "Parse a paragraph from STATE."
  (let ((line (parser-state-current-line state)))
    (when (and line
               (not (string= (trim-string line) ""))
               (or (zerop (length line))
                   (not (char= (char line 0) #\*)))
               (not (starts-with-p line "#+")))
      (let ((content-lines '()))
        (loop while (and (not (end-of-input-p state))
                        (parser-state-current-line state))
              do (let ((current (parser-state-current-line state)))
                   (if (or (string= (trim-string current) "")
                          (and (> (length current) 0)
                               (char= (char current 0) #\*))
                          (starts-with-p current "#+"))
                       (return)
                       (progn
                         (push current content-lines)
                         (advance-line state)))))

        ;; Only return a paragraph if we actually collected content
        (when content-lines
          (make-instance 'org-paragraph
                         :type :paragraph
                         :content (format nil "~{~A~^~%~}" (nreverse content-lines))))))))

;;* Main Element Parser

(defun parse-element (state parent-level)
  "Parse the next element from STATE."
  (when (and (not (end-of-input-p state))
             (parser-state-current-line state))
    (let ((line (parser-state-current-line state)))
      (cond
        ;; Skip empty lines
        ((string= (trim-string line) "")
         (advance-line state)
         (parse-element state parent-level))

        ;; Src Block
        ((starts-with-p (string-downcase line) "#+begin_src")
         (parse-src-block state))

        ;; Skip other settings/directives
        ((starts-with-p line "#+")
         (advance-line state)
         (parse-element state parent-level))

        ;; Heading
        ((and (> (length line) 0)
              (char= (char line 0) #\*))
         (parse-heading state parent-level))

        ;; Paragraph
        (t (parse-paragraph state))))))

;;* Document Parser

(defun parse-settings-line (line)
  "Parse a '#+KEY: VALUE' line. Returns (key . value) or nil."
  (when (starts-with-p line "#+")
    (let ((colon-pos (position #\: line :start 2)))
      (when colon-pos
        (let ((key (intern (string-upcase (trim-string (subseq line 2 colon-pos))) :keyword))
              (value (trim-string (subseq line (1+ colon-pos)))))
          (cons key value))))))

(defun parse-org-string (text)
  "Parse an org-mode string and return an org-document."
  (let ((state (make-parser text))
        (children '())
        (settings '()))

    ;; Parse settings at the beginning
    (loop while (and (not (end-of-input-p state))
                    (parser-state-current-line state)
                    (starts-with-p (parser-state-current-line state) "#+"))
          do (let ((setting (parse-settings-line (parser-state-current-line state))))
               (when setting
                 (push setting settings))
               (advance-line state)))

    ;; Skip preamble content (anything before the first headline)
    (loop while (and (not (end-of-input-p state))
                    (parser-state-current-line state))
          do (let ((line (parser-state-current-line state)))
               (when (and (> (length line) 0)
                         (char= (char line 0) #\*))
                 (return))
               (advance-line state)))

    ;; Create document
    (let ((doc (make-instance 'org-document
                             :type :document
                             :settings (nreverse settings))))

      ;; Parse all elements (only headlines now)
      (loop while (not (end-of-input-p state))
            do (let ((element (parse-element state 0)))
                 (when element
                   (setf (node-parent element) doc)
                   (push element children))))

      (setf (node-children doc) (nreverse children))
      doc)))

(defun parse-org-file (filename)
  "Parse an org-mode file and return an org-document."
  (parse-org-string (uiop:read-file-string filename)))

;;* Tree Traversal

(defun walk-tree (node function)
  "Walk through the tree depth-first, applying FUNCTION to each node."
  (funcall function node)
  (when (node-children node)
    (dolist (child (node-children node))
      (walk-tree child function))))

(defun find-node (root predicate)
  "Find the first node in the tree that satisfies PREDICATE."
  (let ((result nil))
    (block search
      (walk-tree root
                 (lambda (node)
                   (when (funcall predicate node)
                     (setf result node)
                     (return-from search result)))))
    result))

(defun find-all-nodes (root predicate)
  "Find all nodes in the tree that satisfy PREDICATE."
  (let ((results '()))
    (walk-tree root
               (lambda (node)
                 (when (funcall predicate node)
                   (push node results))))
    (nreverse results)))

(defun find-heading-by-id (root id)
  "Find a heading node with the specified ID property."
  (find-node root
             (lambda (node)
               (and (typep node 'org-heading)
                    (let ((props-drawer (find-if (lambda (child)
                                                   (typep child 'org-properties-drawer))
                                                 (node-children node))))
                      (when props-drawer
                        (let ((id-prop (cdr (assoc :ID (node-properties props-drawer)))))
                          (and id-prop (string= id-prop id)))))))))

;;* Tree Manipulation
(defun add-child (parent child)
  "Add CHILD node to PARENT node. Sets parent relationship and returns CHILD."
  (setf (node-parent child) parent)
  (setf (node-children parent)
        (append (node-children parent) (list child)))
  child)

(defun remove-node (node)
  "Remove NODE from its parent. Returns the removed node."
  (let ((parent (node-parent node)))
    (when parent
      (setf (node-children parent)
            (remove node (node-children parent)))
      (setf (node-parent node) nil)))
  node)

(defun insert-after (reference-node new-node)
  "Insert NEW-NODE after REFERENCE-NODE as a sibling."
  (let* ((parent (node-parent reference-node))
         (children (node-children parent))
         (position (position reference-node children)))
    (when (and parent position)
      (setf (node-parent new-node) parent)
      (setf (node-children parent)
            (append (subseq children 0 (1+ position))
                    (list new-node)
                    (subseq children (1+ position)))))))

(defun insert-before (reference-node new-node)
  "Insert NEW-NODE before REFERENCE-NODE as a sibling."
  (let* ((parent (node-parent reference-node))
         (children (node-children parent))
         (position (position reference-node children)))
    (when (and parent position)
      (setf (node-parent new-node) parent)
      (setf (node-children parent)
            (append (subseq children 0 position)
                    (list new-node)
                    (subseq children position))))))

;;* Serialization
(defgeneric org-to-string (node)
  (:documentation "Convert an org-mode node back to its string representation."))

(defmethod org-to-string ((doc org-document))
  (with-output-to-string (s)
    ;; Settings
    (dolist (setting (document-settings doc))
      (format s "#+~A: ~A~%" (symbol-name (car setting)) (cdr setting)))
    (when (document-settings doc)
      (format s "~%"))
    ;; Children (no extra blank lines between top-level headings)
    (dolist (child (node-children doc))
      (princ (org-to-string child) s))))

(defmethod org-to-string ((heading org-heading))
  (with-output-to-string (s)
    ;; Headline
    (format s "~A~A"
            (make-string (heading-level heading) :initial-element #\*)
            (if (plusp (length (heading-title heading))) " " ""))

    (when (heading-todo-keyword heading)
      (format s "~A " (heading-todo-keyword heading)))

    (when (heading-priority heading)
      (format s "[#~A] " (heading-priority heading)))

    (format s "~A" (heading-title heading))

    (when (heading-tags heading)
      (format s " :~{~A~^:~}:" (heading-tags heading)))

    (format s "~%")

    ;; Children
    (loop for child in (node-children heading)
          do (princ (org-to-string child) s))))

(defmethod org-to-string ((drawer org-properties-drawer))
  (with-output-to-string (s)
    (format s ":PROPERTIES:~%")
    (dolist (prop (node-properties drawer))
      (format s ":~A: ~A~%" (symbol-name (car prop)) (cdr prop)))
    (format s ":END:~%")))

(defmethod org-to-string ((para org-paragraph))
  (format nil "~A~%" (paragraph-content para)))

(defmethod org-to-string ((block org-src-block))
  (format nil "#+begin_src ~A~%~A~%#+end_src~%"
          (or (src-block-language block) "")
          (src-block-content block)))

;;* High-Level Utility Helpers

(defun find-zone-heading (doc zone-tag)
  "Find a top-level heading whose tags include ZONE-TAG."
  (find-node
   doc
   (lambda (node)
     (and (typep node 'org-heading)
          (= (heading-level node) 1)
          (member zone-tag (heading-tags node) :test #'string-equal)))))

(defun heading-has-tag-p (heading tag)
  "Check if a heading has a specific tag (case-insensitive)."
  (and (typep heading 'org-heading)
       (member tag (heading-tags heading) :test #'string-equal)))

(defun heading-id (heading)
  "Get the :ID: property of a heading from its properties drawer."
  (when (typep heading 'org-heading)
    (let ((drawer (find-if (lambda (c) (typep c 'org-properties-drawer))
                           (node-children heading))))
      (when drawer
        (cdr (assoc :ID (node-properties drawer)))))))

(defun heading-property (heading prop-key)
  "Get a property value from a heading's properties drawer. PROP-KEY is a keyword."
  (when (typep heading 'org-heading)
    (let ((drawer (find-if (lambda (c) (typep c 'org-properties-drawer))
                           (node-children heading))))
      (when drawer
        (cdr (assoc prop-key (node-properties drawer)))))))

(defun set-heading-property (heading prop-key value)
  "Set a property in a heading's properties drawer. Creates drawer if needed."
  (when (typep heading 'org-heading)
    (let ((drawer (find-if (lambda (c) (typep c 'org-properties-drawer))
                           (node-children heading))))
      (unless drawer
        (setf drawer (make-instance 'org-properties-drawer
                                    :type :properties-drawer
                                    :properties nil))
        ;; Insert at beginning of children
        (setf (node-parent drawer) heading)
        (setf (node-children heading)
              (cons drawer (node-children heading))))
      ;; Update or add property
      (let ((existing (assoc prop-key (node-properties drawer))))
        (if existing
            (setf (cdr existing) value)
            (push (cons prop-key value) (node-properties drawer)))))))

(defun collect-all-headings (doc)
  "Collect all heading nodes in the document, depth-first."
  (let ((headings '()))
    (walk-tree
     doc
     (lambda (node)
       (when (typep node 'org-heading)
         (push node headings))))
    (nreverse headings)))

(defun find-heading-by-custom-id (doc id)
  "Find a heading by its :ID: property."
  (find-node
   doc
   (lambda (node)
     (and (typep node 'org-heading)
          (string= (or (heading-id node) "") id)))))

(defun heading-content-string (heading)
  "Serialize a heading and all its children to an org string."
  (org-to-string heading))

(defun heading-all-tags (heading)
  "Get all tags from a heading, including inherited tags."
  (let ((tags (copy-list (or (heading-tags heading) '()))))
    ;; Walk up parents to collect inherited tags
    (let ((parent (node-parent heading)))
      (loop while (and parent (typep parent 'org-heading))
            do (dolist (tag (heading-tags parent))
                 (pushnew tag tags :test #'string-equal))
               (setf parent (node-parent parent))))
    tags))

(defun heading-depth (heading)
  "Get the depth (level) of a heading relative to document root."
  (heading-level heading))

(defun get-doc-setting (doc key)
  "Get a document-level setting (#+KEY: VALUE). KEY is a keyword like :HACTAR_SCOPE."
  (when (typep doc 'org-document)
    (cdr (assoc key (document-settings doc)))))

(defun set-doc-setting (doc key value)
  "Set a document-level setting. Modifies settings in-place."
  (when (typep doc 'org-document)
    (let ((existing (assoc key (document-settings doc))))
      (if existing
          (setf (cdr existing) value)
          (push (cons key value) (document-settings doc))))))
