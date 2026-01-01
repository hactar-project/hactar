;; A WIP org-mode. util functions for inserting headlines, updating them, searching content etc
;; uses ugly mix of proper parsing and regex. here be dragons!
(defpackage :org-mode
  (:use :cl :cl-ppcre)
  (:export #:insert-child #:insert-sibling #:delete-headline #:select-headlines-by-tag
	   #:get-headline-level #:get-property #:find-headline-region #:get-tags-from-headline
           #:filter-headlines
           #:insert-child! #:insert-sibling! #:delete-headline! #:filter-headlines! #:upsert-files-section
           #:upsert-docs-section #:upsert-errors-section
	   #:escape-for-org-s
           ;; Monolith-style org parsing
           #:parse-org-properties-from-content
           #:parse-org-heading-line
           #:parse-org-links
           #:parse-org-code-blocks-from-content
           #:parse-org-file-full
           #:compute-content-checksum))

(in-package :org-mode)
;;* Helper Functions

(defun get-headline-level (line)
  "Returns the level of a headline line (number of stars), or nil if not a headline."
  (multiple-value-bind (match registers)
      (scan-to-strings "^(\\*+)\\s+" line)
    (when match
      (length (aref registers 0)))))

(defun get-property (prop-name lines)
  "Finds the value of a property (e.g., :ID:) within a list of lines representing a property drawer."
  (let ((in-drawer nil)
        (prop-pattern (format nil "^\\s*:(~A):\\s*(.+)$" (string-upcase prop-name))))
    (dolist (line lines)
      (cond
        ((scan "^\\s*:PROPERTIES:\\s*$" line) (setf in-drawer t))
        ((scan "^\\s*:END:\\s*$" line) (setf in-drawer nil))
        (in-drawer
         (multiple-value-bind (match registers)
             (scan-to-strings prop-pattern line)
           (when match
             (return-from get-property (string-trim '(#\Space #\Tab) (aref registers 1))))))))
    nil))

(defun find-headline-region (lines target-id)
  "Finds the start line index, end line index (exclusive), level, and the headline line with target-id.
   Returns (values start-index end-index level headline-line) or (values nil nil nil nil)."
  (let ((num-lines (length lines))
        (current-start nil)
        (current-level nil)
        (current-headline nil)
        (found-target nil)
        (target-start nil)
        (target-level nil)
        (target-headline nil)
        (target-end nil))
    (loop for i from 0 below num-lines
          for line = (nth i lines)
          for level = (get-headline-level line)
          do
             (when level ; It's a headline
               (when current-start ; Finalize previous headline region
                 (unless found-target
                   (setf current-start nil current-level nil current-headline nil)))

               (when (and current-start (<= level current-level)) ; End of current section
                 (unless found-target
                   (setf current-start nil current-level nil current-headline nil)))

               (unless current-start ; Start of a new potential section
                 (setf current-start i
                       current-level level
                       current-headline line))

               ;; Check properties for ID within the current headline's scope
               (let ((prop-lines '())
                     (j i))
                 ;; Look ahead for properties drawer
                 (loop while (< (incf j) num-lines)
                       for next-line = (nth j lines)
                       for next-level = (get-headline-level next-line)
                       do
                          (when (and next-level (<= next-level level)) (return)) ; Next sibling/parent
                          (push next-line prop-lines)
                          (when (scan "^\\s*:END:\\s*$" next-line) (return)))

                 (when (string= (get-property "ID" (reverse prop-lines)) target-id)
                   (setf found-target t
                         target-start current-start
                         target-level current-level
                         target-headline current-headline)))

               ;; If we found the target, find where its section ends
               (when found-target
                 (setf target-end num-lines) ; Default to end of file
                 (loop for k from (1+ target-start) below num-lines
                       for end-line = (nth k lines)
                       for end-level = (get-headline-level end-line)
                       do
                          (when (and end-level (<= end-level target-level))
                            (setf target-end k)
                            (return))) ; Found next sibling or parent
                 (return-from find-headline-region (values target-start target-end target-level target-headline)))))
    ;; If loop finishes without finding target or if target is the last element
    (if found-target
        (values target-start (or target-end num-lines) target-level target-headline)
        (values nil nil nil nil))))


(defun adjust-headline-levels (org-string delta)
  "Adjusts the level of all headlines in org-string by delta.
   Delta can be positive or negative. Ensures levels don't go below 1."
  (when (null org-string) (return-from adjust-headline-levels ""))
  (let ((lines (split-lines org-string))
        (result-lines '()))
    (dolist (line lines)
      (let ((level (get-headline-level line)))
        (if level
            (let* ((new-level (max 1 (+ level delta)))
                   (stars (make-string new-level :initial-element #\*))
                   (rest (subseq line level))) ; Assumes stars are contiguous at start
              (push (concatenate 'string stars rest) result-lines))
            (push line result-lines))))
    (join-lines (reverse result-lines))))

(defun split-lines (text)
  "Splits text into lines, preserving empty lines."
  (when text (str:lines text :omit-nulls nil)))

(defun join-lines (lines)
  "Joins lines back into a single string, adding newlines."
  (str:join #\Newline lines))

(defun escape-for-org-s (s)
  "Escape S when a line starts with specific characters"
  (with-output-to-string (out)
    (dolist (line (cl-ppcre:split (format nil "~%") s))
      (write-line (cl-ppcre:regex-replace-all "^(\\*|\\#|\\#\\+|,\\*|,\\#\\+)" line ",\\1") out))))

;;* Monolith-style org parsing functions

(defun parse-org-properties-from-content (content)
  "Parse :PROPERTIES: drawer from org content string. Returns alist of (keyword . value)."
  (let ((props nil)
        (in-drawer nil))
    (dolist (line (str:lines content))
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ((string= trimmed ":PROPERTIES:")
           (setf in-drawer t))
          ((string= trimmed ":END:")
           (setf in-drawer nil))
          ((and in-drawer (str:starts-with? ":" trimmed))
           (let* ((colon-pos (position #\: trimmed :start 1))
                  (key (when colon-pos 
                         (string-upcase (subseq trimmed 1 colon-pos))))
                  (value (when colon-pos
                           (string-trim '(#\Space #\Tab) 
                                        (subseq trimmed (1+ colon-pos))))))
             (when (and key value)
               (push (cons (intern key :keyword) value) props)))))))
    (nreverse props)))

(defun parse-org-heading-line (line)
  "Parse an org heading line. Returns (level . title) or nil."
  (when (str:starts-with? "*" line)
    (let* ((stars (loop for c across line
                        while (char= c #\*)
                        count t))
           (title (string-trim '(#\Space #\Tab) (subseq line stars))))
      (cons stars title))))

(defun parse-org-links (content)
  "Extract org-mode links from content. Returns list of (type . target).
   Types: :id, :file, :semantic."
  (let ((links nil))
    ;; ID links: [[id:uuid]]
    (cl-ppcre:do-matches-as-strings (match "\\[\\[id:([^\\]]+)\\]\\]" content)
      (push (cons :id (subseq match 5 (- (length match) 2))) links))
    ;; File links: [[file:path]]
    (cl-ppcre:do-matches-as-strings (match "\\[\\[file:([^\\]]+)\\]\\]" content)
      (push (cons :file (subseq match 7 (- (length match) 2))) links))
    ;; Semantic links: [[Concepts/Auth/JWT]]
    (cl-ppcre:do-matches-as-strings (match "\\[\\[([A-Z][^\\]]+)\\]\\]" content)
      (let ((target (subseq match 2 (- (length match) 2))))
        (unless (or (str:starts-with? "id:" target)
                    (str:starts-with? "file:" target)
                    (str:starts-with? "http" target))
          (push (cons :semantic target) links))))
    (nreverse links)))

(defun parse-org-code-blocks-from-content (content &optional file-path)
  "Extract code blocks from org content string. Returns list of plists with
   :id :language :content :tangle-target :project :checksum :line-start :line-end."
  (let ((blocks nil)
        (current-block nil)
        (in-block nil)
        (line-num 0))
    (dolist (line (str:lines content))
      (incf line-num)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ;; Start of code block
          ((and (not in-block) (str:starts-with? "#+BEGIN_SRC" trimmed))
           (setf in-block t)
           (let* ((rest (subseq trimmed 11))
                  (parts (str:words rest))
                  (lang (first parts))
                  (tangle-target nil)
                  (project nil))
             ;; Parse header args
             (loop for (key val) on (rest parts) by #'cddr
                   do (cond
                        ((string= key ":tangle")
                         (unless (string= val "no")
                           (setf tangle-target val)))
                        ((string= key ":project")
                         (setf project val))))
             (setf current-block (list :id (format nil "~A:~A" 
                                                   (or file-path "unknown") line-num)
                                       :language lang
                                       :tangle-target tangle-target
                                       :project project
                                       :content ""
                                       :line-start line-num
                                       :line-end nil
                                       :checksum nil))))
          
          ;; End of code block
          ((and in-block (str:starts-with? "#+END_SRC" trimmed))
           (setf in-block nil)
           (setf (getf current-block :line-end) line-num)
           (setf (getf current-block :content)
                 (string-trim '(#\Newline) (getf current-block :content)))
           (setf (getf current-block :checksum)
                 (compute-content-checksum (getf current-block :content)))
           (push current-block blocks)
           (setf current-block nil))
          
          ;; Inside code block
          (in-block
           (setf (getf current-block :content)
                 (concatenate 'string 
                              (getf current-block :content)
                              line
                              (string #\Newline)))))))
    (nreverse blocks)))

(defun parse-org-file-full (file-path)
  "Parse an org file completely. Returns plist with :path :properties :links :code-blocks :headings."
  (let* ((content (uiop:read-file-string file-path))
         (props (parse-org-properties-from-content content))
         (links (parse-org-links content))
         (code-blocks (parse-org-code-blocks-from-content content file-path))
         (headings nil)
         (current-heading nil)
         (heading-content (make-string-output-stream)))
    
    ;; Parse headings
    (dolist (line (str:lines content))
      (let ((heading (parse-org-heading-line line)))
        (if heading
            (progn
              ;; Save previous heading
              (when current-heading
                (push (list :level (car current-heading)
                            :title (cdr current-heading)
                            :content (get-output-stream-string heading-content))
                      headings)
                (setf heading-content (make-string-output-stream)))
              (setf current-heading heading))
            (when current-heading
              (write-line line heading-content)))))
    
    ;; Save last heading
    (when current-heading
      (push (list :level (car current-heading)
                  :title (cdr current-heading)
                  :content (get-output-stream-string heading-content))
            headings))
    
    (list :path file-path
          :properties props
          :links links
          :code-blocks code-blocks
          :headings (nreverse headings))))

(defun compute-content-checksum (content)
  "Compute a checksum for content string."
  (format nil "~X" (sxhash content)))

;;* core
;; v2 parser helpers
(defun %v2-headings-in-doc (doc)
  "Return all top-level heading nodes in a parsed v2 document."
  (remove-if-not (lambda (n) (typep n 'org-mode-parser:org-heading))
                 (org-mode-parser:node-children doc)))

(defun %v2-adjust-levels (node delta)
  "Recursively adjust heading levels by DELTA (min level = 1)."
  (cond
    ((typep node 'org-mode-parser:org-heading)
     (setf (org-mode-parser:heading-level node)
           (max 1 (+ (org-mode-parser:heading-level node) delta)))
     (dolist (child (org-mode-parser:node-children node))
       (%v2-adjust-levels child delta)))
    (t
     (dolist (child (org-mode-parser:node-children node))
       (%v2-adjust-levels child delta)))))

(defun %v2-deep-copy (node)
  "Deep copy a v2 node (heading, paragraph, or properties drawer)."
  (cond
    ((typep node 'org-mode-parser:org-heading)
     (let* ((copy (make-instance 'org-mode-parser:org-heading
                                 :type :heading
                                 :level (org-mode-parser:heading-level node)
                                 :title (org-mode-parser:heading-title node)
                                 :tags (copy-list (org-mode-parser:heading-tags node))
                                 :todo-keyword (org-mode-parser:heading-todo-keyword node)
                                 :priority (org-mode-parser:heading-priority node)))
            (children (org-mode-parser:node-children node)))
       (dolist (c children)
         (org-mode-parser:add-child copy (%v2-deep-copy c)))
       copy))
    ((typep node 'org-mode-parser:org-paragraph)
     (make-instance 'org-mode-parser:org-paragraph
                    :type :paragraph
                    :content (org-mode-parser:paragraph-content node)))
    ((typep node 'org-mode-parser:org-properties-drawer)
     (make-instance 'org-mode-parser:org-properties-drawer
                    :type :properties-drawer
                    :properties (copy-list (org-mode-parser:node-properties node))))
    (t
     ;; Fallback: shallow copy structure for unknown nodes
     (let ((copy (make-instance (class-of node)
                                :type (org-mode-parser:node-type node))))
       (dolist (c (org-mode-parser:node-children node))
         (org-mode-parser:add-child copy (%v2-deep-copy c)))
       copy))))
(defun insert-child (org-string target-id new-content-string)
  "Use v2 parser: insert new-content-string as children under headline with target-id.
Adjusts levels to be children of the target. Returns modified Org string or NIL if not found."
  (handler-case
      (let* ((doc (org-mode-parser:parse-org-string (or org-string "")))
             (parent (org-mode-parser:find-heading-by-id doc target-id)))
        (unless parent (return-from insert-child nil))
        (let* ((new-doc (org-mode-parser:parse-org-string (or new-content-string "")))
               (new-headings (%v2-headings-in-doc new-doc)))
          (when (null new-headings)
            (return-from insert-child org-string))
          (let* ((parent-level (org-mode-parser:heading-level parent))
                 (first-level (org-mode-parser:heading-level (first new-headings)))
                 (delta (- (+ parent-level 1) first-level)))
            (dolist (h new-headings)
              (let ((copy (%v2-deep-copy h)))
                (%v2-adjust-levels copy delta)
                (org-mode-parser:add-child parent copy))))
          (string-right-trim '(#\Newline) (org-mode-parser:org-to-string doc))))
    (error (e)
      (declare (ignore e))
      nil)))

(defun insert-sibling (org-string target-id new-content-string)
  "Use v2 parser: insert new-content-string as siblings after the headline with target-id.
Adjusts levels to match the target's level. Returns modified Org string or NIL if not found."
  (handler-case
      (let* ((doc (org-mode-parser:parse-org-string (or org-string "")))
             (target (org-mode-parser:find-heading-by-id doc target-id)))
        (unless target (return-from insert-sibling nil))
        (let* ((new-doc (org-mode-parser:parse-org-string (or new-content-string "")))
               (new-headings (%v2-headings-in-doc new-doc)))
          (when (null new-headings)
            (return-from insert-sibling org-string))
          (let* ((sibling-level (org-mode-parser:heading-level target))
                 (first-level (org-mode-parser:heading-level (first new-headings)))
                 (delta (- sibling-level first-level))
                 (anchor target))
            (dolist (h new-headings)
              (let ((copy (%v2-deep-copy h)))
                (%v2-adjust-levels copy delta)
                (org-mode-parser:insert-after anchor copy)
                (setf anchor copy))))
          (string-right-trim '(#\Newline) (org-mode-parser:org-to-string doc))))
    (error (e)
      (declare (ignore e))
      nil)))

(defun delete-headline (org-string target-id)
  "Use v2 parser: delete the headline (and its subtree) with target-id.
Returns modified Org string or NIL if not found."
  (handler-case
      (let* ((doc (org-mode-parser:parse-org-string (or org-string "")))
             (node (org-mode-parser:find-heading-by-id doc target-id)))
        (unless node (return-from delete-headline nil))
        (org-mode-parser:remove-node node)
        (string-right-trim '(#\Newline) (org-mode-parser:org-to-string doc)))
    (error (e)
      (declare (ignore e))
      nil)))

(defun get-tags-from-headline (headline-line)
  "Extracts tags (like :tag1:tag2:) from a headline string."
  (multiple-value-bind (match registers)
      (scan-to-strings ":([^:]+(?::[^:]+)*):$" headline-line) ; Match full tags string at end
    (when match
      (remove "" (str:split ":" (aref registers 0)) :test #'string=))))

(defun select-headlines-by-tag (org-string tags)
  "Use v2 parser: select headlines whose tags include all of TAGS (case-insensitive).
Returns concatenated org text of matching headlines (subtrees). If TAGS is empty, return original string."
  (when (or (null tags) (equal tags '()))
    (return-from select-headlines-by-tag org-string))
  (let* ((required (mapcar #'string-downcase tags))
         (doc (org-mode-parser:parse-org-string (or org-string "")))
         (matches (org-mode-parser:find-all-nodes
                   doc
                   (lambda (node)
                     (and (typep node 'org-mode-parser:org-heading)
                          (let* ((node-tags (org-mode-parser:heading-tags node))
                                 (lower (mapcar #'string-downcase (or node-tags '()))))
                            (subsetp required lower :test #'string=)))))))
    (let ((buf (make-string-output-stream)))
      (dolist (h matches)
        (write-string (org-mode-parser:org-to-string h) buf))
      (string-right-trim '(#\Newline) (get-output-stream-string buf)))))

(defun filter-headlines (org-string tags-to-filter)
  "Removes headline sections from org-string that contain ANY of the specified tags-to-filter.
   Tags should be a list of strings (case-insensitive). Returns the modified org-mode string."
  (when (or (null org-string) (string= org-string "") (null tags-to-filter))
    (return-from filter-headlines org-string))

  (let* ((lines (split-lines org-string))
         (num-lines (length lines))
         (result-lines '()) ; Use a list to collect lines in order
         (filter-tags-lower (mapcar #'string-downcase tags-to-filter))
         (i 0))
    (loop while (< i num-lines)
          for line = (nth i lines)
          for level = (get-headline-level line)
          do
             (if level ; It's a headline
                 (let* ((headline-tags (mapcar #'string-downcase (get-tags-from-headline line)))
                        (should-filter (intersection filter-tags-lower headline-tags :test #'string=)))
                   (if should-filter
                       ;; Headline matches a filter tag, find its end and skip the whole section
                       (let ((end-index num-lines)) ; Default to end of file
                         (loop for k from (1+ i) below num-lines
                               for end-line = (nth k lines)
                               for end-level = (get-headline-level end-line)
                               do
                                  (when (and end-level (<= end-level level))
                                    (setf end-index k)
                                    (return))) ; Found next sibling or parent
                         (setf i end-index))   ; Skip the filtered section by advancing i
                       ;; Headline doesn't match filter, keep this line and continue
                       (progn
                         (push line result-lines) ; Keep the non-filtered headline
                         (incf i)))) ; Move to the next line
                 ;; Not a headline, just keep the line
                 (progn
                   (push line result-lines)
                   (incf i))))
    ;; Lines were pushed in reverse order, so reverse before joining
    (join-lines (reverse result-lines))))
;;* File Operations
(defun read-file-content (filename)
  "Safely reads the content of a file."
  (handler-case (uiop:read-file-string filename)
    (error (e)
      (format *error-output* "Error reading file ~A: ~A~%" filename e)
      nil)))

(defun write-file-content (filename content)
  "Safely writes content to a file."
  (handler-case (with-open-file (stream filename :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                  (write-string content stream))
    (error (e)
      (format *error-output* "Error writing file ~A: ~A~%" filename e)
      nil)))

(defun find-headline (filename target-id)
  "Reads filename, finds the headline with target-id, and returns its content as a string."
  (let ((org-string (read-file-content filename)))
    (when org-string
      (let ((lines (split-lines org-string)))
        (multiple-value-bind (start-index end-index level headline-line)
            (find-headline-region lines target-id)
          (declare (ignore level headline-line))
          (when start-index
            (join-lines (subseq lines start-index end-index))))))))

(defun insert-child! (filename target-id new-content-string)
  "Reads filename, inserts child content, and writes back to filename. Returns T on success, NIL on failure."
  (let ((org-string (read-file-content filename)))
    (when org-string
      (let ((modified-string (insert-child org-string target-id new-content-string)))
        (when modified-string
          (write-file-content filename modified-string)
          t)))))

(defun filter-headlines! (filename tags-to-filter)
  "Reads filename, removes headlines matching tags, and writes back. Returns T on success, NIL on failure."
  (let ((org-string (read-file-content filename)))
    (when org-string
      (let ((modified-string (filter-headlines org-string tags-to-filter)))
        ;; Only write if content actually changed? Or always write? Let's always write if read succeeded.
        (if modified-string ; filter-headlines should always return a string
            (write-file-content filename modified-string)
            nil)))))

(defun insert-sibling! (filename target-id new-content-string)
  "Reads filename, inserts sibling content, and writes back to filename. Returns T on success, NIL on failure."
  (let ((org-string (read-file-content filename)))
    (when org-string
      (let ((modified-string (insert-sibling org-string target-id new-content-string)))
        (when modified-string
          (write-file-content filename modified-string)
          t)))))

(defun delete-headline! (filename target-id)
  "Reads filename, deletes the headline section, and writes back to filename. Returns T on success, NIL on failure."
  (let ((org-string (read-file-content filename)))
    (when org-string
      (let ((modified-string (delete-headline org-string target-id)))
        (when modified-string
          (write-file-content filename modified-string)
          t)))))

(defun upsert-files-section (org-string files-content-string &key (heading-title "Files") (level 1))
  "Create or update the Files headline with src blocks content using the v2 parser.
Returns the modified Org string.

- Ensures a top-level heading with TITLE exists (default: \"Files\").
- Replaces its children with a single paragraph containing FILES-CONTENT-STRING.
- Note: The parser ignores #+ directives when parsing; this function injects the raw
  src block text as a paragraph so serialization preserves it. On re-parse those
  directives are ignored, but we always regenerate this section from the live context."
  (handler-case
      (let* ((doc (org-mode-parser:parse-org-string (or org-string "")))
             (files-heading
               (org-mode-parser:find-node
                doc
                (lambda (node)
                  (and (typep node 'org-mode-parser:org-heading)
                       (string= (org-mode-parser:heading-title node) heading-title))))))
        (unless files-heading
          (setf files-heading (make-instance 'org-mode-parser:org-heading
                                             :type :heading
                                             :level level
                                             :title heading-title))
          (org-mode-parser:add-child doc files-heading))
        ;; Remove existing children
        (dolist (child (copy-list (org-mode-parser:node-children files-heading)))
          (org-mode-parser:remove-node child))
        ;; Add fresh paragraph with the src blocks content
        (when (and files-content-string (not (string= files-content-string "")))
          (org-mode-parser:add-child
           files-heading
           (make-instance 'org-mode-parser:org-paragraph
                          :type :paragraph
                          :content (string-right-trim '(#\Newline) files-content-string))))
        (string-right-trim '(#\Newline) (org-mode-parser:org-to-string doc)))
    (error (e)
      (declare (ignore e))
      ;; Fallback: minimal document with Files heading
      (with-output-to-string (s)
        (format s "* ~A~%~A~%" heading-title (or files-content-string ""))))))

(defun upsert-docs-section (org-string docs-plists &key (heading-title "Documentation") (level 1))
  "Create or update the Documentation headline and replace its children with headings
for each doc in DOCS-PLISTS.
Each doc heading gets a :SOURCE_ID: property and a src block paragraph containing raw content."
  (handler-case
      (let* ((doc (org-mode-parser:parse-org-string (or org-string "")))
             (docs-heading
               (org-mode-parser:find-node
                doc
                (lambda (node)
                  (and (typep node 'org-mode-parser:org-heading)
                       (string= (org-mode-parser:heading-title node) heading-title))))))
        (unless docs-heading
          (setf docs-heading (make-instance 'org-mode-parser:org-heading
                                            :type :heading
                                            :level level
                                            :title heading-title))
          (org-mode-parser:add-child doc docs-heading))
        ;; Remove existing children
        (dolist (child (copy-list (org-mode-parser:node-children docs-heading)))
          (org-mode-parser:remove-node child))
        ;; Add new children
        (dolist (d docs-plists)
          (let* ((title (or (cdr (assoc :title d)) (cdr (assoc :TITLE d)) "Untitled"))
                 (source-id (or (cdr (assoc :SOURCE_ID d)) (cdr (assoc :source-id d)) (format nil "~A" (uuid:make-v4-uuid))))
                 (raw (or (cdr (assoc :content d)) (cdr (assoc :CONTENT d)) "")))
            (let ((h (make-instance 'org-mode-parser:org-heading
                                    :type :heading
                                    :level (1+ level)
                                    :title title))
                  (props (make-instance 'org-mode-parser:org-properties-drawer
                                        :type :properties-drawer
                                        :properties `((:SOURCE_ID . ,source-id))))
                  (para (make-instance 'org-mode-parser:org-paragraph
                                       :type :paragraph
                                       :content (with-output-to-string (s)
                                                  (format s "#+begin_src text~%~A#+end_src"
                                                          (escape-for-org-s (string-right-trim '(#\Newline) raw)))))))
              (org-mode-parser:add-child h props)
              (org-mode-parser:add-child h para)
              (org-mode-parser:add-child docs-heading h))))
        (string-right-trim '(#\Newline) (org-mode-parser:org-to-string doc)))
    (error (e)
      (declare (ignore e))
      ;; Fallback: minimal section with titles only
      (with-output-to-string (s)
        (format s "* ~A~%" heading-title)
        (dolist (d docs-plists)
          (format s "** ~A~%:PROPERTIES:~%:SOURCE_ID: ~A~%:END:~%"
                  (or (cdr (assoc :title d)) (cdr (assoc :TITLE d)) "Untitled")
                  (or (cdr (assoc :SOURCE_ID d)) (cdr (assoc :source-id d)) "")))))))

(defun upsert-errors-section (org-string errors-plists &key (heading-title "Errors") (level 1))
  "Create or update the Errors headline and replace its children with headings
for each error in ERRORS-PLISTS."
  (handler-case
      (let* ((doc (org-mode-parser:parse-org-string (or org-string "")))
             (errors-heading
               (org-mode-parser:find-node
                doc
                (lambda (node)
                  (and (typep node 'org-mode-parser:org-heading)
                       (string= (org-mode-parser:heading-title node) heading-title))))))
        (unless errors-heading
          (setf errors-heading (make-instance 'org-mode-parser:org-heading
                                              :type :heading
                                              :level level
                                              :title heading-title))
          (org-mode-parser:add-child doc errors-heading))
        ;; Remove existing children
        (dolist (child (copy-list (org-mode-parser:node-children errors-heading)))
          (org-mode-parser:remove-node child))
        ;; Add new children
        (dolist (err errors-plists)
          (let* ((title (or (cdr (assoc :title err)) "Untitled Error"))
                 (code (or (cdr (assoc :code err)) "unknown-code"))
                 (msg (or (cdr (assoc :message err)) ""))
                 (cause (or (cdr (assoc :cause err)) ""))
                 (solution (or (cdr (assoc :solution err)) ""))
                 (tags (cdr (assoc :tags err))))
            (let ((h (make-instance 'org-mode-parser:org-heading
                                    :type :heading
                                    :level (1+ level)
                                    :title title))
                  (props (make-instance 'org-mode-parser:org-properties-drawer
                                        :type :properties-drawer
                                        :properties `((:CODE . ,code)
                                                      ,@(when tags `((:TAGS . ,(format nil "~{~A~^ ~}" (coerce tags 'list))))))))
                  (msg-block (make-instance 'org-mode-parser:org-src-block
                                            :type :src-block
                                            :language "error"
                                            :content msg))
                  (cause-h (make-instance 'org-mode-parser:org-heading
                                          :type :heading
                                          :level (+ level 2)
                                          :title "Cause"))
                  (cause-p (make-instance 'org-mode-parser:org-paragraph
                                          :type :paragraph
                                          :content cause))
                  (sol-h (make-instance 'org-mode-parser:org-heading
                                        :type :heading
                                        :level (+ level 2)
                                        :title "Solution"))
                  (sol-p (make-instance 'org-mode-parser:org-paragraph
                                        :type :paragraph
                                        :content solution)))
              
              (org-mode-parser:add-child h props)
              (org-mode-parser:add-child h msg-block)
              
              (org-mode-parser:add-child cause-h cause-p)
              (org-mode-parser:add-child h cause-h)
              
              (org-mode-parser:add-child sol-h sol-p)
              (org-mode-parser:add-child h sol-h)
              
              (org-mode-parser:add-child errors-heading h))))
        (string-right-trim '(#\Newline) (org-mode-parser:org-to-string doc)))
    (error (e)
      (declare (ignore e))
      ;; Fallback
      (with-output-to-string (s)
        (format s "* ~A~%" heading-title)
        (dolist (err errors-plists)
          (format s "** ~A~%:PROPERTIES:~%:CODE: ~A~%:END:~%#+begin_src error~%~A~%#+end_src~%*** Cause~%~A~%*** Solution~%~A~%"
                  (cdr (assoc :title err))
                  (cdr (assoc :code err))
                  (cdr (assoc :message err))
                  (cdr (assoc :cause err))
                  (cdr (assoc :solution err))))))))
