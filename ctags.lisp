;; simple ctags wrapper
;; parsing and loading of ctags files
(in-package :hactar)

(defstruct tag
  name
  file
  address
  kind
  line)

(defun parse-tag-line (line)
  "Parse a single line from a ctags file."
  (let ((parts (str:split (string #\Tab) line :limit 3)))
    (when (>= (length parts) 3)
      (let* ((name (first parts))
             (file (second parts))
             (rest (third parts))
             ;; The address is usually until ;"
             (addr-end (position #\; rest :test #'char=))
             (address (if addr-end (subseq rest 0 addr-end) rest))
             ;; Fields are after ;"
             (fields-str (if (and addr-end (> (length rest) (+ addr-end 2)))
                           (subseq rest (+ addr-end 2))
                           ""))
             (fields (remove "" (str:split (string #\Tab) fields-str) :test #'string=)))
        
        (make-tag :name name
                  :file file
                  :address address
                  :kind (when fields (first fields)) ;; Simplified kind extraction
                  :line (let ((line-field (find-if (lambda (s) (str:starts-with? "line:" s)) fields)))
                          (if line-field
                              (parse-integer (subseq line-field 5) :junk-allowed t)
                              0)))))))

(defun load-tags ()
  "Load tags from the ctags file into *tags-cache*."
  (let ((tags-path (merge-pathnames *ctags-file* *repo-root*)))
    (if (probe-file tags-path)
        (let ((tags '()))
          (with-open-file (stream tags-path :direction :input :if-does-not-exist nil)
            (loop for line = (read-line stream nil nil)
                  while line
                  unless (str:starts-with? "!_" line) ;; Skip headers
                  do (let ((tag (parse-tag-line line)))
                       (when tag (push tag tags)))))
          (setf *tags-cache* (nreverse tags))
          (format t "Loaded ~A tags.~%" (length *tags-cache*))
          *tags-cache*)
        (progn
          (format t "Tags file not found: ~A~%" tags-path)
          nil))))

(defun tags-index (&rest args)
  "Run ctags to index the codebase. ARGS are passed to ctags."
  (unless (find-executable "ctags")
    (error "ctags executable not found."))
  
  (let* ((excludes (mapcan (lambda (ex) (list (format nil "--exclude=~A" ex))) *exclude-from-ctags*))
         (cmd (append (list "ctags" "-R")
                      excludes
                      (list "-f" (uiop:native-namestring (merge-pathnames *ctags-file* *repo-root*)))
                      args
                      (list (uiop:native-namestring *repo-root*)))))
    (format t "Running ctags: ~{~A~^ ~}~%" cmd)
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program cmd :output :string :error-output :string :ignore-error-status t)
      (if (zerop exit-code)
          (progn
            (format t "Ctags indexing complete.~%")
            (load-tags))
          (format t "Ctags failed: ~A~%" error-output)))))

(define-sub-command code.index (args)
  "Index the codebase using ctags."
  (apply #'tags-index args))

(defun tags-all ()
  "Return all tags, loading them if necessary."
  (or *tags-cache* (load-tags)))

(defun tags-find (query &key (limit 10))
  "Find tags matching QUERY using fuzzy matching."
  (let ((all (tags-all)))
    (if (string= query "")
        (subseq all 0 (min (length all) limit))
        (let ((matches (remove-if-not (lambda (tag)
                                        (str:contains? query (tag-name tag) :ignore-case t))
                                      all)))
          (subseq matches 0 (min (length matches) limit))))))

(defun tags-find-one (query)
  "Find a single tag matching QUERY."
  (first (tags-find query :limit 1)))

(defun tags-get (symbol)
  "Get tags exactly matching SYMBOL."
  (remove-if-not (lambda (tag) (string= symbol (tag-name tag))) (tags-all)))

(defun tags-for (targets)
  "Return tags for a file, directory, or list of them.
   TARGETS can be a string (path) or a list of strings."
  (let ((target-list (uiop:ensure-list targets))
        (all (tags-all)))
    ;; Normalize targets to be relative to repo-root for comparison with tag paths
    (let ((rel-targets (mapcar (lambda (targ) 
                                 (let ((p (uiop:parse-native-namestring targ)))
                                   (if (uiop:absolute-pathname-p p)
                                       (uiop:native-namestring (uiop:enough-pathname p *repo-root*))
                                       targ))) 
                               target-list)))
      (remove-if-not (lambda (tag)
                       (some (lambda (target)
                               (str:starts-with? target (tag-file tag)))
                             rel-targets))
                     all))))

(defun extract-symbols-from-text (text)
  "Simple heuristic to extract potential symbols from text."
  (let ((tokens (cl-ppcre:split "[^a-zA-Z0-9_\\-]+" text)))
    (remove-if (lambda (s) (< (length s) 3)) tokens)))

(defun tags-for-context ()
  "Turn context (*files*) into a map of symbols and return matching tags.
   Scans content of files in *files* for symbols that exist in tags db."
  (let ((symbols (make-hash-table :test 'equal))
        (found-tags '()))
    ;; Extract symbols from all files in context
    (dolist (file *files*)
      (let ((content (get-file-content file)))
        (when content
          (dolist (sym (extract-symbols-from-text content))
            (setf (gethash sym symbols) t)))))
    
    ;; Find tags that match these symbols
    (dolist (tag (tags-all))
      (when (gethash (tag-name tag) symbols)
        (push tag found-tags)))
    
    found-tags))
