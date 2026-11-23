(in-package #:hactar-llm)

;;; Dependency-free model catalog loader for data/models.yaml.

(defun %trim-ws (string)
  (string-trim '(#\Space #\Tab #\Return #\Newline) string))

(defun %strip-quotes (string)
  (let ((s (%trim-ws string)))
    (if (and (>= (length s) 2)
             (or (and (char= (char s 0) #\") (char= (char s (1- (length s))) #\"))
                 (and (char= (char s 0) #\') (char= (char s (1- (length s))) #\'))))
        (subseq s 1 (1- (length s)))
        s)))

(defun %parse-yaml-scalar (raw)
  (let ((s (%trim-ws raw)))
    (cond
      ((or (string= s "true") (string= s "True")) t)
      ((or (string= s "false") (string= s "False")) nil)
      ((and (plusp (length s))
            (every (lambda (c) (or (digit-char-p c) (char= c #\.))) s)
            (<= (count #\. s) 1)
            (some #'digit-char-p s))
       (let ((*read-eval* nil))
         (read-from-string s)))
      (t (%strip-quotes s)))))

(defun %split-kv (line)
  (let ((colon (position #\: line)))
    (when colon
      (cons (%trim-ws (subseq line 0 colon))
            (%trim-ws (subseq line (1+ colon)))))))

(defun parse-models-yaml (content)
  "Parse a models.yaml string into a list of plists without external YAML deps."
  (let ((entries nil)
        (current nil)
        (sub-key nil)
        (nested-key nil))
    (labels ((flush-entry ()
               (when current
                 (push current entries)
                 (setf current nil sub-key nil nested-key nil))))
      (dolist (raw-line (uiop:split-string content :separator '(#\Newline)))
        (let* ((trimmed (%trim-ws raw-line))
               (indent (or (position-if-not (lambda (c) (char= c #\Space)) raw-line)
                           0)))
          (unless (or (string= trimmed "") (char= (char trimmed 0) #\#))
            (cond
              ((and (= indent 0) (>= (length trimmed) 2)
                    (string= (subseq trimmed 0 2) "- "))
               (flush-entry)
               (let ((kv (%split-kv (subseq trimmed 2))))
                 (when kv
                   (setf current (list (intern (string-upcase (substitute #\- #\_ (car kv))) :keyword)
                                       (%parse-yaml-scalar (cdr kv)))))))
              ((and current (= indent 2))
               (setf nested-key nil)
               (let ((kv (%split-kv trimmed)))
                 (when kv
                   (let ((k (intern (string-upcase (substitute #\- #\_ (car kv))) :keyword)))
                     (if (string= (cdr kv) "")
                         (setf sub-key k)
                         (progn
                           (setf sub-key nil)
                           (setf current (list* k (%parse-yaml-scalar (cdr kv))
                                                current))))))))
              ((and current sub-key (= indent 4))
               (let ((kv (%split-kv trimmed)))
                 (when kv
                   (if (string= (cdr kv) "")
                       (setf nested-key (intern (string-upcase (substitute #\- #\_ (car kv))) :keyword))
                       (let* ((k (intern (string-upcase (substitute #\- #\_ (car kv))) :keyword))
                              (existing (getf current sub-key)))
                         (setf (getf current sub-key)
                               (cons (cons k (%parse-yaml-scalar (cdr kv))) existing)))))))
              ((and current sub-key nested-key (>= indent 6))
               (let ((kv (%split-kv trimmed)))
                 (when kv
                   (let ((existing (getf current sub-key)))
                     (setf (getf current sub-key)
                           (cons (cons (car kv) (%parse-yaml-scalar (cdr kv)))
                                 existing))))))))))
      (flush-entry))
    (nreverse entries)))

(defun load-models-yaml (path)
  "Load model definitions from a models.yaml file and register them.
Returns the list of registered model names."
  (let* ((content (uiop:read-file-string path))
         (entries (parse-models-yaml content))
         (names nil))
    (dolist (entry entries)
      (let* ((name (getf entry :name))
             (model-id (getf entry :model-name))
             (ctx (or (getf entry :max-input-tokens) 8192))
             (out (or (getf entry :max-output-tokens) 4096))
             (in-per-tok (or (getf entry :input-cost-per-token) 0.0))
             (out-per-tok (or (getf entry :output-cost-per-token) 0.0))
             (edit-fmt (or (getf entry :edit-format) "diff"))
             (extra (getf entry :extra-params)))
        (when name
          (register-model name
                          :id model-id
                          :context-window ctx
                          :max-output-tokens out
                          :input-cost (* in-per-tok 1000000.0)
                          :output-cost (* out-per-tok 1000000.0)
                          :edit-format edit-fmt
                          :extra-params extra)
          (push name names))))
    (nreverse names)))
