(in-package #:hactar-rag)

;;; In-memory embedding index + retrieval + lexical rerank.

(defstruct (rag-index (:constructor %make-rag-index))
  (entries (make-hash-table :test 'equal))
  model)

(defun make-index (&key model)
  (%make-rag-index :model model))

(defun clear-index (idx)
  (clrhash (rag-index-entries idx))
  idx)

(defun %as-float-vector (vec)
  (map 'vector (lambda (x) (float x 1.0)) vec))

(defun cosine-similarity (a b)
  "Compute cosine similarity between two numeric vectors A and B."
  (let ((len (min (length a) (length b))))
    (when (zerop len) (return-from cosine-similarity 0.0))
    (let ((dot 0.0) (na 0.0) (nb 0.0))
      (dotimes (i len)
        (let ((va (float (aref a i) 1.0))
              (vb (float (aref b i) 1.0)))
          (incf dot (* va vb))
          (incf na (* va va))
          (incf nb (* vb vb))))
      (if (or (zerop na) (zerop nb))
          0.0
          (/ dot (* (sqrt na) (sqrt nb)))))))

(defun index (idx id text &key metadata embedding)
  "Index TEXT under ID in IDX using HACTAR-LLM:EMBED (or supplied EMBEDDING)."
  (let* ((vec (%as-float-vector
               (or embedding
                   (hactar-llm:embed text :model (rag-index-model idx)))))
         (entry (list :id id :text text :metadata metadata :embedding vec)))
    (setf (gethash id (rag-index-entries idx)) entry)
    entry))

(defun %tokenize (text)
  (remove "" (uiop:split-string (string-downcase (or text ""))
                                :separator '(#\Space #\Tab #\Newline #\, #\. #\: #\;))
          :test #'string=))

(defun %lexical-score (query text)
  (let ((words (%tokenize query))
        (hay (string-downcase (or text ""))))
    (if (null words)
        0.0
        (float (loop for w in words
                     count (search w hay))
               1.0))))

(defun retrieve (idx query &key (limit 10) embedding)
  "Retrieve top LIMIT entries from IDX by cosine similarity to QUERY."
  (let* ((qvec (%as-float-vector
                (or embedding
                    (hactar-llm:embed query :model (rag-index-model idx)))))
         (scored nil))
    (maphash (lambda (id entry)
               (declare (ignore id))
               (let ((sim (cosine-similarity qvec (getf entry :embedding)))
                     (lex (%lexical-score query (getf entry :text))))
                 (push (cons (+ sim (* 0.1 lex)) entry) scored)))
             (rag-index-entries idx))
    (let ((sorted (mapcar #'cdr (sort scored #'> :key #'car))))
      (if (and limit (> (length sorted) limit))
          (subseq sorted 0 limit)
          sorted))))

(defun rerank (query candidates &key (key :text) limit)
  "Rerank CANDIDATES (plists) by lexical overlap with QUERY."
  (let* ((scored (loop for c in candidates
                       for txt = (if (stringp c) c (getf c key))
                       collect (cons (%lexical-score query txt) c)))
         (sorted (mapcar #'cdr (sort scored #'> :key #'car))))
    (if (and limit (> (length sorted) limit))
        (subseq sorted 0 limit)
        sorted)))
