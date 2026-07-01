(in-package #:hactar-fzf)

;;; Pure, dependency-free fuzzy subsequence ranking + a non-interactive
;;; selection API usable by agents and tests. No terminal I/O here.

(defun fuzzy-match-p (query candidate)
  "True if QUERY's characters appear in order (case-insensitive) in CANDIDATE."
  (let ((q (string-downcase query))
        (c (string-downcase candidate))
        (i 0))
    (when (zerop (length q)) (return-from fuzzy-match-p t))
    (loop for ch across c
          when (and (< i (length q)) (char= ch (char q i)))
            do (incf i)
          when (= i (length q)) do (return t)
          finally (return (= i (length q))))))

(defun fuzzy-score (query candidate)
  "Return a match score (higher is better) or NIL if QUERY is not a subsequence.
Rewards contiguous runs, early matches, and matches at word boundaries."
  (let* ((q (string-downcase query))
         (c (string-downcase candidate))
         (qi 0) (score 0) (last-match -2) (prev-boundary t))
    (when (zerop (length q)) (return-from fuzzy-score (- 100 (length candidate))))
    (loop for ci from 0 below (length c)
          for ch = (char c ci)
          while (< qi (length q))
          do (let ((boundary (or (zerop ci)
                                 (member (char c (1- ci)) '(#\Space #\- #\_ #\/ #\.)))))
               (when (char= ch (char q qi))
                 (incf score 10)
                 (when (= ci (1+ last-match)) (incf score 15)) ; contiguous
                 (when boundary (incf score 10))               ; word boundary
                 (when (and (zerop qi) prev-boundary) (incf score 5))
                 (setf last-match ci)
                 (incf qi))))
    (when (= qi (length q))
      (- score (length candidate)))))

(defun fuzzy-rank (query candidates &key (key #'identity))
  "Return CANDIDATES that match QUERY, sorted best-first, as a list of
(candidate . score)."
  (let ((scored '()))
    (dolist (cand candidates)
      (let ((s (fuzzy-score query (funcall key cand))))
        (when s (push (cons cand s) scored))))
    (stable-sort scored #'> :key #'cdr)))

(defun fuzzy-filter (query candidates &key (key #'identity))
  "Return the matching CANDIDATES sorted best-first (without scores)."
  (mapcar #'car (fuzzy-rank query candidates :key key)))

(defun select-best (query candidates &key (key #'identity))
  "Return the single best match for QUERY, or NIL."
  (car (first (fuzzy-rank query candidates :key key))))

(defun select (query candidates &key (key #'identity) (limit 10))
  "Return up to LIMIT best matches for QUERY (non-interactive selection)."
  (let ((ranked (fuzzy-filter query candidates :key key)))
    (subseq ranked 0 (min limit (length ranked)))))
