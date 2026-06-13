(in-package :hactar)
;;* RAG (done in-memory/in-lisp)

;;** helpers
(defun %kr-ensure-list (v)
  "Coerce V into a list (treating vectors and scalars appropriately)."
  (cond ((null v) '())
        ((listp v) v)
        ((vectorp v) (coerce v 'list))
        (t (list v))))

(defun %kr-field (record key)
  (cdr (assoc key record)))

(defun %kr-string (x)
  (cond ((null x) "")
        ((stringp x) x)
        (t (princ-to-string x))))

(defun %kr-match-list-field (record key wanted match-any)
  "Check RECORD's list field KEY against WANTED (case-insensitive).
   When MATCH-ANY, succeed if any wanted item is present; else require all."
  (let ((have (mapcar (lambda (x) (string-downcase (%kr-string x)))
                      (%kr-ensure-list (%kr-field record key))))
        (want (mapcar (lambda (x) (string-downcase (%kr-string x)))
                      (%kr-ensure-list wanted))))
    (if match-any
        (some (lambda (w) (member w have :test #'string=)) want)
        (every (lambda (w) (member w have :test #'string=)) want))))

(defun %kr-rank-by-text (records text content-keys)
  "Rank RECORDS by naive token-overlap of TEXT against CONTENT-KEYS fields.
   Returns only records with a non-zero score, highest first."
  (let* ((q (str:words (string-downcase text)))
         (scored (loop for r in records
                       for hay = (string-downcase
                                  (format nil "~{~A ~}"
                                          (mapcar (lambda (k) (%kr-string (%kr-field r k)))
                                                  content-keys)))
                       for score = (loop for w in q
                                         when (search w hay) sum 1)
                       when (> score 0) collect (cons score r))))
    (mapcar #'cdr (sort scored #'> :key #'car))))

(defun %kr-paginate (records limit offset)
  "Apply OFFSET then LIMIT to RECORDS."
  (let* ((len (length records))
         (start (min (or offset 0) len))
         (rest (subseq records start)))
    (if (and limit (> (length rest) limit))
        (subseq rest 0 limit)
        rest)))

;;** docs
(defun docs-find (&key tags covers slug sources text type id (limit 10) (offset 0) (match-any nil))
  "Find documents in the pure Lisp hypertext dispatcher registries."
  (let ((results '()))
    (maphash (lambda (proto dispatcher)
               (unless (member proto '("errors" "error") :test #'string=)
                 (maphash (lambda (path doc-plist)
                            (declare (ignore path))
                            (let ((doc (hypertext-to-alist doc-plist)))
                              (when (and (or (null id) (equal (%kr-field doc :id) id))
                                         (or (null slug) (equal (%kr-field doc :slug) slug))
                                         (or (null type) (equal (%kr-field doc :type) type))
                                         (or (null sources)
                                             (member (%kr-field doc :source) (%kr-ensure-list sources) :test #'equal))
                                         (or (null tags) (%kr-match-list-field doc :tags tags match-any))
                                         (or (null covers) (%kr-match-list-field doc :covers covers match-any)))
                                (push doc results))))
                          (protocol-dispatcher-registry dispatcher))))
             *protocol-dispatchers*)
    (when text
      (setf results (%kr-rank-by-text results text '(:title :content))))
    (%kr-paginate results limit offset)))

(defun docs-create (&key source title content (tags #()) (covers #()) (links_to #()) slug type meta)
  "Create a document entry in the pure Lisp hypertext dispatcher registries."
  (unless (and source title content)
    (error "source, title, and content are required fields for docs-create."))
  (let* ((id (format nil "doc-~A" (uuid:make-v4-uuid)))
         (uri (if (position #\: source)
                  source
                  (format nil "docs:~A" (slugify title))))
         (final-tags (%kr-ensure-list tags))
         (final-covers (%kr-ensure-list covers))
         (final-meta (append meta
                             (list (cons :id id)
                                   (cons :source source)
                                   (cons :slug slug)
                                   (cons :type type)
                                   (cons :links-to (%kr-ensure-list links_to))))))
    (register-hypertext uri
                        (list :title title
                              :content content
                              :tags final-tags
                              :covers final-covers
                              :metadata final-meta))
    (list id)))

;;** starters
(defun starters-find (&rest args &key tags &allow-other-keys)
  "Find starter documents. This is a wrapper around docs-find that adds the 'starter' tag."
  (remf args :tags)
  (apply #'docs-find :tags (cons "starter" tags) args))

;;** errors
(defun errors-find (&key text code tags (match-any nil) (limit 10) (offset 0))
  "Find error entries in the pure Lisp errors dispatcher registry."
  (let ((results '()))
    (dolist (proto '("errors" "error"))
      (let ((dispatcher (gethash proto *protocol-dispatchers*)))
        (when dispatcher
          (maphash (lambda (path err-plist)
                     (declare (ignore path))
                     (let ((err (hypertext-to-error-alist err-plist)))
                       (when (and (or (null code) (equal (%kr-field err :code) code))
                                  (or (null tags) (%kr-match-list-field err :tags tags match-any)))
                         (push err results))))
                   (protocol-dispatcher-registry dispatcher)))))
    (when text
      (setf results (%kr-rank-by-text results text '(:title :message :cause :solution))))
    (%kr-paginate results limit offset)))

(defun errors-create (&key code stack slug title message cause solution (tags #()))
  "Create an error entry in the pure Lisp errors dispatcher registry."
  (unless (and code stack title message cause solution)
    (error "code, stack, title, message, cause, and solution are required fields for errors-create."))
  (let* ((id (format nil "err-~A" (uuid:make-v4-uuid)))
         (final-slug (or slug (format nil "~A:~A" stack code)))
         (uri (format nil "errors:~A" final-slug))
         (content (format nil "Message: ~A~%~%Cause: ~A~%~%Solution: ~A" message cause solution))
         (final-meta (list (cons :id id)
                           (cons :code code)
                           (cons :stack stack)
                           (cons :slug final-slug)
                           (cons :message message)
                           (cons :cause cause)
                           (cons :solution solution))))
    (register-hypertext uri
                        (list :title title
                              :content content
                              :tags (%kr-ensure-list tags)
                              :covers (list stack)
                              :metadata final-meta))
    id))

;;** canonical find aliases
(defun doc-find (&rest args)
  "Canonical alias for docs-find."
  (apply #'docs-find args))

(defun error-find (&rest args)
  "Canonical alias for errors-find."
  (apply #'errors-find args))
