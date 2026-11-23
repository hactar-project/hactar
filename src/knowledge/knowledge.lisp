(in-package #:hactar-knowledge)

;;; Hypertext knowledge store: docs, guides, errors, import, and search.

(defvar *docs-store* (make-hash-table :test 'equal))
(defvar *guides-store* (make-hash-table :test 'equal))
(defvar *errors-store* (make-hash-table :test 'equal))
(defvar *knowledge-index* (hactar-rag:make-index))

(defvar *fetch-fn* nil
  "Pluggable HTTP/source fetch fn: (url) -> string. Keep NIL in offline tests.")

(defun clear-knowledge-stores ()
  (clrhash *docs-store*)
  (clrhash *guides-store*)
  (clrhash *errors-store*)
  (hactar-rag:clear-index *knowledge-index*)
  t)

(defun %key (x) (string-downcase (string x)))

(defun %ensure-list (v)
  (cond ((null v) nil)
        ((listp v) v)
        ((vectorp v) (coerce v 'list))
        (t (list v))))

;;; ---- Docs & Guides --------------------------------------------------------

(defun register-doc (name &key title uri text content source tags covers metadata)
  "Register a documentation entry and index it for retrieval."
  (let* ((k (%key name))
         (body (or text content ""))
         (entry (list :name k
                      :title (or title (string name))
                      :uri uri
                      :text body
                      :source source
                      :tags (%ensure-list tags)
                      :covers (%ensure-list covers)
                      :metadata metadata)))
    (setf (gethash k *docs-store*) entry)
    (hactar-rag:index *knowledge-index* (format nil "doc:~A" k)
                      (format nil "~A~%~A" (getf entry :title) body)
                      :metadata entry)
    entry))

(defmacro defdoc (name &key title uri text content source tags covers metadata)
  `(register-doc ',name :title ,title :uri ,uri :text ,text :content ,content
                        :source ,source :tags ',tags :covers ',covers
                        :metadata ',metadata))

(defun lookup-doc (name)
  "Look up a doc entry by NAME (string/symbol) or URI."
  (gethash (%key name) *docs-store*))

(defun register-guide (name &key title content source tags covers sections)
  (let* ((k (%key name))
         (entry (list :name k :title (or title (string name))
                      :content (or content "") :source source
                      :tags (%ensure-list tags) :covers (%ensure-list covers)
                      :sections sections)))
    (setf (gethash k *guides-store*) entry)
    entry))

(defmacro defguide (name &key title content source tags covers sections)
  `(register-guide ',name :title ,title :content ,content :source ,source
                          :tags ',tags :covers ',covers :sections ',sections))

(defun lookup-guide (name)
  (gethash (%key name) *guides-store*))

(defun %matches-tags-p (have wanted match-any)
  (let ((h (mapcar #'%key (%ensure-list have)))
        (w (mapcar #'%key (%ensure-list wanted))))
    (if match-any
        (some (lambda (x) (member x h :test #'string=)) w)
        (every (lambda (x) (member x h :test #'string=)) w))))

(defun docs-find (&key tags covers text (limit 10) (match-any nil))
  "Find doc entries matching TAGS/COVERS and optionally ranked by TEXT."
  (let ((matches nil))
    (maphash (lambda (k entry)
               (declare (ignore k))
               (when (and (or (null tags) (%matches-tags-p (getf entry :tags) tags match-any))
                          (or (null covers) (%matches-tags-p (getf entry :covers) covers match-any)))
                 (push entry matches)))
             *docs-store*)
    (when (and text (plusp (length text)))
      (setf matches (hactar-rag:rerank text matches :key :text)))
    (if (and limit (> (length matches) limit))
        (subseq matches 0 limit)
        matches)))

;;; ---- Errors ---------------------------------------------------------------

(defun register-error (slug &key code stack title message cause solution tags)
  (let* ((k (%key slug))
         (text (format nil "~@[Code: ~A~%~]~@[Message: ~A~%~]~@[Cause: ~A~%~]~@[Solution: ~A~]"
                       code message cause solution))
         (entry (list :slug k :code code :stack stack
                      :title (or title (string slug))
                      :message message :cause cause :solution solution
                      :text text :tags (%ensure-list tags))))
    (setf (gethash k *errors-store*) entry)
    (hactar-rag:index *knowledge-index* (format nil "error:~A" k)
                      (format nil "~A~%~A" (getf entry :title) text)
                      :metadata entry)
    entry))

(defmacro deferror (slug &key code stack title message cause solution tags)
  `(register-error ',slug :code ,code :stack ,stack :title ,title
                          :message ,message :cause ,cause :solution ,solution
                          :tags ',tags))

(defun lookup-error (slug-or-code)
  (or (gethash (%key slug-or-code) *errors-store*)
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (when (equal (getf v :code) slug-or-code)
                   (return-from lookup-error v)))
               *errors-store*)))

(defun errors-find (&key code stack text tags (limit 10))
  (let ((matches nil))
    (maphash (lambda (k entry)
               (declare (ignore k))
               (when (and (or (null code) (equal (getf entry :code) code))
                          (or (null stack) (string-equal (or (getf entry :stack) "") (string stack)))
                          (or (null tags) (%matches-tags-p (getf entry :tags) tags nil)))
                 (push entry matches)))
             *errors-store*)
    (when (and text (plusp (length text)))
      (setf matches (hactar-rag:rerank text matches :key :text)))
    (if (and limit (> (length matches) limit))
        (subseq matches 0 limit)
        matches)))

(defun parse-errors-from-org (source)
  "Parse an Org document string into error plists and register them."
  (let ((doc (hactar-org:parse-org-document source))
        (created nil))
    (hactar-org:walk-org
     doc
     (lambda (node)
       (when (and (eq :headline (hactar-org:org-node-type node))
                  (= 1 (hactar-org:org-node-level node)))
         (let* ((title (hactar-org:org-node-title node))
                (tags (hactar-org:org-node-tags node))
                (props-node (find :properties (hactar-org:org-node-children node)
                                  :key #'hactar-org:org-node-type))
                (props (and props-node (hactar-org:org-node-properties props-node)))
                (code (cdr (assoc :code props)))
                (stack (cdr (assoc :stack props)))
                (paragraphs (loop for c in (hactar-org:org-node-children node)
                                  when (eq :paragraph (hactar-org:org-node-type c))
                                    collect (hactar-org:org-node-body c)))
                (msg (format nil "~{~A~^~%~}" paragraphs)))
           (push (register-error title :code code :stack stack :title title
                                       :message msg :tags tags)
                 created)))))
    (nreverse created)))

;;; ---- Hypertext URIs & Import ----------------------------------------------

(defun parse-hypertext-uri (uri)
  "Split \"protocol:path\" into (values protocol path)."
  (let ((colon (position #\: uri)))
    (if colon
        (values (string-downcase (subseq uri 0 colon))
                (subseq uri (1+ colon)))
        (values "docs" uri))))

(defun resolve-hypertext-uri (uri)
  "Resolve docs:/errors:/guides: URIs against the knowledge stores."
  (multiple-value-bind (proto path) (parse-hypertext-uri uri)
    (cond
      ((member proto '("docs" "doc") :test #'string=)
       (let ((entry (lookup-doc path)))
         (when entry (values (getf entry :text) "text/plain" entry))))
      ((member proto '("errors" "error") :test #'string=)
       (let ((entry (lookup-error path)))
         (when entry (values (getf entry :text) "text/plain" entry))))
      ((member proto '("guides" "guide") :test #'string=)
       (let ((entry (lookup-guide path)))
         (when entry (values (getf entry :content) "text/plain" entry))))
      (t nil))))

(defun fetch-url (url)
  "Fetch URL through *FETCH-FN*, or read local file paths."
  (cond
    ((probe-file url) (uiop:read-file-string url))
    (*fetch-fn* (funcall *fetch-fn* url))
    (t (error "No *FETCH-FN* configured for URL: ~A" url))))

(defun import-docs (source &key name title tags covers (format :auto))
  "Import documentation from URL/path/string SOURCE using HACTAR-MD / HACTAR-ORG."
  (let* ((raw (if (or (uiop:string-prefix-p "http://" source)
                      (uiop:string-prefix-p "https://" source)
                      (probe-file source))
                  (fetch-url source)
                  source))
         (fmt (if (eq format :auto)
                  (cond ((or (search "#+" raw) (cl-ppcre:scan "^\\*+ " raw)) :org)
                        (t :markdown))
                  format))
         (doc-name (or name source))
         (links (when (eq fmt :markdown) (hactar-md:extract-links raw)))
         (rendered (ecase fmt
                     (:markdown (hactar-md:render-terminal raw))
                     (:org (hactar-org:render-org (hactar-org:parse-org-document raw)))
                     (:text raw))))
    (register-doc doc-name
                  :title (or title doc-name)
                  :uri source
                  :text rendered
                  :source source
                  :tags tags
                  :covers covers
                  :metadata (list :format fmt :links links))))

(defun knowledge-search (query &key (limit 10))
  "Search registered docs and errors using HACTAR-RAG (semantic + lexical)."
  (mapcar (lambda (hit) (or (getf hit :metadata) hit))
          (hactar-rag:retrieve *knowledge-index* query :limit limit)))
