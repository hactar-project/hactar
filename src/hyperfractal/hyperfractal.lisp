(in-package #:hyperfractal)

;;; Plaintext web browser wrapper over hactar-core, hactar-llm, hactar-md, hactar-org.

(defvar *default-model* nil)
(defvar *default-format* :markdown)
(defvar *fetch-fn* nil
  "Optional URL fetcher: (url) -> html-string. Defaults to curl subprocess.")
(defvar *cache* (make-hash-table :test 'equal))
(defvar *history* nil)

(defun clear-cache ()
  (clrhash *cache*)
  (setf *history* nil)
  t)

(defun cache-get (url &optional (format *default-format*))
  (gethash (cons url format) *cache*))

(defun cache-put (url content &optional (format *default-format*))
  (setf (gethash (cons url format) *cache*) content))

(defun fetch-url (url)
  (cond
    (*fetch-fn* (funcall *fetch-fn* url))
    ((probe-file url) (uiop:read-file-string url))
    (t (multiple-value-bind (out err code)
           (uiop:run-program (list "curl" "-fsSL" url)
                             :output :string :error-output :string
                             :ignore-error-status t)
         (unless (zerop code)
           (error "Failed to fetch ~A: ~A" url err))
         out))))

(defun resolve-url (link-url base-url)
  (cond
    ((cl-ppcre:scan "^https?://" link-url) link-url)
    ((str:starts-with-p "/" link-url)
     (multiple-value-bind (m g) (cl-ppcre:scan-to-strings "^(https?://[^/]+)" base-url)
       (declare (ignore m))
       (if g (concatenate 'string (aref g 0) link-url) link-url)))
    (t (concatenate 'string (cl-ppcre:regex-replace "/[^/]*$" base-url "/") link-url))))

(defun extract-links (content &key base-url)
  "Extract (text . resolved-url) links from Markdown or Org content."
  (let ((md-links (hactar-md:extract-links content)))
    (if base-url
        (mapcar (lambda (pair)
                  (cons (car pair) (resolve-url (cdr pair) base-url)))
                md-links)
        md-links)))

(defun html->plaintext (html &key (format *default-format*) model)
  "Convert HTML to Markdown or Org via HACTAR-LLM."
  (let* ((fmt-name (string-downcase (string format)))
         (prompt (format nil "Convert the following HTML into clean ~A:~%~%~A"
                         fmt-name html)))
    (hactar-llm:ask prompt :model (or model *default-model* hactar-core:*current-model*))))

(defun browse (url &key (format *default-format*) model use-cache)
  "Fetch URL, convert to FORMAT (:markdown/:org), record history, and return RESULT."
  (let ((cached (and use-cache (cache-get url format))))
    (when cached
      (return-from browse (hactar-core:ok cached :data (list :url url :cached t)))))
  (let* ((html (fetch-url url))
         (text (html->plaintext html :format format :model model)))
    (cache-put url text format)
    (push url *history*)
    (hactar-core:ok text :data (list :url url :format format
                                     :links (extract-links text :base-url url)))))

(defun navigate-history ()
  (copy-list *history*))
