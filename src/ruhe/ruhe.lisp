(in-package #:ruhe)

;;; Ruhe: plaintext feed compiler over hactar-core, hactar-llm, hactar-org, hactar-md.

(defvar *formats* (make-hash-table :test 'equal))
(defvar *schemas* (make-hash-table :test 'equal))
(defvar *sources* (make-hash-table :test 'equal))
(defvar *fetch-fn* nil
  "Optional URL fetcher: (url) -> string.")

(defstruct format-def name extensions renderer)
(defstruct schema-def name fields prompt)
(defstruct source-def name fetcher)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %key (x) (string-downcase (string x))))

(defmacro defformat (name &key extensions renderer)
  `(setf (gethash ,(%key name) *formats*)
         (make-format-def :name ',name :extensions ',extensions :renderer ,renderer)))

(defun find-format (name) (gethash (%key name) *formats*))

(defmacro defschema (name &key fields prompt)
  `(setf (gethash ,(%key name) *schemas*)
         (make-schema-def :name ',name :fields ',fields :prompt ,prompt)))

(defun find-schema (name) (gethash (%key name) *schemas*))

(defmacro defsource (name (&rest args) &body body)
  `(setf (gethash ,(%key name) *sources*)
         (make-source-def :name ',name :fetcher (lambda ,args ,@body))))

(defun find-source (name) (gethash (%key name) *sources*))

(defun %fetch-url (url)
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

(defun parse-rss-items (xml-string)
  "Extract basic (:title :link) items from RSS/Atom XML."
  (let ((items nil))
    (cl-ppcre:do-register-groups (block)
        ("(?s)<(?:item|entry)>(.*?)</(?:item|entry)>" xml-string)
      (multiple-value-bind (_ title-match)
          (cl-ppcre:scan-to-strings "(?s)<title[^>]*>(.*?)</title>" block)
        (declare (ignore _))
        (multiple-value-bind (__ link-match)
            (cl-ppcre:scan-to-strings "(?s)<link[^>]*>(.*?)</link>" block)
          (declare (ignore __))
          (push (list :title (if title-match (str:trim (aref title-match 0)) "Untitled")
                      :link (if link-match (str:trim (aref link-match 0)) ""))
                items))))
    (nreverse items)))

(defsource rss (url)
  (parse-rss-items (%fetch-url url)))

(defun fetch-source (source-name &rest args)
  (let ((src (find-source source-name)))
    (unless src (error "Unknown Ruhe source: ~A" source-name))
    (apply (source-def-fetcher src) args)))

(defun format-items (items &key (format :org))
  (with-output-to-string (s)
    (ecase format
      (:org
       (dolist (item items)
         (format s "* [[~A][~A]]~%" (or (getf item :link) "") (or (getf item :title) ""))))
      (:markdown
       (dolist (item items)
         (format s "- [~A](~A)~%" (or (getf item :title) "") (or (getf item :link) "")))))))

(defun ruhe-compile (spec-org-text &key (format :org) model)
  "Parse an Org feed spec via HACTAR-ORG and synthesize feed content via HACTAR-LLM."
  (let* ((doc (hactar-org:parse-org-document spec-org-text))
         (urls (loop for link in (hactar-md:extract-links spec-org-text)
                     collect (cdr link)))
         (items (loop for url in urls append (fetch-source 'rss url)))
         (rendered (format-items items :format format))
         (summary (hactar-llm:ask
                   (format nil "Summarize feed for spec (~D nodes):~%~A"
                           (length (hactar-org:org-node-children doc))
                           rendered)
                   :model (or model hactar-core:*current-model*))))
    (hactar-core:ok summary :data (list :items items :rendered rendered))))

(defun ruhe-process (path &key (format :org) model)
  (ruhe-compile (uiop:read-file-string path) :format format :model model))
