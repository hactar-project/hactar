(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(result
            make-result
            result-p
            result-status
            result-message
            result-data
            result-artifacts
            result-renderers
            render-result)))

(defstruct result
  "The standard protocol-independent result type returned by all routes."
  (status :ok :type keyword)             ; :ok, :error, :needs-confirmation
  (message "" :type string)              ; short human-readable message
  (data nil)                             ; structured data payload
  (artifacts nil :type list)             ; list of files/patches/links/commands
  (renderers nil :type list))            ; plist of custom renderers, e.g. (:json #f ...)

(defgeneric render-result (result format &key &allow-other-keys)
  (:documentation "Render a RESULT struct into the requested FORMAT (e.g. :text, :json, :org, :markdown)."))

(defmethod render-result ((res result) (format (eql :text)) &key &allow-other-keys)
  "Render result as plain text (suitable for terminal output)."
  (let ((custom-renderer (getf (result-renderers res) :text)))
    (if custom-renderer
        (funcall custom-renderer res)
        (with-output-to-string (s)
          (format s "~A~%" (result-message res))
          (when (result-artifacts res)
            (format s "~%Artifacts:~%")
            (dolist (art (result-artifacts res))
              (format s "  - ~A~%" art)))))))

(defmethod render-result ((res result) (format (eql :json)) &key &allow-other-keys)
  "Render result as a JSON string."
  (let ((custom-renderer (getf (result-renderers res) :json)))
    (if custom-renderer
        (funcall custom-renderer res)
        (let ((shasht:*write-alist-as-object* t)
              (shasht:*symbol-name-function* #'string-downcase))
          (shasht:write-json
           `((:status . ,(string-downcase (symbol-name (result-status res))))
             (:message . ,(result-message res))
             (:data . ,(result-data res))
             (:artifacts . ,(result-artifacts res)))
           nil)))))

(defmethod render-result ((res result) (format (eql :org)) &key &allow-other-keys)
  "Render result as an Org-mode formatted document."
  (let ((custom-renderer (getf (result-renderers res) :org)))
    (if custom-renderer
        (funcall custom-renderer res)
        (with-output-to-string (s)
          (format s "* Result: ~A~%" (string-upcase (symbol-name (result-status res))))
          (format s "~A~%" (result-message res))
          (when (result-artifacts res)
            (format s "~%** Artifacts~%")
            (dolist (art (result-artifacts res))
              (format s "- ~A~%" art)))
          (when (result-data res)
            (format s "~%** Structured Data~%")
            (format s "#+begin_src lisp~%~S~%#+end_src~%" (result-data res)))))))

(defmethod render-result ((res result) (format (eql :markdown)) &key &allow-other-keys)
  "Render result as a Markdown document."
  (let ((custom-renderer (getf (result-renderers res) :markdown)))
    (if custom-renderer
        (funcall custom-renderer res)
        (with-output-to-string (s)
          (format s "# Result: ~A~%~%" (string-upcase (symbol-name (result-status res))))
          (format s "~A~%~%" (result-message res))
          (when (result-artifacts res)
            (format s "## Artifacts~%~%")
            (dolist (art (result-artifacts res))
              (format s "- ~A~%" art))
            (format s "~%"))
          (when (result-data res)
            (format s "## Structured Data~%~%")
            (format s "```lisp~%~S~%```~%" (result-data res)))))))
