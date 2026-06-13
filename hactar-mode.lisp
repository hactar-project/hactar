;; hactar hactar-mode. we put hactar in your hactar!
;; mostly just a place to define the docs
(in-package :hactar)

(defdoc "Latest Hactar Docs" :source "hactar:docsets/hactar.0.1.org" :version "latest")

(define-command "hactar.dev.copyprompts" (args)
		"Copy prompts from the repository prompts/ directory to the user's config prompts/ directory."
		(declare (ignore args))
		(let* ((repo-dir (%to-pathname *hactar-repo-dir*))
		       (prompts-src (uiop:subpathname (uiop:ensure-directory-pathname repo-dir) "prompts/"))
		       (config-dir (%to-pathname *hactar-config-path*))
		       (prompts-dst (uiop:subpathname (uiop:ensure-directory-pathname config-dir) "prompts/")))
		  (ensure-directories-exist prompts-dst)
		  (if (uiop:directory-exists-p prompts-src)
		      (handler-case
			  (progn
			    (dolist (file (uiop:directory-files prompts-src))
			      (let ((dest-file (make-pathname :name (pathname-name file)
							      :type (pathname-type file)
							      :defaults prompts-dst)))
				(uiop:copy-file file dest-file)))
			    (format t "Successfully copied all prompts to ~A~%" (uiop:native-namestring prompts-dst)))
			(error (e)
			       (format t "Error copying prompts: ~A~%" e)))
		    (format t "Error: Source prompts directory does not exist at ~A~%" (uiop:native-namestring prompts-src)))))

(defun resolve-repo-root ()
  "Resolve the repository root directory, falling back to the current working directory or git root."
  (or *repo-root*
      (ignore-errors (find-git-repo-root (uiop:getcwd)))
      (uiop:ensure-directory-pathname (uiop:getcwd))))

(defun get-repo-lisp-files (root)
  "Get all tracked Lisp files in the repository."
  (let ((files '()))
    (dolist (f (or (ignore-errors (list-git-tracked-files root))
                   (ignore-errors (list-files-with-fd root))))
      (when (string-equal (pathname-type (pathname f)) "lisp")
        (push (merge-pathnames f root) files)))
    (nreverse files)))

(defun extract-functions-from-file (file-path)
  "Read Lisp forms from FILE-PATH and extract functions and macros with their docstrings and signatures.
   Returns a list of alists: ((:name . name) (:signature . sig-str) (:docstring . doc-str))"
  (let* ((content (handler-case (uiop:read-file-string file-path :external-format :utf-8)
                    (error () nil)))
         (forms (when content
                  (handler-case (read-all-forms content)
                    (error () nil))))
         (functions '()))
    (dolist (form forms)
      (when (consp form)
        (let ((op (first form)))
          (when (and (symbolp op)
                     (member (symbol-name op) '("DEFUN" "DEFMACRO") :test #'string=))
            (let* ((name (second form))
                   (sig (third form))
                   (doc (when (stringp (fourth form)) (fourth form)))
                   (name-str (let ((*print-case* :downcase))
                               (if (symbolp name)
                                   (string-downcase (symbol-name name))
                                   (format nil "~A" name))))
                   (sig-str (let ((*print-case* :downcase))
                              (format nil "~A" sig))))
              (push (list (cons :name name-str)
                          (cons :signature sig-str)
                          (cons :docstring (or doc "")))
                    functions))))))
    (nreverse functions)))

(defun gather-apidocs-spec ()
  "Gather all function documentation from Lisp files in the repository.
   Returns a list of alists: ((:file . relative-path) (:functions . function-alists))"
  (let* ((root (resolve-repo-root))
         (lisp-files (get-repo-lisp-files root))
         (spec '()))
    (dolist (file lisp-files)
      (let* ((rel-path (uiop:native-namestring (uiop:enough-pathname file root)))
             (funcs (extract-functions-from-file file)))
        (when funcs
          (push (list (cons :file rel-path)
                      (cons :functions funcs))
                spec))))
    (sort spec #'string< :key (lambda (entry) (cdr (assoc :file entry))))))

(defun serialize-apidocs-to-yaml (spec)
  "Serialize gathered apidocs spec to a YAML string."
  (with-output-to-string (s)
    (dolist (file-entry spec)
      (format s "- file: ~A~%" (cdr (assoc :file file-entry)))
      (format s "  functions:~%")
      (dolist (func (cdr (assoc :functions file-entry)))
        (format s "    - name: ~A~%" (cdr (assoc :name func)))
        (format s "      signature: ~S~%" (cdr (assoc :signature func)))
        (let ((doc (cdr (assoc :docstring func))))
          (if (and doc (find #\Newline doc))
              (progn
                (format s "      docstring: |~%")
                (dolist (line (str:lines doc))
                  (format s "        ~A~%" line)))
              (format s "      docstring: ~S~%" (or doc ""))))))))

(defun serialize-apidocs-to-org (spec)
  "Serialize gathered apidocs spec to an Org-mode string."
  (with-output-to-string (s)
    (dolist (file-entry spec)
      (format s "* ~A~%" (cdr (assoc :file file-entry)))
      (dolist (func (cdr (assoc :functions file-entry)))
        (format s "** ~A~%" (cdr (assoc :name func)))
        (format s "- Signature: =~A=~%" (cdr (assoc :signature func)))
        (let ((doc (cdr (assoc :docstring func))))
          (if (and doc (not (string= doc "")))
              (format s "- Docstring:~%~A~%" doc)
              (format s "- Docstring: None~%")))))))

(defun apidocs-gen-spec ()
  "Generate and return the YAML specification of API docs."
  (serialize-apidocs-to-yaml (gather-apidocs-spec)))

(defun apidocs-gen-org ()
  "Generate and return the Org-mode formatted API docs."
  (serialize-apidocs-to-org (gather-apidocs-spec)))

(defun apidocs-update (&key (model-name *default-llm*))
  "Generate apidocs YAML spec and update docs/API.org using the LLM."
  (let* ((root (resolve-repo-root))
         (api-path (merge-pathnames "docs/API.org" root))
         (existing-content (if (probe-file api-path)
                               (uiop:read-file-string api-path :external-format :utf-8)
                               ""))
         (yaml-spec (apidocs-gen-spec))
         (system-prompt (or (get-prompt 'system.apidocs.update)
                            "You are a technical documentation writer."))
         (prompt (format nil "Existing API Reference:~%~A~%~%Latest YAML Spec:~%~A~%~%Please update the API Reference to reflect the latest YAML Spec. Output the entire updated Org-mode document."
                         (if (string= existing-content "") "Empty / Does not exist" existing-content)
                         yaml-spec))
         (response (ask prompt :system-prompt system-prompt :model-name model-name)))
    (if response
        (let ((cleaned-response (extract-response-content response :org-mode)))
          (ensure-directories-exist api-path)
          (with-open-file (out api-path :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
            (write-string cleaned-response out))
          (format t "Successfully updated ~A~%" (uiop:native-namestring api-path))
          t)
        (progn
          (format t "Error: Failed to get response from LLM.~%")
          nil))))

(define-command "hactar.apidocs.gen.spec" (args)
		"Generate a YAML specification of all repository Lisp functions, their signatures, and docstrings."
		(declare (ignore args))
		(format t "~A" (apidocs-gen-spec))
		:acp t :sub t)

(define-command "hactar.apidocs.gen.org" (args)
  "Generate Org-mode formatted API documentation of all repository Lisp functions."
  (declare (ignore args))
  (format t "~A" (apidocs-gen-org))
  :acp t)

(define-command "hactar.apidocs.update" (args)
  "Update the API documentation in docs/API.org using the LLM."
  (declare (ignore args))
  (apidocs-update)
  :acp t)
