;; conversion commands
(in-package :hactar)

(defun redwood-collect-source-files (project-path)
  "Collect all relevant RedwoodSDK source files from PROJECT-PATH.
   Returns a list of (relative-path . content) pairs."
  (let ((extensions '("ts" "tsx" "css" "json" "toml" "mts"))
        (skip-dirs '("node_modules" ".git" "dist" ".wrangler" ".tmp"))
        (results '()))
    (labels ((skip-dir-p (dir-name)
               (member dir-name skip-dirs :test #'string=))
             (walk (dir)
               (dolist (entry (uiop:directory* (merge-pathnames uiop:*wild-file-for-directory* dir)))
                 (let ((name (uiop:native-namestring (uiop:enough-pathname entry project-path))))
                   (when (uiop:directory-pathname-p entry)
                     (unless (skip-dir-p (car (last (pathname-directory entry))))
                       (walk entry)))
                   (unless (uiop:directory-pathname-p entry)
                     (let ((ext (pathname-type entry)))
                       (when (and ext (member (string-downcase ext) extensions :test #'string=))
                         (handler-case
                             (push (cons name (uiop:read-file-string entry :external-format :utf-8))
                                   results)
                           (error (e)
                             (format t "Warning: could not read ~A: ~A~%" name e))))))))))
      (walk (uiop:ensure-directory-pathname project-path)))
    (nreverse results)))

(defun redwood-build-user-message (file-pairs)
  "Build the user message from FILE-PAIRS, each (relative-path . content)."
  (with-output-to-string (s)
    (format s "Please convert the following RedwoodSDK project into a single Hactar Lisp file.~%~%")
    (dolist (pair file-pairs)
      (let* ((path (car pair))
             (content (cdr pair))
             (ext (pathname-type (pathname path)))
             (lang (or (get-language-hint-from-extension ext) "")))
        (format s "```~A ~A~%~A~%```~%~%" lang path content)))))

(defun %convert-redwood-run (project-path-str output-file &key stream)
  "Core conversion: collect files, call LLM, return response string or nil.
   Prints errors to stdout and returns nil on invalid inputs."
  (cond
    ((not project-path-str)
     (format t "Usage: /convert.redwood <project-path> [-o output.lisp]~%")
     nil)
    (t
     (let* ((project-path (uiop:ensure-directory-pathname
                             (uiop:ensure-absolute-pathname
                               (uiop:parse-native-namestring project-path-str)
                               *default-pathname-defaults*))))
        (cond
          ((not (probe-file project-path))
           (format t "Error: project path not found: ~A~%" project-path-str)
           nil)
          ((not (get-prompt 'redwood-to-lisp))
           (format t "Error: prompt not found: redwood-to-lisp~%")
           nil)
          (t
           (let* ((system-prompt (get-prompt 'redwood-to-lisp))
                 (file-pairs (redwood-collect-source-files project-path))
                 (_ (format t "Found ~A files in ~A~%" (length file-pairs) project-path-str))
                 (user-message (redwood-build-user-message file-pairs))
                 (response (get-llm-response user-message
                                             :custom-system-prompt system-prompt
                                             :stream stream
                                             :add-to-history nil)))
            (declare (ignore _))
            (when (and response output-file)
              (write-file-content output-file response)
              (format t "~%Wrote converted Lisp to: ~A~%" output-file))
            response)))))))

(defun %convert-redwood-acp (cmd-args)
  "ACP handler for convert.redwood. Returns an alist result."
  (let* ((parsed (parse-cli-args-s
                  (format nil "convert.redwood ~{~A~^ ~}" cmd-args)
                  '(("o" . "output"))))
         (output-file (getf parsed :output))
         (pos-args (append (uiop:ensure-list (getf parsed :subcommand))
                           (uiop:ensure-list (getf parsed :args))))
         (project-path-str (first pos-args)))
    (cond
      ((not project-path-str)
       `(("text" . "Usage: /convert.redwood <project-path> [-o output.lisp]")))
      (t
       (let* ((project-path (uiop:ensure-directory-pathname
                              (uiop:ensure-absolute-pathname
                                (uiop:parse-native-namestring project-path-str)
                                *default-pathname-defaults*)))
              (prompt-path (get-prompt-path "redwood-to-lisp.org")))
         (cond
           ((not (probe-file project-path))
            `(("text" . ,(format nil "Error: project path not found: ~A" project-path-str))))
           ((not (get-prompt 'redwood-to-lisp))
            `(("text" . "Error: prompt not found: redwood-to-lisp")))
           (t
            (let* ((system-prompt (get-prompt 'redwood-to-lisp))
                   (file-pairs (redwood-collect-source-files project-path))
                   (user-message (redwood-build-user-message file-pairs))
                   (response (get-llm-response user-message
                                               :custom-system-prompt system-prompt
                                               :stream nil
                                               :add-to-history nil)))
              (when (and response output-file)
                (write-file-content output-file response))
              `(("text" . ,(or response "No response from LLM."))
                ("data" . (("fileCount" . ,(length file-pairs))
                           ,@(when output-file
                               `(("outputFile" . ,output-file))))))))))))))

(define-command convert.redwood (args)
  "Convert a RedwoodSDK project into a single Hactar Lisp file.
Usage: /convert.redwood <project-path> [-o output.lisp]
Reads all TS/TSX/CSS/JSON/TOML files from <project-path>, sends them to the LLM
with the redwood-to-lisp prompt, and returns a single .lisp file."
  (let* ((output-file (getf args :output))
         (pos-args (append (uiop:ensure-list (getf args :subcommand))
                           (uiop:ensure-list (getf args :args))))
         (project-path-str (first pos-args))
         (response (%convert-redwood-run project-path-str output-file :stream t)))
    (when (and response (not output-file))
      (format t "~%~A~%" response)))
  :slash t
  :sub t
  :cli-options ((:short "o" :long "output" :description "Output file path for the generated .lisp"))
  :acp #'%convert-redwood-acp)

