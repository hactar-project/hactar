;; sinatra mode
(in-package :hactar)

(defun sinatra--pandoc-to-org (content)
  (if (find-executable "pandoc")
      (uiop:run-program (list "pandoc" "-f" "markdown" "-t" "org")
                        :input (make-string-input-stream content)
                        :output :string
                        :error-output :interactive)
      content))

(define-sub-command sinatra.docs.gen (args)
  "Generate API reference docs for Sinatra using rdoc and pandoc.
   Usage: hactar sinatra.docs.gen"
  (declare (ignore args))
  (unless (find-executable "rdoc")
    (format t "Error: 'rdoc' not found. Please install it (gem install rdoc).~%")
    (uiop:quit 1))
  (unless (find-executable "pandoc")
    (format t "Error: 'pandoc' not found.~%")
    (uiop:quit 1))

  (let ((repo-path (fetch-github-repo "sinatra" "sinatra")))
    (if repo-path
        (progn
          (unless *silent* (format t "Running rdoc in ~A...~%" repo-path))
          (uiop:run-program (list "rdoc" "--format=markdown" "--output" "doc")
                            :directory repo-path
                            :output :interactive
                            :error-output :interactive
                            :ignore-error-status t)

          (let ((index-csv (merge-pathnames "doc/index.csv" repo-path)))
            (if (probe-file index-csv)
                (let ((entries (cl-csv:read-csv index-csv)))
                  (format t "#+TITLE: Sinatra API Reference~%")
                  (dolist (entry entries)
                    (when (and entry (>= (length entry) 3))
                      (destructuring-bind (name type path) entry
                        (declare (ignore name type))
                        ;; Skip header if present
                        (unless (string-equal path "path")
                          (let* ((clean-path (if (str:ends-with-p ".md" path)
                                                 (subseq path 0 (- (length path) 3))
                                                 path))
                                 (parts (uiop:split-string clean-path :separator "/"))
                                 (depth (length parts))
                                 (headline-title (car (last parts)))
                                 (md-file (merge-pathnames (format nil "doc/~A" path) repo-path)))
                            
                            (format t "~&~A ~A~%" (make-string depth :initial-element #\*) headline-title)
                            
                            (if (probe-file md-file)
                                (let ((content (uiop:read-file-string md-file)))
                                  (format t "~A~%~%" (sinatra--pandoc-to-org content)))
                                (format t "Content file not found: ~A~%~%" path))))))))
                (format t "Error: doc/index.csv not found.~%"))))
        (format t "Error: Failed to fetch repo.~%"))))

(defdoc "Latest Sinatra Docs" :version "latest" :source "hactar:docsets/sinatra.latest.org")
