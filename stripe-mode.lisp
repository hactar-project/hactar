;; stripe mode
(in-package :hactar)

(defstruct stripe-doc-node
  name
  children
  content)

(defun stripe--normalize-filename (filename)
  "Converts snake_case filename to Title Case string."
  (let* ((name-without-ext (pathname-name filename))
         (parts (uiop:split-string name-without-ext :separator "_")))
    (format nil "~{~A~^ ~}" (mapcar #'string-capitalize parts))))

(defun stripe--extract-blocks (content target-lang)
  "Extract contents of code blocks matching target-lang."
  (let ((lines (split-lines content))
        (in-block nil)
        (capturing nil)
        (buffer nil)
        (results '()))
    (dolist (line lines)
      (if (str:starts-with? "```" line)
          (if in-block
              (progn
                ;; Closing fence
                (when capturing
                  (push (format nil "~{~A~^~%~}" (nreverse buffer)) results))
                (setf in-block nil capturing nil buffer nil))
              (progn
                ;; Opening fence
                (setf in-block t)
                (let* ((info-string (string-trim '(#\Space #\Tab) (subseq line 3)))
                       (lang (first (uiop:split-string info-string)))) ; take first word
                  (when (and lang (string-equal lang target-lang))
                    (setf capturing t)))))
          (when capturing
            (push line buffer))))
    (nreverse results)))

(defun stripe--get-or-create-child (node name)
  (let ((children (stripe-doc-node-children node)))
    (or (gethash name children)
        (setf (gethash name children)
              (make-stripe-doc-node :name name :children (make-hash-table :test 'equal))))))

(defun stripe--add-file-to-tree (root rel-path blocks)
  "Add blocks to the tree based on relative path."
  (let* ((parts (uiop:split-string rel-path :separator "/"))
         (current root))
    (dolist (part (butlast parts))
      (setf current (stripe--get-or-create-child current (stripe--normalize-filename part))))
    (let* ((filename (car (last parts)))
           (title (stripe--normalize-filename filename))
           (leaf (stripe--get-or-create-child current title)))
      (setf (stripe-doc-node-content leaf) 
            (append (stripe-doc-node-content leaf) blocks)))))

(defun stripe--print-tree (node level lang)
  (when (stripe-doc-node-name node)
    (format t "~A ~A~%" (make-string level :initial-element #\*) (stripe-doc-node-name node)))
  
  (when (stripe-doc-node-content node)
    (dolist (block (stripe-doc-node-content node))
      (format t "#+begin_src ~A~%~A~%#+end_src~%~%" lang block)))
  
  (let ((sorted-names (sort (alexandria:hash-table-keys (stripe-doc-node-children node)) #'string<)))
    (dolist (name sorted-names)
      (stripe--print-tree (gethash name (stripe-doc-node-children node)) 
                          (if (stripe-doc-node-name node) (1+ level) level)
                          lang))))

(define-sub-command stripe.docs.gen (args)
  "Generate API reference docs for Stripe from hactar-project/llm-txts.
   Usage: hactar stripe.docs.gen [--lang <lang>]
   Allowed langs: curl, ruby, node, go, java, php, python, cli. Default: curl."
  (let* ((lang (or (getf args :lang) "curl"))
         (allowed-langs '("curl" "ruby" "node" "go" "java" "php" "python" "cli")))
    (if (not (member lang allowed-langs :test #'string-equal))
        (format t "Error: Invalid language '~A'. Allowed: ~{~A~^, ~}~%" lang allowed-langs)
        (progn
          (format t "Fetching hactar-project/llm-txts...~%")
          (let ((repo-path (fetch-github-repo "hactar-project" "llm-txts")))
            (if (not repo-path)
                (format t "Error: Failed to fetch repository.~%")
                (let ((stripe-dir (merge-pathnames "stripe/" repo-path)))
                  (if (not (uiop:directory-exists-p stripe-dir))
                      (format t "Error: stripe/ directory not found in repo.~%")
                      (let ((stripe-root (make-stripe-doc-node :name "Stripe" :children (make-hash-table :test 'equal))))
                        (let ((files (directory (merge-pathnames "**/*.md" stripe-dir))))
                          (format t "Processing ~A files...~%" (length files))
                          (dolist (file files)
                            (when (and (uiop:file-pathname-p file) (string-equal (pathname-type file) "md"))
                              (let* ((content (uiop:read-file-string file))
                                     (blocks (stripe--extract-blocks content lang)))
                                (when blocks
                                  (let ((rel-path (uiop:native-namestring (uiop:enough-pathname file stripe-dir))))
                                    (stripe--add-file-to-tree stripe-root rel-path blocks)))))))
                        (stripe--print-tree stripe-root 1 lang)))))))))
  :cli-options ((:long "lang" :description "Language for code blocks (default: curl)")))

(defun update-stripe-docs (&rest args)
  (declare (ignore args))
  (defdoc "Stripe API Reference"
    :source (cond
              ((member "go" *stack* :test #'string=) "hactar:docsets/stripe.go.latest.org")
              ((member "node" *stack* :test #'string=) "hactar:docsets/stripe.node.latest.org")
              ((member "ruby" *stack* :test #'string=) "hactar:docsets/stripe.ruby.latest.org")
              (t "hactar:docsets/stripe.curl.latest.org"))
    :uri "stripe-api-docs"
    :version "latest"))

(nhooks:add-hook *stack-changed-hook* (make-instance 'nhooks:handler :fn #'update-stripe-docs :name 'stripe-docs-update))
(update-stripe-docs)
