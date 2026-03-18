;; error references and fixes
(in-package :hactar)

(defun format-error-for-preview (err)
  (format nil "Title: ~A~%Code: ~A~%Stack: ~A~%Slug: ~A~%Message: ~A~%~%Cause: ~A~%~%Solution: ~A"
          (cdr (assoc :title err))
          (cdr (assoc :code err))
          (cdr (assoc :stack err))
          (cdr (assoc :slug err))
          (cdr (assoc :message err))
          (cdr (assoc :cause err))
          (cdr (assoc :solution err))))

(defun select-error-with-fzf (errors)
  "Select an error from a list using fzf."
  (let* ((items (loop for err in errors
                      collect `((:item . ,(format nil "~A (Code: ~A)" (cdr (assoc :title err)) (cdr (assoc :code err))))
                                (:preview . ,(format-error-for-preview err)))))
         (selected (fuzzy-select items)))
    (when selected
      (let* ((title-code (cdr (assoc :item selected)))
             (code (second (cl-ppcre:split "\\(Code: " (string-trim ")" title-code)))))
        (find code errors :key (lambda (e) (cdr (assoc :code e))) :test #'string=)))))

(defun extract-org-content-from-node (node)
  "Extract content from a node, removing property drawers."
  (with-output-to-string (s)
    (dolist (child (org-mode-parser:node-children node))
      (unless (typep child 'org-mode-parser:org-properties-drawer)
        (princ (org-mode-parser:org-to-string child) s)))))

(defun parse-errors-from-org-file (filename)
  "Parse errors from an Org file. Returns a list of error plists."
  (let* ((doc (org-mode-parser:parse-org-file filename))
         (errors '()))
    (org-mode-parser:walk-tree
     doc
     (lambda (node)
       (when (and (typep node 'org-mode-parser:org-heading)
                  (member "error" (org-mode-parser:heading-tags node) :test #'string-equal))
         (let ((title (org-mode-parser:heading-title node))
               (tags (remove "error" (org-mode-parser:heading-tags node) :test #'string-equal))
               (children (org-mode-parser:node-children node))
               code stack slug message cause solution)

           ;; Extract Code, Stack, Slug
           (let ((props-drawer (find-if (lambda (c) (typep c 'org-mode-parser:org-properties-drawer)) children)))
             (when props-drawer
               (let ((props (org-mode-parser:node-properties props-drawer)))
                 (setf code (cdr (assoc :CODE props)))
                 (setf stack (cdr (assoc :STACK props)))
                 (setf slug (cdr (assoc :SLUG props))))))

           ;; Extract Message (from #+begin_src error)
           (let ((msg-node (find-if (lambda (c)
                                      (and (typep c 'org-mode-parser:org-src-block)
                                           (string-equal (org-mode-parser:src-block-language c) "error")))
                                    children)))
             (when msg-node
               (setf message (string-trim '(#\Space #\Tab #\Newline #\Return) (org-mode-parser:src-block-content msg-node)))))

           ;; Extract Cause and Solution
           (dolist (child children)
             (when (typep child 'org-mode-parser:org-heading)
               (cond
                 ((string-equal (org-mode-parser:heading-title child) "Cause")
                  (setf cause (string-trim '(#\Space #\Tab #\Newline #\Return) (extract-org-content-from-node child))))
                 ((string-equal (org-mode-parser:heading-title child) "Solution")
                  (setf solution (string-trim '(#\Space #\Tab #\Newline #\Return) (extract-org-content-from-node child)))))))

           (when (and title code message cause solution)
             (push (list (cons :title title)
                         (cons :code code)
                         (cons :stack stack)
                         (cons :slug slug)
                         (cons :message message)
                         (cons :cause cause)
                         (cons :solution solution)
                         (cons :tags tags))
                   errors))))))
    (nreverse errors)))

(defmacro deferror (title &rest args)
  "Define an error document.
   Required keys: :code :stack :message :cause :solution.
   Optional keys: :slug :tags"
  (let ((code (getf args :code))
        (stack (getf args :stack))
        (slug (getf args :slug))
        (message (getf args :message))
        (cause (getf args :cause))
        (solution (getf args :solution))
        (tags (getf args :tags)))
    (unless (and title code stack message cause solution)
      (error "deferror: title, code, stack, message, cause, and solution are required."))
    `(let* ((final-slug (or ,slug (if ,stack (format nil "~A:~A" ,stack ,code) ,code)))
            (error-obj (list (cons :title ,title)
                             (cons :code ,code)
                             (cons :stack ,stack)
                             (cons :slug final-slug)
                             (cons :message ,message)
                             (cons :cause ,cause)
                             (cons :solution ,solution)
                             (cons :tags ,tags))))
       (setf *errors* (remove ,code *errors* :key (lambda (e) (cdr (assoc :code e))) :test #'string=))
       (push error-obj *errors*))))

(define-command errors (args)
  "Find and select errors relevant to the current project/context."
  (declare (ignore args))
  (let ((results (if *errors*
                     *errors*
                     (errors-find :limit 20)))) ; Fallback to all errors if no stack/context specific filter yet
    (if results
        (let ((selected (select-error-with-fzf results)))
          (when selected
            (add-error-to-context selected)))
        (format t "No errors found.~%"))))

(define-command errors-add (args)
  "Add a new error entry manually.
   Usage: /errors-add --code CODE --title TITLE --message MSG --cause CAUSE --solution SOL --stack STACK --slug SLUG"
  (let* ((cli-string (format nil "errors-add ~{~A~^ ~}" args))
         (parsed (parse-cli-args-s cli-string)))
    (let ((code (getf parsed :code))
          (stack (getf parsed :stack))
          (slug (getf parsed :slug))
          (title (getf parsed :title))
          (message (getf parsed :message))
          (cause (getf parsed :cause))
          (solution (getf parsed :solution))
          (tags (uiop:ensure-list (getf parsed :tags))))
      (if (and code title message cause solution)
          (progn
            (errors-create :code code :stack stack :slug slug :title title :message message :cause cause :solution solution :tags tags)
            (format t "Error '~A' added.~%" title))
          (format t "Missing required arguments. Need --code, --title, --message, --cause, --solution.~%")))))

(defun run-errors-lookup (args)
  "Common logic for errors-find and errors-lookup."
  (let* ((format-opt (getf args :format))
         (pos-args (append (uiop:ensure-list (getf args :subcommand))
                           (uiop:ensure-list (getf args :args))))
         (query (format nil "~{~A~^ ~}" pos-args)))
    (if (string= query "")
        (format t "Usage: /errors-lookup query [--format=json]~%")
        (let ((results (errors-find :text query :limit 20)))
          (if (string-equal format-opt "json")
              (let ((json-str (format nil "#+begin_src json :type errors-lookup-output~%~A~%#+end_src~%" (to-json (coerce results 'vector)))))
                (editor-output json-str :type "json" :success "true"))
              (if results
                  (let ((selected (select-error-with-fzf results)))
                    (when selected
                      (add-error-to-context selected)))
                  (format t "No errors found matching query: ~A~%" query)))))))

(define-command errors-find (args)
  "Search in DB using a query and add selected to context.
   Usage: /errors-find <query> [--format=json]"
  (run-errors-lookup args)
  :cli-options ((:long "format" :description "Output format (json)")))

(define-command errors-lookup (args)
  "Lookup errors by text query.
Usage: /errors-lookup query [--format=json]"
  (run-errors-lookup args)
  :cli-options ((:long "format" :description "Output format (json)")))

(define-command errors-add-to-context (args)
  "Select and add a known error to context (from *errors*)."
  (declare (ignore args))
  (if *errors*
      (let ((selected (select-error-with-fzf *errors*)))
        (when selected
          (add-error-to-context selected)))
      (format t "No errors in *errors* list.~%")))

(defun run-errors-db (args)
  (declare (ignore args))
  (let ((results (errors-find :limit 500)))
    (if results
        (let ((selected (select-error-with-fzf results)))
          (when selected
            (add-error-to-context selected)))
        (format t "No errors found in database.~%"))))

(define-command errors-db (args)
  "List all known error docs and select one to add to context."
  (run-errors-db args))

(define-sub-command errors.db (args)
  "Alias to errors-db."
  (run-errors-db args))

(defun errors-import (args)
  (let ((filename (if (keywordp (first args))
                      (first (getf args :args))
                      (first args))))
    (if (and filename (probe-file filename))
        (let ((errors (parse-errors-from-org-file filename))
              (count 0))
          (if errors
              (progn
                (dolist (err errors)
                  (errors-create :code (cdr (assoc :code err))
                                 :stack (cdr (assoc :stack err))
                                 :slug (cdr (assoc :slug err))
                                 :title (cdr (assoc :title err))
                                 :message (cdr (assoc :message err))
                                 :cause (cdr (assoc :cause err))
                                 :solution (cdr (assoc :solution err))
                                 :tags (cdr (assoc :tags err)))
                  (incf count))
                (format t "Imported ~A errors from ~A~%" count filename))
              (format t "No valid errors found in ~A~%" filename)))
        (format t "File not found: ~A~%" filename))))

(define-command errors.import (args)
  "Import an error document (Org format).
   Usage: hactar errors.import <file.org>"
  (errors-import args))

(define-command errors-db-clear (args)
  "Clear database of errors."
  (declare (ignore args))
  (errors-clear-database))
