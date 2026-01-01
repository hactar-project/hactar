;; Documentation and Guides Handling
(in-package :hactar)
(defun add-doc (title source &key (uri 'source) tags covers meta related id)
  "Add a documentation entry to the global *docs* list."
  (let* ((final-uri (if (eq uri 'source) source uri))
         (raw (if (str:starts-with? "raw:" source) t nil))
	 (src
	       (if raw
		   (let ((raw-content (subseq source 4)))
		     (values raw-content title nil))
		   (if (or (str:starts-with? "http://" source)
			   (str:starts-with? "https://" source)
			   (str:starts-with? "npm:" source)
			   (str:starts-with? "file:" source)
			   (str:starts-with? "hactar:" source))
		       source
		       (format nil "file:~A" source))))
	 )
    (handler-case
        (multiple-value-bind (content source-title source-meta)
	    (if raw
		(values src title meta)
		(fetch-import-content src))
          (let* ((final-title (or title source-title))
                 (doc (list (cons :title final-title)
                            (cons :source source)
                            (cons :uri final-uri)
                            (cons :content content)
                            (cons :tags tags)
                            (cons :covers covers)
                            (cons :related related)
                            (cons :meta (append meta source-meta))
                            (cons :id (or id (format nil "mem-~A" (uuid:make-v4-uuid)))))))
            (setf *docs* (remove final-uri *docs* :key (lambda (d) (cdr (assoc :uri d))) :test #'equal))
            (push doc *docs*)))
      (error (e)
        (format t "Error adding doc '~A': ~A~%" title e)))))

(defmacro defdocforsymbol (symbol-spec source &key (uri ''source) (type :function) (version "latest") (format :markdown) related covers tags meta)
  "Define documentation for a specific symbol, creating graph connections.
   SYMBOL-SPEC: Symbol or string name of the entity.
   SOURCE: Source of the documentation.
   URI: Unique identifier for the documentation.
   TYPE: Type of symbol (:function, :class, :variable, etc.).
   VERSION: Version string.
   FORMAT: Documentation format (:markdown, :org, etc.).
   RELATED: List of related symbols/docs.
   COVERS: Topics covered.
   TAGS: Tags."
  (let* ((symbol-name (string-downcase (string symbol-spec)))
         (title (format nil "~A (~A)" symbol-name type)))
    `(add-doc ,title ,source 
              :uri ,uri
              :covers (cons ,symbol-name ,covers)
              :tags (append (list "symbol" (string-downcase (symbol-name ,type))) ,tags)
              :related (mapcar (lambda (r) (list :target (string-downcase (string r)) :type :related)) (uiop:ensure-list ,related))
              :meta (append ,meta 
                            `((:symbol . ,,symbol-name)
                              (:type . ,,type)
                              (:version . ,,version)
                              (:format . ,,format))))))

(defmacro defdoc (title &rest args)
  "Define a documentation entry."
  (let ((source (getf args :source))
        (version (getf args :version))
        (covers (getf args :covers)))
    (unless (or version covers)
      (error "Defdoc for ~S requires either :version or :covers." title))

    (let ((covers-expr (if covers
                           covers
                           `(uiop:ensure-list ,version))))
      `(add-doc ,title ,source
                :uri ,(getf args :uri ''source)
                :tags ,(getf args :tags)
                :covers ,covers-expr
                :meta ,(getf args :meta)
                :related ,(getf args :related)
                :id ,(getf args :id)))))

(defdoc "Getting Started with Hactar"
  :source "raw: a guide to hactar"
  :uri "file:~/everything/starters/RustStarters.org"
  :tags '("getting-started" "hactar")
  :covers '("hactar/getting-started"))

(defun parse-package-string (pkg-string)
  "Parses a package string like '@p/react@18.0.0' into (name version)."
  (when (str:starts-with? "@p/" pkg-string)
    (let* ((name-version (subseq pkg-string 3))
           (at-pos (position #\@ name-version :from-end t)))
      (if at-pos
          (list (subseq name-version 0 at-pos) (subseq name-version (1+ at-pos)))
          (list name-version nil))))) ; No version specified

(defun %get-headline-level (line)
  "Internal helper: Returns the level of a headline line (number of stars), or nil."
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings "^(\\*+)\\s+" line)
    (when match
      (length (aref registers 0)))))

(defun %get-tags-from-headline (headline-line)
  "Internal helper: Extracts tags (like :tag1:tag2:) from a headline string."
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings ":([^:]+(?::[^:]+)*):\\s*$" headline-line) ; Match tags at end, allowing trailing space
    (when match
      (remove "" (str:split ":" (aref registers 0)) :test #'string=))))

(defun find-first-headline-with-tag (lines tag)
  "Finds the line index and level of the first headline containing the specified tag.
   Returns (values line-index level) or (values nil nil)."
  (loop for line in lines
        for i from 0
        for level = (%get-headline-level line)
        when level
        do (let ((tags (%get-tags-from-headline line)))
             (when (member tag tags :test #'string=)
               (return-from find-first-headline-with-tag (values i level))))
        finally (return (values nil nil))))

(defun format-doc-as-org-entry (doc-plist &optional (level 2))
  "Formats a documentation plist into an Org-mode entry string."
  (format t "doc-plist ~A~%" doc-plist)
  (let* ((title (cdr (assoc :title doc-plist)))
         (content (cdr (assoc :content doc-plist)))
         (source (cdr (assoc :source doc-plist)))
         (source-id (cdr (assoc :source-id doc-plist))) ; Use source_id for linking back
         (tags (cdr (assoc :tags doc-plist)))
         (covers (cdr (assoc :covers doc-plist)))
         (entry-id (format nil "~A" (uuid:make-v4-uuid))) ; Generate a new unique ID for this entry instance
         (stars (make-string level :initial-element #\*)))
    (format t "source-id ~A~%" source-id)
    (with-output-to-string (s)
      (format s "~A ~A~%" stars title)
      (format s ":PROPERTIES:~%")
      (format s ":ID:       ~A~%" entry-id)
      (when source (format s ":SOURCE:   ~A~%" source))
      (when source-id (format s ":DOC_ID:   ~A~%" source-id))
      (when tags (format s ":DOC_TAGS: ~{~A~^ ~}~%" (coerce tags 'list)))
      (when covers (format s ":DOC_COVERS: ~{~A~^ ~}~%" (coerce covers 'list)))
      (format s ":END:~%")
      ;; Ensure content ends with a newline if it doesn't already
      (format s "~A~%" (string-trim '(#\Newline) content)))))

(defun guide-add (doc-tag headline-title headline-tag query-text)
  "Generic function to add documentation to a guide file under a specific headline.
   Searches docs with DOC-TAG and QUERY-TEXT, prompts user selection,
   finds/creates headline with HEADLINE-TITLE and HEADLINE-TAG,
   updates/inserts the entry based on DOC_ID, and saves the guide."
  (unless (and *active-guide-file* (probe-file *active-guide-file*))
    (format t "No active guide file set or found. Use /guides to select one.~%")
    (return-from guide-add nil))

  (let ((results (docs-find :text query-text :tags (list doc-tag) :limit 20)))
    (unless results
      (format t "No documents found matching tag '~A' and query: ~A~%" doc-tag query-text)
      (return-from guide-add nil))

    (let* ((items (loop for doc in results
                        collect `((:item . ,(format nil "~A (ID: ~A)" (cdr (assoc :title doc)) (cdr (assoc :id doc))))
                                  (:preview . ,(format-doc-for-fzf-preview doc)))))
           (selected-item (fuzzy-select items)))

      (unless selected-item
        (format t "Selection cancelled.~%")
        (return-from guide-add nil))

      (let* ((selected-doc-title (cdr (assoc :item selected-item)))
             (id-str (multiple-value-bind (match regs)
                         (cl-ppcre:scan-to-strings "\\(ID: (\\d+)\\)$" selected-doc-title)
                       (when match (aref regs 0))))
             (selected-doc-id (when id-str (ignore-errors (parse-integer id-str))))
             (selected-doc (when selected-doc-id
                             (find selected-doc-id results :key (lambda (d) (cdr (assoc :id d))))))
             (doc-source-id (when selected-doc (cdr (assoc :source-id selected-doc)))))

        (unless selected-doc
          (format t "Error: Could not retrieve selected document data.~%")
          (return-from guide-add nil))

        (handler-case
            (let* ((guide-content-string (uiop:read-file-string *active-guide-file*))
                   (lines (split-lines guide-content-string))
                   (num-lines (length lines))
                   (parent-id nil)
                   (final-guide-string guide-content-string)) ; Start with current content

              ;; 1. Find Parent Headline ID by tag
              (loop for i from 0 below num-lines
                    for line = (nth i lines)
                    for level = (org-mode:get-headline-level line)
                    do (when level
                         (let ((tags (org-mode:get-tags-from-headline line)))
                           (when (member headline-tag tags :test #'string=)
                             ;; Found headline with tag, now find its ID
                             (let ((prop-lines '())
                                   (j i))
                               ;; Look ahead for properties drawer
                               (loop while (< (incf j) num-lines)
                                     for next-line = (nth j lines)
                                     for next-level = (org-mode:get-headline-level next-line)
                                     do
                                        (when (and next-level (<= next-level level)) (return)) ; Next sibling/parent
                                        (push next-line prop-lines)
                                        (when (scan "^\\s*:END:\\s*$" next-line) (return)))
                               (setf parent-id (org-mode:get-property "ID" (reverse prop-lines)))
                               (when parent-id (return))))))) ; Exit outer loop once parent ID is found

              ;; 2. Create Parent Headline if not found
              (unless parent-id
                (setf parent-id (format nil "~A" (uuid:make-v4-uuid)))
                (let ((new-headline-string
                        (format nil "~%* ~A :~A:~%:PROPERTIES:~%:ID:       ~A~%:END:~%" ; Add newline before
                                headline-title headline-tag parent-id)))
                  ;; Append the new headline string to the current content
                  (setf final-guide-string (concatenate 'string final-guide-string new-headline-string))
                  (format t "Created headline '~A' with tag :~A:~%" headline-title headline-tag)))

              ;; 3. Find and Delete Existing Entry (if any) with the same DOC_ID
              (let ((existing-entry-id-to-delete nil))
                ;; Re-read lines if we appended the headline
                (setf lines (split-lines final-guide-string))
                (setf num-lines (length lines))
                (loop for i from 0 below num-lines
                      for line = (nth i lines)
                      for level = (org-mode:get-headline-level line)
                      do (when level
                           ;; Look ahead for properties drawer of this headline
                           (let ((prop-lines '())
                                 (j i))
                             (loop while (< (incf j) num-lines)
                                   for next-line = (nth j lines)
                                   for next-level = (org-mode:get-headline-level next-line)
                                   do
                                      (when (and next-level (<= next-level level)) (return)) ; Next sibling/parent
                                      (push next-line prop-lines)
                                      (when (scan "^\\s*:END:\\s*$" next-line) (return)))
                             ;; Check DOC_ID in the extracted drawer
                             (let ((entry-doc-id (org-mode:get-property "DOC_ID" (reverse prop-lines))))
                               (when (and entry-doc-id (string= entry-doc-id doc-source-id))
                                 ;; Found matching DOC_ID, get its actual ID
                                 (setf existing-entry-id-to-delete (org-mode:get-property "ID" (reverse prop-lines)))
                                 (when existing-entry-id-to-delete (return))))))) ; Exit loop

                (when existing-entry-id-to-delete
                  (format t "Found existing entry with DOC_ID ~A (Entry ID: ~A). Replacing.~%" doc-source-id existing-entry-id-to-delete)
                  (let ((deleted-string (org-mode:delete-headline final-guide-string existing-entry-id-to-delete)))
                    (if deleted-string
                      (setf final-guide-string deleted-string)
                      (warn "Failed to delete headline with ID: ~A" existing-entry-id-to-delete)))))

              ;; 4. Insert New Entry using org-mode:insert-child
              (let ((new-entry-string (format-doc-as-org-entry selected-doc)))
                (let ((inserted-string (org-mode:insert-child final-guide-string parent-id new-entry-string)))
                  (if inserted-string
                    (setf final-guide-string inserted-string)
                    (error "Failed to insert child entry under parent ID: ~A" parent-id))))

              ;; 5. Write Back and Commit
              (write-file-content *active-guide-file* final-guide-string)
              (format t "Added/Updated entry '~A' under '~A' in guide file: ~A~%"
                      (cdr (assoc :title selected-doc))
                      headline-title
                      (uiop:native-namestring *active-guide-file*))

              (when *git-autocommit*
                (git-add (list (uiop:native-namestring *active-guide-file*)))
                (git-commit (format nil "Updated guide: Added/Updated '~A' under '~A'"
                                    (cdr (assoc :title selected-doc)) headline-title))
                (format t "Guide update committed.~%")))
          (error (e)
            (format t "Error processing guide file ~A: ~A~%" (uiop:native-namestring *active-guide-file*) e)))))))
;;* Documentation Helpers
(defun format-doc-for-fzf-preview (doc-plist)
  "Formats document details for fzf preview."
  (format nil "Title: ~A~%Source: ~A~%Covers: ~A~%Tags: ~A~%---~%~A"
          (cdr (assoc :title doc-plist))
          (cdr (assoc :source doc-plist))
	  (format nil "~{~A~^, ~}" (coerce (cdr (assoc :covers doc-plist)) 'list))
	  (format nil "~{~A~^, ~}" (coerce (cdr (assoc :tags doc-plist)) 'list))
          (cdr (assoc :content doc-plist))))

(defun ask-llm-for-doc-url (query prompt-template-path)
  "Asks the LLM to find a documentation URL based on the query."
  (unless *current-model*
    (format t "No LLM model selected. Cannot ask for URL.~%")
    (return-from ask-llm-for-doc-url nil))
  (let* ((prompt-template (uiop:read-file-string prompt-template-path))
         (prompt (mustache:render* prompt-template `((:query . ,query))))
         (response (get-llm-response prompt)))
    (when response
      (parse-url-from-text response))))

(define-command docs-context (args)
                "Show documentation currently added to the context."
                (declare (ignore args))
                (if *docs-context*
                  (progn
                    (format t "Documentation in context:~%")
                    (dolist (doc *docs-context*)
                      (format t "  - ~A (ID: ~A, Source: ~A)~%"
                              (cdr (assoc :title doc))
                              (cdr (assoc :id doc))
                              (cdr (assoc :source doc)))))
                  (format t "No documentation in context.~%")))

(define-command docs-add-to-context (args)
  "Add a known document (from *docs*) to the context by its URI or Source.
   Usage: /docs-add-to-context <uri>"
  (let ((uri (format nil "~{~A~^ ~}" args)))
    (if (string= uri "")
        (format t "Please provide a URI or Source to add.~%")
        (let ((doc (find uri *docs* :key (lambda (d) (cdr (assoc :uri d))) :test #'string=)))
          (unless doc
             (setf doc (find uri *docs* :key (lambda (d) (cdr (assoc :source d))) :test #'string=)))
          
          (if doc
              (add-doc-to-context doc)
              (format t "No document found in *docs* matching URI/Source: ~A~%" uri))))))

(define-command docs-add (args)
                "Add documentation from file, URL, package name (@p/name@ver), or fzf selection.
   Optionally include metadata: /docs-add <source> -tags=tag1,tag2 -covers=cover1"
                (block docs-add
                  (if (null args)
                    (let* ((docs-dir-path (uiop:ensure-directory-pathname
                                            (merge-pathnames *docs-folder* *repo-root*)))
                         (files (when (uiop:directory-exists-p docs-dir-path)
                                  (mapcar #'namestring (uiop:directory-files docs-dir-path)))))
                    (if (not files)
                      (progn
                        (format t "No files found in documentation folder: ~A~%" docs-dir-path)
                        nil)
                      (let ((selected-file (select-with-fzf files)))
                        (when selected-file
                          (let ((content (get-file-content selected-file))
                                (title (pathname-name selected-file)))
                            (when content
                              (handler-case
                                  (let ((created-ids (docs-create :source selected-file :title title :content content)))
                                    (when created-ids
                                      (format t "Successfully created doc(s) with ID(s): ~A~%" created-ids)
                                      (let ((new-doc (car (docs-find :id (car created-ids)))))
                                        (format t "new-doc: ~A~%" new-doc)
                                        (when new-doc (add-doc-to-context new-doc)))))
                                (error (e)
                                  (format t "Error creating doc from file ~A: ~A~%" selected-file e)))))))))
                  (let* ((cli-string (format nil "docs-add ~{~A~^ ~}" args))
                         (parsed-args (parse-cli-args-s cli-string '(("t" . "tags") ("c" . "covers"))))
                         (primary-arg (getf parsed-args :subcommand))
                         (tags (uiop:ensure-list (getf parsed-args :tags)))
                         (covers (uiop:ensure-list (getf parsed-args :covers)))
                         (meta (getf parsed-args :meta))
                         (package-info (when primary-arg (parse-package-string primary-arg)))
                         (is-url (and primary-arg
                                      (or (str:starts-with? "http://" primary-arg)
                                          (str:starts-with? "https://" primary-arg)))))
                    (unless primary-arg
                      (format t "No source specified.~%")
                      (return-from docs-add nil))
                    (debug-log "Primary Arg:" primary-arg)
                    (debug-log "Parsed Args:" parsed-args)
                    (debug-log "Tags:" tags "Covers:" covers "Meta:" meta)
                    (cond
                      ;; Package Name (@p/...)
                      (package-info
                        (let* ((name (first package-info))
                               (version (second package-info))
                               (default-cover (if version
                                                (format nil "~A@~A" name version)
                                                name))
                               (final-covers (if covers covers (list default-cover))))
                          (format t "Looking up documentation for package: ~A~%" default-cover)
                          (let ((existing-docs (docs-find :covers final-covers :limit 1)))
                            (if existing-docs
                              (progn
                                (format t "Found existing documentation in database.~%")
                                (add-doc-to-context (car existing-docs)))
                              (progn
                                (format t "No existing documentation found. Asking LLM for URL...~%")
                                (let* ((query (format nil "~A (language: ~A)" default-cover (language)))
                                       (url (ask-llm-for-doc-url query "docs-find-package.mustache")))
                                  (if url
                                    (let ((content (fetch-url-content url)))
                                      (when content
                                        (handler-case
                                            (let ((created-ids (docs-create :source url
                                                                            :title default-cover
                                                                            :content content
                                                                            :covers final-covers
                                                                            :tags tags
                                                                            :meta meta)))
                                              (when created-ids
                                                (format t "Successfully created doc(s) from URL with ID(s): ~A~%" created-ids)
                                                (let ((new-doc (car (docs-find :id (car created-ids)))))
                                                  (when new-doc (add-doc-to-context new-doc)))))
                                          (error (e)
                                            (format t "Error creating doc from URL ~A: ~A~%" url e)))))
                                    (format t "LLM could not provide a URL for ~A.~%" default-cover))))))))
                      ;; URL
                      (is-url
                        (let ((content (fetch-url-content primary-arg)))
                          (when content
                            (handler-case
                                (let ((created-ids (docs-create :source primary-arg
                                                                :title primary-arg
                                                                :content content
                                                                :tags tags
                                                                :covers covers
                                                                :meta meta)))
                                  (when created-ids
                                    (format t "Successfully created doc(s) from URL with ID(s): ~A~%" created-ids)
                                    (let ((new-doc (car (docs-find :id (car created-ids)))))
                                      (when new-doc (add-doc-to-context new-doc)))))
                              (error (e)
                                (format t "Error creating doc from URL ~A: ~A~%" primary-arg e))))))
                      (t
                        (let ((file-path (uiop:native-namestring (merge-pathnames primary-arg *repo-root*))))
                          (if (probe-file file-path)
                            (let ((content (uiop:read-file-string file-path))
                                  (title (pathname-name file-path)))
                              (when content
                                (handler-case
                                    (let ((created-ids (docs-create :source file-path
                                                                    :title title
                                                                    :content content
                                                                    :tags tags
                                                                    :covers covers
                                                                    :meta meta)))
                                      (when created-ids
                                        (format t "Successfully created doc(s) from file with ID(s): ~A~%" created-ids)
                                        (let ((new-doc (car (docs-find :id (car created-ids)))))
                                          (when new-doc (add-doc-to-context new-doc)))))
                                  (error (e)
                                    (format t "Error creating doc from file ~A: ~A~%" file-path e)))))
                            (format t "File not found: ~A~%" file-path)))))))))

(define-command docs (args)
  "Find and select documentation relevant to the current project stack. In non-interactive mode, prints the path of the selected doc.
The *docs* global is considered to always be relevant to current stack. When there are no docs in *docs* we query the DB.
Use docs-db to always select from the DB.
Usage: /docs [--format=json]"
  (let ((format-opt (getf args :format)))
    (let ((results (if *docs*
                       *docs*
		       (if *stack*
                           (docs-find :covers *stack* :limit 20)
			   (docs-find :limit 20)))))
      (if results
          (if (string-equal format-opt "json")
              (let ((json-str (format nil "#+begin_src json :type docs-output~%~A~%#+end_src~%" (to-json (coerce results 'vector)))))
                (editor-output json-str :type "json" :success "true"))
              (let ((selected-doc (select-doc-with-fzf results)))
                (when selected-doc
                  (if *silent*
                      (format t "~A~%" (cdr (assoc :source selected-doc)))
                      (add-doc-to-context selected-doc)))))
          (unless *silent* (format t "No documentation found matching the current stack: ~{~A~^, ~}~%" *stack*)))))
  :cli-options ((:long "format" :description "Output format (json)")))

(defun run-docs-db (parsed-args)
  "Common logic for docs-db and docs.db commands."
  (let* ((tags (uiop:ensure-list (getf parsed-args :tags)))
         (limit (or (and (getf parsed-args :limit)
                         (parse-integer (getf parsed-args :limit) :junk-allowed t))
                    500))
         (results (docs-find :limit limit :tags tags)))
    (if results
        (let ((selected-doc (select-doc-with-fzf results)))
          (when selected-doc
            (if *silent*
                (format t "~A~%" (cdr (assoc :source selected-doc)))
                (add-doc-to-context selected-doc))))
        (unless *silent* (format t "No documentation found.~%")))))

(define-command docs-db (args)
  "List all known documentation and select one. In non-interactive mode, prints the path of the selected doc.
Options: -t/--tags <tag>, -l/--limit <n>"
  (run-docs-db args)
  :cli-options ((:short "t" :long "tags" :description "Filter by tags")
                (:short "l" :long "limit" :description "Limit number of results")))

(define-sub-command docs.db (args)
  "List all known documentation and select one. In non-interactive mode, prints the path of the selected doc.
Options: -t/--tags <tag>, -l/--limit <n>"
  (run-docs-db args)
  :cli-options ((:short "t" :long "tags" :description "Filter by tags")
                (:short "l" :long "limit" :description "Limit number of results")))

(define-command docs-find (args)
  "Search documentation database by text query and add selected doc to context."
  (let ((query (format nil "~{~A~^ ~}" args)))
    (if (string= query "")
        (format t "Please provide a search query.~%")
        (let ((results (docs-find :text query :limit 20)))
          (if results
              (let ((selected-doc (select-doc-with-fzf results)))
                (when selected-doc
                  (add-doc-to-context selected-doc)))
              (format t "No documentation found matching query: ~A~%" query))))))

(define-command docs-guess (args)
                "Guess relevant documentation based on the last added file's content."
                (declare (ignore args))
                (let ((last-file (car *files*))) ; Get the most recently added file
                  (if last-file
                    (let ((content (get-file-content last-file)))
                      (if content
                        (let ((results (docs-find :text content :limit 20)))
                          (if results
                            (progn
                              (format t "Found potential documentation based on content of ~A:~%" last-file)
                              (let ((selected-doc (select-doc-with-fzf results)))
                                (when selected-doc
                                  (add-doc-to-context selected-doc))))
                            (format t "No documentation found matching content of ~A~%" last-file)))
                        (format t "Could not read content of last added file: ~A~%" last-file)))
                    (format t "No files have been added to the context yet.~%"))))

(define-command docs-ask (args)
                "Ask the LLM to find documentation for a query, fetch it, and add it."
                (let ((query (format nil "~{~A~^ ~}" args)))
                  (if (string= query "")
                    (format t "Please provide a query for the documentation.~%")
                    (let ((url (ask-llm-for-doc-url query (get-prompt-path "docs-ask-url.mustache"))))
                      (if url
                        (let ((content (fetch-url-content url)))
                          (when content
                            (handler-case
                                (let ((created-ids (docs-create :source url :title query :content content)))
                                  (when created-ids
                                    (format t "Successfully created doc(s) from LLM suggestion with ID(s): ~A~%" created-ids)
                                    (let ((new-doc (car (docs-find :id (car created-ids)))))
                                      (when new-doc (add-doc-to-context new-doc)))))
                              (error (e)
                                (format t "Error creating doc from LLM suggested URL ~A: ~A~%" url e)))))
                        (format t "LLM could not provide a suitable URL for your query.~%"))))))

(define-command docs-clear (args)
                "Delete all documents from the database (requires confirmation)."
                (declare (ignore args))
                (docs-clear-database))

(defun select-starter (query)
  "Searches for starters by text query, allows user selection, and returns the selected starter doc plist."
  (if (string= query "")
      (progn
	(format t "Please provide a search query for starters.~%")
	nil)
      (let ((results (starters-find :text query :limit 20)))
	(if results
            (select-doc-with-fzf results)
            (progn
              (format t "No starters found matching query: ~A~%" query)
              nil)))))

(define-command starters (args)
  "Search for starters by text query, allow selection, and add the selected starter to the documentation context.
Usage: /starters <query> [--format=json]"
  (let* ((format-opt (getf args :format))
         (pos-args (append (uiop:ensure-list (getf args :subcommand))
                           (uiop:ensure-list (getf args :args))))
         (query (format nil "~{~A~^ ~}" pos-args)))
    (if (string-equal format-opt "json")
        (let ((results (if (string= query "")
                           (starters-find :limit 50)
                           (starters-find :text query :limit 20))))
          (let ((json-str (format nil "#+begin_src json :type starters-output~%~A~%#+end_src~%" (to-json (coerce results 'vector)))))
            (editor-output json-str :type "json" :success "true")))
        (let ((selected-starter (select-starter query)))
          (when selected-starter
            (add-doc-to-context selected-starter)))))
  :cli-options ((:long "format" :description "Output format (json)")))

(defun perform-github-code-search (user-query-string &key (stream-output-p t))
  "Performs a GitHub code search and extracts snippets using LLMs."
  ;; 1. Check for search-gh-code
  (unless (find-executable "search-gh-code")
    (let ((msg "search-gh-code executable not found in PATH."))
      (if stream-output-p (format t "~&Error: ~A~%" msg))
      (return-from perform-github-code-search msg)))

  ;; 2. Generate GitHub Search Query
  (let* ((searchquery-prompt-template-path (get-prompt-path "searchquery.mustache"))
         (searchquery-prompt-template (handler-case (uiop:read-file-string searchquery-prompt-template-path)
                                       (error (e)
                                         (let ((msg (format nil "Error reading prompts/searchquery.mustache: ~A" e)))
                                           (if stream-output-p (format t "~&~A~%" msg))
                                           (return-from perform-github-code-search msg)))))
         (current-context-for-sq (generate-context))
         (full-sq-prompt (format nil "User Query: ~A~%~%Current Context:~%~A~%~%Follow these instructions to generate the GitHub search query:~%~A"
                                 user-query-string
                                 current-context-for-sq
                                 searchquery-prompt-template))
         (github-search-query-response (get-llm-response full-sq-prompt
                                                         :stream nil
                                                         :add-to-history nil
                                                         :custom-system-prompt (uiop:read-file-string (get-prompt-path "github-search-query-system.org")))))
    (unless (and github-search-query-response (string/= github-search-query-response ""))
      (let ((msg "LLM failed to generate GitHub search query or returned empty."))
        (if stream-output-p (format t "~&Error: ~A~%" msg))
        (return-from perform-github-code-search msg)))

    (let* ((extracted-sq-block (extract-md-fenced-code-block github-search-query-response))
           (github-query (if extracted-sq-block
                             (string-trim '(#\Space #\Tab #\Newline #\Return) (cdr (assoc :contents extracted-sq-block)))
                             (string-trim '(#\Space #\Tab #\Newline #\Return) github-search-query-response))))
      (when (string= github-query "")
        (let ((msg "LLM generated an empty GitHub search query after extraction."))
          (if stream-output-p (format t "~&Error: ~A~%" msg))
          (return-from perform-github-code-search msg)))

      (if stream-output-p (format t "~&Generated GitHub Search Query: ~A~%" github-query))

      ;; 3. Run search-gh-code
      (multiple-value-bind (gh-search-output gh-search-error-output gh-search-exit-code)
          (handler-case
            (uiop:run-program (list "search-gh-code" github-query)
                              :output :string :error-output :string :ignore-error-status t)
            (error (e)
              (let ((msg (format nil "Failed to execute search-gh-code: ~A" e)))
                (if stream-output-p (format t "~&Error: ~A~%" msg))
                (return-from perform-github-code-search msg))))
        (when (not (zerop gh-search-exit-code))
          (let ((msg (format nil "search-gh-code failed (Exit Code ~A): ~A" gh-search-exit-code gh-search-error-output)))
            (if stream-output-p (format t "~&Error: ~A~%" msg))
            (return-from perform-github-code-search msg)))

        (if stream-output-p (format t "~&Successfully fetched search results from GitHub.~%"))

        ;; 4. Extract Code Snippets
        (let* ((extractsnippets-prompt-template-path (get-prompt-path "extractcodesnippets.mustache"))
               (extractsnippets-prompt-template (handler-case (uiop:read-file-string extractsnippets-prompt-template-path)
                                                 (error (e)
                                                   (let ((msg (format nil "Error reading prompts/extractcodesnippets.mustache: ~A" e)))
                                                     (if stream-output-p (format t "~&~A~%" msg))
                                                     (return-from perform-github-code-search msg)))))
               (rules-string (with-output-to-string (s)
                               (maphash (lambda (key value) (declare (ignore key)) (format s "~A~%~%" value)) *active-rules*)))
               (context-string (generate-context))
               (extract-prompt (mustache:render* extractsnippets-prompt-template
                                                 `((:search-results . ,gh-search-output)
                                                   (:rules . ,(if (string= rules-string "") "(No active rules)" rules-string))
                                                   (:context . ,context-string)))))
          (get-llm-response extract-prompt
                            :stream stream-output-p
                            :add-to-history t
                            :custom-system-prompt (uiop:read-file-string (get-prompt-path "github-extract-snippets-system.org"))))))))

(define-command search (args)
                "Search GitHub for code snippets based on a query.
Usage: /search <natural language query for code>"
                (let ((user-query (format nil "~{~A~^ ~}" args)))
                  (if (string= user-query "")
		      (perform-github-code-search nil :stream-output-p t)
                      (perform-github-code-search user-query :stream-output-p t))))

(define-command search-add-files (args)
  "Search for files containing a text pattern using rg, and add them to context."
  (let ((query (format nil "~{~A~^ ~}" args)))
    (if (string= query "")
        (format t "Please provide a search query.~%")
        (if (find-executable "rg")
            (let ((files (search-files-with-rg query *repo-root*)))
              (if files
                  (let* ((rel-files (mapcar (lambda (f) (uiop:native-namestring (uiop:enough-pathname f *repo-root*))) files))
                         (selected-rel-files (select-with-fzf-multi rel-files)))
                    (if selected-rel-files
                        (progn
                          (dolist (f selected-rel-files)
                            (let ((full-path (merge-pathnames f *repo-root*)))
                              (pushnew (uiop:native-namestring full-path) *files* :test #'string=)
                              (format t "Added: ~A~%" f)))
                          (format t "Added ~A files to context.~%" (length selected-rel-files)))
                        (format t "No files selected.~%")))
                  (format t "No files found matching '~A'.~%" query)))
            (format t "Error: 'rg' (ripgrep) is not installed or not in PATH.~%")))))
(defun find-guide-files ()
  "Finds all *.guide.* files in the repository root."
  (when *repo-root*
    (directory (merge-pathnames "*.guide.*" *repo-root*))))

(defun set-active-guide (guide-path)
  "Sets the active guide file after performing size checks."
  (if (probe-file guide-path)
    (handler-case
        (let* ((content-len (with-open-file (s guide-path) (file-length s)))
               (relative-path (uiop:enough-pathname guide-path *repo-root*)))
          (cond
            ((> content-len *guide-max-chars*)
              (format t "~&Error: Guide file '~A' exceeds maximum size (~A > ~A chars). Cannot activate.~%"
                      relative-path content-len *guide-max-chars*)
              nil)
            ((> content-len *guide-warn-chars*)
              (format t "~&Warning: Guide file '~A' is large (~A > ~A chars). Activated.~%"
                      relative-path content-len *guide-warn-chars*)
              (setf *active-guide-file* guide-path)
              t)
            (t
              (setf *active-guide-file* guide-path)
              (format t "Activated guide: ~A~%" relative-path)
              t)))
      (error (e)
        (format t "~&Error accessing guide file '~A': ~A~%" (uiop:native-namestring guide-path) e)
       nil))
    (progn
      (format t "Error: Guide file not found: ~A~%" (uiop:native-namestring guide-path))
      nil)))

(define-command guides (args)
                "List available *.guide.* files and select one to activate."
                (declare (ignore args))
                (let* ((guide-files (find-guide-files))
                       (relative-paths (mapcar (lambda (p) (uiop:native-namestring (uiop:enough-pathname p *repo-root*))) guide-files)))
                  (if guide-files
                    (let ((selected-relative-path (select-with-fzf relative-paths)))
                      (when selected-relative-path
                        (let ((selected-full-path (merge-pathnames selected-relative-path *repo-root*)))
                          (set-active-guide selected-full-path))))
                    (format t "No *.guide.* files found in the repository root.~%"))))

(define-command guide (args)
                "Print the content of the currently active guide file."
                (declare (ignore args))
                (if (and *active-guide-file* (probe-file *active-guide-file*))
                  (progn
                    (format t "--- Active Guide: ~A ---~%" (uiop:native-namestring *active-guide-file*))
                    (format t "~A~%" (get-active-guide-content))
                    (format t "--- End of Guide ---~%"))
                  (format t "No active guide file set. Use /guides to select one.~%")))

(defun generate-all-files-context-for-guide ()
  "Generates a list of plists for all files in *files* for the guide generation prompt."
  (loop for file-path in *files*
        collect (let* ((content (get-file-content file-path))
                       (relative-path (uiop:native-namestring (uiop:enough-pathname file-path *repo-root*)))
                       (extension (pathname-type file-path))
                       (lang-hint (get-language-hint-from-extension extension)))
                  `((:filename . ,relative-path)
                    (:language . ,(or lang-hint ""))
                    (:content . ,(or content ""))))))

(define-command guides-gen (args)
  "Generate or update the .hactar.guide.* file using the LLM."
  (declare (ignore args))
  (if (not *current-model*)
      (format t "No LLM model selected. Cannot generate guide.~%")
      (if (not *files*)
          (format t "No files added to context. Add files with /add before generating a guide.~%")
          (let* ((guide-filename (format nil ".hactar.guide.~A" *guide-file-extension*))
                 (guide-file-path (merge-pathnames guide-filename *repo-root*))
                 (existing-guide-content (when (probe-file guide-file-path)
                                           (uiop:read-file-string guide-file-path)))
                 (files-context-list (generate-all-files-context-for-guide))
                 (prompt-template (uiop:read-file-string (get-prompt-path "generate-guide.mustache")))
                 (guide-format-name (if (string-equal *guide-file-extension* "md") "Markdown" "Org-mode"))
                 (prompt (mustache:render* prompt-template
                                           `((:files . ,files-context-list)
                                             (:existing_guide . ,existing-guide-content)
                                             (:guide_filename . ,guide-filename)
                                             (:guide_format . ,guide-format-name))))
		 ;; Estimate cost based on prompt and all file contents
                 (prompt-tokens (estimate-tokens prompt))
                 (files-tokens (reduce #'+ files-context-list :key (lambda (f) (estimate-tokens (cdr (assoc :content f))))))
                 (total-estimated-tokens (+ prompt-tokens files-tokens))
                 (input-cost-per-token (model-config-input-cost-per-token *current-model*))
                 (estimated-cost (* total-estimated-tokens input-cost-per-token))
                 (max-input-tokens (model-config-max-input-tokens *current-model*)))

            (when (> total-estimated-tokens max-input-tokens)
              (format t "Warning: Estimated token count (~A) for guide generation exceeds model's input limit (~A). The request might fail.~%"
                      total-estimated-tokens max-input-tokens))

            (format t "Estimated tokens for guide generation: ~A~%" total-estimated-tokens)
            (format t "Estimated cost: $~8F~%" estimated-cost)

            (when (confirm-action "Proceed with guide generation?")
              (format t "Generating guide... (This might take a while)~%")
              (let ((response (get-llm-response prompt :custom-system-prompt (uiop:read-file-string (get-prompt-path "guide-author-system.org")) :stream nil :add-to-history nil)))
		(when response
                  (let* ((search-replace-blocks (parse-search-replace-blocks response))
                         (guide-content nil)
                         (file-written nil)
                         (extracted-block (extract-md-fenced-code-block response)))
                    (cond
		      ;; Case 1: Search/Replace blocks found (Highest priority)
                      (search-replace-blocks
                       (format t "Found SEARCH/REPLACE blocks in response. Applying...~%")
                       (apply-search-replace-blocks search-replace-blocks :autocommit nil) ; Defer commit
		       ;; Assume the guide file itself might have been modified
                       (when (probe-file guide-file-path)
                         (setf file-written t))) ; Mark as potentially written for commit step

		      ;; Case 2: Specific guide code block found
                      ((and extracted-block (string= (cdr (assoc :filename extracted-block)) guide-filename))
                       (setf guide-content (cdr (assoc :contents extracted-block)))
                       (format t "Extracted guide content from ```... ~A block.~%" guide-filename))

		      ;; Case 3: Any code block found (fallback if specific filename not found)
                      (extracted-block
                       (setf guide-content (cdr (assoc :contents extracted-block)))
                       (format t "Extracted content from first code block (filename mismatch or missing).~%"))

		      ;; Case 4: No blocks, assume raw content
                      (t
                       (setf guide-content (string-trim '(#\Space #\Tab #\Newline #\Return) response))
                       (format t "Using full response as guide content (no code blocks found).~%")))

		    ;; Write content if extracted (Cases 2 & 3)
                    (when guide-content
                      (handler-case
                          (with-open-file (stream guide-file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
                            (write-string guide-content stream)
                            (setf file-written t) ; Mark as written
                            (format t "Successfully wrote guide content to ~A~%" (uiop:native-namestring guide-file-path)))
                        (error (e)
                          (format t "Error writing guide file ~A: ~A~%" (uiop:native-namestring guide-file-path) e))))

		    ;; Activate the guide if the file exists/was written
                    (when (probe-file guide-file-path)
                      (set-active-guide guide-file-path))

		    ;; Autocommit if enabled and file was written/modified
                    (when (and *git-autocommit* file-written (probe-file guide-file-path))
                      (format t "Autocommitting guide update...~%")
                      (git-add (list (uiop:native-namestring guide-file-path)))
                      (git-commit (format nil "Updated guide (~A)" guide-filename)) ; Include filename in commit
                      (format t "Guide committed.~%"))))))))))

(define-command guide-add-example (args)
  "Search docs tagged 'example', select one, and add/update it under the 'Examples' (:examples:) headline."
  (let ((query (format nil "~{~A~^ ~}" args)))
    (if (string= query "")
      (format t "Please provide a search query text for the example.~%")
      (guide-add "example" "Examples" "examples" query))))

(define-command guide-add-api (args)
  "Search docs tagged 'api', select one, and add/update it under the 'APIs' (:apis:) headline."
  (let ((query (format nil "~{~A~^ ~}" args)))
    (if (string= query "")
      (format t "Please provide a search query text for the API documentation.~%")
      (guide-add "api" "APIs" "apis" query))))

(define-command guide-add-docs (args)
  "Search docs tagged 'docs', select one, and add/update it under the 'Documentation' (:docs:) headline."
  (let ((query (format nil "~{~A~^ ~}" args)))
    (if (string= query "")
      (format t "Please provide a search query text for the documentation.~%")
      (guide-add "docs" "Documentation" "docs" query))))

(define-command lookup (args)
  "Lookup symbols in documentation covers.
Usage: /lookup symbol1,symbol2 [--format=json]"
  (let* ((format-opt (getf args :format))
         (pos-args (append (uiop:ensure-list (getf args :subcommand))
                           (uiop:ensure-list (getf args :args))))
         (symbols-str (format nil "~{~A~^ ~}" pos-args))
         (symbols (mapcar (lambda (s) (string-trim '(#\Space) s))
                          (uiop:split-string symbols-str :separator ","))))
    (setf symbols (remove "" symbols :test #'string=))
    (if (null symbols)
        (format t "Usage: /lookup symbol1,symbol2~%")
        (let ((results (docs-find :covers symbols :match-any t)))
          (if (string-equal format-opt "json")
              (let ((json-str (format nil "#+begin_src json :type lookup-output~%~A~%#+end_src~%" (to-json (coerce results 'vector)))))
                (editor-output json-str :type "json" :success "true"))
              (if results
                  (let ((selected (select-doc-with-fzf results)))
                    (when selected
                      (format t "Selected: ~A~%" (cdr (assoc :title selected)))
                      (format t "Source: ~A~%" (cdr (assoc :source selected)))
                      (format t "~A~%" (cdr (assoc :content selected)))))
                  (format t "No documents found matching covers: ~{~A~^, ~}~%" symbols))))))
  :cli-options ((:long "format" :description "Output format (json)")))

(define-command lookup-in-files (args)
  "Lookup symbols in files using rg.
Usage: /lookup-in-files symbol1,symbol2 [--format=json]"
  (let* ((format-opt (getf args :format))
         (pos-args (append (uiop:ensure-list (getf args :subcommand))
                           (uiop:ensure-list (getf args :args))))
         (symbols-str (format nil "~{~A~^ ~}" pos-args))
         (symbols (mapcar (lambda (s) (string-trim '(#\Space) s))
                          (uiop:split-string symbols-str :separator ","))))
    (setf symbols (remove "" symbols :test #'string=))

    (if (null symbols)
        (format t "Usage: /lookup-in-files symbol1,symbol2~%")
        (if (find-executable "rg")
            (let ((files (search-files-with-rg symbols *repo-root*)))
              (if (string-equal format-opt "json")
                  (let ((json-str (format nil "~A~%" (to-json (coerce files 'vector)))))
                    (editor-output json-str :type "json" :success "true"))
                  (if files
                      (let* ((rel-files (mapcar (lambda (f) (uiop:native-namestring (uiop:enough-pathname f *repo-root*))) files))
                             (selected-rel-file (select-with-fzf rel-files)))
                        (when selected-rel-file
                          (let ((full-path (merge-pathnames selected-rel-file *repo-root*)))
                            (format t "Selected: ~A~%" selected-rel-file)
                            (format t "~A~%" (get-file-content full-path)))))
                      (format t "No files found matching: ~{~A~^, ~}~%" symbols))))
            (format t "Error: 'rg' (ripgrep) is not installed or not in PATH.~%"))))
  :cli-options ((:long "format" :description "Output format (json)")))
