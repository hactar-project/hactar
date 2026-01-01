(in-package :hactar)
;;* database and knowledge repository handling stuff
;;** docs
(defun docs-find (&key tags covers slug sources text type id (limit 10) (offset 0) (match-any nil))
  "Find documents based on various optional criteria.
   - tags: List of strings, documents must contain ALL tags (or ANY if match-any is T).
   - covers: List of strings, documents must contain ALL covered items (or ANY if match-any is T).
   - slug: String, exact match.
   - sources: List of strings, documents must match ANY source.
   - text: String, performs vector similarity search.
   - type: String, exact match.
   - id: Integer, exact match for the document ID.
   - limit: Integer, maximum number of results to return.
   - match-any: Boolean, if T, tags and covers use OR logic instead of AND."
  (with-db-connection
      (let ((where-clauses '())
            (params '())
            (joins '())
            (order-by nil)
            (select-cols "d.id, d.title, d.content, d.tags, d.covers, d.slug, d.source, d.type, d.meta, d.source_id"))

	(when text
          (let ((embedding (llm:ollama-embed text :model *embedding-model*)))
            (if embedding
		(progn
                  (push "JOIN vec_documents v ON d.id = v.rowid" joins)
                  (push "v.embedding MATCH ?" where-clauses)
                  (push "k = ?" where-clauses)
                  (push (format-vector-for-sqlite-vec embedding) params)
                  (push (+ limit offset) params)
                  (setf select-cols (concatenate 'string select-cols ", v.distance"))
                  (setf order-by "ORDER BY v.distance"))
		(warn "Could not generate embedding for text search: ~A" text))))

	(when id (push "d.id = ?" where-clauses) (push id params))
	(when slug (push "d.slug = ?" where-clauses) (push slug params))
	(when type (push "d.type = ?" where-clauses) (push type params))

	(when sources
          (push (format nil "d.source IN (~A)" (str:join ", " (make-list (length sources) :initial-element "?"))) where-clauses)
          (dolist (s sources)
            (push s params)))

	;; For tags and covers, we search the JSON array text. This is less efficient than Postgres GIN but works for SQLite.
	(when tags
          (if match-any
              (progn
                (push (format nil "EXISTS (SELECT 1 FROM json_each(d.tags) WHERE value IN (~A))"
                              (str:join ", " (make-list (length tags) :initial-element "?")))
                      where-clauses)
                (dolist (tag tags) (push tag params)))
              (dolist (tag tags)
                (push "EXISTS (SELECT 1 FROM json_each(d.tags) WHERE value = ?)" where-clauses)
                (push tag params))))

	(when covers
          (if match-any
              (progn
                (push (format nil "EXISTS (SELECT 1 FROM json_each(d.covers) WHERE value IN (~A))"
                              (str:join ", " (make-list (length covers) :initial-element "?")))
                      where-clauses)
                (dolist (cover covers) (push cover params)))
              (dolist (cover covers)
                (push "EXISTS (SELECT 1 FROM json_each(d.covers) WHERE value = ?)" where-clauses)
                (push cover params))))

	(let* ((base-sql (format nil "SELECT ~A FROM documents d ~{~A~^ ~}" select-cols joins))
               (where-sql (if where-clauses
                              (format nil "WHERE ~A" (str:join " AND " (reverse where-clauses)))
                              ""))
               (order-by-sql (or order-by "ORDER BY d.id DESC"))
               (limit-sql "LIMIT ? OFFSET ?")
               (final-sql (format nil "~A ~A ~A ~A" base-sql where-sql order-by-sql limit-sql))
               (final-params (append (reverse params) (list limit offset))))

          (debug-log "Executing docs-find SQL:" final-sql)
          (debug-log "With params:" final-params)

          (let* ((stmt (sqlite:prepare-statement *connection* final-sql))
		 (results
                   (unwind-protect
			(progn
                          (loop for p in final-params for i from 1 do (sqlite:bind-parameter stmt i p))
                          (let ((column-names (sqlite:statement-column-names stmt))
				(rows '()))
                            (loop while (sqlite:step-statement stmt)
                                  do (let ((row-alist (loop for i from 0 below (length column-names)
                                                            collect (cons (intern (string-upcase (elt column-names i)) :keyword)
                                                                          (sqlite:statement-column-value stmt i)))))
                                       (let ((processed-row (loop for (k . v) in row-alist
                                                                  collect (cons k (cond
                                                                                    ((and (member k '(:tags :covers :meta)) (stringp v) (not (str:blankp v)))
                                                                                     (handler-case (cl-json:decode-json-from-string v)
                                                                                       (error () v))) ; Return raw string on parse error
                                                                                    (t v))))))
                                         (push (cons :uri (cdr (assoc :source processed-row))) processed-row)
                                         (push processed-row rows))))
                            (nreverse rows)))
                     (sqlite:finalize-statement stmt))))
            results)))))



(defun docs-create (&key source title content (tags #()) (covers #()) (links_to #()) slug type meta)
  "Creates one or more document entries in the database.
   Required: source, title, content.
   Handles content splitting and embedding generation.
   If documents with the same source exist, they will be replaced."
  (unless (and source title content)
    (error "source, title, and content are required fields for docs-create."))
  (with-db-connection
    (sqlite:with-transaction *connection*
      ;; Check for and delete existing documents with the same source
      (let ((existing-ids (sqlite:execute-to-list *connection* "SELECT id FROM documents WHERE source = ?" source)))
        (when existing-ids
          (format t "Found ~A existing document(s) for source '~A'. Replacing content.~%" (length existing-ids) source)
          (sqlite:execute-non-query *connection* "DELETE FROM vec_documents WHERE rowid IN (SELECT id FROM documents WHERE source = ?)" source)
          (sqlite:execute-non-query *connection* "DELETE FROM documents WHERE source = ?" source)))

      ;; Proceed with creating the new document(s)
      (let ((created-ids '())
            (source-id (format nil "~(~A~)" (uuid:make-v4-uuid)))
            (content-chunks (if (> (length content) *max-content-chars*)
                              (progn
                                (warn "Content length (~A) exceeds limit (~A), splitting."
                                      (length content) *max-content-chars*)
                                (split-content content *max-content-chars*))
                              (list content)))
            (insert-doc-sql "INSERT INTO documents (tags, covers, links_to, slug, source, meta, content, title, type, source_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
            (insert-vec-sql "INSERT INTO vec_documents (rowid, embedding) VALUES (?, ?)"))

        (loop for chunk in content-chunks
              for i from 1
              do (let* ((chunk-title (if (> (length content-chunks) 1)
					 (format nil "~A (Part ~A)" title i)
                                       title))
                        (chunk-slug (if (> (length content-chunks) 1)
					(when slug (format nil "~A-part~A" slug i))
                                      slug))
                        (embedding (llm:ollama-embed chunk :model *embedding-model*))
                        (meta-json-string (to-json meta)))

                   (unless embedding
                     (error "Failed to generate embedding for document chunk: ~A" chunk-title))

                   (sqlite:execute-non-query *connection* insert-doc-sql
                                (format-array-for-sqlite (coerce tags 'list))
                                (format-array-for-sqlite (coerce covers 'list))
                                (format-array-for-sqlite (coerce links_to 'list))
                                chunk-slug
                                source
                                meta-json-string
                                chunk
                                chunk-title
                                type
                                source-id)
                   (let ((inserted-id (sqlite:last-insert-rowid *connection*)))
                     (sqlite:execute-non-query *connection* insert-vec-sql inserted-id (format-vector-for-sqlite-vec embedding))
                     (push inserted-id created-ids))
		   ))
        (nreverse created-ids)))))
;;** starters
(defun starters-find (&rest args &key tags &allow-other-keys)
  "Find starter documents. This is a wrapper around docs-find that adds the 'starter' tag."
  (remf args :tags)
  (apply #'docs-find :tags (cons "starter" tags) args))

;;** errors
(defun errors-find (&key text code tags (match-any nil) (limit 10) (offset 0))
  "Find error entries.
   - text: String, performs vector similarity search against message+cause+solution.
   - code: String, exact match.
   - tags: List of strings.
   - match-any: Boolean for tags logic.
   - limit: Integer.
   - offset: Integer."
  (with-db-connection
      (let ((where-clauses '())
            (params '())
            (joins '())
            (order-by nil)
            (select-cols "e.id, e.code, e.stack, e.slug, e.title, e.message, e.cause, e.solution, e.tags"))

	(when text
          (let ((embedding (llm:ollama-embed text :model *embedding-model*)))
            (if embedding
		(progn
                  (push "JOIN vec_errors v ON e.id = v.rowid" joins)
                  (push "v.embedding MATCH ?" where-clauses)
                  (push "k = ?" where-clauses)
                  (push (format-vector-for-sqlite-vec embedding) params)
                  (push (+ limit offset) params)
                  (setf select-cols (concatenate 'string select-cols ", v.distance"))
                  (setf order-by "ORDER BY v.distance"))
		(warn "Could not generate embedding for text search: ~A" text))))

	(when code (push "e.code = ?" where-clauses) (push code params))

	(when tags
          (if match-any
              (progn
                (push (format nil "EXISTS (SELECT 1 FROM json_each(e.tags) WHERE value IN (~A))"
                              (str:join ", " (make-list (length tags) :initial-element "?")))
                      where-clauses)
                (dolist (tag tags) (push tag params)))
              (dolist (tag tags)
                (push "EXISTS (SELECT 1 FROM json_each(e.tags) WHERE value = ?)" where-clauses)
                (push tag params))))

	(let* ((base-sql (format nil "SELECT ~A FROM errors e ~{~A~^ ~}" select-cols joins))
               (where-sql (if where-clauses
                              (format nil "WHERE ~A" (str:join " AND " (reverse where-clauses)))
                              ""))
               (order-by-sql (or order-by "ORDER BY e.id DESC"))
               (limit-sql "LIMIT ? OFFSET ?")
               (final-sql (format nil "~A ~A ~A ~A" base-sql where-sql order-by-sql limit-sql))
               (final-params (append (reverse params) (list limit offset))))

          (debug-log "Executing errors-find SQL:" final-sql)
          (debug-log "With params:" final-params)

          (let* ((stmt (sqlite:prepare-statement *connection* final-sql))
		 (results
                   (unwind-protect
			(progn
                          (loop for p in final-params for i from 1 do (sqlite:bind-parameter stmt i p))
                          (let ((column-names (sqlite:statement-column-names stmt))
				(rows '()))
                            (loop while (sqlite:step-statement stmt)
                                  do (let ((row-alist (loop for i from 0 below (length column-names)
                                                            collect (cons (intern (string-upcase (elt column-names i)) :keyword)
                                                                          (sqlite:statement-column-value stmt i)))))
                                       (let ((processed-row (loop for (k . v) in row-alist
                                                                  collect (cons k (cond
                                                                                    ((and (eq k :tags) (stringp v) (not (str:blankp v)))
                                                                                     (handler-case (cl-json:decode-json-from-string v)
                                                                                       (error () v)))
                                                                                    (t v))))))
                                         (push processed-row rows))))
                            (nreverse rows)))
                     (sqlite:finalize-statement stmt))))
            results)))))

(defun errors-create (&key code stack slug title message cause solution (tags #()))
  "Creates an error entry in the database. All fields are required."
  (unless (and code stack title message cause solution)
    (error "code, stack, title, message, cause, and solution are required fields for errors-create."))
  (with-db-connection
    (sqlite:with-transaction *connection*
      (let* ((embedding-text (format nil "Title: ~A~%Message: ~A~%Cause: ~A~%Solution: ~A" title message cause solution))
             (embedding (llm:ollama-embed embedding-text :model *embedding-model*))
             (final-slug (or slug (if stack (format nil "~A:~A" stack code) code)))
             (insert-err-sql "INSERT INTO errors (code, stack, slug, title, message, cause, solution, tags) VALUES (?, ?, ?, ?, ?, ?, ?, ?)")
             (insert-vec-sql "INSERT INTO vec_errors (rowid, embedding) VALUES (?, ?)"))

        (unless embedding
          (error "Failed to generate embedding for error."))

        (sqlite:execute-non-query *connection* insert-err-sql code stack final-slug title message cause solution (format-array-for-sqlite (coerce tags 'list)))
        (let ((inserted-id (sqlite:last-insert-rowid *connection*)))
          (sqlite:execute-non-query *connection* insert-vec-sql inserted-id (format-vector-for-sqlite-vec embedding))
          inserted-id)))))
