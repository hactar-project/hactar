(in-package :hactar)

(defun perform-github-code-search (user-query-string &key (stream-output-p t))
  "Performs a GitHub code search and extracts snippets using LLMs."
  ;; 1. Check for search-gh-code
  (unless (find-executable "search-gh-code")
    (let ((msg "search-gh-code executable not found in PATH."))
      (if stream-output-p (format t "~&Error: ~A~%" msg))
      (return-from perform-github-code-search msg)))

  ;; 2. Generate GitHub Search Query
  (let* ((searchquery-prompt-template (handler-case (get-prompt 'searchquery "searchquery.mustache")
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
                                                         :custom-system-prompt (get-prompt 'github-search-query-system "github-search-query-system.org"))))
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
        (let* ((extractsnippets-prompt-template (handler-case (get-prompt 'extractsnippets "extractcodesnippets.mustache")
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
                            :custom-system-prompt (get-prompt 'github-extract-snippets-system "github-extract-snippets-system.org"))))))))

(define-command search-gh (args)
                "Search GitHub for code snippets based on a query.
Usage: /search <natural language query for code>"
                (let ((user-query (format nil "~{~A~^ ~}" args)))
                  (if (string= user-query "")
		      (perform-github-code-search nil :stream-output-p t)
                      (perform-github-code-search user-query :stream-output-p t))))
