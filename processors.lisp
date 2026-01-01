;;* Processors - handle output from the LLM
(in-package :hactar)
(nhooks:define-hook-type process-history (function (list) t)
  "Hook run after an LLM response is processed, allowing modification or action based on history. Handler takes the full chat history.")
(defvar *process-history-hook* (make-instance 'hook-process-history))
(defmacro def-processor (name (&rest args) &body body)
  "Define a processor function and attach it to the process-history hook."
  `(progn
     (defun ,name ,args
       ,@body)
     (nhooks:add-hook *process-history-hook*
                      (make-instance 'nhooks:handler
                                     :fn #',name
                                     :name ',name))))
(defun parse-file-blocks (text)
  "Parses file creation blocks from text.
   Expects markdown code blocks with a filename in the opening fence.
   e.g. ```python path/to/file.py
        file content
        ```
   Returns a list of alists: ((:filename . \"path\") (:content . \"content\"))."
  (let ((blocks '())
        (lines (when text (str:lines text)))
        (state :looking)
        (current-filename nil)
        (current-content (make-string-output-stream)))
    (dolist (line lines)
      (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        (case state
          (:looking
           (when (str:starts-with? "```" trimmed-line)
             (let* ((fence-content (string-trim '(#\Space #\Tab) (subseq trimmed-line 3)))
                    (parts (remove "" (cl-ppcre:split "\\s+" fence-content) :test #'string=)))
               (cond
                 ((>= (length parts) 2)
                  (setf current-filename (second parts))
                  (setf state :in-block))
                 ((and (= (length parts) 1)
                       (or (find #\. (first parts))
                           (find #\/ (first parts))
                           (find #\\ (first parts))))
                  (setf current-filename (first parts))
                  (setf state :in-block))
                 (t nil)))))
          (:in-block
           (cond
             ((or (string= trimmed-line "<<<<<<< SEARCH")
                  (string= trimmed-line "=======")
                  (string= trimmed-line ">>>>>>> REPLACE"))
              (setf current-filename nil)
              (setf current-content (make-string-output-stream))
              (setf state :looking))
             ((string= trimmed-line "```")
              (let ((content (get-output-stream-string current-content)))
                (when (str:ends-with? #\Newline content)
                  (setf content (subseq content 0 (1- (length content)))))
                (push `((:filename . ,current-filename)
                        (:content . ,content))
                      blocks))
              (setf current-filename nil)
              (setf current-content (make-string-output-stream))
              (setf state :looking))
             (t
              (write-string line current-content)
              (write-char #\Newline current-content)))))))
    (nreverse blocks)))

(defun apply-file-blocks (file-blocks)
  "Processes a list of file blocks, creating them if they don't exist and user confirms.
   file-blocks is a list of alists ((:filename . \"...\") (:content . \"...\"))."
  (dolist (block file-blocks)
    (let* ((filename (cdr (assoc :filename block)))
           (content (cdr (assoc :content block)))
           (full-path (merge-pathnames filename *repo-root*)))
      (if (probe-file full-path)
          (format t "~&Skipping file creation: '~A' already exists.~%" filename)
          (if (confirm-action (format nil "Create new file '~A'?" filename))
              (progn
                (ensure-directories-exist full-path)
                (write-file-content full-path content)
                (format t "~&Created file: ~A~%" filename))
              (format t "~&Skipped creating file: ~A~%" filename))))))

(defun parse-search-replace-blocks (text)
  "Parses SEARCH/REPLACE blocks from the given text using line-by-line state machine.
   Returns a list of plists, each representing a block:
   '((:filename \"path/to/file\") (:search \"search text\") (:replace \"replace text\"))"
  (let* ((blocks '())
         (lines (str:lines text))
         (state :looking-for-opening-fence) ; Start by looking for the fence
         (current-filename nil)
         (current-search (make-string-output-stream))
         (current-replace (make-string-output-stream))
         (line-number 0))

    (dolist (line lines)
      (debug-log "Parsing line:" line " State:" state)
      (incf line-number)
      (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        (case state
          (:looking-for-opening-fence
            (when (str:starts-with? "```" trimmed-line)
              (let* ((fence-content (subseq trimmed-line 3))
                     (first-space (position #\Space fence-content)))
                (if first-space
                  (setf current-filename (string-trim '(#\Space #\Tab) (subseq fence-content (1+ first-space))))
                  (setf current-filename (string-trim '(#\Space #\Tab) fence-content))))
              (debug-log "Found filename:" current-filename)
              (setf state :looking-for-search-marker)))
          (:looking-for-search-marker
            (when (string= trimmed-line "<<<<<<< SEARCH")
              (setf state :in-search-block)))
          (:in-search-block
            (if (string= trimmed-line "=======")
              (setf state :in-replace-block)
              (progn
                (write-string line current-search)
                (write-char #\Newline current-search)))) ; Preserve line structure
          (:in-replace-block
            (if (string= trimmed-line ">>>>>>> REPLACE")
              (setf state :looking-for-closing-fence)
              (progn
                (write-string line current-replace)
                (write-char #\Newline current-replace)))) ; Preserve line structure
          (:looking-for-closing-fence
            (when (string= trimmed-line "```")
              (let ((search-str (get-output-stream-string current-search))
                    (replace-str (get-output-stream-string current-replace)))
                ;; Remove the last newline added by the loop if present
                (when (str:ends-with? #\Newline search-str)
                  (setf search-str (subseq search-str 0 (1- (length search-str)))))
                (when (str:ends-with? #\Newline replace-str)
                  (setf replace-str (subseq replace-str 0 (1- (length replace-str)))))

                (push `((:filename . ,current-filename)
                        (:search . ,search-str)
                        (:replace . ,replace-str))
                      blocks))
              (setf current-filename nil)
              (setf current-search (make-string-output-stream))
              (setf current-replace (make-string-output-stream))
              (setf state :looking-for-filename)))
          (otherwise
            (debug-log "Warning: Unexpected s" state "at line" line-number)))))

    (when (or (eq state :in-search-block) (eq state :in-replace-block))
      (debug-log "Warning: Input ended while parsing a block for file:" current-filename))
    (nreverse blocks)))

(defun apply-search-replace-blocks (blocks &key (autocommit *git-autocommit*))
  "Applies the changes defined in the parsed blocks list to the filesystem.
   Optionally performs a git commit if autocommit is enabled."
  (let ((modified-files '()))
    (dolist (block blocks)
      (let* ((filename (cdr (assoc :filename block)))
             (search-text (cdr (assoc :search block)))
             (replace-text (cdr (assoc :replace block)))
             (full-path (uiop:native-namestring (merge-pathnames filename *repo-root*))))
        (handler-case
            (progn
              (ensure-directories-exist full-path)
              (if (string= search-text "")
                (if (confirm-action (format nil "Create new file '~A'?" filename))
                  (progn
                    (with-open-file (stream full-path :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                      (write-string replace-text stream))
                    (format t "~&Created file: ~A~%" filename)
                    (pushnew full-path modified-files :test #'string=))
                  (format t "~&Skipped creating file: ~A~%" filename))
                (if (probe-file full-path)
                  (let* ((file-content (uiop:read-file-string full-path))
                         (search-pos (search search-text file-content)))
                    (if search-pos
                      (let ((new-content (concatenate 'string
                                                      (subseq file-content 0 search-pos)
                                                      replace-text
                                                      (subseq file-content (+ search-pos (length search-text))))))
                        (with-open-file (stream full-path :direction :output :if-exists :supersede)
                          (write-string new-content stream))
                        (format t "~&Applied changes to: ~A~%" filename)
                        (pushnew full-path modified-files :test #'string=))
                      (format t "~&Error: Search block not found in ~A~%--- Search Text ---~%~A~%--- End Search ---~%" filename search-text)))
                  (format t "~&Error: File to modify not found: ~A~%" filename))))
          (file-error (e)
            (format t "~&Error processing file ~A: ~A~%" filename e))
          (error (e)
            (format t "~&An unexpected error occurred while processing ~A: ~A~%" filename e)))))

    (when (and autocommit modified-files)
      (format t "~&Generating commit message...~%")
      (format t "~&Staging files: ~{~A~^, ~}~%" (mapcar (lambda (f) (uiop:enough-pathname f *repo-root*)) modified-files))
      (git-add modified-files)
      (let ((commit-message (generate-commit-message)))
        (if (and commit-message (not (string= commit-message "")))
            (progn
              (format t "~&Committing with message: ~A~%" commit-message)
              (git-commit commit-message)
              (format t "~&Changes committed.~%"))
          (format t "~&Skipping commit: Could not generate commit message.~%"))))))

(def-processor search-replace-processor (history)
               "Parses and applies SEARCH/REPLACE blocks from the last assistant message."
               (when history
                 (let ((last-message (car (last history))))
                   (when (string= (cdr (assoc :role last-message)) "assistant")
		     (let* ((content (cdr (assoc :content last-message)))
                            (blocks (parse-search-replace-blocks content)))
                       (when blocks
                         (format t "~&Applying ~A SEARCH/REPLACE block(s)...~%" (length blocks))
                         (apply-search-replace-blocks blocks)
                         (format t "~&Finished applying blocks.~%")))))))

(def-processor file-block-processor (history)
               "Parses and processes file creation blocks from the last assistant message."
               (when history
                 (let ((last-message (car (last history))))
                   (when (string= (cdr (assoc :role last-message)) "assistant")
                     (let* ((content (cdr (assoc :content last-message)))
                            (blocks (parse-file-blocks content)))
                       (when blocks
                         (apply-file-blocks blocks)))))))

;;* Tool Call Processor (XML-in-prompt mode)

(def-processor xml-tool-call-processor (history)
  "Parses and executes XML tool calls from the last assistant message.
   Only active when *tools-in-system-prompt* is T."
  (when (and (boundp '*tools-in-system-prompt*) *tools-in-system-prompt* 
             (boundp '*tool-use-enabled*) *tool-use-enabled* 
             history)
    (let ((last-message (car (last history))))
      (when (string= (cdr (assoc :role last-message)) "assistant")
        (let* ((content (cdr (assoc :content last-message)))
               (tool-calls (when (fboundp 'parse-xml-tool-calls)
                            (parse-xml-tool-calls content))))
          (when tool-calls
            (format t "~&Detected ~A tool call~:P in response.~%" (length tool-calls))
            (let ((results (execute-xml-tool-calls tool-calls)))
              ;; Format results and continue conversation
              (let ((results-text (format-tool-results-for-prompt results)))
                (format t "~&Tool results:~%~A~%" results-text)
                ;; Add tool results to history and get follow-up response
                (add-to-chat-history "user" results-text)
                ;; Get follow-up response from LLM
                (get-llm-response "" :stream t :add-to-history t :is-continuation t)))))))))
