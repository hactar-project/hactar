(in-package :hactar-tests)

(def-suite ai-comment-tests
  :description "Tests for AI comment detection and processing.")

(in-suite ai-comment-tests)

(test is-ai-comment-event-detects-ai-bang
  "Detect that is-ai-comment-event? returns truthy when file contains AI! and event is added/changed."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "normal line~%AI! please modify this~%"))
    (is (hactar::is-ai-comment-event? p :file-added))
    (is (hactar::is-ai-comment-event? p :file-changed))
    (is (null (hactar::is-ai-comment-event? p :file-removed)))))

(test is-ai-edit-comment-event-detects-ai-bang
  "Detect that is-ai-edit-comment-event? returns truthy when file contains AI! and event is added/changed."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "prefix~%AI! edit me~%suffix"))
    (is (hactar::is-ai-edit-comment-event? p :file-added))
    (is (hactar::is-ai-edit-comment-event? p :file-changed))
    (is (null (hactar::is-ai-edit-comment-event? p :file-removed)))))

(test is-ai-question-comment-event-detects-ai-q
  "Detect that is-ai-question-comment-event? returns truthy when file contains AI? and event is added/changed."
  (uiop:with-temporary-file (:pathname p :keep t)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "foo~%AI? what does this do?~%bar"))
    (is (hactar::is-ai-question-comment-event? p :file-added))
    (is (hactar::is-ai-question-comment-event? p :file-changed))
    (is (null (hactar::is-ai-question-comment-event? p :file-removed)))))

(test process-ai-comment-file-invokes-llm-with-relative-path
  "Ensure process-ai-comment-file adds file to context and calls LLM with a prompt containing the relative path."
  (uiop:with-temporary-file (:pathname p :keep t)
    ;; Create a fake repo root around the temp file
    (let* ((repo-root (uiop:pathname-directory-pathname p))
           (added-path nil)
           (llm-prompt nil))
      (with-open-file (s p :direction :output :if-exists :supersede)
        (format s "AI! change something here~%"))

      (let ((hactar::*repo-root* repo-root))
        (with-dynamic-stubs ((hactar::add-file-to-context (lambda (path) (setf added-path path) nil))
                             (hactar::get-prompt-path (lambda (name)
                                                        (declare (ignore name))
                                                        "/dev/null/fake-ai-comment-template.mustache"))
                             (uiop:read-file-string (lambda (path)
                                                      (declare (ignore path))
                                                      "Analyze AI! comments in {{file}}"))
                             (hactar::get-llm-response (lambda (prompt &key &allow-other-keys)
                                                         (setf llm-prompt prompt)
                                                         "ok")))
          (hactar::process-ai-comment-file p)
          (is (equal added-path p))
          ;; The prompt should contain the relative path (native namestring) inside the rendered template
          (let* ((relative (uiop:native-namestring (uiop:enough-pathname p repo-root))))
            (is (and llm-prompt (search relative llm-prompt :test #'char-equal)))))))))

(test maybe-process-next-ai-comment-processes-one-item
  "maybe-process-next-ai-comment should pop one pathname from the queue and call process-ai-comment-file."
  (uiop:with-temporary-file (:pathname p :keep t)
    (let* ((processed nil)
           (lock (bt:make-lock)))
      ;; Ensure the queue has exactly one item to avoid background thread spawning
      (setf hactar::*ai-comment-queue* (list p))
      (setf hactar::*ai-comment-processor-lock* lock)
      (with-dynamic-stubs ((hactar::process-ai-comment-file (lambda (path) (push path processed))))
        (hactar::maybe-process-next-ai-comment)
        (is (= (length processed) 1))
        (is (equal (first processed) p))
        (is (null hactar::*ai-comment-queue*))))))

(test process-ai-question-file-invokes-llm-with-content
  "Ensure process-ai-question-file adds file to context and calls LLM with prompt containing relative path and file content."
  (uiop:with-temporary-file (:pathname p :keep t)
    (let* ((repo-root (uiop:pathname-directory-pathname p))
           (file-content "Line 1\nAI? What is this?\nLine 3\n")
           (added-path nil)
           (captured-prompt nil)
           (captured-stream nil)
           (captured-add-to-history nil))
      ;; Write content to the temporary file
      (with-open-file (s p :direction :output :if-exists :supersede)
        (write-string file-content s))

      (let ((hactar::*repo-root* repo-root))
        (with-dynamic-stubs ((hactar::add-file-to-context (lambda (path) (setf added-path path) nil))
                             (hactar::read-file-content (lambda (path)
                                                          (declare (ignore path))
                                                          file-content))
                             (hactar::get-llm-response (lambda (prompt &key stream add-to-history &allow-other-keys)
                                                         (setf captured-prompt prompt
                                                               captured-stream stream
                                                               captured-add-to-history add-to-history)
                                                         "ok")))
          (hactar::process-ai-question-file p)
          (is (equal added-path p))
          (let* ((relative (uiop:native-namestring (uiop:enough-pathname p repo-root))))
            (is (and captured-prompt (search relative captured-prompt :test #'char-equal)))
            (is (search file-content captured-prompt :test #'char-equal)))
          (is (eq captured-stream t))
          (is (eq captured-add-to-history t)))))))
