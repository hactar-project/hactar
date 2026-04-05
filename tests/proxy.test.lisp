(in-package :hactar-tests)

(def-suite proxy-tests
  :description "Tests for the OpenRouter-compatible LLM proxy.")

(in-suite proxy-tests)

;;* proxy-extract-system-prompt tests

(test extract-system-prompt-single
  "Extract a single system message from a messages vector."
  (let* ((sys-msg (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "role" ht) "system")
                    (setf (gethash "content" ht) "You are helpful.")
                    ht))
         (user-msg (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "role" ht) "user")
                     (setf (gethash "content" ht) "Hello")
                     ht))
         (messages (vector sys-msg user-msg)))
    (multiple-value-bind (system-prompt remaining)
        (hactar::proxy-extract-system-prompt messages)
      (is (string= "You are helpful." system-prompt))
      (is (= 1 (length remaining)))
      (is (string= "user" (gethash "role" (first remaining)))))))

(test extract-system-prompt-multiple
  "Multiple system messages are concatenated."
  (let* ((sys1 (let ((ht (make-hash-table :test 'equal)))
                 (setf (gethash "role" ht) "system")
                 (setf (gethash "content" ht) "Rule one.")
                 ht))
         (sys2 (let ((ht (make-hash-table :test 'equal)))
                 (setf (gethash "role" ht) "system")
                 (setf (gethash "content" ht) "Rule two.")
                 ht))
         (user-msg (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "role" ht) "user")
                     (setf (gethash "content" ht) "Hi")
                     ht))
         (messages (vector sys1 user-msg sys2)))
    (multiple-value-bind (system-prompt remaining)
        (hactar::proxy-extract-system-prompt messages)
      (is (search "Rule one." system-prompt))
      (is (search "Rule two." system-prompt))
      (is (= 1 (length remaining))))))

(test extract-system-prompt-none
  "No system messages returns nil system prompt and all messages."
  (let* ((user-msg (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "role" ht) "user")
                     (setf (gethash "content" ht) "Hello")
                     ht))
         (asst-msg (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "role" ht) "assistant")
                     (setf (gethash "content" ht) "Hi there")
                     ht))
         (messages (vector user-msg asst-msg)))
    (multiple-value-bind (system-prompt remaining)
        (hactar::proxy-extract-system-prompt messages)
      (is (null system-prompt))
      (is (= 2 (length remaining))))))

(test extract-system-prompt-from-list
  "Works with a list of messages (not just vector)."
  (let* ((sys-msg (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "role" ht) "system")
                    (setf (gethash "content" ht) "Be concise.")
                    ht))
         (user-msg (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "role" ht) "user")
                     (setf (gethash "content" ht) "What?")
                     ht))
         (messages (list sys-msg user-msg)))
    (multiple-value-bind (system-prompt remaining)
        (hactar::proxy-extract-system-prompt messages)
      (is (string= "Be concise." system-prompt))
      (is (= 1 (length remaining))))))

;;* proxy-reassemble-messages tests

(test reassemble-messages-with-system-prompt
  "System prompt is prepended as the first message."
  (let* ((user-msg (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "role" ht) "user")
                     (setf (gethash "content" ht) "Hello")
                     ht))
         (result (hactar::proxy-reassemble-messages "You are helpful." (list user-msg))))
    (is (= 2 (length result)))
    (is (string= "system" (gethash "role" (aref result 0))))
    (is (string= "You are helpful." (gethash "content" (aref result 0))))
    (is (string= "user" (gethash "role" (aref result 1))))))

(test reassemble-messages-without-system-prompt
  "No system prompt means messages are returned as-is."
  (let* ((user-msg (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "role" ht) "user")
                     (setf (gethash "content" ht) "Hello")
                     ht))
         (result (hactar::proxy-reassemble-messages nil (list user-msg))))
    (is (= 1 (length result)))
    (is (string= "user" (gethash "role" (aref result 0))))))

(test reassemble-messages-empty-system-prompt
  "Empty string system prompt is not included."
  (let* ((user-msg (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "role" ht) "user")
                     (setf (gethash "content" ht) "Hello")
                     ht))
         (result (hactar::proxy-reassemble-messages "" (list user-msg))))
    (is (= 1 (length result)))))

;;* proxy-messages-contain-p tests

(test messages-contain-p-found
  "Detects search string in message content (case-insensitive)."
  (let* ((msg (let ((ht (make-hash-table :test 'equal)))
                (setf (gethash "role" ht) "user")
                (setf (gethash "content" ht) "I'm building a React component with useState")
                ht)))
    (is-true (hactar::proxy-messages-contain-p (list msg) "react" "vue"))
    (is-true (hactar::proxy-messages-contain-p (list msg) "usestate"))
    (is-true (hactar::proxy-messages-contain-p (list msg) "REACT"))))

(test messages-contain-p-not-found
  "Returns nil when no search strings match."
  (let* ((msg (let ((ht (make-hash-table :test 'equal)))
                (setf (gethash "role" ht) "user")
                (setf (gethash "content" ht) "I'm building a Python script")
                ht)))
    (is (null (hactar::proxy-messages-contain-p (list msg) "react" "vue" "angular")))))

(test messages-contain-p-empty
  "Returns nil for empty messages."
  (is (null (hactar::proxy-messages-contain-p '() "react"))))

(test messages-contain-p-vector
  "Works with vector of messages."
  (let* ((msg (let ((ht (make-hash-table :test 'equal)))
                (setf (gethash "role" ht) "user")
                (setf (gethash "content" ht) "Fix the JSX syntax")
                ht)))
    (is-true (hactar::proxy-messages-contain-p (vector msg) "jsx"))))

;;* proxy-model-matches-p tests

(test model-matches-p-found
  "Detects pattern in model name."
  (is-true (hactar::proxy-model-matches-p "openai/gpt-4o" "gpt" "claude"))
  (is-true (hactar::proxy-model-matches-p "anthropic/claude-3-sonnet" "claude")))

(test model-matches-p-not-found
  "Returns nil when model doesn't match."
  (is (null (hactar::proxy-model-matches-p "openai/gpt-4o" "claude" "gemini"))))

(test model-matches-p-nil-model
  "Returns nil for nil model."
  (is (null (hactar::proxy-model-matches-p nil "gpt"))))

;;* proxy-inject-context tests

(test inject-context-with-existing-system-prompt
  "Context is appended to existing system prompt."
  (let* ((hactar::*active-rules* (make-hash-table :test 'equal))
         (hactar::*stack* '("lisp"))
         (hactar::*files* '())
         (hactar::*docs-context* '())
         (request-plist (list :system-prompt "Existing prompt."
                              :messages '()
                              :model "test"
                              :raw-body (make-hash-table :test 'equal)
                              :headers '())))
    (setf (gethash 'test-rule hactar::*active-rules*) "Always test your code.")
    (let ((result (hactar::proxy-inject-context request-plist)))
      (is (search "Existing prompt." (getf result :system-prompt)))
      (is (search "Hactar Context" (getf result :system-prompt)))
      (is (search "Always test your code." (getf result :system-prompt)))
      (is (search "lisp" (getf result :system-prompt))))))

(test inject-context-without-system-prompt
  "Context becomes the system prompt when none exists."
  (let* ((hactar::*active-rules* (make-hash-table :test 'equal))
         (hactar::*stack* '("python"))
         (hactar::*files* '())
         (hactar::*docs-context* '())
         (request-plist (list :system-prompt nil
                              :messages '()
                              :model "test"
                              :raw-body (make-hash-table :test 'equal)
                              :headers '())))
    (setf (gethash 'my-rule hactar::*active-rules*) "Use type hints.")
    (let ((result (hactar::proxy-inject-context request-plist)))
      (is (search "Hactar Context" (getf result :system-prompt)))
      (is (search "python" (getf result :system-prompt))))))

(test inject-context-empty-context
  "When no rules/stack/files/docs, system prompt is preserved as-is."
  (let* ((hactar::*active-rules* (make-hash-table :test 'equal))
         (hactar::*stack* '())
         (hactar::*files* '())
         (hactar::*docs-context* '())
         (request-plist (list :system-prompt "Just this."
                              :messages '()
                              :model "test"
                              :raw-body (make-hash-table :test 'equal)
                              :headers '())))
    (let ((result (hactar::proxy-inject-context request-plist)))
      (is (string= "Just this." (getf result :system-prompt))))))

;;* proxy-build-context-section tests

(test build-context-section-with-stack
  "Stack info appears in context section."
  (let ((hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*stack* '("react" "typescript"))
        (hactar::*files* '())
        (hactar::*docs-context* '()))
    (let ((section (hactar::proxy-build-context-section)))
      (is (search "Technology Stack" section))
      (is (search "react" section))
      (is (search "typescript" section)))))

(test build-context-section-with-rules
  "Active rules appear in context section."
  (let ((hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*stack* '())
        (hactar::*files* '())
        (hactar::*docs-context* '()))
    (setf (gethash 'style-rule hactar::*active-rules*) "Use 2-space indentation.")
    (let ((section (hactar::proxy-build-context-section)))
      (is (search "Active Rules" section))
      (is (search "Use 2-space indentation." section)))))

(test build-context-section-empty
  "Empty context returns empty string."
  (let ((hactar::*active-rules* (make-hash-table :test 'equal))
        (hactar::*stack* '())
        (hactar::*files* '())
        (hactar::*docs-context* '()))
    (let ((section (hactar::proxy-build-context-section)))
      (is (string= "" section)))))

;;* Hook composition tests

(test proxy-request-hook-composition
  "Hooks compose: each handler receives previous handler's output."
  (let* ((hactar::*proxy-request-hook*
           (make-instance 'hactar::hook-proxy-request
                          :combination #'nhooks:combine-composed-hook))
         (call-log '()))
    ;; Add two handlers that modify :system-prompt
    (nhooks:add-hook hactar::*proxy-request-hook*
                     (make-instance 'nhooks:handler
                                    :fn (lambda (plist)
                                          (push :first call-log)
                                          (setf (getf plist :system-prompt)
                                                (format nil "~A [first]"
                                                        (or (getf plist :system-prompt) "")))
                                          plist)
                                    :name 'test-hook-first))
    (nhooks:add-hook hactar::*proxy-request-hook*
                     (make-instance 'nhooks:handler
                                    :fn (lambda (plist)
                                          (push :second call-log)
                                          (setf (getf plist :system-prompt)
                                                (format nil "~A [second]"
                                                        (or (getf plist :system-prompt) "")))
                                          plist)
                                    :name 'test-hook-second))
    (let* ((input (list :system-prompt "base" :messages '() :model "" :raw-body nil :headers nil))
           (result (nhooks:run-hook hactar::*proxy-request-hook* input)))
      ;; Combined composed hook runs oldest first, so first then second
      (is (search "[first]" (getf result :system-prompt)))
      (is (search "[second]" (getf result :system-prompt))))))

;;* React proxy hook tests

(test react-hook-detects-stack
  "React hook injects rules when react is in stack."
  (let* ((hactar::*stack* '("react"))
         (hactar::*files* '())
         (request-plist (list :messages '()
                              :model "gpt-4o"
                              :system-prompt ""
                              :raw-body (make-hash-table :test 'equal)
                              :headers '())))
    (let ((result (hactar::proxy-react-hook request-plist)))
      (is (search "React Development Rules" (getf result :system-prompt)))
      (is (search "functional components" (getf result :system-prompt))))))

(test react-hook-detects-message-content
  "React hook injects rules when messages mention React."
  (let* ((hactar::*stack* '())
         (hactar::*files* '())
         (msg (let ((ht (make-hash-table :test 'equal)))
                (setf (gethash "role" ht) "user")
                (setf (gethash "content" ht) "Fix the React component that uses useState")
                ht))
         (request-plist (list :messages (list msg)
                              :model "gpt-4o"
                              :system-prompt ""
                              :raw-body (make-hash-table :test 'equal)
                              :headers '())))
    (let ((result (hactar::proxy-react-hook request-plist)))
      (is (search "React Development Rules" (getf result :system-prompt))))))

(test react-hook-detects-tsx-files
  "React hook injects rules when .tsx files are in context."
  (let* ((hactar::*stack* '())
         (hactar::*files* '("/tmp/App.tsx" "/tmp/utils.ts"))
         (request-plist (list :messages '()
                              :model "gpt-4o"
                              :system-prompt ""
                              :raw-body (make-hash-table :test 'equal)
                              :headers '())))
    (let ((result (hactar::proxy-react-hook request-plist)))
      (is (search "React Development Rules" (getf result :system-prompt))))))

(test react-hook-no-detection
  "React hook does not inject when no React signals present."
  (let* ((hactar::*stack* '("python" "django"))
         (hactar::*files* '("/tmp/app.py"))
         (msg (let ((ht (make-hash-table :test 'equal)))
                (setf (gethash "role" ht) "user")
                (setf (gethash "content" ht) "Write a Django view")
                ht))
         (request-plist (list :messages (list msg)
                              :model "gpt-4o"
                              :system-prompt "You are a Python expert."
                              :raw-body (make-hash-table :test 'equal)
                              :headers '())))
    (let ((result (hactar::proxy-react-hook request-plist)))
      (is (null (search "React Development Rules" (getf result :system-prompt))))
      (is (string= "You are a Python expert." (getf result :system-prompt))))))

(test react-hook-no-double-inject
  "React hook does not double-inject if rules already present."
  (let* ((hactar::*stack* '("react"))
         (hactar::*files* '())
         (existing-prompt (format nil "## React Development Rules~%~%~A"
                                  (hactar::react-proxy-rules-text)))
         (request-plist (list :messages '()
                              :model "gpt-4o"
                              :system-prompt existing-prompt
                              :raw-body (make-hash-table :test 'equal)
                              :headers '())))
    (let* ((result (hactar::proxy-react-hook request-plist))
           (sys (getf result :system-prompt)))
      ;; Count occurrences of the marker text - should be exactly 1
      (let ((count 0) (start 0)
            (needle "Prefer functional components and hooks"))
        (loop
          (let ((pos (search needle sys :start2 start)))
            (if pos
                (progn (incf count) (setf start (1+ pos)))
                (return))))
        (is (= 1 count))))))

;;* proxy-extract-request-headers tests

(test extract-request-headers-basic
  "Extracts HTTP headers from Lack environment plist."
  (let ((env (list :http-authorization "Bearer token123"
                   :http-content-type "application/json"
                   :http-x-title "My App"
                   :content-type "application/json"
                   :request-method :post)))
    (let ((headers (hactar::proxy-extract-request-headers env)))
      ;; Should have extracted authorization header
      (is (assoc "authorization" headers :test #'string-equal)))))

;;* Command registration tests

(test proxy-start-command-registered
  "proxy.start command is registered."
  (is-true (gethash "/proxy.start" hactar::*commands*)))

(test proxy-status-command-registered
  "proxy.status command is registered."
  (is-true (gethash "/proxy.status" hactar::*commands*)))

;;* Default hook handler registration test

(test default-context-injector-registered
  "The default proxy-inject-context handler is installed on the hook."
  (let ((handlers (nhooks:handlers hactar::*proxy-request-hook*)))
    (is-true (find 'hactar::proxy-inject-context handlers :key #'nhooks:name))))

(test react-hook-registered
  "The React proxy hook handler is installed on the hook."
  (let ((handlers (nhooks:handlers hactar::*proxy-request-hook*)))
    (is-true (find 'hactar::proxy-react-hook handlers :key #'nhooks:name))))

;;* End-to-end request plist processing test

(test full-request-processing-pipeline
  "A request plist flows through inject-context and react-hook correctly."
  (let* ((hactar::*active-rules* (make-hash-table :test 'equal))
         (hactar::*stack* '("react" "typescript"))
         (hactar::*files* '())
         (hactar::*docs-context* '())
         (msg (let ((ht (make-hash-table :test 'equal)))
                (setf (gethash "role" ht) "user")
                (setf (gethash "content" ht) "Create a new component")
                ht))
         (request-plist (list :messages (list msg)
                              :model "anthropic/claude-3-sonnet"
                              :system-prompt "You write clean code."
                              :raw-body (make-hash-table :test 'equal)
                              :headers '())))
    (setf (gethash 'ts-rule hactar::*active-rules*) "Use strict TypeScript.")
    ;; Simulate the pipeline: inject-context then react-hook
    (let* ((after-inject (hactar::proxy-inject-context request-plist))
           (after-react (hactar::proxy-react-hook after-inject))
           (final-prompt (getf after-react :system-prompt)))
      ;; Original prompt preserved
      (is (search "You write clean code." final-prompt))
      ;; Hactar context injected
      (is (search "Hactar Context" final-prompt))
      (is (search "Use strict TypeScript." final-prompt))
      (is (search "Technology Stack" final-prompt))
      ;; React rules injected
      (is (search "React Development Rules" final-prompt))
      (is (search "functional components" final-prompt)))))

;;* State variable tests

(test proxy-state-variables-exist
  "Proxy state variables are defined."
  (is (boundp 'hactar::*proxy-port*))
  (is (boundp 'hactar::*proxy-upstream-url*))
  (is (boundp 'hactar::*proxy-auto-start*))
  (is (stringp hactar::*proxy-upstream-url*))
  (is (numberp hactar::*proxy-port*)))
