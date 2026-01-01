;;* React Mode
(in-package :hactar)

;;** React Detection

(def-analyzer react-dependency ((*package-json-analyzed-hook*)) nil (metadata)
  "Checks for React dependency and updates the stack."
  (debug-log "Running react-dependency analyzer.")
  (let* ((deps (mapcar #'car (gethash "dependencies" metadata)))
         (deps (mapcar (lambda (dep) (string-downcase (string dep))) deps))
         (dev-deps (mapcar #'car (gethash "dev-dependencies" metadata)))
         (dev-deps (mapcar (lambda (dep) (string-downcase (string dep))) dev-deps)))
    (when (or (member "react" deps :test #'string=)
              (member "react" dev-deps :test #'string=))
      (unless *silent* (format t "~&React detected, adding to stack.~%"))
      (add-to-stack "react"))))

;;** React Generators
(defgenerator component
  "Generate a React functional component with TypeScript"
  :when (member "react" *stack* :test #'string-equal)
  :priority 10
  :args (name &key props)
  :operations
  ((:create :file "src/components/${name}.tsx"
            :template "react/component.tsx.mustache")))

(defgenerator hook
  "Generate a custom React hook"
  :when (member "react" *stack* :test #'string-equal)
  :priority 10
  :args (name &key args return-type)
  :operations
  ((:create :file "src/hooks/use${name}.ts"
            :template "react/hook.ts.mustache")))

(defgenerator test
  "Generate React component tests with Testing Library"
  :when (member "react" *stack* :test #'string-equal)
  :priority 10
  :args (target &key coverage)
  :operations
  ((:create :file "src/components/${name}.test.tsx"
            :template "react/component.test.tsx.mustache")))

;;** React Patterns

(defpattern error-boundary
  "React Error Boundary component"
  :when (member "react" *stack* :test #'string-equal)
  :priority 10
  :operations
  ((:create :file "src/components/ErrorBoundary.tsx"
            :template "react/error-boundary.tsx.mustache")))

;;** React Rule

(defrule react-rule (*package-json-analyzed-hook*) (metadata)
  "Adds React-specific instructions to the system prompt if React is detected."
  (let* ((deps (mapcar (lambda (dep) (string-downcase (string (car dep)))) 
                       (gethash "dependencies" metadata)))
         (dev-deps (mapcar (lambda (dep) (string-downcase (string (car dep)))) 
                           (gethash "dev-dependencies" metadata))))
    (when (or (member "react" deps :test #'string=)
              (member "react" dev-deps :test #'string=))
      "When working with React components:
- Prefer functional components and hooks (useState, useEffect, etc.) over class components.
- Use JSX for templating.
- Follow standard React coding conventions.
- Use TypeScript for type safety.
- Prefer composition over inheritance.")))

(defun extract-react-doc-paths (toc)
  "Recursively extract paths from the React docs TOC structure."
  (let ((paths '()))
    (labels ((recurse (items)
               (dolist (item items)
                 (let ((path (cdr (assoc :path item)))
                       (routes (cdr (assoc :routes item))))
                   (when path (push path paths))
                   (when routes (recurse routes))))))
      (recurse (cdr (assoc :routes toc))))
    (nreverse paths)))

(define-sub-command react.docs.gen (args)
  "Generate API reference docs for React from react.dev.
   Usage: hactar react.docs.gen [--format <fmt>] [--output <file>] [--process]"
  (let* ((format-opt (or (getf args :format) "markdown"))
         ;; Version is currently unused as we only support latest/main
         (version-opt (or (getf args :version) "latest"))
         (output-file (getf args :output))
         (process-flag (getf args :process))
         (toc-url "https://raw.githubusercontent.com/reactjs/react.dev/main/src/sidebarReference.json")
         (base-content-url "https://raw.githubusercontent.com/reactjs/react.dev/main/src/content"))

    (unless *silent* (format t "Generating docs for version: ~A~%" version-opt))
    (unless *silent* (format t "Fetching TOC from ~A...~%" toc-url))
    (let ((toc-json-str (fetch-url-content toc-url)))
      (if toc-json-str
          (let* ((toc (cl-json:decode-json-from-string toc-json-str))
                 (paths (extract-react-doc-paths toc))
                 (chunks '()))

            (unless *silent* (format t "Found ~A pages. Fetching content...~%" (length paths)))

            (dolist (path paths)
              (let ((url (format nil "~A~A.md" base-content-url path)))
                (unless *silent* (format t "Fetching ~A...~%" path))
                (let ((content (fetch-url-content url)))
                  (if content
                      (push content chunks)
                      (unless *silent* (format t "Warning: Failed to fetch ~A~%" url))))))
            
            (setf chunks (nreverse chunks))

            (if process-flag
                (process-docs-with-llm chunks format-opt output-file)
                (let ((markdown-content (str:join (format nil "~%~%---~%~%") chunks)))
                  (cond
                    ((string= format-opt "markdown")
                     (output-docs-markdown markdown-content output-file))
                    (t
                     (output-docs-converted markdown-content format-opt output-file))))))
          (format t "Error: Failed to fetch TOC.~%"))))
  :cli-options ((:long "format" :description "Output format (default: markdown)")
                (:long "version" :description "Version of docs (default: latest)")
                (:long "output" :description "Output file")
                (:long "process" :flag t :description "Use LLM to process/convert content")
                (:short "h" :long "help" :description "Show help")))

(defdoc "Latest React Docs" :source "hactar:docsets/react.19.2.3.org" :version "latest")
