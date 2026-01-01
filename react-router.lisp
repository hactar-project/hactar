;; React Router Mode
(in-package :hactar)

;;* React Router Detection

(def-analyzer react-router-dependency ((*package-json-analyzed-hook*)) nil (metadata)
  "Checks for React Router dependency and updates the stack."
  (let* ((deps (mapcar (lambda (dep) (string-downcase (string (car dep))))
                       (gethash "dependencies" metadata)))
         (dev-deps (mapcar (lambda (dep) (string-downcase (string (car dep))))
                           (gethash "dev-dependencies" metadata))))
    (when (or (member "react-router" deps :test #'string=)
              (member "react-router-dom" deps :test #'string=)
              (member "react-router" dev-deps :test #'string=)
              (member "react-router-dom" dev-deps :test #'string=))
      (debug-log "React Router detected, adding to stack.")
      (add-to-stack "react-router"))))

;;* React Router Generators
(defgenerator frameworkroute
  "Generate a React Router route with loader and action"
  :hooks ((*stack-changed-hook* 
           (lambda (stack) (member "react-router" stack :test #'string-equal))))
  :priority 10
  :args (path &key method handler middleware)
  :operations
  ((:create :file "src/routes/${name}.tsx"
            :template "react-router/route.tsx.mustache")
   (:modify :file "src/routes.tsx"
            :search "// ROUTE_IMPORTS"
            :replace "// ROUTE_IMPORTS\nimport { ${name}Route } from './routes/${name}';")))

(defgenerator page
              "Generate a React Router page component with loader"
              ;; this line causes an error
              :hooks ((*stack-changed-hook*
                       (lambda (stack) (member "react-router" stack :test #'string-equal))))
              :priority 10
              :args (path &key layout)
              :operations
              ((:create :file "src/pages/${name}.tsx"
                        :template "react-router/page.tsx.mustache")))

;;* React Router Entity Implementations (BOT System)
(defskill :react-router-route-patterns
  "React Router patterns and conventions for routes"
  :instructions "When working with React Router:
- Use createBrowserRouter for route configuration
- Implement loaders for data fetching (runs before render)
- Implement actions for form submissions and mutations
- Use errorElement for route-level error boundaries
- Prefer <Form> over <form> for automatic form handling
- Use useLoaderData() to access loader data
- Use useActionData() to access action results
- Use defer() for streaming/suspense data loading")

(defskill :create/react-router-route-skill
  "Specific instructions for React Router route creation"
  :instructions "When creating a new route:
1. Create route file in src/routes/<name>.tsx
2. Export a route configuration object with path, element, loader, action
3. Import and add to router configuration in src/routes.tsx
4. If route needs data, implement loader function
5. If route handles forms, implement action function")

(defframeworkroute react-router-route
  "Generate a React Router route with loader and action"
  :priority 10
  :when (member "react-router" *stack* :test #'string-equal)
  :options
  ((:long "name" :short "n" :description "Route name" :required t)
   (:long "path" :short "p" :description "URL path")
   (:long "loader" :short "l" :description "Include data loader" :flag t)
   (:long "action" :short "a" :description "Include form action" :flag t))
  :skills (:react-router-route-patterns :create/react-router-route-skill)
  :schema-additions
  ((:loader :type string)
   (:action :type string)
   (:error-element :type string))
  :create
  (lambda (name &rest args)
    (let ((path (or (getf args :path) (format nil "/~A" (kebab-case name)))))
      (format t "~&Creating React Router route: ~A at ~A~%" name path)))
  :list
  (lambda (&rest args)
    (declare (ignore args))
    (find-entity-instances 'route))
  :delete
  (lambda (name &rest args)
    (declare (ignore args))
    (let ((instances (find-entity-instances 'route)))
      (dolist (i instances)
        (when (string-equal (entity-get i :name) name)
           (delete-entity-instance 'route (entity-instance-id i)))))))

;;* React Router Patterns
(defpattern error-boundary
  "React Router errorElement integration"
  :when (member "react-router" *stack* :test #'string-equal)
  :priority 15
  :operations
  ((:create :file "src/components/RouteErrorBoundary.tsx"
            :template "react-router/error-boundary.tsx.mustache")))

;;* React Router Analyzer

(define-framework-analyzer react-router
  "React Router framework analyzer"
  :detect (member "react-router" *stack* :test #'string-equal)
  :queries
  (list
   (cons :routes
         (lambda ()
           "Find all route definitions"
           (code/query :matches "createBrowserRouter\\|createRoutesFromElements\\|Route"
                       :in "**/*.tsx")))
   
   (cons :loaders
         (lambda ()
           "Find route loader functions"
           (code/query :matches "loader:\\s*async\\|export async function loader"
                       :in "**/*.tsx")))
   
   (cons :actions
         (lambda ()
           "Find route action functions"
           (code/query :matches "action:\\s*async\\|export async function action"
                       :in "**/*.tsx")))))

;;* React Router Rule

(defrule react-router-rule (*stack-changed-hook*) (stack)
  "Adds React Router-specific instructions when React Router is detected."
  (when (member "react-router" stack :test #'string-equal)
    "When working with React Router:
- Use createBrowserRouter for route configuration
- Implement loaders for data fetching (runs before render)
- Implement actions for form submissions and mutations
- Use errorElement for route-level error boundaries
- Prefer <Form> over <form> for automatic form handling
- Use useLoaderData() to access loader data
- Use useActionData() to access action results
- Use defer() for streaming/suspense data loading"))

;;* React Router Query Shortcuts

(defun react-router/routes ()
  "Query React Router route definitions. Zero LLM."
  (let ((analyzer (get-analyzer 'react-router)))
    (when analyzer
      (let ((query-fn (cdr (assoc :routes (framework-analyzer-queries analyzer)))))
        (when query-fn (funcall query-fn))))))

(defun react-router/loaders ()
  "Query React Router loader functions. Zero LLM."
  (let ((analyzer (get-analyzer 'react-router)))
    (when analyzer
      (let ((query-fn (cdr (assoc :loaders (framework-analyzer-queries analyzer)))))
        (when query-fn (funcall query-fn))))))

(defun react-router/actions ()
  "Query React Router action functions. Zero LLM."
  (let ((analyzer (get-analyzer 'react-router)))
    (when analyzer
      (let ((query-fn (cdr (assoc :actions (framework-analyzer-queries analyzer)))))
        (when query-fn (funcall query-fn))))))

;;* Documentation Helpers

(defun extract-frontmatter-title (content)
  "Extract title from YAML frontmatter."
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "(?s)^---\\s*\\n.*?title:\\s*(.*?)\\n.*?---" content)
    (if match
        (string-trim '(#\Space #\Return #\Newline) (aref regs 0))
        nil)))

(defun strip-frontmatter (content)
  "Remove YAML frontmatter from content."
  (cl-ppcre:regex-replace "(?s)^---\\s*\\n.*?---\\s*\\n" content ""))

(define-sub-command react-router.docs.gen (args)
  "Generate API reference docs for React Router from GitHub.
   Usage: hactar react-router.docs.gen [--format <fmt>] [--output <file>] [--process]"
  (let* ((format-opt (or (getf args :format) "markdown"))
         (output-file (getf args :output))
         (process-flag (getf args :process))
         (user "remix-run")
         (repo "react-router")
         (base-path "docs/api")
         (folder-contents '()))

    (unless *silent* (format t "Fetching React Router docs from ~A/~A/~A...~%" user repo base-path))
    
    (let ((repo-dir (fetch-github-repo user repo)))
      (if repo-dir
          (let* ((base-dir (merge-pathnames (uiop:ensure-directory-pathname base-path) repo-dir))
                 (subdirs (uiop:subdirectories base-dir)))
            
            (dolist (subdir subdirs)
              (let* ((folder-name (car (last (pathname-directory subdir))))
                     (index-file (merge-pathnames "index.md" subdir))
                     (files (uiop:directory-files subdir))
                     (title folder-name) ; Default to folder name
                     (folder-text ""))
                
                (when (probe-file index-file)
                  (let ((content (uiop:read-file-string index-file)))
                    (let ((fm-title (extract-frontmatter-title content)))
                      (when fm-title (setf title fm-title))
                      (setf folder-text (format nil "# ~A~%~%~A" title (strip-frontmatter content))))))
                
                (dolist (file files)
                  (let ((fname (file-namestring file)))
                    (when (and (str:ends-with-p ".md" fname)
                               (not (string= fname "index.md")))
                      (let ((content (uiop:read-file-string file)))
                        (setf folder-text (format nil "~A~%~%~A" 
                                                  folder-text 
                                                  (strip-frontmatter content)))))))
                
                (when (> (length folder-text) 0)
                  (push folder-text folder-contents))))

            (setf folder-contents (nreverse folder-contents))
            
            (if process-flag
                (process-docs-with-llm folder-contents format-opt output-file)
                (let ((markdown-content (str:join (format nil "~%~%---~%~%") folder-contents)))
                  (cond
                    ((string= format-opt "markdown")
                     (output-docs-markdown markdown-content output-file))
                    (t
                     (output-docs-converted markdown-content format-opt output-file))))))
          (format t "Error: Failed to fetch docs from GitHub.~%"))))
  :cli-options ((:long "format" :description "Output format (default: markdown)")
                (:long "output" :description "Output file")
                (:long "process" :flag t :description "Use LLM to process/convert content")
                (:short "h" :long "help" :description "Show help")))
(defdoc "Latest React Router Docs" :source "hactar:docsets/react-router.7.0.0.org" :version "latest")
