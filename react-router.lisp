;;* React Router Mode
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

;;* mode
(defmode react-router
  "React Router navigation and routing conventions."
  :when (member "react-router" *stack* :test #'string-equal)
  :rules "When working with React Router:
- Use createBrowserRouter for route configuration
- Implement loaders for data fetching (runs before render)
- Implement actions for form submissions and mutations
- Use errorElement for route-level error boundaries
- Prefer <Form> over <form> for automatic form handling
- Use useLoaderData() to access loader data
- Use useActionData() to access action results
- Use defer() for streaming/suspense data loading"
  :commands
  ((defmode-command react-router "route.create" (args)
     "Generate a React Router route. Usage: /react-router.route.create <Name>"
     (if (null args)
         (format t "Usage: /react-router.route.create <Name>~%")
         (let* ((name (first args))
                (class-name (pascal-case name))
                (file-name (format nil "src/routes/~A.tsx" class-name))
                (content (format nil "import React from 'react';~%import { useLoaderData } from 'react-router-dom';~%~%export const loader = async () => {~%  return { message: 'Hello from loader!' };~%};~%~%export const ~ARoute: React.FC = () => {~%  const data = useLoaderData() as { message: string };~%  return <div>{data.message}</div>;~%};~%"
                                 class-name)))
           (scaffold-create-file file-name content)
           (scaffold-modify-file
            "src/routes.tsx"
            "// ROUTE_IMPORTS"
            (format nil "// ROUTE_IMPORTS~%import { ~ARoute } from './routes/~A';"
                    class-name class-name)))))

   (defmode-command react-router "page.create" (args)
     "Generate a React Router page component. Usage: /react-router.page.create <Name>"
     (if (null args)
         (format t "Usage: /react-router.page.create <Name>~%")
         (let* ((name (first args))
                (class-name (pascal-case name))
                (file-name (format nil "src/pages/~A.tsx" class-name))
                (content (format nil "import React from 'react';~%~%export const ~APage: React.FC = () => {~%  return <div>~APage</div>;~%};~%"
                                 class-name class-name)))
           (scaffold-create-file file-name content))))

   (defmode-command react-router "error-boundary.create" (args)
     "Generate a React Router error boundary. Usage: /react-router.error-boundary.create"
     (identity args)
     (let* ((file-name "src/components/RouterErrorBoundary.tsx")
            (content (format nil "import React from 'react';~%import { useRouteError } from 'react-router-dom';~%~%export const RouterErrorBoundary: React.FC = () => {~%  const error = useRouteError() as any;~%  console.error(error);~%  return <div>Route Error: {error?.message || 'Something went wrong'}</div>;~%};~%")))
       (scaffold-create-file file-name content)))))

;;* rules
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

;;* helpers
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

;;* commands
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
