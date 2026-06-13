;;* React Mode
(in-package :hactar)

;;** Analyzers

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

;;** Mode
(defmode react
  "React functional component and hooks development mode."
  :when (member "react" *stack* :test #'string-equal)
  :rules "When working with React components:
- Prefer functional components and hooks (useState, useEffect, useCallback, useMemo, etc.) over class components.
- Use JSX for templating.
- Follow standard React coding conventions and best practices.
- Use TypeScript for type safety when .tsx/.ts files are present.
- Prefer composition over inheritance.
- Use React.memo() for expensive pure components.
- Keep components small and focused (single responsibility).
- Lift state up only when necessary; prefer local state and context.
- Use custom hooks to extract reusable logic.
- Always provide a key prop when rendering lists.
- Use Error Boundaries for graceful error handling.
- Prefer controlled components for form inputs."
  :commands
  ((defmode-command react "component.create" (args)
     "Generate a React functional component. Usage: /react.component.create <Name>"
     (if (null args)
         (format t "Usage: /react.component.create <Name>~%")
         (let* ((name (first args))
                (class-name (pascal-case name))
                (file-name (format nil "src/components/~A.tsx" class-name))
                (content (format nil "import React from 'react';~%~%export interface ~AProps {~%  // props~%}~%~%export const ~A: React.FC<~AProps> = () => {~%  return (~%    <div>~%      ~A~%    </div>~%  );~%};~%"
                                 class-name class-name class-name class-name)))
           (scaffold-create-file file-name content))))

   (defmode-command react "hook.create" (args)
     "Generate a custom React hook. Usage: /react.hook.create <Name>"
     (if (null args)
         (format t "Usage: /react.hook.create <Name>~%")
         (let* ((name (first args))
                (hook-name (format nil "use~A" (pascal-case name)))
                (file-name (format nil "src/hooks/~A.ts" hook-name))
                (content (format nil "import { useState, useEffect } from 'react';~%~%export const ~A = () => {~%  return {};~%};~%"
                                 hook-name)))
           (scaffold-create-file file-name content))))

   (defmode-command react "test.create" (args)
     "Generate component tests with Testing Library. Usage: /react.test.create <Target>"
     (if (null args)
         (format t "Usage: /react.test.create <Target>~%")
         (let* ((name (first args))
                (class-name (pascal-case name))
                (file-name (format nil "src/components/~A.test.tsx" class-name))
                (content (format nil "import React from 'react';~%import { render, screen } from '@testing-library/react';~%import { ~A } from './~A';~%~%describe('~A', () => {~%  it('renders without crashing', () => {~%    render(<~A />);~%  });~%});~%"
                                 class-name class-name class-name class-name)))
           (scaffold-create-file file-name content))))

   (defmode-command react "error-boundary.create" (args)
     "Generate a React Error Boundary component. Usage: /react.error-boundary.create"
     (identity args)
     (let* ((file-name "src/components/ErrorBoundary.tsx")
            (content (format nil "import React, { Component, ErrorInfo, ReactNode } from 'react';~%~%interface Props {~%  children: ReactNode;~%  fallback?: ReactNode;~%}~%~%interface State {~%  hasError: boolean;~%}~%~%export class ErrorBoundary extends Component<Props, State> {~%  public state: State = {~%    hasError: false~%  };~%~%  public static getDerivedStateFromError(_: Error): State {~%    return { hasError: true };~%  }~%~%  public componentDidCatch(error: Error, errorInfo: ErrorInfo) {~%    console.error('Uncaught error:', error, errorInfo);~%  }~%~%  public render() {~%    if (this.state.hasError) {~%      return this.props.fallback || <h1>Sorry, something went wrong.</h1>;~%    }~%    return this.props.children;~%  }~%}~%")))
       (scaffold-create-file file-name content)))

   (defmode-command react "auth-context.create" (args)
     "Generate AuthContext provider and useAuth hook for React. Usage: /react.auth-context.create"
     (identity args)
     (let* ((ctx-file "src/auth/AuthContext.tsx")
            (ctx-content (format nil "import React, { createContext, useState, useContext, ReactNode } from 'react';~%~%interface AuthContextType {~%  user: any;~%  login: (userData: any) => void;~%  logout: () => void;~%  isAuthenticated: boolean;~%}~%~%const AuthContext = createContext<AuthContextType | undefined>(undefined);~%~%export const AuthProvider: React.FC<{ children: ReactNode }> = ({ children }) => {~%  const [user, setUser] = useState<any>(null);~%  const login = (userData: any) => setUser(userData);~%  const logout = () => setUser(null);~%  return (~%    <AuthContext.Provider value={{ user, login, logout, isAuthenticated: !!user }}>~%      {children}~%    </AuthContext.Provider>~%  );~%};~%"))
            (hook-file "src/auth/useAuth.ts")
            (hook-content (format nil "import { useContext } from 'react';~%import { AuthContext } from './AuthContext';~%export const useAuth = () => {~%  const context = useContext(AuthContext);~%  if (!context) throw new Error('useAuth must be used within an AuthProvider');~%  return context;~%};~%")))
       (scaffold-create-file ctx-file ctx-content)
       (scaffold-create-file hook-file hook-content)))

   (defmode-command react "auth-middleware.create" (args)
     "Generate auth middleware for Express/Node. Usage: /react.auth-middleware.create"
     (identity args)
     (let* ((file-name "src/server/middleware/auth.ts")
            (content (format nil "import { Request, Response, NextFunction } from 'express';~%~%export const authMiddleware = (req: Request, res: Response, next: NextFunction) => {~%  const authHeader = req.headers.authorization;~%  if (!authHeader) return res.status(401).json({ error: 'Unauthorized' });~%  next();~%};~%")))
       (scaffold-create-file file-name content)))

   (defmode-command react "protected-route.create" (args)
     "Generate React ProtectedRoute component. Usage: /react.protected-route.create"
     (identity args)
     (let* ((file-name "src/components/ProtectedRoute.tsx")
            (content (format nil "import React from 'react';~%import { Navigate } from 'react-router-dom';~%import { useAuth } from '../auth/useAuth';~%~%export const ProtectedRoute: React.FC<{ children: React.ReactNode }> = ({ children }) => {~%  const { isAuthenticated } = useAuth();~%  if (!isAuthenticated) return <Navigate to='/login' replace />;~%  return <>{children}</>;~%};~%")))
       (scaffold-create-file file-name content)))

   (defmode-command react "auth-guard.create" (args)
     "Client-side route guard using AuthContext. Usage: /react.auth-guard.create"
     (identity args)
     (let* ((file-name "src/components/AuthGuard.tsx")
            (content (format nil "import React from 'react';~%import { useAuth } from '../auth/useAuth';~%~%export const AuthGuard: React.FC<{ children: React.ReactNode }> = ({ children }) => {~%  const { isAuthenticated } = useAuth();~%  if (!isAuthenticated) return <div>Access Denied</div>;~%  return <>{children}</>;~%};~%")))
       (scaffold-create-file file-name content)))

   (defmode-command react "token-refresh.create" (args)
     "Automatic token refresh with interceptor. Usage: /react.token-refresh.create"
     (identity args)
     (let* ((file-name "src/auth/tokenRefresh.ts")
            (content (format nil "export const setupTokenRefresh = () => {~%  console.log('Token refresh initialized');~%};~%")))
       (scaffold-create-file file-name content)))))

;;** Rules
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
                 (doc-contents '()))
            (unless *silent* (format t "Fetched TOC containing ~A doc paths.~%" (length paths)))
            (dolist (path paths)
              (let ((full-url (format nil "~A/~A.md" base-content-url path)))
                (unless *silent* (format t "Fetching doc: ~A...~%" path))
                (let ((doc-content (fetch-url-content full-url)))
                  (when doc-content
                    (push doc-content doc-contents)))))

            (setf doc-contents (nreverse doc-contents))

            (if process-flag
                (process-docs-with-llm doc-contents format-opt output-file)
                (let ((markdown-content (str:join (format nil "~%~%---~%~%") doc-contents)))
                  (cond
                    ((string= format-opt "markdown")
                     (output-docs-markdown markdown-content output-file))
                    (t
                     (output-docs-converted markdown-content format-opt output-file))))))
          (format t "Error: Failed to fetch React docs TOC.~%")))))

;;** Hooks
;; Detects React-related content in proxy requests and injects React-specific rules.

(defun react-proxy-rules-text ()
  "Return the React rules text to inject into proxy requests."
  "When working with React components:
- Prefer functional components and hooks (useState, useEffect, useCallback, useMemo, etc.) over class components.
- Use JSX for templating.
- Follow standard React coding conventions and best practices.
- Use TypeScript for type safety when .tsx/.ts files are present.
- Prefer composition over inheritance.
- Use React.memo() for expensive pure components.
- Keep components small and focused (single responsibility).
- Lift state up only when necessary; prefer local state and context.
- Use custom hooks to extract reusable logic.
- Always provide a key prop when rendering lists.
- Use Error Boundaries for graceful error handling.
- Prefer controlled components for form inputs.")

(defun proxy-react-hook (request-plist)
  "Proxy hook handler: detect React in request and inject React rules.
   Detection checks: stack, message content mentions of React/JSX/hooks,
   file extensions (.tsx, .jsx), and model context."
  (let* ((messages (getf request-plist :messages))
         (system-prompt (getf request-plist :system-prompt))
         (react-detected nil))

    ;; Check 1: React is in the stack
    (when (member "react" *stack* :test #'string-equal)
      (setf react-detected t))

    ;; Check 2: Messages mention React-related terms
    (unless react-detected
      (when (proxy-messages-contain-p messages
                                       "react" "jsx" "tsx" "usestate" "useeffect"
                                       "usecallback" "usememo" "useref" "usecontext"
                                       "react-dom" "next.js" "nextjs" "remix"
                                       "component" ".tsx" ".jsx")
        (setf react-detected t)))

    ;; Check 3: Files in context include React files
    (unless react-detected
      (when (some (lambda (f)
                    (let ((ext (pathname-type (pathname f))))
                      (or (string-equal ext "tsx")
                          (string-equal ext "jsx"))))
                  *files*)
        (setf react-detected t)))

    ;; Check 4: System prompt already mentions React
    (unless react-detected
      (when (and system-prompt
                 (search "react" (string-downcase system-prompt)))
        (setf react-detected t)))

    ;; Inject React rules if detected
    (when react-detected
      (debug-log "Proxy: React detected, injecting React rules.")
      (let* ((react-rules (react-proxy-rules-text))
             (current-sys (or (getf request-plist :system-prompt) ""))
             ;; Don't double-inject if rules already present
             (already-injected (search "Prefer functional components and hooks" current-sys)))
        (unless already-injected
          (setf (getf request-plist :system-prompt)
                (if (> (length current-sys) 0)
                    (format nil "~A~%~%## React Development Rules~%~%~A" current-sys react-rules)
                    (format nil "## React Development Rules~%~%~A" react-rules))))))

    request-plist))

;; Register the React proxy hook
(nhooks:add-hook *proxy-request-hook*
                 (make-instance 'nhooks:handler
                                :fn #'proxy-react-hook
                                :name 'proxy-react-hook))

;; Register dynamic route for react-mode hypertext
(register-protocol-route "docs" "^js/react/([^/]+)/dynamic$"
  (lambda (version)
    (list :content (format nil "# React ~A Dynamic Documentation~%Generated dynamically for React Mode!" version)
          :title "React Dynamic Docs"
          :tags '("react" "dynamic"))))
