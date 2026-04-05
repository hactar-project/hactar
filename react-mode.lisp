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

;;** Auth Skills for React Features

(defskill :auth-patterns
  "Authentication and authorization patterns for web apps"
  :instructions "When implementing authentication:

## Password Handling
- ALWAYS hash passwords with bcrypt, minimum 12 rounds
- NEVER store plaintext passwords anywhere (DB, logs, error messages)
- Use constant-time comparison for password verification

## Token Management
- Use short-lived access tokens (15 min) with long-lived refresh tokens (7 days)
- Store access tokens in memory (JS variable), NEVER in localStorage
- Store refresh tokens in httpOnly, Secure, SameSite=Strict cookies
- Implement token rotation: issue new refresh token on each refresh
- Invalidate refresh tokens on logout (server-side blocklist or DB flag)

## Session Security
- Generate cryptographically random session IDs (min 128 bits entropy)
- Implement CSRF protection on all state-changing endpoints
- Set appropriate CORS headers (restrict origins)
- Use Secure and HttpOnly flags on all auth cookies

## Route Protection
- Protect routes server-side, NEVER rely solely on client-side guards
- Return 401 for unauthenticated, 403 for unauthorized
- Implement role-based access control (RBAC) via middleware

## Error Handling
- Use generic error messages for auth failures ('Invalid credentials')
- Do NOT reveal whether email/username exists on login failure
- Rate-limit login attempts (e.g., 5 per minute per IP)
- Log failed auth attempts for monitoring"
  :rules "Always hash passwords with bcrypt. Never store tokens in localStorage. Use httpOnly cookies.")

(defskill :react-context-patterns
  "React Context API patterns for global state management"
  :instructions "When using React Context for state management:

## Context Structure
- Create a dedicated context file per domain (AuthContext, ThemeContext, etc.)
- Export both the Provider component and a custom hook (useAuth, useTheme, etc.)
- NEVER export the raw context object; always use the hook for consumption
- Throw an error in the hook if used outside the Provider

## Provider Pattern
- Place Providers at the appropriate level (not always app root)
- Compose multiple providers using a single AppProviders wrapper
- Keep Provider components focused: one concern per context
- Initialize state from external sources in useEffect, not in the default value

## Performance
- Split frequently-changing values from rarely-changing ones into separate contexts
- Memoize context values with useMemo to prevent unnecessary re-renders
- Memoize callback functions passed via context with useCallback
- Consider using useReducer for complex state transitions

## TypeScript
- Define explicit types for context value (never use 'any' or 'undefined | T' without null check)
- Use a factory pattern: createContext<T | null>(null) + hook with null guard
- Export the type interface for consumers that need to type their props"
  :rules "Always provide a custom hook. Never export raw context. Memoize context values.")

(defskill :oauth-patterns
  "OAuth 2.0 / OpenID Connect integration patterns"
  :instructions "When implementing OAuth flows:

## Authorization Code Flow (recommended for web apps)
1. Redirect user to provider's authorize URL with state + PKCE code_challenge
2. Provider redirects back with authorization code
3. Exchange code for tokens server-side (NEVER client-side)
4. Store access/refresh tokens encrypted at rest

## Security
- ALWAYS use the 'state' parameter to prevent CSRF (random, per-session, verified on callback)
- ALWAYS use PKCE (code_verifier / code_challenge) even for server-side apps
- Request minimum necessary scopes
- Validate the id_token signature and claims (iss, aud, exp, nonce)
- Store OAuth tokens encrypted at rest (use AES-256-GCM or similar)

## Token Lifecycle
- Implement automatic token refresh before expiry
- Handle token revocation on logout (call provider's revoke endpoint)
- Clear all stored tokens on disconnect/unlink

## Account Linking
- Match OAuth accounts to existing users by email (with email verification check)
- Allow users to link/unlink multiple OAuth providers
- Store provider + provider_user_id as unique key"
  :rules "Always use state parameter for CSRF. Always use PKCE. Exchange codes server-side only.")

;;** React-specific Auth Generators

(defgenerator auth-context
  "Generate AuthContext provider and useAuth hook for React"
  :when (member "react" *stack* :test #'string-equal)
  :priority 15
  :args (name &key user-type)
  :operations
  ((:create :file "src/auth/AuthContext.tsx"
            :template "react/auth-context.tsx.mustache")
   (:create :file "src/auth/useAuth.ts"
            :template "react/use-auth.ts.mustache")))

(defgenerator auth-middleware
  "Generate Express/Node auth middleware for JWT verification"
  :when (member "react" *stack* :test #'string-equal)
  :priority 10
  :args (name &key strategy)
  :operations
  ((:create :file "src/server/middleware/auth.ts"
            :template "node/auth-middleware.ts.mustache")))

(defgenerator protected-route
  "Generate React ProtectedRoute wrapper component"
  :when (member "react" *stack* :test #'string-equal)
  :priority 15
  :args (name &key role-field)
  :operations
  ((:create :file "src/components/ProtectedRoute.tsx"
            :template "react/protected-route.tsx.mustache")))

;;** React Auth Patterns

(defpattern auth-guard
  "Client-side route guard using AuthContext"
  :when (member "react" *stack* :test #'string-equal)
  :priority 15
  :operations
  ((:create :file "src/components/AuthGuard.tsx"
            :template "react/auth-guard.tsx.mustache")))

(defpattern token-refresh
  "Automatic token refresh with interceptor"
  :when (member "react" *stack* :test #'string-equal)
  :priority 15
  :operations
  ((:create :file "src/auth/tokenRefresh.ts"
            :template "react/token-refresh.ts.mustache")))

;;** React Proxy Hook
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
         (model (getf request-plist :model))
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

;;** Auth Feature Definition

(deffeature auth
  "Authentication and authorization for React apps"
  :variants
  ((:default
    :description "JWT auth with React Context"
    :skills (:auth-patterns :react-context-patterns)
    :rules "For React authentication:
- Use AuthContext with useAuth hook
- Wrap protected routes with ProtectedRoute component
- Store tokens in httpOnly cookies, never localStorage
- Implement automatic token refresh"
    :entities
    ((:type component :name "AuthProvider" :tags (:auth :context))
     (:type component :name "ProtectedRoute" :tags (:auth :routing))
     (:type component :name "LoginForm" :tags (:auth :form :public))
     (:type frameworkroute :name "/login" :tags (:auth :public))
     (:type frameworkroute :name "/register" :tags (:auth :public))
     (:type frameworkroute :name "/dashboard" :tags (:auth :protected)))
    :generators (:auth-context :auth-middleware :protected-route)
    :patterns (:auth-guard :token-refresh)
    :packages ("jwt-decode" "bcrypt" "jsonwebtoken")
    :tags (:authenticated :protected))

   (:github
    :description "GitHub OAuth for React"
    :inherits :default
    :skills (:oauth-patterns)
    :rules "For GitHub OAuth:
- Store OAuth tokens encrypted at rest
- Request minimum necessary scopes (read:user, user:email)
- Implement token refresh flow
- Use state parameter to prevent CSRF"
    :entities
    ((:type component :name "GitHubLoginButton" :tags (:auth :oauth))
     (:type frameworkroute :name "/auth/github" :method :get :tags (:auth :oauth :public))
     (:type frameworkroute :name "/auth/github/callback" :method :get :tags (:auth :oauth :public)))
    :packages ("@octokit/auth-oauth-app")
    :env (("GITHUB_CLIENT_ID" . "Your GitHub OAuth App client ID")
          ("GITHUB_CLIENT_SECRET" . "Your GitHub OAuth App client secret")))

   (:google
    :description "Google OAuth integration"
    :inherits :default
    :skills (:oauth-patterns)
    :packages ("passport-google-oauth20")
    :env (("GOOGLE_CLIENT_ID" . "Google OAuth client ID")
          ("GOOGLE_CLIENT_SECRET" . "Google OAuth client secret")))))

;;** Shopping Feature

(deffeature shopping
  "E-commerce / shopping cart functionality"
  :variants
  ((:default
    :description "Basic shopping cart with product catalog"
    :rules "For shopping features:
- Use optimistic UI updates for cart operations
- Validate stock availability server-side before checkout
- Implement idempotent payment processing
- Store cart in both client state and server session"
    :entities
    ((:type model :name "product"
      :fields ((:name :string) (:price :number) (:description :string)
               (:image-url :string) (:stock :integer) (:category :string)))
     (:type model :name "cart-item"
      :fields ((:product-id :string) (:quantity :integer) (:user-id :string)))
     (:type model :name "order"
      :fields ((:user-id :string) (:items :list) (:total :number)
               (:status :keyword) (:created-at :timestamp)))
     (:type frameworkroute :name "/api/products" :method :get :tags (:shop :public))
     (:type frameworkroute :name "/api/products/:id" :method :get :tags (:shop :public))
     (:type frameworkroute :name "/api/cart" :method :get :tags (:shop :protected))
     (:type frameworkroute :name "/api/cart/add" :method :post :tags (:shop :protected))
     (:type frameworkroute :name "/api/cart/remove" :method :delete :tags (:shop :protected))
     (:type frameworkroute :name "/api/checkout" :method :post :tags (:shop :protected))
     (:type frameworkroute :name "/api/orders" :method :get :tags (:shop :protected))
     (:type component :name "ProductList" :tags (:shop :catalog))
     (:type component :name "ProductCard" :tags (:shop :catalog))
     (:type component :name "CartDrawer" :tags (:shop :cart))
     (:type component :name "CheckoutForm" :tags (:shop :checkout)))
    :packages ("stripe"))

   (:stripe
    :description "Stripe payment integration"
    :inherits :default
    :rules "For Stripe integration:
- Use Stripe Elements for PCI compliance
- Always verify webhook signatures
- Implement idempotency keys for payment intents"
    :entities
    ((:type frameworkroute :name "/api/stripe/webhook" :method :post :tags (:shop :stripe :public))
     (:type frameworkroute :name "/api/stripe/create-intent" :method :post :tags (:shop :stripe :protected)))
    :packages ("@stripe/stripe-js" "@stripe/react-stripe-js")
    :env (("STRIPE_SECRET_KEY" . "Stripe secret key")
          ("STRIPE_PUBLISHABLE_KEY" . "Stripe publishable key")
          ("STRIPE_WEBHOOK_SECRET" . "Stripe webhook signing secret")))))
