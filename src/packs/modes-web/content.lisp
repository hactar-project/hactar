(in-package :hactar-user)

(defmode nextjs
  :priority 15
  :stack ("typescript" "react" "nextjs")
  :detect (:any (:file-contains ("package.json" "\"next\""))
                (:file-exists "next.config.js")
                (:file-exists "next.config.mjs")
                (:file-exists "next.config.ts"))
  :stack-commands (:test "npx vitest run" :lint "npx next lint"
                   :typecheck "npx tsc --noEmit" :build "npx next build")
  :forms ((deftool test (&optional file)
            "Run unit tests for the current stack."
            (run-test-cmd file))
          (deftool lint ()
            "Lint the project."
            (run-lint-cmd))
          (deftool typecheck ()
            "Type-check the project."
            (run-typecheck-cmd))
          (deftool ast-edit (path &rest operations)
            "Edit TypeScript/JavaScript via AST (fern backend)."
            (apply #'run-ast-edit path operations))
          (defskill nextjs :doc "Next.js App Router conventions." :docs ("nextjs.org"))
          (defsandbox project :allow ("npx" "node" "pnpm" "yarn" "git"))
          (defaction page.create
            :doc "Create a Next.js App Router page."
            :prompt "Create a Next.js App Router page {{name}} under app/, with a typed server component. {{doc:nextjs}}"
            :apply :create)
          (defaction route.create
            :doc "Create a Next.js route handler."
            :prompt "Create a Next.js route handler {{name}} under app/api using typed Request/Response."
            :apply :create)
          (defagent dev
            :model "anthropic/claude-sonnet-4-6"
            :tools (test lint typecheck ast-edit)
            :skills (nextjs)
            :sandbox project
            :instructions "Build Next.js features end-to-end. Use (test) to verify. Edit TypeScript through ast-edit when possible.")))

(defmode typescript-react-router
  :priority 20
  :stack ("typescript" "react" "react-router" "vite")
  :detect (:file-contains ("package.json" "react-router"))
  :stack-commands (:test "npx vitest run" :lint "npx biome check ."
                   :typecheck "npx tsc --noEmit" :build "npm run build")
  :forms ((deftool test (&optional file)
            "Run unit tests for the current stack."
            (run-test-cmd file))
          (deftool lint ()
            "Lint the project."
            (run-lint-cmd))
          (deftool typecheck ()
            "Type-check the project."
            (run-typecheck-cmd))
          (deftool ast-edit (path &rest operations)
            "Edit TypeScript/JavaScript via AST."
            (apply #'run-ast-edit path operations))
          (deftool rr-routes ()
            "List the route tree."
            (shell-cmd "npx react-router routes" :sandbox "project"))
          (defskill react-router :doc "React Router project conventions."
            :docs ("react-router.org"))
          (defsandbox project :allow ("npx" "node" "git"))
          (defaction route.create
            :doc "Create a typed React Router route + loader/action."
            :prompt "Create a React Router route {{name}} under src/routes using a typed loader and action, then register it in src/routes.tsx. {{doc:react-router}}"
            :apply :create)
          (defaction api.create
            :doc "Create a typed API resource route."
            :prompt "Create an API resource route {{name}} with typed handlers for this {{stack}} project."
            :apply :create)
          (defagent dev
            :model "anthropic/claude-sonnet-4-6"
            :tools (test lint typecheck rr-routes ast-edit)
            :skills (react-router)
            :sandbox project
            :instructions "Build React Router features end-to-end. Use (test) to verify. Edit TypeScript through ast-edit when possible.")))

(defmode redwood
  :priority 22
  :stack ("typescript" "react" "redwood")
  :detect (:file-contains ("package.json" "rwsdk"))
  :stack-commands (:test "npm test" :typecheck "npx tsc --noEmit" :build "npm run build")
  :forms ((deftool test (&optional file)
            "Run RedwoodSDK tests."
            (run-test-cmd file))
          (defskill redwood :doc "RedwoodSDK Worker + React Server Component conventions.")
          (defsandbox project :allow ("npx" "node" "npm" "git"))
          (defagent dev
            :model "anthropic/claude-sonnet-4-6"
            :tools (test) :skills (redwood) :sandbox project
            :instructions "Build RedwoodSDK routes and components cleanly.")))

(defmode js
  :priority 40
  :stack ("javascript" "node")
  :detect (:file-exists "package.json")
  :stack-commands (:test "npm test")
  :forms ((deftool test (&optional file)
            "Run Node/JS tests."
            (run-test-cmd file))
          (defskill javascript :doc "Modern JavaScript / TypeScript conventions.")
          (defsandbox project :allow ("node" "npm" "npx" "git"))
          (defagent dev
            :model "anthropic/claude-sonnet-4-6"
            :tools (test) :skills (javascript) :sandbox project
            :instructions "Build JS/TS features end-to-end. Use (test) to verify.")))
