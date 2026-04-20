(in-package :hactar-tests)

(def-suite redwood-target-tests :description "Redwood target tests.")
(in-suite redwood-target-tests)

;;* Helpers 
(defun rw-compile (source)
  "Compile to redwood, return VFS for multi-file inspection."
  (hactar::compile-string source :target :redwood :check nil))

(defun rw-compile-single (source)
  "Compile to redwood, return first file content."
  (string-trim '(#\Space #\Newline #\Tab)
               (hactar::compile-file-to-string source :target :redwood)))
;;* Tests
(test rw-defapp-produces-worker-and-client
  "defapp generates worker.tsx and client.tsx."
  (let* ((source "(defapp :wrapper *Document :routes ((\"/\" *Chat)))")
         (vfs (rw-compile source))
         (paths (hactar::vfs-all-paths vfs)))
    (is (member "src/worker.tsx" paths :test #'string=))
    (is (member "src/client.tsx" paths :test #'string=))
    (let ((worker (hactar::vfs-get-content vfs "src/worker.tsx")))
      (is (search "import { defineApp }" worker))
      (is (search "import { Document } from \"./app/Document\"" worker))
      (is (search "import { Chat } from \"./app/pages/Chat/Chat\"" worker))
      (is (search "export default defineApp" worker)))))

(test rw-defaction-server-pragma
  "defaction emits 'use server' pragma."
  (let ((result (rw-compile-single
                 "(defaction send-message (prompt)
                    (console.log \"Running AI\" prompt))")))
    (is (search "\"use server\"" result))
    (is (search "export async function sendMessage" result))))

(test rw-defclient-client-pragma
  "defclient emits 'use client' pragma and named export."
  (let ((result (rw-compile-single
                 "(defclient *Chat ()
                    (console.log \"hello\"))")))
    (is (search "\"use client\"" result))
    (is (search "export function Chat" result))
    (is (not (search "export default function Chat" result)))
    (is (= 0 (search "\"use client\"" result))
        "'use client' must be the first statement in the file")))

(test rw-let-state-uses-destructuring-const
  "let-state expands to destructuring const + useState."
  (let ((result (rw-compile-single
                 "(let-state
                    ((count set-count) 0)
                    (return count))")))
    (is (search "const [count, setCount] = useState(0);" result))))

(test rw-defcomponent-uses-const-typed
  "defcomponent emits typed const component."
  (let ((result (rw-compile-single
                 "(defcomponent *Document ((children :type 'React.ReactNode))
                    (div children))")))
    (is (search "export const Document: React.FC<" result))
    (is (search "({ children }) =>" result))))

(test rw-script-dynamic-import-renders-as-script-body
  "Dynamic import inside a script tag is emitted as script contents, not as a JSX expression child."
  (let ((result (rw-compile-single
                 "(script (import \"/src/client.tsx\"))")))
    (is (search "<script>import(\"/src/client.tsx\")</script>" result))
    (is (not (search "<script>{import(\"/src/client.tsx\")}</script>" result)))))

(test rw-jsx-tags-expand-via-jsx-primitive
  "HTML tags still compile correctly through jsx macro expansion."
  (let ((result (rw-compile-single "(div :class-name \"x\" (span \"ok\"))")))
    (is (search "<div className=\"x\"><span>ok</span></div>" result))))

(test rw-defapp-imports-named-client-component
  "defapp imports route components as named exports to match defclient output."
  (let* ((source
           "(in-file \"src/app/pages/Chat/Chat.tsx\")
            (defclient *Chat ()
              (return (div \"hello\")))
            (defapp :wrapper *Document :routes ((\"/\" *Chat)))")
         (vfs (rw-compile source))
         (chat (hactar::vfs-get-content vfs "src/app/pages/Chat/Chat.tsx"))
         (worker (hactar::vfs-get-content vfs "src/worker.tsx")))
    (is (search "export function Chat" chat)
        "Chat component should be a named export")
    (is (not (search "export default function Chat" chat))
        "Chat component should not be a default export")
    (is (search "import { Chat } from \"./app/pages/Chat/Chat\"" worker)
        "Worker should import Chat as a named export")))

(test rw-full-chat-app
  "A full chat app written as one Lisp source produces correct multi-file output."
  (let* ((source
           "(in-file \"src/app/Document.tsx\")
            (import styles :from \"../style.css?url\")

            (defcomponent *Document ((children :type 'React.ReactNode))
              (html :lang \"en\"
                (head
                  (meta :char-set \"utf-8\")
                  (meta :name \"viewport\" :content \"width=device-width, initial-scale=1\")
                  (title \"@redwoodjs/starter-minimal\")
                  (link :rel \"stylesheet\" :href styles)
                  (link :rel \"modulepreload\" :href \"/src/client.tsx\"))
                (body
                  (div :id \"root\" children)
                  (script (import \"/src/client.tsx\")))))

            (in-file \"src/pages/Chat/functions.ts\")
            (import (env) :from \"cloudflare:workers\")

            (defaction send-message ((prompt :type 'string))
              (console.log \"Running AI with Prompt:\" prompt)
              (let* ((response (await (env-call :ai :run \"@cf/meta/llama-4-scout-17b-16e-instruct\"
                                                               (create :prompt prompt :stream t))))))
                (return response))

            (in-file \"src/pages/Chat/Chat.tsx\")
            (import (send-message) :from \"./functions\")
            (import (use-state) :from \"react\")
            (import (consume-event-stream) :from \"rwsdk/client\")

            (defclient *Chat ()
              (let-state
                ((message set-message) \"\")
                ((reply set-reply) \"\")
                ((is-loading set-is-loading) :false)
                (let* ((on-submit (async (lambda ((e :type 'React.FormEvent<HTMLFormElement>))
                                          (chain e (.prevent-default))
                                          (set-is-loading t)
                                          (set-reply \"\")
                                          (chain (await (send-message message))
                                                 (.pipe-to (consume-event-stream
                                                            (create :on-chunk
                                                                    (lambda (event)
                                                                      (set-reply (lambda (prev)
                                                                                   (if (= (@ event data) \"[DONE]\")
                                                                                       (progn
                                                                                         (set-is-loading :false)
                                                                                         (return prev))
                                                                                       (return (+ prev (@ (json-parse (@ event data)) response)))))))))))))))
                  (return
                    (div :class-name \"flex flex-col h-screen\"
                      (div :class-name \"flex-1 overflow-y-auto p-4\" reply)
                      (form :on-submit on-submit :class-name \"bg-white flex rounded-lg p-4\"
                        (input :type \"text\" :value message :placeholder \"Type a message...\"
                               :on-change (lambda (e) (set-message (@ e target value))))
                        (button :type \"submit\" :disabled (or (= (@ message length) 0) is-loading)
                                (|?:| is-loading \"Sending...\" \"Send\"))))))))

            (defapp :wrapper *Document :routes (\"/\" *Chat))")
         (vfs (rw-compile source))
         (paths (hactar::vfs-all-paths vfs)))

    ;; Check all expected files exist
    (is (member "src/app/Document.tsx" paths :test #'string=)
        "Document file exists")
    (is (member "src/pages/Chat/functions.ts" paths :test #'string=)
        "Functions file exists")
    (is (member "src/pages/Chat/Chat.tsx" paths :test #'string=)
        "Chat file exists")
    (is (member "src/worker.tsx" paths :test #'string=)
        "Worker file exists")
    (is (member "src/client.tsx" paths :test #'string=)
        "Client file exists")

    ;; Check Document component
    (let ((doc (hactar::vfs-get-content vfs "src/app/Document.tsx")))
      (is (search "import styles from \"../style.css?url\"" doc)
          "Document has style import")
      (is (search "export const Document: React.FC<" doc)
          "Document is exported as React.FC")
      (is (search "<html" doc) "Document has html tag")
      (is (search "<head" doc) "Document has head tag")
      (is (search "<body" doc) "Document has body tag")
      (is (search "<div" doc) "Document has div tag"))

    ;; Check server action
    (let ((fns (hactar::vfs-get-content vfs "src/pages/Chat/functions.ts")))
      (is (search "\"use server\"" fns)
          "Functions file has use server pragma")
      (is (search "export async function sendMessage" fns)
          "sendMessage is exported async")
      (is (search "import { env } from \"cloudflare:workers\"" fns)
          "Functions imports env")
      (is (search "prompt" fns)
          "Functions references prompt parameter"))

    ;; Check client component
    (let ((chat (hactar::vfs-get-content vfs "src/pages/Chat/Chat.tsx")))
      (is (search "\"use client\"" chat)
          "Chat has use client pragma")
      (is (search "function Chat" chat)
          "Chat function is defined")
      (is (search "useState" chat)
          "Chat uses useState")
      (is (search "import { sendMessage } from \"./functions\"" chat)
          "Chat imports sendMessage")
      (is (search "import { useState } from \"react\"" chat)
          "Chat imports useState")
      (is (search "onSubmit" chat)
          "Chat has onSubmit handler")
      (is (search "preventDefault" chat)
          "Chat calls preventDefault")
      (is (search "consumeEventStream" chat)
          "Chat uses consumeEventStream"))

    ;; Check worker.tsx
    (let ((worker (hactar::vfs-get-content vfs "src/worker.tsx")))
      (is (search "import { defineApp } from \"rwsdk/worker\"" worker)
          "Worker imports defineApp")
      (is (search "import { render, route } from \"rwsdk/router\"" worker)
          "Worker imports render and route")
      (is (search "export const appContext = {  }" worker)
          "Worker exports appContext as const")
      (is (search "export default defineApp" worker)
          "Worker exports defineApp")
      (is (search "import { Document } from \"./app/Document\"" worker)
          "Worker imports Document from app dir")
      (is (search "import { Chat } from \"./app/pages/Chat/Chat\"" worker)
          "Worker imports Chat as named import from app/pages dir")
      (is (search "route(\"/\", Chat)" worker)
          "Worker has Chat route"))

    ;; Check client.tsx
    (let ((client (hactar::vfs-get-content vfs "src/client.tsx")))
      (is (search "import { initClient } from \"rwsdk/client\"" client)
          "Client imports initClient")
      (is (search "initClient();" client)
          "Client calls initClient"))))

(test rw-defapp-import-casing
  "defapp generates correctly-cased import names for wrapper and route components."
  (let* ((source "(defapp :wrapper *Document :routes (\"/\" *Chat))")
         (vfs (rw-compile source))
         (worker (hactar::vfs-get-content vfs "src/worker.tsx")))
    (is (search "import { Document } from" worker)
        "Wrapper import should be capitalized as Document")
    (is (search "import { Chat } from" worker)
        "Route component import should be named import")
    (is (search "import { Chat } from \"./app/pages/Chat/Chat\"" worker)
        "Route import should default to app/pages path")
    (is (not (search "import { document }" worker))
        "Wrapper import must not be lowercase")
    (is (not (search "import { chat }" worker))
        "Route import must not be lowercase")))

(test rw-defapp-custom-route-import-path
  "defapp allows customizing a route component import path."
  (let* ((source "(defapp :wrapper *Document :routes ((\"/\" *Chat \"./custom/Chat\")))")
         (vfs (rw-compile source))
         (worker (hactar::vfs-get-content vfs "src/worker.tsx")))
    (is (search "import { Chat } from \"./custom/Chat\"" worker)
        "Route import should use explicit custom path")
    (is (search "route(\"/\", Chat)" worker)
        "Route should still reference the component symbol")))

;; Config macro tests
(test rw-default-configs-emitted
  "When no config macros are called, default configs are still generated."
  (let* ((vfs (rw-compile "(defvar x 1)"))
         (paths (hactar::vfs-all-paths vfs)))
    (is (member "output.tsx" paths :test #'string=))
    (is (member "package.json" paths :test #'string=))
    (is (member "tsconfig.json" paths :test #'string=))
    (is (member "vite.config.mts" paths :test #'string=))
    (is (member "wrangler.toml" paths :test #'string=))
    ;; Check they have content
    (is (search "\"name\"" (hactar::vfs-get-content vfs "package.json")))
    (is (search "\"compilerOptions\"" (hactar::vfs-get-content vfs "tsconfig.json")))
    (is (search "defineConfig" (hactar::vfs-get-content vfs "vite.config.mts")))
    (is (search "redwood" (hactar::vfs-get-content vfs "vite.config.mts")))
    (is (search "name = " (hactar::vfs-get-content vfs "wrangler.toml")))))

(test rw-explicit-config-not-overwritten
  "When defconfig is called, the finalizer does not overwrite it."
  (let* ((vfs (rw-compile "(defconfig :name \"custom-app\")"))
         (pkg (hactar::vfs-get-content vfs "package.json")))
    (is (search "\"name\": \"custom-app\"" pkg))
    (is (not (search "\"name\": \"my-app\"" pkg)))))

(test rw-defconfig-produces-package-json
  "defconfig generates package.json with dependencies."
  (let* ((vfs (rw-compile
               "(defconfig :name \"my-chat-app\"
                  :dependencies (react \"^18.2.0\" react-dom \"^18.2.0\" lodash \"^4.17.21\")
                  :dev-dependencies (typescript \"^5.0.0\" vite \"^7.0.0\")
                  :package-manager \"pnpm@9.12.3\")"))
         (paths (hactar::vfs-all-paths vfs)))
    (is (member "package.json" paths :test #'string=))
    (let ((pkg (hactar::vfs-get-content vfs "package.json")))
      (is (search "\"name\": \"my-chat-app\"" pkg))
      (is (search "\"react\": \"^18.2.0\"" pkg))
      (is (search "\"react-dom\": \"^18.2.0\"" pkg))
      (is (search "\"lodash\": \"^4.17.21\"" pkg))
      (is (search "\"react-server-dom-webpack\": \"19.2.5\"" pkg))
      (is (search "\"rwsdk\": \"1.2.3\"" pkg))
      (is (search "\"typescript\": \"^5.0.0\"" pkg))
      (is (search "\"vite\": \"^7.0.0\"" pkg))
      (is (search "\"@cloudflare/vite-plugin\": \"1.31.0\"" pkg))
      (is (search "\"@cloudflare/workers-types\": \"4.20260405.1\"" pkg))
      (is (search "\"@types/node\": \"~25.3.5\"" pkg))
      (is (search "\"@types/react\": \"19.2.14\"" pkg))
      (is (search "\"@types/react-dom\": \"19.2.3\"" pkg))
      (is (search "\"wrangler\": \"4.80.0\"" pkg))
      (is (search "\"packageManager\": \"pnpm@9.12.3\"" pkg))
      (is (search "\"dev\":" pkg))
      (is (search "\"build\":" pkg)))))

(test rw-defconfig-defaults
  "defconfig uses pnpm as default package manager and emits default Redwood dependencies."
  (let* ((vfs (rw-compile "(defconfig :name \"test\")"))
         (pkg (hactar::vfs-get-content vfs "package.json")))
    (is (search "\"packageManager\": \"pnpm\"" pkg))
    (is (search "\"react\": \"19.2.5\"" pkg))
    (is (search "\"react-dom\": \"19.2.5\"" pkg))
    (is (search "\"react-server-dom-webpack\": \"19.2.5\"" pkg))
    (is (search "\"rwsdk\": \"1.2.3\"" pkg))
    (is (search "\"@cloudflare/vite-plugin\": \"1.31.0\"" pkg))
    (is (search "\"@cloudflare/workers-types\": \"4.20260405.1\"" pkg))
    (is (search "\"@types/node\": \"~25.3.5\"" pkg))
    (is (search "\"@types/react\": \"19.2.14\"" pkg))
    (is (search "\"@types/react-dom\": \"19.2.3\"" pkg))
    (is (search "\"typescript\": \"6.0.2\"" pkg))
    (is (search "\"vite\": \"~7.3.2\"" pkg))
    (is (search "\"wrangler\": \"4.80.0\"" pkg))))

(test rw-defconfig-overrides-default-dependencies
  "Custom dependencies and dev-dependencies override defaults while preserving unspecified defaults."
  (let* ((vfs (rw-compile
               "(defconfig :name \"override-test\"
                  :dependencies (react \"^18.3.1\" rwsdk \"^1.1.0\" zod \"^3.24.0\")
                  :dev-dependencies (typescript \"^5.9.0\" vite \"^7.1.0\" eslint \"^9.0.0\"))"))
         (pkg (hactar::vfs-get-content vfs "package.json")))
    (is (search "\"react\": \"^18.3.1\"" pkg))
    (is (search "\"react-dom\": \"19.2.5\"" pkg))
    (is (search "\"react-server-dom-webpack\": \"19.2.5\"" pkg))
    (is (search "\"rwsdk\": \"^1.1.0\"" pkg))
    (is (search "\"zod\": \"^3.24.0\"" pkg))
    (is (search "\"typescript\": \"^5.9.0\"" pkg))
    (is (search "\"vite\": \"^7.1.0\"" pkg))
    (is (search "\"@cloudflare/vite-plugin\": \"1.31.0\"" pkg))
    (is (search "\"@types/node\": \"~25.3.5\"" pkg))
    (is (search "\"wrangler\": \"4.80.0\"" pkg))
    (is (search "\"eslint\": \"^9.0.0\"" pkg))
    (is (not (search "\"react\": \"^19.0.0\"" pkg)))
    (is (not (search "\"rwsdk\": \"^1.0.9\"" pkg)))
    (is (not (search "\"typescript\": \"6.0.2\"" pkg)))
    (is (not (search "\"vite\": \"~7.3.2\"" pkg)))))

(test rw-deftsconfig-defaults
  "deftsconfig with no args produces sensible defaults."
  (let* ((vfs (rw-compile "(deftsconfig)"))
         (paths (hactar::vfs-all-paths vfs)))
    (is (member "tsconfig.json" paths :test #'string=))
    (let ((ts (hactar::vfs-get-content vfs "tsconfig.json")))
      (is (search "\"compilerOptions\"" ts))
      (is (search "\"target\": \"es2021\"" ts))
      (is (search "\"jsx\": \"react-jsx\"" ts))
      (is (search "\"strict\": true" ts))
      (is (search "\"noEmit\": true" ts))
      (is (search "\"skipLibCheck\": true" ts))
      (is (search "node_modules" ts)))))

(test rw-deftsconfig-custom
  "deftsconfig with custom options."
  (let* ((vfs (rw-compile "(deftsconfig :target \"ES2022\" :strict t :jsx \"react-jsx\" :lib (\"ESNext\" \"DOM\"))"))
         (ts (hactar::vfs-get-content vfs "tsconfig.json")))
    (is (search "\"target\": \"ES2022\"" ts))
    (is (search "\"strict\": true" ts))
    (is (search "\"lib\": [\"ESNext\", \"DOM\"]" ts))))

(test rw-defviteconfig
  "defviteconfig produces vite.config.mts with Cloudflare and Redwood plugins."
  (let* ((vfs (rw-compile "(defviteconfig :plugins ((react-plugin \"@vitejs/plugin-react\")))"))
         (paths (hactar::vfs-all-paths vfs)))
    (is (member "vite.config.mts" paths :test #'string=))
    (let ((vite (hactar::vfs-get-content vfs "vite.config.mts")))
      (is (search "import { defineConfig } from \"vite\"" vite))
      (is (search "import { redwood } from \"rwsdk/vite\"" vite))
      (is (search "import { cloudflare } from \"@cloudflare/vite-plugin\"" vite))
      (is (search "import { reactPlugin } from \"@vitejs/plugin-react\"" vite))
      (is (search "cloudflare({ viteEnvironment: { name: \"worker\" } })" vite))
      (is (search "plugins: [cloudflare({ viteEnvironment: { name: \"worker\" } }), redwood(), reactPlugin()]" vite))
      (is (search "export default defineConfig" vite)))))

(test rw-defwrangler
  "defwrangler produces wrangler.toml."
  (let* ((vfs (rw-compile "(defwrangler :name \"my-app\" :compatibility-date \"2025-08-21\" :compatibility-flags (\"nodejs_compat\"))"))
         (paths (hactar::vfs-all-paths vfs)))
    (is (member "wrangler.toml" paths :test #'string=))
    (let ((wrangler (hactar::vfs-get-content vfs "wrangler.toml")))
      (is (search "name = \"my-app\"" wrangler))
      (is (search "compatibility-date = \"2025-08-21\"" wrangler))
      (is (search "compatibility-flags = [\"nodejs_compat\"]" wrangler)))))

(test rw-defwrangler-with-bindings
  "defwrangler supports TOML table sections via nested lists."
  (let* ((vfs (rw-compile "(defwrangler :name \"app\" :ai (:binding \"AI\"))"))
         (wrangler (hactar::vfs-get-content vfs "wrangler.toml")))
    (is (search "name = \"app\"" wrangler))
    (is (search "[ai]" wrangler))
    (is (search "binding = \"AI\"" wrangler))))

(test rw-css-import-adds-tailwind-plugin
  "When a CSS import is present, tailwindcss plugin is added to vite config."
  (let* ((vfs (rw-compile "(import styles :from \"./style.css?url\") (defvar x 1)"))
         (vite (hactar::vfs-get-content vfs "vite.config.mts")))
    (is (search "tailwindcss" vite)
        "Vite config should include tailwindcss plugin")
    (is (search "import tailwindcss from \"@tailwindcss/vite\"" vite)
        "Vite config should import tailwindcss as a default import")
    (is (search "cloudflare({ viteEnvironment: { name: \"worker\" } })" vite)
        "Vite config should include cloudflare worker plugin")
    (is (search "redwood()" vite)
        "Vite config should still include redwood plugin")))

(test rw-css-import-adds-tailwind-devdeps
  "When a CSS import is present, tailwindcss devDependencies are added to package.json."
  (let* ((vfs (rw-compile "(import styles :from \"./style.css?url\") (defvar x 1)"))
         (pkg (hactar::vfs-get-content vfs "package.json")))
    (is (search "\"@tailwindcss/vite\": \"^4.2.2\"" pkg)
        "package.json should include @tailwindcss/vite devDependency")
    (is (search "\"tailwindcss\": \"^4.2.2\"" pkg)
        "package.json should include tailwindcss devDependency")))

(test rw-no-css-import-no-tailwind-devdeps
  "When no CSS import is present, tailwindcss devDependencies are not added."
  (let* ((vfs (rw-compile "(defvar x 1)"))
         (pkg (hactar::vfs-get-content vfs "package.json")))
    (is (not (search "tailwindcss" pkg))
        "package.json should not include tailwindcss without CSS import")))

(test rw-no-css-import-no-tailwind-plugin
  "When no CSS import is present, tailwindcss plugin is not added."
  (let* ((vfs (rw-compile "(defvar x 1)"))
         (vite (hactar::vfs-get-content vfs "vite.config.mts")))
    (is (not (search "tailwindcss" vite))
        "Vite config should not include tailwindcss plugin without CSS import")
    (is (search "cloudflare({ viteEnvironment: { name: \"worker\" } })" vite)
        "Vite config should still include cloudflare worker plugin")
    (is (search "redwood()" vite)
        "Vite config should still include redwood plugin")))

(test rw-defviteconfig-defaults-to-named-imports
  "Vite plugin imports default to named imports unless configured otherwise."
  (let* ((vfs (rw-compile "(defviteconfig :plugins ((react-plugin \"@vitejs/plugin-react\")))"))
         (vite (hactar::vfs-get-content vfs "vite.config.mts")))
    (is (search "import { reactPlugin } from \"@vitejs/plugin-react\"" vite)
        "Unconfigured plugin sources should use named imports")
    (is (not (search "import reactPlugin from \"@vitejs/plugin-react\"" vite))
        "Unconfigured plugin sources should not use default imports")))

(test rw-inherits-typescript
  "Redwood inherits TypeScript type annotations."
  (let ((result (rw-compile-single
                 "(defun helper ((x :type 'number)) :return 'string
                    (return (+ \"val: \" x)))")))
    (is (search "function helper(x: number): string" result))))

(test rw-json-configs-compile-via-forms
  "Config files compile through JSON-form emitters."
  (let* ((vfs (rw-compile "(defconfig :name \"json-app\") (deftsconfig :strict t :lib (\"ESNext\" \"DOM\"))"))
         (pkg (hactar::vfs-get-content vfs "package.json"))
         (ts (hactar::vfs-get-content vfs "tsconfig.json")))
    (is (search "\"name\": \"json-app\"" pkg))
    (is (search "\"strict\": true" ts))
    (is (search "\"lib\": [\"ESNext\", \"DOM\"]" ts))))

(test rw-toml-config-compiles-via-forms
  "Wrangler TOML compiles through TOML-form emitters."
  (let* ((vfs (rw-compile "(defwrangler :name \"app\" :ai (:binding \"AI\") :compatibility-flags (\"nodejs_compat\"))"))
         (wrangler (hactar::vfs-get-content vfs "wrangler.toml")))
    (is (search "name = \"app\"" wrangler))
    (is (search "[ai]" wrangler))
    (is (search "binding = \"AI\"" wrangler))
    (is (search "compatibility-flags = [\"nodejs_compat\"]" wrangler))))

(test rw-no-stray-default-output-file-when-source-selects-files
  "Redwood compilation should not create an empty default output file when source uses in-file forms."
  (let* ((vfs (rw-compile (uiop:read-file-string "tests/data/RedwoodChatApp.lisp")))
         (paths (hactar::vfs-all-paths vfs)))
    (is (not (member "output.tsx" paths :test #'string=)))))

(test rw-defclient-no-stray-semicolons
  "defclient output has no double ;; semicolons."
  (let ((result (rw-compile-single
                 "(defclient *Chat ()
                    (return (div \"hello\")))")))
    (is (not (search ";;" result))
        "Output must not contain ;; double semicolons")))

(test rw-lambda-if-body-uses-block
  "Lambda with if body in redwood uses block form, not expression form."
  (let ((result (rw-compile-single
                 "(lambda (prev) (if (= x 1) (return prev) (return (+ prev 1))))")))
    (is (search "=> {" result)
        "Lambda with if body should use block form")
    (is (not (search "=> if" result))
        "Lambda should not use expression form for if")))

(test rw-false-keyword-emits-false
  ":false emits as false, not null."
  (let ((result (rw-compile-single "(defvar x :false)")))
    (is (search "var x = false" result))))

(test rw-typed-lambda-arg
  "Lambda with typed argument emits type annotation."
  (let ((result (rw-compile-single
                 "(lambda ((e :type 'React.FormEvent)) (chain e (.prevent-default)))")))
    (is (search "e: React.FormEvent" result))))

(test rw-progn-in-if-no-double-semicolons
  "progn inside an if block does not produce double semicolons."
  (let ((result (rw-compile-single
                 "(if (= x 1) (progn (foo) (bar)))")))
    (is (not (search ";;" result))
        "progn in if block must not produce ;;")))

(test rw-defclient-pragma-precedes-imports
  "'use client' is at the top of client files even when imports come first."
  (let* ((vfs (rw-compile
               "(in-file \"src/Chat.tsx\")
                (import (use-state) :from \"react\")
                (import (send-message) :from \"./functions\")
                (defclient *Chat ()
                  (let-state ((msg set-msg) \"\")
                    (return (div msg))))"))
         (chat (hactar::vfs-get-content vfs "src/Chat.tsx")))
    (is (= 0 (search "\"use client\"" chat))
        "'use client' must be at the very beginning of the file")
    (is (search "import" chat))
    (is (< (search "\"use client\"" chat) (search "import" chat))
        "'use client' must precede all imports")))
