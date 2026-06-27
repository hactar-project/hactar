;;* Redwood SDK Mode
(in-package :hactar)

(defmode redwood
  "Cloudflare Workers-based Redwood SDK full-stack React framework mode."
  :when (member "redwood" *stack* :test #'string-equal)
  :rules "When working with Redwood SDK (rwsdk):
- Use src/worker.tsx as the primary server entry point.
- Define client-side entry in src/client.tsx.
- Define application structure via define-app() in worker.tsx.
- Put UI components under src/app/components/ or src/app/pages/.
- Access wrangler environment variables and bindings through env objects.
- Leverage vite and cloudflare plugins in configurations."
  :commands
  ((defmode-command redwood "redwood.info" (args)
     "Display Redwood SDK project structure summary."
     (declare (ignore args))
     (let ((pcli (get-project-cli *pcli-current-project*)))
       (if pcli
           (progn
             (format t "Redwood SDK Project: ~A~%" (project-cli-name pcli))
             (format t "Routes count: ~A~%" (length (project-routes (project-cli-name pcli))))
             (format t "Models count: ~A~%" (length (project-models (project-cli-name pcli)))))
           (format t "No active project CLI found.~%"))))))

(deffusion redwood-sdk
    (:trigger :on-mode
     :mode :redwood
     :watch ("src/worker.tsx" "wrangler.toml" "package.json" "src/app/**/*.tsx" "src/app/**/*.ts")
     :debounce 1000
     :output-form (defroutes defmodules defcomponents defenvs))

  ;; Stage 1: Use Fern to extract/track files in the workspace.
  (:fern-extract
    :language :tsx
    :files (let* ((root (project-cli-root state))
                  (candidates (list "src/worker.tsx" "wrangler.toml" "package.json"))
                  (collected candidates))
             (dolist (f (pcli-list-files root))
               (when (and (or (str:ends-with-p ".tsx" f)
                              (str:ends-with-p ".ts" f))
                          (search "src/app/" f))
                 (push f collected)))
             (mapcar (lambda (f) (merge-pathnames f root))
                     (remove-if-not (lambda (f) (probe-file (merge-pathnames f root))) (nreverse collected))))
    :query 'call-expression
    :as sdk-nodes)

  ;; Stage 2: Detect changes.
  (:diff-detect
    :current sdk-nodes
    :previous nil
    :as (new-nodes changed-nodes))

  ;; Stage 3: Send to diffusion model using the specialized prompt template.
  (:diffuse
    :when (or new-nodes changed-nodes)
    :prompt :diffusion-redwood-sdk
    :args (let* ((root (project-cli-root state))
                 (candidates (list "src/worker.tsx" "wrangler.toml" "package.json"))
                 (collected candidates))
            (dolist (f (pcli-list-files root))
              (when (and (or (str:ends-with-p ".tsx" f)
                             (str:ends-with-p ".ts" f))
                         (search "src/app/" f))
                (push f collected)))
            (let ((files (remove-if-not (lambda (f) (probe-file (merge-pathnames f root))) (nreverse collected))))
              (list files
                    (mapcar (lambda (f) (uiop:read-file-string (merge-pathnames f root))) files)
                    '(defroutes defmodules defcomponents defenvs))))
    :as raw-forms)

  ;; Stage 4: Validate against the set of expected schemas.
  (:validate
    :forms raw-forms
    :schema '(defroutes defmodules defcomponents defenvs)
    :as valid-forms)

  ;; Stage 5: Emit the extracted forms to .hactar.lisp.
  (:emit
    :forms valid-forms
    :merge t))
