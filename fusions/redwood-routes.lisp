(in-package :hactar)

(deffusion redwood-routes 
    (:trigger :on-mode
     :mode :redwood
     :watch ("web/src/Routes.tsx" "web/src/pages/**/*.tsx" "src/worker.tsx" "src/app/pages/**/*.tsx")
     :debounce 1000
     :output-form defroutes)
  
  ;; Stage 1: Use Fern to parse route files
  (:fern-extract
    :language :tsx
    :files (let* ((root (project-cli-root state))
                  (default-file (merge-pathnames "web/src/Routes.tsx" root)))
             (if (probe-file default-file)
                 (list default-file)
                 (mapcar (lambda (f) (merge-pathnames f root))
                         (pcli-route-files root))))
    :query 'call-expression
    :as route-nodes)
  
  ;; Stage 2: Determine new/changed routes  
  (:diff-detect
    :current route-nodes
    :previous nil
    :as (new-routes changed-routes))
  
  ;; Stage 3: Send to diffusion model for LISP generation
  (:diffuse
    :when (or new-routes changed-routes)
    :prompt :diffusion-routes
    :args (list "redwood" 
                (mapcar #'fern-node-text route-nodes)
                (list "web/src/Routes.tsx"))
    :as raw-forms)
  
  ;; Stage 4: Validate and correct
  (:validate
    :forms raw-forms
    :schema 'defroutes
    :as valid-forms)
  
  ;; Stage 5: Emit to .hactar.lisp
  (:emit
    :forms valid-forms
    :merge t))
