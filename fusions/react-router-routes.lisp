(in-package :hactar)

(deffusion react-router-routes
    (:trigger :on-mode
     :mode :react-router
     :watch ("src/routes.ts" "src/routes.tsx" "src/app/routes.ts" "src/app/routes.tsx")
     :debounce 1000
     :output-form defroutes)
  
  (:fern-extract
    :language :tsx
    :files (let* ((root (project-cli-root state))
                  (candidates (list (merge-pathnames "src/routes.ts" root)
                                    (merge-pathnames "src/routes.tsx" root)
                                    (merge-pathnames "src/app/routes.ts" root)
                                    (merge-pathnames "src/app/routes.tsx" root))))
             (remove-if-not #'probe-file candidates))
    :query 'jsx-self-closing
    :as route-nodes)
  
  (:diff-detect
    :current route-nodes
    :previous nil
    :as (new-routes changed-routes))
  
  (:diffuse
    :when (or new-routes changed-routes)
    :prompt :diffusion-routes
    :args (list "react-router"
                (mapcar #'fern-node-text route-nodes)
                (list "src/routes.tsx"))
    :as raw-forms)
  
  (:validate :forms raw-forms :schema 'defroutes :as valid-forms)
  (:emit :forms valid-forms :merge t))
