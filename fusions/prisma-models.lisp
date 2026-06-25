(in-package :hactar)

(deffusion prisma-models
    (:trigger :on-mode
     :mode :prisma
     :watch ("prisma/schema.prisma" "schema.prisma" "db/schema.prisma")
     :output-form defmodels)
  
  (:fern-extract
    :language :typescript
    :files (let ((root (project-cli-root state)))
             (multiple-value-bind (full rel) (pcli-schema-file root)
               (when full (list full))))
    :query '(identifier)
    :as model-nodes)
  
  (:diff-detect
    :current model-nodes
    :previous nil
    :as (new-models changed-models))
  
  (:diffuse
    :when (or new-models changed-models)
    :prompt :diffusion-models
    :args (list (let ((root (project-cli-root state)))
                  (multiple-value-bind (full rel) (pcli-schema-file root)
                    (if full (uiop:read-file-string full) ""))))
    :as raw-forms)
  
  (:validate :forms raw-forms :schema 'defmodels :as valid-forms)
  (:emit :forms valid-forms :merge t))
