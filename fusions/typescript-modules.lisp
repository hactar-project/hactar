(in-package :hactar)

(deffusion typescript-modules
    (:trigger :on-mode
     :mode :typescript
     :watch ("src/**/*.ts" "src/**/*.tsx")
     :debounce 2000
     :output-form defmodules)
  
  (:fern-extract
    :language :typescript
    :files (let* ((root (project-cli-root state))
                  (all-files (pcli-list-files root)))
             (remove-if-not
              (lambda (f)
                (let ((ext (pathname-type f)))
                  (and ext (member ext '("ts" "tsx") :test #'string-equal))))
              all-files))
    :query 'import-statement
    :as module-nodes)
  
  (:diff-detect
    :current module-nodes
    :previous nil
    :as (new-modules changed-modules))
  
  (:diffuse
    :when (or new-modules changed-modules)
    :prompt :diffusion-modules
    :args (list "TypeScript module map"
                (format nil "Modules found: ~{~A~^, ~}" (mapcar #'fern-node-text module-nodes)))
    :as raw-forms)
  
  (:validate :forms raw-forms :schema 'defmodules :as valid-forms)
  (:emit :forms valid-forms :merge t))
