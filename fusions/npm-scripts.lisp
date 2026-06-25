(in-package :hactar)

(deffusion npm-scripts
    (:trigger :on-mode
     :mode :npm
     :watch ("package.json")
     :output-form defscripts)
  
  (:fern-extract
    :language :typescript ; JSON can be parsed using typescript grammar or basic identifiers
    :files (let ((root (project-cli-root state)))
             (list (merge-pathnames "package.json" root)))
    :query '(identifier)
    :as script-nodes)
  
  (:diff-detect
    :current script-nodes
    :previous nil
    :as (new-scripts changed-scripts))
  
  (:diffuse
    :when (or new-scripts changed-scripts)
    :prompt :diffusion-modules
    :args (list (format nil "package.json scripts: ~{~A~^, ~}" (mapcar #'fern-node-text script-nodes)) "")
    :as raw-forms)
  
  (:validate :forms raw-forms :schema 'defscripts :as valid-forms)
  (:emit :forms valid-forms :merge t))
