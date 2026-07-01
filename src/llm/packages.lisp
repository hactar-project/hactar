(defpackage #:hactar-llm
  (:use #:cl)
  (:export #:model #:make-model #:model-name #:model-provider
           #:model-context-window #:model-input-cost #:model-output-cost
           #:register-model #:find-model #:list-models
           #:*llm-client* #:make-mock-client #:*mock-llm-output*
           #:*mock-diffusion-output*
           #:complete #:ask #:chat #:diffuse
           #:normalize-tool-calls #:estimate-tokens #:estimate-cost))
