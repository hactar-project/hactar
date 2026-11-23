(defpackage #:hactar-llm
  (:use #:cl)
  (:export #:model #:make-model #:model-name #:model-provider
           #:model-id #:model-context-window #:model-max-output-tokens
           #:model-input-cost #:model-output-cost #:model-edit-format
           #:model-extra-params
           #:register-model #:find-model #:list-models #:resolve-model
           #:parse-model-spec #:parse-models-yaml #:load-models-yaml
           #:provider #:make-provider #:provider-name
           #:provider-complete #:provider-chat #:provider-stream
           #:provider-diffuse #:provider-embed #:provider-oauth
           #:register-provider #:find-provider #:list-providers
           #:make-client #:*llm-client* #:make-mock-client
           #:*mock-llm-output* #:*mock-diffusion-output*
           #:*default-system-prompt* #:prepare-messages
           #:complete #:ask #:chat #:diffuse #:llm-stream
           #:diffusion-complete #:embed #:oauth-login
           #:normalize-tool-calls #:estimate-tokens #:token-count
           #:estimate-cost
           ;; OAuth & credentials
           #:*oauth-http-fn* #:*oauth-credentials*
           #:oauth-credentials-path #:save-oauth-credentials
           #:load-oauth-credentials #:get-oauth-credentials
           #:set-oauth-credentials #:oauth-expired-p
           #:oauth-access-token #:oauth-refresh #:generate-pkce
           #:register-default-oauth-providers
           #:oauth-parse-code-input #:oauth-creds-from-token-response
           #:oauth-jwt-account-id))
