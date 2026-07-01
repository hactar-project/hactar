(defpackage #:hactar-rebuild
  (:use #:cl)
  (:nicknames #:hactar-engine)
  (:export
   ;; Result protocol.
   #:result #:make-result #:result-p #:result-status #:result-message
   #:result-data #:result-artifacts #:render-result #:ok #:err #:result-ok-p
   ;; State and registries.
   #:defstate #:*state-registry* #:*repo-root* #:*stack* #:*project-map*
   #:*auto-update* #:*update-ledger* #:reset-registries #:registry-count
   #:state-summary
   ;; Hooks runtime.
   #:add-handler #:run-hook #:clear-handlers
   ;; DSL surface.
   #:defproject #:defmodels #:defroutes #:defhook #:defmode #:defagent
   #:definterface #:defcommand #:defrule #:defskill #:defprompt #:defaction
   #:deftool #:defroute #:defhypertext #:defdoc #:defsandbox #:generated
   ;; Tool/shell/agent execution surface (usable inside tool bodies).
   #:shell-cmd #:sandbox-check #:run-tool #:run-action #:run-agent #:run-command
   #:run-ast-edit #:with-agent #:agent-run #:run-test-cmd #:run-lint-cmd
   #:run-typecheck-cmd #:run-build-cmd
   ;; LLM surface (mockable).
   #:ask #:llm-chat #:diffusion-complete #:*mock-llm-output* #:*mock-diffusion-output*
   ;; Prompt rendering.
   #:render-prompt #:lookup-doc
   ;; Routes.
   #:ensure-leading-slash #:parse-route
   ;; Validation and file IO.
   #:*surface-operators* #:validate-forms #:read-forms #:load-hactar-file
   #:write-hactar-file #:render-generated-block #:extract-user-region
   #:hactar-file-path
   ;; Modes / detection.
   #:register-stack-mode #:detect-stack #:extract-project-map #:generate-harness
   #:stack-harness-forms
   ;; Bootstrap / doctor / dispatch / main.
   #:bootstrap #:doctor #:ensure-line #:build-project-binary #:find-repo-root
   #:dispatch #:args->plist #:main
   ;; Self-update.
   #:run-self-update #:record-update-success #:record-update-failure
   #:last-update-failed-p #:update-status))

;; .hactar.lisp runs here; it sees the DSL surface without package prefixes.
(defpackage #:hactar-user
  (:use #:cl #:hactar-rebuild))
