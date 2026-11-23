(defpackage #:hactar-core
  (:use #:cl)
  (:nicknames #:hactar-rebuild #:hactar-engine)
  (:import-from #:hactar-llm #:*mock-llm-output* #:*mock-diffusion-output*)
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
   #:deftool #:register-tool-fn #:list-tools #:list-commands
   #:defroute #:defhypertext #:defdoc #:defsandbox #:defstarter #:generated
   #:*starters*
   ;; Tool/shell/agent execution surface (usable inside tool bodies).
   #:shell-cmd #:sandbox-check #:run-tool #:run-action #:run-agent #:run-command
   #:run-ast-edit #:with-agent #:agent-run #:run-test-cmd #:run-lint-cmd
   #:run-typecheck-cmd #:run-build-cmd #:*codegen-emitter* #:run-emit
   ;; LLM surface (mockable).
   #:ask #:llm-chat #:diffusion-complete #:*mock-llm-output* #:*mock-diffusion-output*
   ;; Prompt rendering.
   #:render-prompt #:lookup-doc
   ;; Routes.
   #:ensure-leading-slash #:parse-route
   ;; Validation and file IO.
   #:*surface-operators* #:validate-forms #:read-forms #:load-hactar-file
   #:load-pack-file #:write-hactar-file #:render-generated-block
   #:extract-user-region #:hactar-file-path
   ;; Modes / detection.
   #:register-stack-mode #:compile-detect-spec #:eval-detect-spec
   #:detect-stack #:extract-project-map #:generate-harness #:stack-harness-forms
   ;; Bootstrap / doctor / dispatch / main.
   #:bootstrap #:doctor #:ensure-line #:build-project-binary #:find-repo-root
   #:dispatch #:args->plist #:main
   ;; Self-update.
   #:run-self-update #:record-update-success #:record-update-failure
   #:last-update-failed-p #:update-status #:record-fs-event #:make-watch-handler
   #:collect-signal #:signal-ready-p
   ;; Session, history, context, presets, assistant, doctor.
   #:*chat-history* #:*active-rules* #:*presets* #:*active-presets*
   #:*current-model* #:*debug-mode*
   #:append-history #:clear-history #:serialize-history-yaml
   #:defpreset #:register-preset #:find-preset #:apply-preset
   #:render-system-prompt #:capture-state #:apply-state
   #:save-session #:load-session #:list-sessions
   #:chat-turn #:run-tool-loop
   #:debug-log #:git-command #:git-staged-diff #:git-status-clean-p
   #:generate-commit-message #:run-doctor-checks
   ;; Interfaces.
   #:render-interface #:parse-interface #:write-interface #:read-interface
   #:sync-interfaces #:context-dir #:context-add-file #:context-drop-file
   #:render-context-files #:parse-context-files #:render-context-payload
   #:render-context-org #:parse-context-org #:extract-org-src-blocks
   #:shell-exec #:*context-files*))

;; .hactar.lisp runs here; it sees the DSL surface without package prefixes.
(defpackage #:hactar-user
  (:use #:cl #:hactar-rebuild))
