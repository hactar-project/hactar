(defpackage #:hactar-rebuild
  (:use #:cl)
  (:export
   ;; Result protocol.
   #:result
   #:make-result
   #:result-p
   #:result-status
   #:result-message
   #:result-data
   #:result-artifacts
   #:render-result
   ;; State and registries.
   #:defstate
   #:*state-registry*
   #:reset-registries
   #:registry-count
   ;; DSL surface.
   #:defproject
   #:defmodels
   #:defroutes
   #:defhook
   #:defmode
   #:defagent
   #:definterface
   #:defcommand
   #:defrule
   #:defskill
   #:defprompt
   #:defaction
   #:deftool
   #:defroute
   #:defhypertext
   #:defdoc
   #:defsandbox
   #:generated
   ;; Runtime helpers.
   #:ensure-leading-slash
   #:parse-route
   #:validate-forms
   #:read-forms
   #:load-hactar-file))

(defpackage #:hactar-user
  (:use #:cl #:hactar-rebuild))
