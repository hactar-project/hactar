(in-package :hactar-user)

(defmode stripe
  :when (:file-contains ("package.json" "stripe"))
  :rules "Verify Stripe webhook signatures, pin API version, and use idempotency keys on writes."
  :skills (stripe))

(defmode skills
  :when (:file-exists "SKILL.md")
  :rules "Follow Claude/Hactar SKILL.md metadata and progressive-disclosure conventions.")

(defmode gpt
  :when t
  :rules "Keep prompts concise, structured, and deterministic.")

(defskill stripe :doc "Stripe Checkout, Webhooks, and Billing integration conventions.")
(defdoc stripe-webhooks
  :uri "docs:stripe/webhooks"
  :text "Always verify the Stripe-Signature header against the raw request body before parsing JSON."
  :tags ("stripe" "webhooks"))
