(in-package #:hactar-rebuild)

;;; Engine bridge to the hactar-llm library. The engine keeps its historical
;;; entry points (ask / llm-chat / diffusion-complete) but delegates to
;;; hactar-llm, which owns the model registry, client protocol, and the
;;; deterministic offline mock (*mock-llm-output* / *mock-diffusion-output* are
;;; imported from hactar-llm so callers bind the same specials the client reads).

(defun ask (prompt &key model)
  "Single-shot completion via hactar-llm."
  (hactar-llm:ask prompt :model model))

(defun llm-chat (model system user &optional tools)
  "Composed chat turn via hactar-llm."
  (hactar-llm:chat model system user tools))

(defun diffusion-complete (prompt)
  "Whole-region diffusion rewrite via hactar-llm."
  (hactar-llm:diffuse prompt))

;;; with-agent / agent-run stay engine-side because they return the engine
;;; RESULT type and compose with tools.
(defvar *agent-model* "hactar-mock")

(defmacro with-agent ((&key (model "hactar-mock")) &body body)
  `(let ((*agent-model* ,model)) ,@body))

(defun agent-run (request)
  "Run a one-off agent request inside a WITH-AGENT block."
  (ok (hactar-llm:chat *agent-model* "You are a focused sub-agent." request)))
