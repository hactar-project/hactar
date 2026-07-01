(in-package #:hactar-rebuild)

;;; Network-free LLM + diffusion clients. Real providers live in hactar-llm;
;;; these deterministic mocks keep `make test` and bootstrap offline-capable.
;;; Binding *mock-llm-output* / *mock-diffusion-output* overrides the reply.

(defvar *mock-llm-output* nil
  "When non-nil, ASK/LLM-CHAT return this string instead of a canned reply.")
(defvar *mock-diffusion-output* nil
  "When non-nil, DIFFUSION-COMPLETE returns this string.")

(defun ask (prompt &key model)
  "Single-shot completion. Deterministic offline stub."
  (declare (ignore model))
  (or *mock-llm-output*
      (format nil "[hactar-mock] ~A"
              (subseq prompt 0 (min 200 (length prompt))))))

(defun llm-chat (model system user &optional tools)
  "Composed chat turn. Deterministic offline stub."
  (declare (ignore tools))
  (or *mock-llm-output*
      (format nil "[hactar-mock model=~A] ~A"
              model (subseq user 0 (min 200 (length user))))))

(defun diffusion-complete (prompt)
  "Fast whole-region rewrite. Deterministic offline stub.
Returns the current generated region unchanged unless overridden."
  (or *mock-diffusion-output* prompt))

;;; with-agent / agent-run: let a tool body spin up a throwaway agent turn.
(defmacro with-agent ((&key (model "hactar-mock")) &body body)
  `(let ((*agent-model* ,model)) ,@body))

(defvar *agent-model* "hactar-mock")

(defun agent-run (request)
  "Run a one-off agent request inside a WITH-AGENT block."
  (ok (llm-chat *agent-model* "You are a focused sub-agent." request)))
