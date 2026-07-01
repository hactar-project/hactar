(in-package #:hactar-llm)

;;; Provider-agnostic LLM wrapper. Ships with a deterministic, network-free
;;; mock client so the whole stack (bootstrap, self-update, agents) runs
;;; offline. Real provider clients plug in by binding *LLM-CLIENT*.

;;; ---- model registry -------------------------------------------------------

(defstruct (model (:conc-name model-))
  name provider (context-window 8192) (input-cost 0.0) (output-cost 0.0))

(defvar *models* (make-hash-table :test 'equal))

(defun register-model (name &key provider (context-window 8192)
                              (input-cost 0.0) (output-cost 0.0))
  (setf (gethash name *models*)
        (make-model :name name :provider provider
                    :context-window context-window
                    :input-cost input-cost :output-cost output-cost)))

(defun find-model (name) (gethash name *models*))
(defun list-models () (loop for m being the hash-values of *models* collect m))

(register-model "anthropic/claude-sonnet-4-6" :provider :anthropic
                :context-window 200000 :input-cost 3.0 :output-cost 15.0)
(register-model "anthropic/claude-haiku" :provider :anthropic
                :context-window 200000 :input-cost 0.8 :output-cost 4.0)
(register-model "hactar-mock" :provider :mock)

;;; ---- client protocol ------------------------------------------------------

(defvar *mock-llm-output* nil
  "When non-nil, COMPLETE/CHAT return this string (deterministic tests).")
(defvar *mock-diffusion-output* nil
  "When non-nil, DIFFUSE returns this string.")

(defun make-mock-client ()
  "A deterministic client: a plist of :complete/:chat/:diffuse functions."
  (list
   :complete (lambda (prompt &key model)
               (or *mock-llm-output*
                   (format nil "[hactar-mock~@[ model=~A~]] ~A" model
                           (subseq prompt 0 (min 200 (length prompt))))))
   :chat (lambda (model system user &optional tools)
           (declare (ignore system tools))
           (or *mock-llm-output*
               (format nil "[hactar-mock model=~A] ~A" model
                       (subseq user 0 (min 200 (length user))))))
   :diffuse (lambda (prompt &key model)
              (declare (ignore model))
              (or *mock-diffusion-output* prompt))))

(defvar *llm-client* (make-mock-client)
  "Active client. Bind to a real provider plist to go live.")

(defun client-fn (op) (getf *llm-client* op))

(defun complete (prompt &key model)
  "Single-shot completion."
  (funcall (client-fn :complete) prompt :model model))

(defun ask (prompt &key model) (complete prompt :model model))

(defun chat (model system user &optional tools)
  "Composed chat turn with optional tool specs."
  (funcall (client-fn :chat) model system user tools))

(defun diffuse (prompt &key model)
  "Fast whole-region rewrite (diffusion-style edit)."
  (funcall (client-fn :diffuse) prompt :model model))

;;; ---- helpers --------------------------------------------------------------

(defun normalize-tool-calls (raw)
  "Normalize provider tool-call entries into a canonical list of
(:name string :arguments plist). Accepts a list of plists."
  (mapcar (lambda (tc)
            (list :name (string (or (getf tc :name) (getf tc :tool) ""))
                  :arguments (or (getf tc :arguments) (getf tc :args) '())))
          raw))

(defun estimate-tokens (text)
  "Rough token estimate (~4 chars/token)."
  (ceiling (length text) 4))

(defun estimate-cost (model-name input-text output-text)
  "Estimate USD cost for a call given input/output text and a model's rates."
  (let ((m (find-model model-name)))
    (if m
        (+ (* (/ (estimate-tokens input-text) 1000000.0) (model-input-cost m))
           (* (/ (estimate-tokens output-text) 1000000.0) (model-output-cost m)))
        0.0)))
