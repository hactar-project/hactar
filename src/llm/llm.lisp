(in-package #:hactar-llm)

;;; Provider-agnostic LLM wrapper. Ships with a deterministic, network-free
;;; mock client so the whole stack (bootstrap, self-update, agents) runs
;;; offline. Real provider clients plug in by binding *LLM-CLIENT*.

(defvar *default-system-prompt* "You are a helpful assistant.")

;;; ---- model registry -------------------------------------------------------

(defstruct (model (:conc-name model-))
  name
  provider
  id
  (context-window 8192)
  (max-output-tokens 4096)
  (input-cost 0.0)
  (output-cost 0.0)
  (edit-format "diff")
  extra-params)

(defvar *models* (make-hash-table :test 'equal))

(defun parse-model-spec (spec)
  "Split \"provider/model-id\" into (values provider-keyword model-id).
If SPEC has no slash, returns (values NIL SPEC)."
  (let* ((str (string spec))
         (slash (position #\/ str)))
    (if slash
        (values (intern (string-upcase (subseq str 0 slash)) :keyword)
                (subseq str (1+ slash)))
        (values nil str))))

(defun register-model (name &key provider id (context-window 8192)
                              (max-output-tokens 4096)
                              (input-cost 0.0) (output-cost 0.0)
                              (edit-format "diff") extra-params)
  (multiple-value-bind (inferred-provider inferred-id) (parse-model-spec name)
    (setf (gethash name *models*)
          (make-model :name name
                      :provider (or provider inferred-provider)
                      :id (or id inferred-id)
                      :context-window context-window
                      :max-output-tokens max-output-tokens
                      :input-cost input-cost
                      :output-cost output-cost
                      :edit-format edit-format
                      :extra-params extra-params))))

(defun find-model (name) (gethash name *models*))
(defun list-models () (loop for m being the hash-values of *models* collect m))
(defun resolve-model (name)
  "Resolve NAME or signal an explicit error."
  (or (find-model name) (error "Unknown model: ~A" name)))

(register-model "anthropic/claude-sonnet-4-6" :provider :anthropic
                :context-window 200000 :input-cost 3.0 :output-cost 15.0)
(register-model "anthropic/claude-haiku" :provider :anthropic
                :context-window 200000 :input-cost 0.8 :output-cost 4.0)
(register-model "hactar-mock" :provider :mock)

;;; ---- client protocol ------------------------------------------------------

(defstruct provider name complete chat stream diffuse embed oauth)
(defvar *providers* (make-hash-table :test 'equal))

(defun register-provider (provider)
  (setf (gethash (string-downcase (string (provider-name provider)))
                 *providers*)
        provider))

(defun find-provider (name)
  (gethash (string-downcase (string name)) *providers*))

(defun list-providers ()
  (loop for provider being the hash-values of *providers* collect provider))

(defun make-client (&key complete chat stream diffuse embed oauth)
  "Create a client protocol value from operation functions."
  (list :complete complete :chat chat :stream stream :diffuse diffuse
        :embed embed :oauth oauth))

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
   :stream (lambda (prompt callback &key model)
             (declare (ignore model))
             (let ((output (or *mock-llm-output* prompt)))
               (funcall callback output)
               output))
   :diffuse (lambda (prompt &key model)
              (declare (ignore model))
              (or *mock-diffusion-output* prompt))
   :embed (lambda (input &key model)
            (declare (ignore model))
            (vector (float (sxhash input) 1.0)))))

(defvar *llm-client* (make-mock-client)
  "Active client. Bind to a real provider plist to go live.")

(defun client-fn (op)
  (or (getf *llm-client* op)
      (error "Active LLM client does not implement ~A" op)))

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

(defun llm-stream (prompt callback &key model)
  "Stream PROMPT chunks to CALLBACK using the active client."
  (funcall (client-fn :stream) prompt callback :model model))

(defun diffusion-complete (prompt &key model)
  (diffuse prompt :model model))

(defun embed (input &key model)
  "Return an embedding from the active client."
  (funcall (client-fn :embed) input :model model))

(defun oauth-login (provider &rest args)
  "Delegate provider authentication to its registered OAuth function."
  (let ((definition (or (find-provider provider)
                        (error "Unknown LLM provider: ~A" provider))))
    (unless (provider-oauth definition)
      (error "Provider ~A does not support OAuth" provider))
    (apply (provider-oauth definition) args)))

;;; ---- helpers --------------------------------------------------------------

(defun prepare-messages (messages system-prompt &key (add-system-prompt t))
  "Normalize MESSAGES (string, single message alist, or list of alists)
and optionally prepend SYSTEM-PROMPT."
  (let ((normalized
          (cond
            ((stringp messages)
             (list `((:role . "user") (:content . ,messages))))
            ((and (consp messages) (consp (first messages))
                  (atom (car (first messages))))
             (list messages))
            (t (copy-list messages)))))
    (when (and add-system-prompt system-prompt (plusp (length system-prompt)))
      (unless (and normalized
                   (equal (cdr (assoc :role (first normalized))) "system"))
        (push `((:role . "system") (:content . ,system-prompt)) normalized)))
    normalized))

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

(defun token-count (text)
  (estimate-tokens text))

(defun estimate-cost (model-name input-text output-text)
  "Estimate USD cost for a call given input/output text and a model's rates."
  (let ((m (find-model model-name)))
    (if m
        (+ (* (/ (estimate-tokens input-text) 1000000.0) (model-input-cost m))
           (* (/ (estimate-tokens output-text) 1000000.0) (model-output-cost m)))
        0.0)))
