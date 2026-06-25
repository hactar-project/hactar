;; llm utility functions
(in-package :hactar)

(defun ask (question &key (model-name *default-llm*) (system-prompt llm:*default-system-prompt*))
  "Ask a single question to the LLM and return the response string.
   MODEL-NAME defaults to *default-llm*. Returns NIL on failure."
  (let* ((model (find-model-by-name model-name))
         (provider (if model
                       (intern (string-upcase (model-config-provider model)) :keyword)
                       :ollama))
         (model-id (if model (model-config-model-name model) model-name)))
    (unless model
      (debug-log "Warning: Model" model-name "not found, falling back to :ollama"))
    (multiple-value-bind (content tool-calls messages)
        (llm:complete provider
                     question
                     :model model-id
                     :system-prompt system-prompt
                     :stream nil)
      (declare (ignore tool-calls messages))
      content)))

(defun moderate (text &key (model-name *default-llm*))
  "Send TEXT to the LLM for content moderation.
   Returns a plist (:flagged T/NIL :reason <string-or-nil>).
   The LLM is asked to respond with JSON containing \"flagged\" and \"reason\" fields."
  (let* ((model (find-model-by-name model-name))
         (provider (if model
                       (intern (string-upcase (model-config-provider model)) :keyword)
                       :ollama))
         (model-id (if model (model-config-model-name model) model-name))
         (system-prompt "You are a content moderation system. Analyze the provided text and respond ONLY with a JSON object containing two fields: \"flagged\" (boolean, true if the content violates safety policies) and \"reason\" (string, brief explanation if flagged, null if not flagged). Do not include any other text.")
         (messages `(((:role . "user") (:content . ,text)))))
    (unless model
      (debug-log "Warning: Model" model-name "not found for moderation, falling back to :ollama"))
    (multiple-value-bind (content tool-calls msgs)
        (llm:complete provider
                     messages
                     :model model-id
                     :system-prompt system-prompt
                     :stream nil
                     :response-format "json_object")
      (declare (ignore tool-calls msgs))
      (if content
          (handler-case
              (let* ((cleaned (string-trim '(#\Space #\Tab #\Newline #\Return) content))
                     ;; Strip markdown code fences if present
                     (json-str (if (str:starts-with-p "```" cleaned)
                                   (let ((block (extract-md-fenced-code-block cleaned)))
                                     (if block
                                         (cdr (assoc :contents block))
                                         cleaned))
                                   cleaned))
                     (parsed (cl-json:decode-json-from-string json-str))
                     (flagged-raw (cdr (assoc :flagged parsed)))
                     (reason (cdr (assoc :reason parsed))))
                (list :flagged (if flagged-raw t nil)
                      :reason reason))
            (error (e)
              (debug-log "Error parsing moderation response:" e "Raw:" content)
              (list :flagged nil :reason nil)))
          (progn
            (debug-log "Moderation returned no content")
            (list :flagged nil :reason nil))))))

(defun embed (text &key (model "nomic-embed-text") (endpoint nil))
  "Return an embedding vector (list of numbers) for TEXT using Ollama.
   MODEL defaults to \"nomic-embed-text\".
   ENDPOINT defaults to the Ollama embeddings endpoint."
  (if endpoint
      (llm:ollama-embed text :model model :endpoint endpoint)
      (llm:ollama-embed text :model model)))

(defun generate-fallback-lisp-forms-string (prompt)
  "Generate programmatic fallback LISP forms string based on local scanning."
  (let ((prompt-lower (string-downcase prompt))
        (root (or *repo-root* (uiop:getcwd))))
    (cond
      ((search "routes" prompt-lower)
       (let ((routes (extract-routes root)))
         (with-output-to-string (s)
           (format s "(defroutes~%")
           (dolist (r routes)
             (let ((name (and (route-entry-name r) (string-downcase (symbol-name (route-entry-name r)))))
                   (tags (mapcar (lambda (tag) (string-downcase (symbol-name tag))) (route-entry-tags r))))
               (format s "  (:file ~S :path ~S~@[ :name ~A~] :tags (~{~A~^ ~})~@[ :line-start ~A~]~@[ :line-end ~A~])~%"
                       (route-entry-file r)
                       (route-entry-path r)
                       name
                       tags
                       (route-entry-line-start r)
                       (route-entry-line-end r))))
           (format s ")~%"))))
      ((or (search "model" prompt-lower) (search "prisma" prompt-lower))
       (let ((models (extract-models root)))
         (with-output-to-string (s)
           (format s "(defmodels~%")
           (dolist (m models)
             (let ((name (and (model-entry-name m) (string-downcase (symbol-name (model-entry-name m)))))
                   (fields (mapcar #'string-downcase (model-entry-fields m))))
               (format s "  (:name ~A :file ~S :fields (~{~A~^ ~}))~%"
                       name
                       (model-entry-file m)
                       fields)))
           (format s ")~%"))))
      ((or (search "script" prompt-lower) (search "package.json" prompt-lower))
       (let* ((json (pcli-read-package-json root))
              (scripts (cdr (assoc :scripts json))))
         (with-output-to-string (s)
           (format s "(defscripts~%")
           (loop for (k . v) in scripts
                 do (format s "  (:script ~S :command ~S)~%"
                            (string-downcase (string k)) v))
           (format s ")~%"))))
      (t
       ""))))

(defun diffusion-complete (prompt &key (model "diffusion-gemma"))
  "Send a prompt to the diffusion model for LISP form generation.
   Returns raw text that should be parsed/validated by the LISP form parser."
  (if (or (string-equal model "fast-diffusion")
          (null (uiop:getenv "HACTAR_DIFFUSION_MODEL")))
      (generate-fallback-lisp-forms-string prompt)
      (let* ((model-def (or (find-model-by-name model)
                            (find-model-by-name "diffusion-gemma")))
             (provider (if model-def
                           (intern (string-upcase (model-config-provider model-def)) :keyword)
                           :nvidia-nim))
             (model-id (if model-def
                           (model-config-model-name model-def)
                           "google/diffusiongemma-26b-a4b-it"))
             (system-prompt (or (get-prompt :diffusion-system-prompt)
                                "You are a code intelligence system that generates Common Lisp S-expressions to describe code structure. Output ONLY valid LISP forms.")))
        (handler-case
            (multiple-value-bind (content tool-calls msgs)
                (llm:complete provider
                              prompt
                              :model model-id
                              :system-prompt system-prompt
                              :stream nil)
              (declare (ignore tool-calls msgs))
              (or content (generate-fallback-lisp-forms-string prompt)))
          (error ()
            (generate-fallback-lisp-forms-string prompt))))))
