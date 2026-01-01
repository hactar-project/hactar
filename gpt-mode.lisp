;; special gpt handling stuff
(in-package :hactar)

;;; GPT-5-mini mode: when using gpt-5-mini, always instruct it to use read_file tool

(defun gpt-5-mini-model-p (model)
  "Return T if MODEL is a gpt-5-mini variant."
  (when model
    (let ((name (model-config-name model)))
      (and name (search "gpt-5-mini" (string-downcase name))))))

(defrule gpt-5-mini-read-file-rule *model-changed-hook* (new-model old-model)
  "When gpt-5-mini is active, add a rule instructing it to always use read_file."
  (let ((_ old-model))
    (declare (ignore _)))
  (when (gpt-5-mini-model-p new-model)
    "IMPORTANT: You MUST always use the read_file tool to read file contents. Do NOT use execute_command to read files."))
