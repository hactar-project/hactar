(in-package :hactar)

(defun collect-signal (project)
  "Collect recent history, logs, or file changes to build a synthesis context."
  (let* ((pcli (get-project-cli project))
         (stack (and pcli (project-cli-stack pcli)))
         (routes (and pcli (project-cli-routes pcli)))
         (models (and pcli (project-cli-models pcli)))
         (recent-events (or *project-cli-update-queue* '())))
    (list :project project
          :stack stack
          :routes routes
          :models models
          :events recent-events)))

(defun signal-ready-p (signal current-forms)
  "Debounce / threshold logic. Returns T if there are meaningful new events or changes."
  (declare (ignore current-forms))
  (let ((events (getf signal :events)))
    (and events (> (length events) 0))))

(defun diffuse-forms (project signal current-forms)
  "Generate candidate forms using the LLM/diffusion model."
  (let* ((events (getf signal :events))
         (stack (getf signal :stack))
         (prompt (format nil "Update the Hactar project ~S with stack ~A.
Recent file changes/events: ~A.
Current project forms:
~S"
                         project stack events current-forms))
         (raw-resp (diffusion-complete prompt)))
    (when (and raw-resp (not (string= raw-resp "")))
      (parse-diffusion-output raw-resp))))

(defun validate-forms (forms)
  "Validate forms against the allowed Surface schema."
  (let ((valid-forms '()))
    (dolist (form forms)
      (multiple-value-bind (valid-p errors corrected)
          (validate-hactar-form form)
        (declare (ignore errors))
        (when valid-p
          (push (or corrected form) valid-forms))))
    (nreverse valid-forms)))

(defun apply-update (project merged-forms)
  "Write merged forms back to the project's .hactar.lisp and reload them."
  (let* ((pcli (get-project-cli project))
         (root (or (and pcli (project-cli-root pcli)) *repo-root*))
         (file (project-cli-pathname root)))
    (when file
      (write-hactar-lisp merged-forms file :merge-p nil)
      (load-project-cli-file root))))

(defun synth-update (project &key (apply t))
  "One self-update tick. Returns the merged forms (or NIL if no change)."
  (let* ((pcli-name (if (stringp project) project (project-cli-name project)))
         (pcli (get-project-cli pcli-name))
         (root (or (and pcli (project-cli-root pcli)) *repo-root*))
         (file (project-cli-pathname root)))
    (when (and file (probe-file file))
      (let* ((signal (collect-signal pcli-name))
             (current (read-hactar-lisp file))
             (candidates (when (signal-ready-p signal current)
                           (diffuse-forms pcli-name signal current))))
        (when candidates
          (let ((valid (validate-forms candidates)))
            (when valid
              (let ((merged (merge-hactar-forms current valid)))
                (when apply
                  (apply-update pcli-name merged))
                merged))))))))
