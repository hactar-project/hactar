(in-package #:hactar-rebuild)

;;; Self-update: hooks around turns diffuse edits into the generated region,
;;; validate against the closed surface, then reload or roll back.

(defvar *last-update-at* 0)
(defvar *update-debounce-seconds* 5)

;; Declare the primary hook and attach the default debounced handler. Wrapped in
;; an install function and registered so it survives reset-registries (reloads).
(defun record-fs-event (event)
  "Record a filesystem event plist (e.g. from hactar-watch) on *UPDATE-LEDGER*."
  (push (list :kind :fs
              :event (getf event :kind)
              :path (getf event :path)
              :at (get-universal-time))
        *update-ledger*))

(defun make-watch-handler ()
  "Return a callback suitable for HACTAR-WATCH:ON-CHANGE."
  #'record-fs-event)

(defun install-self-update-defaults ()
  (defhook turn.after :args (turn activity)
    :doc "Runs after a completed user/agent turn to update the harness.")
  (defprompt diffusion-system ()
    "You are a code intelligence system generating valid Hactar DSL forms only. Output nothing except valid closed-surface forms.")
  (defprompt diffusion-incremental (current-forms ledger)
    "Current generated region:~%~S~%~%Recent activity:~%~{~S~%~}~%~
Return a complete replacement generated region (DSL forms only).")
  (add-handler 'turn.after
    (lambda (turn activity)
      (declare (ignore turn))
      (push activity *update-ledger*)
      (when (and *auto-update*
                 (> (- (get-universal-time) *last-update-at*)
                    *update-debounce-seconds*))
        (run-self-update)))))

(install-self-update-defaults)
(register-post-reset 'install-self-update-defaults)

(defun collect-signal ()
  "Collect current engine context and recent activity for self-update synthesis."
  (list :stack *stack*
        :project-map *project-map*
        :events (copy-list *update-ledger*)))

(defun signal-ready-p (signal)
  "Return true when SIGNAL has recorded activity."
  (plusp (length (getf signal :events))))

(defun current-generated-region (path)
  "Return the forms currently inside the generated region of PATH."
  (let ((forms (ignore-errors (read-forms path))))
    (loop for form in forms
          when (and (consp form) (symbolp (car form))
                    (string-equal (symbol-name (car form)) "generated"))
            do (return (strip-generated-metadata (cdr form))))))

(defun build-update-prompt (current-forms ledger)
  "Compose the diffusion prompt from the current region and activity ledger."
  (format nil "Current generated region:~%~S~%~%Recent activity:~%~{~S~%~}~%~
               Return a complete replacement generated region (DSL forms only)."
          current-forms (subseq ledger 0 (min 20 (length ledger)))))

(defun parse-diffusion-output (raw)
  "Parse RAW diffusion output into a list of DSL forms (reader-eval disabled)."
  (with-input-from-string (s raw)
    (let ((*read-eval* nil)
          (*package* (find-package "HACTAR-REBUILD"))
          (forms '()))
      (loop for form = (read s nil :eof)
            until (eq form :eof)
            do (push form forms))
      (nreverse forms))))

(defun record-update-success (&optional detail)
  (setf *update-status*
        (list :status :ok :at (get-universal-time) :detail detail)))

(defun record-update-failure (errors)
  (setf *update-status*
        (list :status :error :at (get-universal-time) :errors errors)))

(defun last-update-failed-p ()
  (eq (getf *update-status* :status) :error))

(defun update-status ()
  "Return the last self-update outcome as a RESULT."
  (if *update-status*
      (make-result :status (getf *update-status* :status)
                   :message (format nil "~S" *update-status*)
                   :data *update-status*)
      (ok "No self-update has run yet.")))

(defun run-self-update ()
  "Diffuse edits into the generated region, validate, and reload or roll back."
  (setf *last-update-at* (get-universal-time))
  (let* ((path (hactar-file-path *repo-root*))
         (backup (and (probe-file path) (uiop:read-file-string path)))
         (current (current-generated-region path))
         (raw (diffusion-complete (build-update-prompt current *update-ledger*)))
         (forms (handler-case (parse-diffusion-output raw)
                  (error (e) (record-update-failure (list (princ-to-string e)))
                    (return-from run-self-update (update-status))))))
    (multiple-value-bind (ok-p errs) (validate-forms forms)
      (cond
        ((not ok-p) (record-update-failure errs) (update-status))
        (t (handler-case
               (progn
                 (write-hactar-file path forms)
                 (load-hactar-file path)
                 (record-update-success (format nil "~D form(s)" (length forms)))
                 (update-status))
             (error (e)
               (when backup (atomic-write path backup))
               (record-update-failure (list (princ-to-string e)))
               (update-status))))))))
