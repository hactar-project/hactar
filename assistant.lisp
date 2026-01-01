(in-package :hactar)
(nhooks:define-hook-type assistant-extraction-updated (function (string) t)
  "Hook run after the assistant's screen extraction is updated. Handler takes the new extraction string.")
(defvar *assistant-extraction-hook* (make-instance 'hook-assistant-extraction-updated)
                                    "Hook for assistant screen extraction updates.")
(defun handle-assistant-extraction-update (extraction-text)
  "Handles updates to the assistant's screen extraction.
   Writes to file if *assistant-output-file* is set.
   Generates and plays TTS if *assistant-audio-enabled* is set and not muted."
  (debug-log "Assistant extraction updated. Running hook handler.")
  (when *assistant-output-file*
    (handler-case
        (write-file-content *assistant-output-file* extraction-text)
      (error (e)
        (format t "~&Assistant: Error writing extraction to ~A: ~A~%" *assistant-output-file* e))))

  (when (and *assistant-audio-enabled* (not *assistant-audio-muted*))
    (debug-log "Assistant audio enabled and not muted. Generating TTS.")
    ;; Use a temporary file that will be kept until the next audio generation
    (let ((audio-output-path (uiop:with-temporary-file (:pathname p :type "wav" :prefix "hactar-assistant-audio-" :keep t) p)))
      (if (generate-tts-audio extraction-text audio-output-path)
          (progn
            (when *assistant-last-audio-file*
              (ignore-errors (delete-file *assistant-last-audio-file*))) ; Delete old audio file
            (setf *assistant-last-audio-file* audio-output-path)
            (format t "~&Assistant: TTS audio generated: ~A~%" (uiop:native-namestring *assistant-last-audio-file*))
            (play-audio-file *assistant-last-audio-file*))
        (format t "~&Assistant: Failed to generate TTS audio.~%"))))
  (debug-log "Assistant extraction hook handler finished."))

(nhooks:add-hook *assistant-extraction-hook* #'handle-assistant-extraction-update)

(defun is-ai-edit-comment-event? (pathname event-type)
  "Detects AI! edit requests on file add/change."
  (when (and (member event-type '(:file-added :file-changed))
             (probe-file pathname))
    (let ((content (read-file-content pathname)))
      (when content
        (search "AI!" content)))))

(defun is-ai-question-comment-event? (pathname event-type)
  "Detects AI? question requests on file add/change."
  (when (and (member event-type '(:file-added :file-changed))
             (probe-file pathname))
    (let ((content (read-file-content pathname)))
      (when content
        (search "AI?" content)))))

(defun process-ai-question-file (pathname)
  "Processes AI? comments by asking the LLM to answer questions about the code."
  (debug-log "Processing AI? comments in file:" pathname)
  (add-file-to-context pathname)
  (let* ((relative-path (uiop:native-namestring (uiop:enough-pathname pathname *repo-root*)))
         (content (read-file-content pathname)))
    (when content
      (let ((prompt (format nil "Answer the questions marked with 'AI?' comments in the file: ~A.~%Base your answers on the code and current project context. Provide clear explanations and examples if helpful. Do not propose code edits or SEARCH/REPLACE blocks.~%~%File content:~%```~%~A~%```"
                            relative-path content)))
        (format t "~&Asking LLM to answer AI? questions for: ~A~%" relative-path)
        (get-llm-response prompt :stream t :add-to-history t)))))

(def-analyzer ai-comment-edit ((*file-event-hook* #'is-ai-edit-comment-event?)) nil (pathname event-type)
  "Detects 'AI!' comments in files and queues them for LLM processing to make code changes."
  (declare (ignore event-type))
  (debug-log "Analyzer-AI-Comment-Edit: Detected 'AI!' in" pathname)
  (add-to-ai-comment-queue pathname)
  (maybe-process-next-ai-comment))

(def-analyzer ai-comment-question ((*file-event-hook* #'is-ai-question-comment-event?)) nil (pathname event-type)
  "Detects 'AI?' comments in files and asks the LLM to answer questions about the code."
  (declare (ignore event-type))
  (debug-log "Analyzer-AI-Comment-Question: Detected 'AI?' in" pathname)
  (process-ai-question-file pathname))
(defun is-ai-comment-event? (pathname event-type)
  "Checks if a file event is relevant for the AI comment analyzer.
   Specifically, if the file was added/changed and contains 'AI!' on a line."
  (when (and (member event-type '(:file-added :file-changed))
             (probe-file pathname))
    (let ((content (read-file-content pathname)))
      (when content
        (search "AI!" content)))))

(defun add-to-ai-comment-queue (pathname)
  "Adds a pathname to the AI comment processing queue if not already present."
  (pushnew pathname *ai-comment-queue* :test #'equal)
  (debug-log "AI Comment Queue (after add):" *ai-comment-queue*))

(defun process-ai-comment-file (pathname)
  "Processes a single file for AI! comments."
  (debug-log "Processing AI! comments in file:" pathname)
  (add-file-to-context pathname)
  (let* ((relative-path (uiop:native-namestring (uiop:enough-pathname pathname *repo-root*)))
         (prompt-template-path (get-prompt-path "ai-comment-analyzer.mustache"))
         (prompt-template (handler-case (uiop:read-file-string prompt-template-path)
                            (error (e)
                              (unless *silent*
                                (format t "Error reading AI comment analyzer prompt '~A': ~A~%" prompt-template-path e))
                              (return-from process-ai-comment-file nil))))
         (prompt (mustache:render* prompt-template `((:file . ,relative-path)))))
    (unless *silent*
      (format t "~&Asking LLM to process AI! comments in: ~A~%" relative-path))
    ;; Response will be handled by search-replace-processor hook
    (get-llm-response prompt :stream nil :add-to-history t)))

(defun maybe-process-next-ai-comment ()
  "Checks the AI comment queue and processes the next file if the lock is free."
  (when (bt:acquire-lock *ai-comment-processor-lock* nil) ; Try to acquire lock, don't wait
    (unwind-protect
         (when *ai-comment-queue*
           (let ((pathname (pop *ai-comment-queue*)))
             (debug-log "Popped from AI comment queue:" pathname)
             (process-ai-comment-file pathname)))
      (bt:release-lock *ai-comment-processor-lock*))
    ;; After releasing the lock, if there's more work, spawn a thread to attempt it.
    (when *ai-comment-queue*
      (bt:make-thread (lambda () (maybe-process-next-ai-comment))
                      :name "AI Comment Processor Trigger"))))
(define-command copy (args)
  "Copy the last assistant message to the clipboard."
  (declare (ignore args))
  (let ((last-assistant-message (find "assistant" *chat-history*
                                      :key (lambda (msg) (cdr (assoc :role msg)))
                                      :test #'string=
                                      :from-end t)))
    (if last-assistant-message
        (let ((content-to-copy (cdr (assoc :content last-assistant-message))))
          (if (and content-to-copy (> (length content-to-copy) 0))
              (copy-to-clipboard content-to-copy)
              (format t "Last assistant message has no content to copy.~%")))
        (format t "No assistant messages found in history.~%"))))

(define-command assistant-update (args)
                "Manually trigger assistant screen analysis with an optional prompt."
                (if *assistant-mode-active*
                    (let ((user-prompt (if args
                                         (format nil "~{~A~^ ~}" args)
                                         "Describe what you see.")))
                      (assistant-update-query user-prompt))
                    (format t "Assistant mode is not active. Use --assistant flag to enable it.~%")))


(defun take-focused-window-screenshot ()
  "Takes a screenshot of the focused window. Supports niri for now.
   Returns the pathname to the saved screenshot (PNG) or nil on failure."
  (let ((desktop-env (uiop:getenv "XDG_CURRENT_DESKTOP")))
    (cond
      ((search "niri" desktop-env :test #'string-equal)
       (let ((temp-png-path (uiop:with-temporary-file (:pathname p :type "png" :prefix "hactar-assistant-screenshot-" :keep t) p)))
         (handler-case
             (progn
               (debug-log "Attempting to take screenshot with niri...")
               (uiop:run-program '("niri" "msg" "action" "screenshot-window") :output *debug-stream* :error-output *debug-stream*)
               (debug-log "niri screenshot action triggered. Attempting to paste from clipboard...")
               (sleep 0.5) ; Give a moment for clipboard to populate
               (uiop:run-program (format nil "wl-paste -t image/png > ~A" (uiop:native-namestring temp-png-path))
                                 :output *debug-stream* :error-output *debug-stream* :shell t)
               (if (and (probe-file temp-png-path) (> (file-length (open temp-png-path)) 0))
                   (progn
                     (debug-log "Screenshot saved to:" temp-png-path)
                     temp-png-path)
                   (progn
                     (format t "~&Error: Failed to save screenshot from clipboard or screenshot is empty.~%")
                     (ignore-errors (delete-file temp-png-path))
                     nil)))
           (error (e)
             (format t "~&Error taking screenshot with niri/wl-paste: ~A~%" e)
             (ignore-errors (delete-file temp-png-path))
             nil))))
      (t
       (format t "~&Warning: Focused window screenshot not supported for current desktop environment: ~A. Only niri is supported.~%" desktop-env)
       nil))))

(defun generate-tts-audio (text output-pathname)
  "Generates a WAV audio file from TEXT using Piper TTS.
   Returns T on success, NIL on failure."
  (unless (find-executable "piper")
    (format t "~&Error: 'piper' executable not found. Cannot generate TTS.~%")
    (return-from generate-tts-audio nil))
  (unless (probe-file *piper-model-path*)
    (format t "~&Error: Piper model '~A' not found. Cannot generate TTS.~%" *piper-model-path*)
    (return-from generate-tts-audio nil))

  (handler-case
      (with-input-from-string (input-stream text)
        (uiop:run-program (list "piper" "--model" (uiop:native-namestring *piper-model-path*)
                                "--output_file" (uiop:native-namestring output-pathname))
                          :input input-stream
                          :output *debug-stream* :error-output *debug-stream*)
        (debug-log "TTS audio generated:" output-pathname)
        t)
    (error (e)
      (format t "~&Error generating TTS audio with Piper: ~A~%" e)
      nil)))

(define-command playaudio (args)
                "Plays an audio file. Usage: /playaudio <file_path>"
                (if (null args)
                    (format t "Usage: /playaudio <file_path>~%")
                    (let ((file-path (first args)))
                      (if (probe-file file-path)
                          (play-audio-file file-path)
                          (format t "File not found: ~A~%" file-path)))))

(defun assistant-update-query (user-prompt)
  "Handles assistant mode updates: takes a screenshot if applicable, then gets an LLM response.
   USER-PROMPT: The prompt text from the user."
  (unless *current-model*
    (return-from assistant-update-query
      (format nil "Error: No model selected. Use /model to select a model.")))
  (unless (and *assistant-mode-active*
               (member "vision" (model-config-supports *current-model*) :test #'string=))
    (format t "The model does not support vision ~%")
    (return-from assistant-update-query nil))

  (progn
    (debug-log "Assistant mode active, taking screenshot for assistant-update.")
    (let ((new-screenshot-path (take-focused-window-screenshot)))
      (if new-screenshot-path
          (progn
            (when *assistant-last-screenshot-path*
              (drop-image-from-context (uiop:native-namestring *assistant-last-screenshot-path*))
              (ignore-errors (delete-file *assistant-last-screenshot-path*))
              (debug-log "Assistant: Removed old screenshot:" *assistant-last-screenshot-path*))
            (setf *assistant-last-screenshot-path* new-screenshot-path)
            (add-image-to-context (uiop:native-namestring new-screenshot-path)
                                  (or *assistant-previous-image-description* user-prompt))
            (format t "~&Assistant: Screenshot taken and added to context: ~A~%" (uiop:native-namestring new-screenshot-path)))
          (format t "~&Assistant: Failed to take screenshot. Proceeding without it.~%")))
    ;; Call get-llm-response, which will now use the potentially updated context (with new screenshot)
    (get-llm-response user-prompt :add-to-history t)))
