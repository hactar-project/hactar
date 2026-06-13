(in-package :hactar)

(defvar *commands*)  ; forward declaration — defined in commands.lisp

;;* state
(defvar *tui-running* nil "T when the 3-column TUI is active.")
(defvar *tui-chat-lines* '() "List of chat line plists for the main panel.")
(defvar *tui-sidebar-model* nil "Current model name string for sidebar display.")
(defvar *tui-sidebar-files* '() "List of modified file strings for sidebar.")
(defvar *tui-sidebar-docs* '() "List of doc titles in context for sidebar.")
(defvar *tui-sidebar-lsps* '() "List of LSP plists ((:name . n) (:status . s)).")
(defvar *tui-sidebar-mcps* '() "List of MCP name strings.")
(defvar *tui-sidebar-tool-calls* '() "List of active tool call plists: (:id :name :status :kind).")
(defvar *tui-input-buffer* "" "Current input line text.")
(defvar *tui-input-cursor* 0 "Cursor position in input buffer.")
(defvar *tui-scroll-offset* 0 "Scroll offset for chat panel (0 = bottom).")
(defvar *tui-command-modal-open* nil "T when the command palette modal is open.")
(defvar *tui-command-query* "" "Filter query for the command modal.")
(defvar *tui-command-selected* 0 "Selected index in filtered command list.")
(defvar *tui-command-tab* :system "Active tab in command modal: :system or :user.")
(defvar *tui-status-line* "" "Status bar text at the very bottom.")
(defvar *tui-status-type* :info "Status bar type: :info, :ok, :error.")
(defvar *tui-project-name* "" "Project name for sidebar header.")
(defvar *tui-project-path* "" "Project path for sidebar.")
(defvar *tui-thinking-seconds* nil "If non-nil, show 'Thought for Ns' indicator.")
(defvar *tui-completions* '() "Current list of completion candidates.")
(defvar *tui-completion-index* -1 "Current index in completion list (-1 = none selected).")
(defvar *tui-completion-prefix* "" "The prefix text that was completed.")
(defvar *tui-expand-tool-calls* nil "Whether tool call details are expanded in the chat panel.")
(defvar *tui-autocomplete-disabled* nil "T if the user explicitly dismissed autocomplete via Esc.")

;;** history
(defvar *tui-input-history* '() "List of previously sent input strings (newest first).")
(defvar *tui-history-index* -1 "Current position in history (-1 = not browsing).")
(defvar *tui-history-saved-input* "" "Saved input buffer from before entering history mode.")

;;** modals
(defvar *tui-modal-completions* '() "Completion candidates in the command modal.")
(defvar *tui-modal-completion-index* -1 "Selected index in modal completion list.")
(defvar *tui-modal-completion-prefix* "" "The query prefix when modal completion started.")

;;* utils
(defun tui-safe-write (win str col row max-width)
  "Write STR to WIN at (COL, ROW), truncating to MAX-WIDTH. Safe against out-of-bounds.
   Strips newlines to prevent ncurses from writing to unintended rows."
  (when (and (> max-width 0) (>= row 0) (>= col 0))
    (let* ((one-line (substitute #\Space #\Newline
                                 (substitute #\Space #\Return str)))
           (safe-str (if (> (length one-line) max-width)
                         (subseq one-line 0 max-width)
                       one-line)))
      (handler-case
          (charms:write-string-at-point win safe-str col row)
        (error () nil)))))

(defun tui-hline (win col row length &optional (ch #\─))
  "Draw a horizontal line on WIN."
  (loop for x from col below (+ col length)
        do (handler-case
               (charms:write-char-at-point win ch x row)
             (error () nil))))

(defun tui-vline (win col row length &optional (ch #\│))
  "Draw a vertical line on WIN."
  (loop for y from row below (+ row length)
        do (handler-case
               (charms:write-char-at-point win ch col y)
             (error () nil))))

(defun tui-box (win col row width height)
  "Draw a box outline on WIN."
  (let ((right (+ col width -1))
        (bottom (+ row height -1))) ;; corners
    (handler-case (charms:write-char-at-point win #\┌ col row) (error () nil))
    (handler-case (charms:write-char-at-point win #\┐ right row) (error () nil))
    (handler-case (charms:write-char-at-point win #\└ col bottom) (error () nil))
    (handler-case (charms:write-char-at-point win #\┘ right bottom) (error () nil))
    ;; edges
    (tui-hline win (1+ col) row (- width 2) #\─)
    (tui-hline win (1+ col) bottom (- width 2) #\─)
    (tui-vline win col (1+ row) (- height 2) #\│)
    (tui-vline win right (1+ row) (- height 2) #\│)))

;;* completion helpers
(defun tui-reset-completions ()
  "Clear any active completion state."
  (setf *tui-completions* '())
  (setf *tui-completion-index* -1)
  (setf *tui-completion-prefix* ""))

(defun tui-get-command-description (cmd-name)
  "Get the short description string for CMD-NAME."
  (let ((info (gethash cmd-name *commands*)))
    (if info
        (or (second info) "")
        "")))

(defun tui-completions-height ()
  "Return the number of lines required by the autocomplete UI below the input line."
  (if (and *tui-completions* (not *tui-command-modal-open*))
      (let* ((count (length *tui-completions*))
             (max-visible 5)
             (completions-height (min count max-visible))
             (has-more (> count max-visible))
             (more-height (if has-more 1 0))
             (hints-height 2))
        (+ completions-height more-height hints-height))
      0))

(defun tui-auto-complete-update ()
  "Populate *tui-completions* based on current input buffer."
  (let* ((input *tui-input-buffer*)
         (trimmed (string-trim '(#\Space #\Tab) input)))
    (if (and (> (length input) 0)
             (char= (char input 0) #\/))
        (let* ((words (str:split #\Space trimmed :omit-nulls t))
               (first-word (or (first words) "/")))
          (if (and (str:starts-with? "/" first-word)
                   (or (= (length words) 0)
                       (and (= (length words) 1)
                            (not (position #\Space input :from-end t :end (length input))))))
              ;; Complete command names
              (let* ((partial (if (string= first-word "") "/" first-word))
                     (matches (loop for cmd being the hash-keys of *commands*
                                    unless (gethash cmd *tui-hidden-commands*)
                                    when (str:starts-with-p partial cmd :ignore-case t)
                                    collect cmd)))
                (if matches
                    (progn
                      (setf *tui-completion-prefix* input)
                      (setf *tui-completions* (sort matches #'string<))
                      (setf *tui-completion-index* 0))
                    (tui-reset-completions)))
              ;; Complete command arguments
              (let* ((cmd-name first-word)
                     (rest-args (rest words))
                     (partial (if (and (> (length input) 0)
                                       (char= (char input (1- (length input))) #\Space))
                                  ""
                                  (or (car (last rest-args)) "")))
                     (args-without-partial (if (string= partial "")
                                               rest-args
                                               (butlast rest-args)))
                     (completions (or (get-command-completions cmd-name partial args-without-partial)
                                      (tui-path-completions partial))))
                (if completions
                    (progn
                      (setf *tui-completion-prefix* input)
                      (setf *tui-completions* completions)
                      (setf *tui-completion-index* 0))
                    (tui-reset-completions)))))
        (tui-reset-completions))))

(defun tui-on-input-change ()
  "Called whenever *tui-input-buffer* is modified by character insertion or deletion."
  (let ((input *tui-input-buffer*))
    ;; If the buffer is empty, reset the disabled flag
    (when (string= input "")
      (setf *tui-autocomplete-disabled* nil))
    ;; Update completions if not disabled
    (if (and (not *tui-autocomplete-disabled*)
             (str:starts-with? "/" input))
        (tui-auto-complete-update)
        (tui-reset-completions))))

(defun tui-accept-completion ()
  "Apply the currently selected completion to the input buffer."
  (when (and *tui-completions* (>= *tui-completion-index* 0)
             (< *tui-completion-index* (length *tui-completions*)))
    (let* ((completion (nth *tui-completion-index* *tui-completions*))
           (input *tui-completion-prefix*)
           (trimmed (string-trim '(#\Space #\Tab) input))
           (words (str:split #\Space trimmed :omit-nulls t))
           (first-word (first words)))
      ;; Determine if this was a command name or argument completion
      (if (and (str:starts-with? "/" first-word)
               (= (length words) 1)
               (not (position #\Space input :from-end t :end (length input))))
          ;; Command name completion - replace whole input
          (setf *tui-input-buffer* completion)
          ;; Argument completion - replace last word
          (let* ((prefix-end (or (position #\Space *tui-completion-prefix* :from-end t)
                                 -1))
                 (base (if (> prefix-end -1)
                           (subseq *tui-completion-prefix* 0 (1+ prefix-end))
                           "")))
            (setf *tui-input-buffer* (concatenate 'string base completion))))))
  (tui-reset-completions))

(defun tui-path-completions (text)
  "Return path completions for TEXT relative to *repo-root*."
  (let* ((prefix (if (string= text "") "./" text))
         (dir-part (or (directory-namestring prefix) ""))
         (name-part (file-namestring prefix))
         (base-dir (if (and dir-part (not (string= dir-part "")))
                       (merge-pathnames dir-part (or *repo-root* (uiop:getcwd)))
                       (or *repo-root* (uiop:getcwd)))))
    (when (probe-file base-dir)
      (sort
       (loop for entry in (directory (merge-pathnames (make-pathname :name :wild :type :wild :defaults base-dir) base-dir))
             for rel = (if *repo-root*
                           (uiop:native-namestring (uiop:enough-pathname entry *repo-root*))
                           (uiop:native-namestring entry))
             when (str:starts-with-p name-part (file-namestring rel) :ignore-case t)
             collect (if (uiop:directory-pathname-p entry)
                         (if (str:ends-with-p "/" rel) rel (format nil "~A/" rel))
                         rel))
       #'string<))))

(defun tui-try-complete ()
  "Attempt to complete the current input buffer. Populates the completion list for display."
  (if *tui-completions*
      ;; Already showing completions - cycle forward
      (setf *tui-completion-index*
            (mod (1+ *tui-completion-index*) (length *tui-completions*)))
      ;; Start new completion
      (let* ((input *tui-input-buffer*)
             (trimmed (string-trim '(#\Space #\Tab) input)))
        (when (> (length trimmed) 0)
          (let* ((words (str:split #\Space trimmed :omit-nulls t))
                 (first-word (first words)))
            ;; Check if typing a command name (no space yet or still on first word)
            (if (and (str:starts-with? "/" first-word)
                     (= (length words) 1)
                     (not (position #\Space input :from-end t :end (length input))))
                ;; Complete command names
                (let* ((partial first-word)
                       (matches (loop for cmd being the hash-keys of *commands*
                                      unless (gethash cmd *tui-hidden-commands*)
                                      when (str:starts-with-p partial cmd :ignore-case t)
                                      collect cmd)))
                  (when matches
                    (setf *tui-completion-prefix* input)
                    (setf *tui-completions* (sort matches #'string<))
                    (setf *tui-completion-index* 0)))
                ;; Complete command arguments
                (when (str:starts-with? "/" first-word)
                  (let* ((cmd-name first-word)
                         (rest-args (rest words))
                         (partial (if (and (> (length input) 0)
                                           (char= (char input (1- (length input))) #\Space))
                                      ""
                                      (or (car (last rest-args)) "")))
                         (args-without-partial (if (string= partial "")
                                                   rest-args
                                                   (butlast rest-args)))
                         (completions (or (get-command-completions cmd-name partial args-without-partial)
                                          (tui-path-completions partial))))
                    (when completions
                      (setf *tui-completion-prefix* input)
                      (setf *tui-completions* completions)
                      (setf *tui-completion-index* 0))))))))))

;;* input history helpers
(defun tui-history-push (text)
  "Push TEXT onto the input history stack (newest first). Deduplicates consecutive."
  (when (and (stringp text) (> (length text) 0))
    (when (or (null *tui-input-history*)
              (not (string= text (first *tui-input-history*))))
      (push text *tui-input-history*))))

(defun tui-history-navigate (direction)
  "Navigate input history. DIRECTION is :up (older) or :down (newer).
   Saves the current input on first :up, restores on exiting history."
  (when (null *tui-input-history*) (return-from tui-history-navigate))
  (let ((max-index (1- (length *tui-input-history*))))
    (case direction
      (:up
       (when (= *tui-history-index* -1)
         ;; Entering history mode — save current input
         (setf *tui-history-saved-input* *tui-input-buffer*))
       (setf *tui-history-index* (min max-index (1+ *tui-history-index*)))
       (setf *tui-input-buffer* (nth *tui-history-index* *tui-input-history*)))
      (:down
       (cond
         ((<= *tui-history-index* 0)
          ;; Exiting history mode — restore saved input
          (setf *tui-history-index* -1)
          (setf *tui-input-buffer* *tui-history-saved-input*))
         (t
          (decf *tui-history-index*)
          (setf *tui-input-buffer* (nth *tui-history-index* *tui-input-history*))))))))

(defun tui-history-reset ()
  "Reset history navigation state without changing the input buffer."
  (setf *tui-history-index* -1)
  (setf *tui-history-saved-input* ""))

;;* modal completion helpers
(defun tui-modal-reset-completions ()
  "Clear modal completion state."
  (setf *tui-modal-completions* '())
  (setf *tui-modal-completion-index* -1)
  (setf *tui-modal-completion-prefix* ""))

(defun tui-modal-accept-completion ()
  "Apply the selected modal completion to *tui-command-query*."
  (when (and *tui-modal-completions*
             (>= *tui-modal-completion-index* 0)
             (< *tui-modal-completion-index* (length *tui-modal-completions*)))
    (let* ((completion (nth *tui-modal-completion-index* *tui-modal-completions*))
           (query *tui-modal-completion-prefix*)
           (trimmed (string-trim '(#\Space #\Tab) query))
           (space-pos (position #\Space trimmed)))
      (if space-pos
          ;; Has command + args: replace the last partial arg
          (let* ((cmd-part (subseq trimmed 0 space-pos))
                 (args-part (string-trim '(#\Space #\Tab) (subseq trimmed space-pos)))
                 (words (if (string= args-part "")
                            '()
                            (str:split #\Space args-part :omit-nulls t)))
                 (trailing-space-p (and (> (length query) 0)
                                        (char= (char query (1- (length query))) #\Space)))
                 (prior-args (if trailing-space-p words (butlast words)))
                 (prior-str (if prior-args
                                (format nil "~{~A~^ ~}" prior-args)
                                ""))
                 (new-query (if (string= prior-str "")
                                (format nil "~A ~A" cmd-part completion)
                                (format nil "~A ~A ~A" cmd-part prior-str completion))))
            (setf *tui-command-query* new-query))
          ;; No space - shouldn't normally happen for arg completion
          (setf *tui-command-query* completion))))
  (tui-modal-reset-completions))

(defun tui-modal-try-complete ()
  "Populate modal completions based on current command query."
  (if *tui-modal-completions*
      ;; Already showing - cycle forward
      (setf *tui-modal-completion-index*
            (mod (1+ *tui-modal-completion-index*) (length *tui-modal-completions*)))
      ;; Start new completion
      (let* ((query *tui-command-query*)
             (trimmed (string-trim '(#\Space #\Tab) query))
             (space-pos (position #\Space trimmed)))
        (when space-pos
          ;; We have a command and are completing arguments
          (let* ((cmd-part (subseq trimmed 0 space-pos))
                 (cmd-name (if (str:starts-with? "/" cmd-part)
                               cmd-part
                               (format nil "/~A" cmd-part)))
                 (args-part (string-trim '(#\Space #\Tab) (subseq trimmed space-pos)))
                 (words (if (string= args-part "")
                            '()
                            (str:split #\Space args-part :omit-nulls t)))
                 (trailing-space-p (and (> (length query) 0)
                                        (char= (char query (1- (length query))) #\Space)))
                 (partial (if trailing-space-p "" (or (car (last words)) "")))
                 (prior-args (if trailing-space-p words (butlast words)))
                 (completions (get-command-completions cmd-name partial prior-args)))
            (when completions
              (setf *tui-modal-completion-prefix* query)
              (setf *tui-modal-completions* completions)
              (setf *tui-modal-completion-index* 0)))))))

;;* chat lines
(defun tui-add-chat-line (text &key (type :text) (role "system"))
  "Add a line to the chat panel. TYPE can be :text, :thought, :bash, :assistant, :user, :error."
  (push (list :text text :type type :role role) *tui-chat-lines*))

(defun tui-add-user-message (text)
  (tui-add-chat-line (format nil "> ~A" text) :type :user :role "user"))

(defun tui-add-assistant-message (text)
  (tui-add-chat-line (render-md-ansi text) :type :assistant :role "assistant"))

(defun tui-add-chat-tool-call (id name args)
  "Add a tool call line to the chat panel."
  (push (list :id id :name name :args args :status "in_progress" :type :tool-call :role "assistant" :result nil)
        *tui-chat-lines*))

(defun tui-update-chat-tool-call (id status &optional result)
  "Update the status and result of a tool call in the chat panel."
  (let ((entry (find id *tui-chat-lines* :key (lambda (line) (getf line :id)) :test #'string-equal)))
    (when entry
      (setf (getf entry :status) status)
      (when result
        (setf (getf entry :result) result)))))

(defun tui-add-thought (seconds)
  (tui-add-chat-line (format nil "Thought for ~As" seconds) :type :thought))

(defun tui-add-bash-output (task-name command output)
  (tui-add-chat-line (format nil "[ok] Bash task ~A" task-name) :type :bash)
  (when command
    (tui-add-chat-line (format nil "  task: ~A" command) :type :text))
  (when output
    (dolist (line (str:lines output))
      (tui-add-chat-line (format nil "    ~A" line) :type :text))))

;;* tool calls
(defun tui-add-tool-call (id name kind)
  "Register a new tool call in the sidebar. STATUS starts as \"pending\"."
  (push (list :id id :name name :status "pending" :kind (or kind "other"))
        *tui-sidebar-tool-calls*))

(defun tui-update-tool-call (id new-status)
  "Update the status of a tool call by ID. Status: pending, in_progress, completed, failed."
  (let ((entry (find id *tui-sidebar-tool-calls* :key (lambda (tc) (getf tc :id)) :test #'string=)))
    (when entry
      (setf (getf entry :status) new-status))))

(defun tui-remove-tool-call (id)
  "Remove a completed/failed tool call from the sidebar after a delay."
  (setf *tui-sidebar-tool-calls*
        (remove id *tui-sidebar-tool-calls* :key (lambda (tc) (getf tc :id)) :test #'string=)))

(defun tui-clear-tool-calls ()
  "Remove all tool calls from the sidebar."
  (setf *tui-sidebar-tool-calls* nil))

(defun tui-tool-call-status-color (status)
  "Return the color pair ID for a tool call status string."
  (tui-color-pair (tui-tool-call-status-color-role status)))

(defun tui-tool-call-status-icon (status)
  "Return a short icon/prefix for a tool call status."
  (cond
    ((string= status "pending") "◌")
    ((string= status "in_progress") "⟳")
    ((string= status "completed") "✓")
    ((string= status "failed") "✗")
    (t "?")))

(defun tui-set-status (text &optional (type :info))
  (setf *tui-status-line* text)
  (setf *tui-status-type* type))

;;* Sidebar
(defun tui-refresh-sidebar-data ()
  "Pull current state into sidebar display variables."
  (setf *tui-sidebar-model*
        (if *current-model* (model-config-name *current-model*) "None"))
  (setf *tui-project-name* (or *name* ""))
  (setf *tui-project-path*
        (if *repo-root* (format nil "~A" (uiop:native-namestring *repo-root*)) ""))
  (setf *tui-sidebar-files*
        (mapcar (lambda (f)
                  (let ((path (if (pathnamep f) f (pathname f))))
                    (if *repo-root*
                        (uiop:native-namestring (uiop:enough-pathname path *repo-root*))
                        (uiop:native-namestring path))))
                *files*))
  (setf *tui-sidebar-docs*
        (mapcar (lambda (d) (cdr (assoc :title d)))
                *docs-context*)))

(defun clean-tool-name (name)
  (let* ((name-str (format nil "~A" name))
         (colon-pos (position #\: name-str :from-end t)))
    (if colon-pos
        (subseq name-str (1+ colon-pos))
        name-str)))

(defun extract-tool-target-arg (args)
  "Find the most representative argument to display in the tool header."
  (let ((keys '(:path :command :message :msg :query :name :url :target-file :targetfile :target :value)))
    (loop for key in keys
          for val = (getf args key)
          when val
          return (format nil "~A" val)
          finally (return (if args
                              (format nil "~A" (cadr args))
                              "")))))

(defun format-tool-call-header (name args status)
  (let* ((cleaned-name (clean-tool-name name))
         (target-arg (extract-tool-target-arg args))
         (bullet-color (cond
                         ((string= status "completed") :tool-done)
                         ((string= status "failed") :tool-failed)
                         ((string= status "in_progress") :tool-running)
                         (t :tool-pending)))
         (hint-str (if *tui-expand-tool-calls* " (ctrl+o to collapse)" " (ctrl+o to expand)")))
    (list (cons "● " (list :color bullet-color :bold t))
          (cons (format nil "~A(~A)" cleaned-name target-arg) (list :bold t))
          (cons hint-str (list :color :muted)))))

(defun format-tool-call-details-string (args status result)
  "Format the expanded tool call details as a string."
  (with-output-to-string (s)
    (format s "  Arguments:~%")
    (if args
        (loop for (key val) on args by #'cddr
              do (format s "    ~S: ~S~%" key val))
        (format s "    None~%"))
    (cond
      ((string= status "completed")
       (format s "  Result:~%")
       (let ((res-str (if result (format nil "~A" result) "nil")))
         (dolist (line (str:lines res-str))
           (format s "    ~A~%" line))))
      ((string= status "failed")
       (format s "  Error:~%")
       (let ((err-str (if result (format nil "~A" result) "unknown error")))
         (dolist (line (str:lines err-str))
           (format s "    ~A~%" line)))))))

(defun dim-run (run)
  "Return a copy of RUN with :dim T added to its style plist."
  (cons (car run) (list* :dim t (cdr run))))

;;* Rendering
(defun tui-render-chat-panel (win col row width height)
  "Render the main chat area with visual distinction between message blocks."
  (let* ((content-width (- width 2))
         (all-display-lines '())
         (len (length *tui-chat-lines*))
         (most-recent-user-idx (position-if (lambda (line) (eq (getf line :type) :user))
                                            *tui-chat-lines*)))
    (loop for i from (1- len) downto 0
          for chat-line = (nth i *tui-chat-lines*)
          do (let* ((type (getf chat-line :type))
                    (dim-p (and most-recent-user-idx (> i most-recent-user-idx))))
               ;; If this is the most recent user prompt, insert the separator before it
               (when (and most-recent-user-idx (= i most-recent-user-idx))
                 (push (list :runs (list (cons "_____________________________" '(:color :muted :bold nil :dim nil))) :type :separator)
                       all-display-lines))

               (cond
                 ((eq type :tool-call)
                  (let* ((name (getf chat-line :name))
                         (args (getf chat-line :args))
                         (status (getf chat-line :status))
                         (result (getf chat-line :result))
                         (header-runs (format-tool-call-header name args status)))
                    (when dim-p
                      (setf header-runs (mapcar #'dim-run header-runs)))
                    (push (list :runs header-runs :type :tool-call) all-display-lines)

                    (when *tui-expand-tool-calls*
                      (let* ((details-str (format-tool-call-details-string args status result))
                             (wrapped-details (wrap-ansi-text details-str content-width)))
                        (dolist (wl wrapped-details)
                          (let ((styled-wl (mapcar (lambda (run)
                                                     (let ((style (cdr run)))
                                                       (cons (car run)
                                                             (list* :color (if (string= status "failed") :ansi-red :muted)
                                                                    style))))
                                                   wl)))
                            (when dim-p
                              (setf styled-wl (mapcar #'dim-run styled-wl)))
                            (push (list :runs styled-wl :type :tool-call-detail) all-display-lines)))))))

                 (t
                  (let* ((text (getf chat-line :text))
                         (wrapped-lines (wrap-ansi-text text content-width)))
                    (dolist (wl (or wrapped-lines (list (list (cons "" '(:color nil :bold nil :dim nil))))))
                      (let ((styled-wl wl))
                        (when dim-p
                          (setf styled-wl (mapcar #'dim-run styled-wl)))
                        (push (list :runs styled-wl :type type) all-display-lines))))))))
    (setf all-display-lines (nreverse all-display-lines))
    (let* ((total-lines (length all-display-lines))
           (visible-lines height)
           (start (max 0 (- total-lines visible-lines *tui-scroll-offset*))))
      (loop for i from 0 below visible-lines
            for line-idx = (+ start i)
            for screen-row = (+ row i)
            do (if (and (>= line-idx 0) (< line-idx total-lines))
                   (let* ((dline (nth line-idx all-display-lines))
                          (runs (getf dline :runs))
                          (type (getf dline :type)))
                     ;; Set base color and attributes based on type
                     (let ((base-pair (case type
                                        (:user (tui-color-pair :chat-user))
                                        (:assistant (tui-color-pair :chat-assistant))
                                        (:thought (tui-color-pair :chat-thought))
                                        (:bash (tui-color-pair :chat-bash))
                                        (:error (tui-color-pair :chat-error))
                                        (:separator 0)
                                        (:role-header (tui-color-pair :muted))
                                        (:tool-call 0)
                                        (:tool-call-detail (tui-color-pair :muted))
                                        (t 0))))
                       ;; Write the runs one by one
                       (tui-write-runs win runs col screen-row content-width base-pair (eq type :user))))
                   ;; Empty line
                   nil)))))

(defun tui-render-sidebar (win col row width height)
  "Render the right sidebar using the widget system.
   Iterates *tui-sidebar-widgets* top to bottom, drawing separators between widgets."
  (let ((content-col (+ col 2))
        (content-width (- width 4))
        (y row)
        (max-y (+ row height)))
    (dolist (widget *tui-sidebar-widgets*)
      (when (>= y max-y) (return))
      (when (or (null (tui-sidebar-widget-visible-fn widget))
                (funcall (tui-sidebar-widget-visible-fn widget)))
        ;; Draw separator between widgets (not before the first one)
        (when (> y row)
          (tui-hline win col y width #\─)
          (incf y))
        (let ((remaining (- max-y y)))
          (when (> remaining 0)
            (let ((used (funcall (tui-sidebar-widget-render-fn widget)
                                 win content-col y content-width remaining)))
              (incf y (1+ used)))))))))

(defun tui-render-input-bar (win col row width)
  "Render the input bar at the bottom with prompt marker."
  (charms/ll:attron (charms/ll:color-pair (tui-color-pair :prompt-marker)))
  (tui-safe-write win "> " col row 2)
  (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :prompt-marker)))
  (let ((text-width (- width 3)))
    (tui-safe-write win (str:shorten text-width *tui-input-buffer* :ellipsis "")
                    (+ col 2) row text-width)))

(defun tui-render-autocomplete-popup (win col start-row width)
  "Render the new autocomplete popup below the input line."
  (when (and *tui-completions* (not *tui-command-modal-open*))
    (let* ((count (length *tui-completions*))
           (max-visible 5)
           (visible-count (min count max-visible))
           (has-more (> count max-visible))
           ;; Scroll offset so selected item is always visible
           (scroll (max 0 (min (- count visible-count)
                               (- *tui-completion-index* (1- visible-count)))))
           (current-row start-row))
      ;; Draw visible completions
      (loop for vi from 0 below visible-count
            for idx = (+ scroll vi)
            for selected-p = (= idx *tui-completion-index*)
            for cmd = (nth idx *tui-completions*)
            for help-text = (or (tui-get-command-description cmd) "")
            do (progn
                 ;; 1. Draw selection indicator/prefix
                 (if selected-p
                     (progn
                       (charms/ll:attron (charms/ll:color-pair (tui-color-pair :prompt-marker)))
                       (tui-safe-write win "> " col current-row 2)
                       (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :prompt-marker))))
                     (tui-safe-write win "  " col current-row 2))
                 ;; 2. Draw command name
                 (if selected-p
                     (charms/ll:attron (charms/ll:color-pair (tui-color-pair :prompt-marker)))
                     (charms/ll:attron (charms/ll:color-pair (tui-color-pair :completion))))
                 (tui-safe-write win cmd (+ col 2) current-row 18) ; reserve 18 cols for command name
                 (if selected-p
                     (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :prompt-marker)))
                     (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :completion))))
                 ;; 3. Draw description next to command name (in muted color)
                 (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
                 (tui-safe-write win help-text (+ col 20) current-row (- width 21))
                 (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
                 (incf current-row)))
      ;; Draw "↓ N more" indicator
      (when has-more
        (let ((remaining (- count visible-count)))
          (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
          (tui-safe-write win (format nil "  ↓ ~A more" remaining) col current-row (- width 1))
          (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
          (incf current-row)))
      ;; Draw hints
      ;; Hint line 1: ↑/↓ Navigate  ·  enter Select  ·  tab Complete
      (let ((hint1-col (+ col 2)))
        ;; Write ↑/↓ in prompt-marker color
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :prompt-marker)))
        (tui-safe-write win "↑/↓" hint1-col current-row 3)
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :prompt-marker)))
        ;; Write " Navigate  ·  " in muted color
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
        (tui-safe-write win " Navigate  ·  " (+ hint1-col 3) current-row 14)
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
        ;; Write enter in prompt-marker color
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :prompt-marker)))
        (tui-safe-write win "enter" (+ hint1-col 17) current-row 5)
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :prompt-marker)))
        ;; Write " Select  ·  " in muted color
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
        (tui-safe-write win " Select  ·  " (+ hint1-col 22) current-row 12)
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
        ;; Write tab in prompt-marker color
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :prompt-marker)))
        (tui-safe-write win "tab" (+ hint1-col 34) current-row 3)
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :prompt-marker)))
        ;; Write " Complete" in muted color
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
        (tui-safe-write win " Complete" (+ hint1-col 37) current-row 9)
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
        (incf current-row))
      ;; Hint line 2: esc to cancel
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
      (tui-safe-write win "  esc to cancel" col current-row (- width 1))
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted))))))

(defun tui-render-status-bar (win col row width)
  "Render the status bar at the very bottom."
  (let ((pair (case *tui-status-type*
                (:ok (tui-color-pair :status-ok))
                (:error (tui-color-pair :status-error))
                (t (tui-color-pair :status-info)))))
    (charms/ll:attron (charms/ll:color-pair pair))
    ;; Fill the entire row with spaces first for background color
    (tui-safe-write win (make-string width :initial-element #\Space) col row width)
    (tui-safe-write win (str:shorten width *tui-status-line* :ellipsis "..") col row width)
    (charms/ll:attroff (charms/ll:color-pair pair))))

;;* Command Palette

(defun tui-get-command-help-text (cmd-name)
  "Get the full help text for a command by name (with leading /).
   Returns a string with the description and any CLI options."
  (let ((info (gethash cmd-name *commands*)))
    (if info
        (let ((description (second info))
              (cli-options (third info)))
          (with-output-to-string (s)
            (format s "~A~%" cmd-name)
            (format s "~%~A~%" (or description "No description available."))
            (when cli-options
              (format s "~%Options:~%")
              (dolist (opt cli-options)
                (let ((opt-short (getf opt :short))
                      (opt-long (getf opt :long))
                      (opt-desc (getf opt :description)))
                  (format s "  ")
                  (when opt-short (format s "-~A" opt-short))
                  (when (and opt-short opt-long) (format s ", "))
                  (when opt-long (format s "--~A" opt-long))
                  (format s "~%    ~A~%" (or opt-desc "")))))))
        (format nil "~A~%~%No help available." cmd-name))))

(defun tui-get-all-commands ()
  "Get all commands as a list of plists with :name and :description."
  (let ((cmds '()))
    (maphash (lambda (name info)
               (unless (gethash name *tui-hidden-commands*)
                 (push (list :name name :description (second info)) cmds)))
             *commands*)
    (sort cmds #'string< :key (lambda (c) (getf c :name)))))

(defun tui-filter-commands (query commands)
  "Filter commands by fuzzy matching query against command names."
  (if (string= query "")
      commands
      (let* ((names (mapcar (lambda (c) (getf c :name)) commands))
             (matched (fuzzy-match:fuzzy-match query names)))
        (loop for name in matched
              for cmd = (find name commands :key (lambda (c) (getf c :name)) :test #'string=)
              when cmd collect cmd))))

(defun tui-render-command-modal (win total-width total-height)
  "Render the command palette modal overlay with help preview on the left."
  (let* ((full-modal-width (min 120 (- total-width 4)))
         (modal-height (min 24 (- total-height 4)))
         ;; Help panel takes ~40% of the modal width on the left
         (help-panel-width (max 20 (floor (* full-modal-width 5) 12)))
         ;; Command panel takes the rest on the right
         (cmd-panel-width (- full-modal-width help-panel-width 1)) ;; 1 for divider
         (modal-col (floor (- total-width full-modal-width) 2))
         (modal-row (floor (- total-height modal-height) 2))
         ;; Help panel layout
         (help-col (+ modal-col 2))
         (help-width (- help-panel-width 3))
         ;; Divider column
         (divider-col (+ modal-col help-panel-width))
         ;; Command panel layout
         (content-col (+ divider-col 2))
         (content-width (- cmd-panel-width 3))
         (all-commands (tui-get-all-commands))
         (filtered (tui-filter-commands *tui-command-query* all-commands)))

    ;; Clamp selection
    (when filtered
      (setf *tui-command-selected*
            (max 0 (min *tui-command-selected* (1- (length filtered))))))

    ;; Draw modal background (fill with spaces)
    (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-bg)))
    (loop for y from modal-row below (+ modal-row modal-height)
          do (tui-safe-write win (make-string full-modal-width :initial-element #\Space)
                             modal-col y full-modal-width))
    (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-bg)))

    ;; Draw border
    (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-border)))
    (tui-box win modal-col modal-row full-modal-width modal-height)
    (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-border)))

    ;; Draw vertical divider between help and command panels
    (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-border)))
    (tui-vline win divider-col (1+ modal-row) (- modal-height 2) #\│)
    ;; Connect divider to top and bottom borders
    (handler-case (charms:write-char-at-point win #\┬ divider-col modal-row) (error () nil))
    (handler-case (charms:write-char-at-point win #\┴ divider-col (+ modal-row modal-height -1)) (error () nil))
    (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-border)))

    ;; --- Help panel (left side) ---
    (let* ((selected-cmd (when (and filtered (< *tui-command-selected* (length filtered)))
                           (nth *tui-command-selected* filtered)))
           (cmd-name (when selected-cmd (getf selected-cmd :name)))
           (help-text (if cmd-name
                          (tui-get-command-help-text cmd-name)
                          "No command selected."))
           (help-lines (wrap-text help-text help-width))
           (help-start-row (+ modal-row 2))
           (help-avail-height (- modal-height 4)))

      ;; Help panel header
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (charms/ll:attron charms/ll:a_bold)
      (tui-safe-write win "Help" help-col (1+ modal-row) help-width)
      (charms/ll:attroff charms/ll:a_bold)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :sidebar-label)))

      ;; Render help text lines (clear all rows, then write text)
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-bg)))
      (loop for i from 0 below help-avail-height
            for y = (+ help-start-row i)
            do (tui-safe-write win (make-string help-width :initial-element #\Space)
                               help-col y help-width)
               (when (< i (length help-lines))
                 (tui-safe-write win (nth i help-lines) help-col y help-width)))
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-bg))))

    ;; --- Command panel (right side) ---

    ;; Title bar with tabs
    (let ((title-row (1+ modal-row))
          (title-text "Commands"))
      (charms/ll:attron charms/ll:a_bold)
      (tui-safe-write win title-text content-col title-row (length title-text))
      (charms/ll:attroff charms/ll:a_bold)

      ;; Tab indicators
      (let ((tab-col (+ content-col (length title-text) 2)))
        ;; System tab
        (if (eq *tui-command-tab* :system)
            (charms/ll:attron (charms/ll:color-pair (tui-color-pair :tab-active)))
            (charms/ll:attron (charms/ll:color-pair (tui-color-pair :tab-inactive))))
        (tui-safe-write win " * System " tab-col title-row 10)
        (if (eq *tui-command-tab* :system)
            (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :tab-active)))
            (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :tab-inactive))))
        ;; User tab
        (let ((user-tab-col (+ tab-col 11)))
          (if (eq *tui-command-tab* :user)
              (charms/ll:attron (charms/ll:color-pair (tui-color-pair :tab-active)))
              (charms/ll:attron (charms/ll:color-pair (tui-color-pair :tab-inactive))))
          (tui-safe-write win " - User " user-tab-col title-row 8)
          (if (eq *tui-command-tab* :user)
              (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :tab-active)))
              (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :tab-inactive)))))))

    ;; Search input
    (let ((input-row (+ modal-row 3)))
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-input)))
      (tui-safe-write win (format nil "> ~A" *tui-command-query*)
                      content-col input-row content-width)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-input))))

    ;; Render modal completions dropdown (below the search input)
    (when (and *tui-modal-completions* (>= *tui-modal-completion-index* 0))
      (let* ((comp-count (length *tui-modal-completions*))
             (max-comp-visible (min comp-count 8 (- modal-height 9)))
             (comp-width (min content-width
                              (+ 4 (reduce #'max *tui-modal-completions*
                                           :key #'length :initial-value 0))))
             (comp-col content-col)
             (comp-start-row (+ modal-row 4))
             ;; Scroll offset so selected item is always visible
             (comp-scroll (max 0 (min (- comp-count max-comp-visible)
                                      (- *tui-modal-completion-index* (1- max-comp-visible))))))
        (loop for vi from 0 below max-comp-visible
              for idx = (+ comp-scroll vi)
              for screen-row = (+ comp-start-row vi)
              when (and (>= idx 0) (< idx comp-count))
              do (let* ((item (nth idx *tui-modal-completions*))
                        (selected-p (= idx *tui-modal-completion-index*))
                        (display (str:shorten comp-width
                                              (format nil " ~A " item)
                                              :ellipsis "..")))
                   (if selected-p
                       (charms/ll:attron (charms/ll:color-pair (tui-color-pair :selected)))
                       (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-bg))))
                   (tui-safe-write win (make-string comp-width :initial-element #\Space)
                                   comp-col screen-row comp-width)
                   (tui-safe-write win display comp-col screen-row comp-width)
                   (if selected-p
                       (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :selected)))
                       (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-bg))))))))

    ;; Command list (push down when completions are visible)
    (let* ((comp-visible-count (if (and *tui-modal-completions* (>= *tui-modal-completion-index* 0))
                                   (min (length *tui-modal-completions*) 8 (- modal-height 9))
                                   0))
           (list-start (+ modal-row 5 comp-visible-count))
           (list-height (- modal-height 8 comp-visible-count))
           (display-count (min list-height (length filtered))))
      (loop for i from 0 below display-count
            for cmd = (nth i filtered)
            for y = (+ list-start i)
            do (let ((name (getf cmd :name))
                     (desc (getf cmd :description)))
                 (when (= i *tui-command-selected*)
                   (charms/ll:attron (charms/ll:color-pair (tui-color-pair :selected))))
                 ;; Fill line for highlight
                 (tui-safe-write win (make-string content-width :initial-element #\Space)
                                 content-col y content-width)
                 (tui-safe-write win (str:shorten content-width
                                                  (format nil "~A" name)
                                                  :ellipsis "..")
                                 content-col y content-width)
                 ;; Show description on the right if space allows (first line only)
                 (let ((desc-col (+ content-col (min 25 (1+ (length name))))))
                   (when (and desc (< desc-col (+ content-col content-width)))
                     (let ((first-line (first (str:lines desc))))
                       (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
                       (tui-safe-write win (str:shorten (- (+ content-col content-width) desc-col)
                                                        (or first-line "") :ellipsis "..")
                                       desc-col y (- (+ content-col content-width) desc-col))
                       (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted))))))
                 (when (= i *tui-command-selected*)
                   (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :selected)))))))

    ;; Key hints at bottom of modal
    (let ((hint-row (+ modal-row modal-height -2)))
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :keyhint)))
      (tui-safe-write win "tab complete cmd/args    up/dn choose    enter confirm    esc canc"
                      content-col hint-row content-width)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :keyhint))))))

;;; --- Main render ---

(defun tui-render (win)
  "Full render pass of the 3-column TUI."
  (charms:clear-window win :force-repaint t)
  (multiple-value-bind (total-width total-height)
      (charms:window-dimensions win)
    (let* ((sidebar-width (max 20 (min 35 (floor total-width 4))))
           (chat-width (- total-width sidebar-width 1)) ;; 1 for divider
           (comp-height (tui-completions-height))
           (input-row (- total-height comp-height 3))
           (status-row (- total-height 1))
           (chat-height (- total-height comp-height 4)) ;; room for input + status + border + completions
           (sidebar-col (- total-width sidebar-width))
           (divider-col (1- sidebar-col)))

      ;; Draw vertical divider between chat and sidebar
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :border)))
      (tui-vline win divider-col 0 (- total-height 2) #\│)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :border)))

      ;; Draw horizontal separator above input
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :border)))
      (tui-hline win 0 input-row chat-width #\─)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :border)))

      ;; Render chat panel
      (tui-render-chat-panel win 1 0 chat-width chat-height)

      ;; Render input bar
      (tui-render-input-bar win 1 (1+ input-row) chat-width)

      ;; Render completion dropdown below input
      (when (> comp-height 0)
        (tui-render-autocomplete-popup win 1 (+ input-row 2) chat-width))

      ;; Render sidebar
      (tui-render-sidebar win sidebar-col 1 sidebar-width (- total-height 2))

      ;; Render status bar (full width, bottom)
      (unless (> comp-height 0)
        (tui-render-status-bar win 0 status-row total-width))

      ;; Render command modal if open
      (when *tui-command-modal-open*
        (tui-render-command-modal win total-width total-height))))

  (charms:refresh-window win))

;;; --- Input handling ---

(defun tui-handle-modal-completion-input (c)
  "Handle input when modal completions are visible.
   Returns :handled if consumed, or :passthrough to let modal handler process it."
  (cond
    ;; Up arrow or Ctrl-P: move selection up
    ((or (eql c (code-char charms/ll:key_up))
         (eql c (code-char 16)))  ;; Ctrl-P
     (setf *tui-modal-completion-index*
           (max 0 (1- *tui-modal-completion-index*)))
     :handled)

    ;; Down arrow or Ctrl-N: move selection down
    ((or (eql c (code-char charms/ll:key_down))
         (eql c (code-char 14)))  ;; Ctrl-N
     (setf *tui-modal-completion-index*
           (min (1- (length *tui-modal-completions*)) (1+ *tui-modal-completion-index*)))
     :handled)

    ;; Enter: accept the selected completion
    ((or (eql c #\Return) (eql c #\Newline))
     (tui-modal-accept-completion)
     :handled)

    ;; Tab: cycle to next completion
    ((eql c #\Tab)
     (setf *tui-modal-completion-index*
           (mod (1+ *tui-modal-completion-index*) (length *tui-modal-completions*)))
     :handled)

    ;; Escape: dismiss completions (but keep modal open)
    ((eql c #\Esc)
     (tui-modal-reset-completions)
     :handled)

    ;; Anything else: dismiss completions and pass through
    (t
     (tui-modal-reset-completions)
     :passthrough)))

(defun tui-handle-modal-input (c)
  "Handle input when the command modal is open. Returns :close, (:execute . cmd-name), or nil."
  ;; If modal completions are visible, let them handle input first
  (when *tui-modal-completions*
    (let ((result (tui-handle-modal-completion-input c)))
      (when (eq result :handled)
        (return-from tui-handle-modal-input nil))))

  (let ((all-commands (tui-get-all-commands))
        (filtered nil))
    (setf filtered (tui-filter-commands *tui-command-query* all-commands))
    (cond
      ;; ESC - close modal
      ((eql c #\Esc)
       (setf *tui-command-modal-open* nil)
       (tui-modal-reset-completions)
       :close)

      ;; Tab - trigger completion UI
      ((eql c #\Tab)
       (let* ((trimmed-query (string-trim '(#\Space #\Tab) *tui-command-query*))
              (space-pos (position #\Space trimmed-query)))
         (if space-pos
             ;; Query has a space: show argument completions
             (tui-modal-try-complete)
             ;; No space yet: complete the command name from filtered list
             (when (and filtered (< *tui-command-selected* (length filtered)))
               (let* ((cmd (nth *tui-command-selected* filtered))
                      (name (getf cmd :name)))
                 (when name
                   (setf *tui-command-query*
                         (if (str:starts-with? "/" name)
                             (subseq name 1)
                             name)))))))
       nil)

      ;; Up arrow
      ((eql c (code-char charms/ll:key_up))
       (setf *tui-command-selected* (max 0 (1- *tui-command-selected*)))
       nil)

      ;; Down arrow
      ((eql c (code-char charms/ll:key_down))
       (setf *tui-command-selected*
             (if filtered
                 (min (1- (length filtered)) (1+ *tui-command-selected*))
                 0))
       nil)

      ;; Enter - execute selected command or typed command text
      ((or (eql c #\Return) (eql c #\Newline))
       (let ((query (string-trim '(#\Space #\Tab) *tui-command-query*)))
         (setf *tui-command-modal-open* nil)
         (tui-modal-reset-completions)
         (cond
           ((not (string= query ""))
            (cons :execute (if (str:starts-with? "/" query)
                               query
                               (format nil "/~A" query))))
           ((and filtered (< *tui-command-selected* (length filtered)))
            (let ((cmd (nth *tui-command-selected* filtered)))
              (cons :execute (getf cmd :name))))
           (t nil))))

      ;; Backspace
      ((or (eql c #\Backspace) (eql c #\Rubout) (eql c (code-char 127))
           (eql c (code-char charms/ll:key_backspace)))
       (tui-modal-reset-completions)
       (when (> (length *tui-command-query*) 0)
         (setf *tui-command-query* (subseq *tui-command-query* 0 (1- (length *tui-command-query*))))
         (setf *tui-command-selected* 0))
       nil)

      ;; Printable character - add to query
      ((and c (graphic-char-p c))
       (tui-modal-reset-completions)
       (setf *tui-command-query* (concatenate 'string *tui-command-query* (string c)))
       (setf *tui-command-selected* 0)
       nil)

      (t nil))))

(defun tui-handle-completion-input (c)
  "Handle input when the completion dropdown is visible.
   Returns :handled if consumed, or :passthrough to let main handler process it."
  (cond
    ;; Up arrow or Ctrl-P: move selection up
    ((or (eql c (code-char charms/ll:key_up))
         (eql c (code-char 16)))  ;; Ctrl-P
     (setf *tui-completion-index*
           (max 0 (1- *tui-completion-index*)))
     :handled)

    ;; Down arrow or Ctrl-N: move selection down
    ((or (eql c (code-char charms/ll:key_down))
         (eql c (code-char 14)))  ;; Ctrl-N
     (setf *tui-completion-index*
           (min (1- (length *tui-completions*)) (1+ *tui-completion-index*)))
     :handled)

    ;; Enter: accept the selected completion
    ((or (eql c #\Return) (eql c #\Newline))
     (tui-accept-completion)
     :handled)

    ;; Tab: accept the selected completion (tab Complete in mockup)
    ((eql c #\Tab)
     (tui-accept-completion)
     :handled)

    ;; Escape: dismiss completions and disable autotrigger
    ((eql c #\Esc)
     (tui-reset-completions)
     (setf *tui-autocomplete-disabled* t)
     :handled)

    ;; Anything else: pass through to main input handler
    (t
     :passthrough)))

(defun tui-handle-main-input (c)
  "Handle input in the main view (not modal). Returns :quit, :send, or nil."
  ;; If completions are visible, let the completion handler try first
  (when *tui-completions*
    (let ((result (tui-handle-completion-input c)))
      (when (eq result :handled)
        (return-from tui-handle-main-input nil))))

  (cond
    ;; Tab: trigger completion
    ((eql c #\Tab)
     (setf *tui-autocomplete-disabled* nil)
     (tui-try-complete)
     nil)

    ;; Ctrl-O: toggle tool call details expansion
    ((eql c (code-char 15))  ;; Ctrl-O
     (setf *tui-expand-tool-calls* (not *tui-expand-tool-calls*))
     nil)

    ;; Ctrl-P or Ctrl-K: open command palette
    ((or (eql c (code-char 16))   ;; Ctrl-P
         (eql c (code-char 11)))  ;; Ctrl-K
     (setf *tui-command-modal-open* t)
     (setf *tui-command-query* "")
     (setf *tui-command-selected* 0)
     (setf *tui-command-tab* :system)
     (tui-modal-reset-completions)
     nil)

    ;; Ctrl-C: quit
    ((eql c (code-char 3))
     :quit)

    ;; Enter: send message
    ((or (eql c #\Return) (eql c #\Newline))
     (when (> (length *tui-input-buffer*) 0)
       :send))

    ;; Up/Down: scroll chat
    ((eql c (code-char charms/ll:key_up))
     (incf *tui-scroll-offset*)
     nil)

    ((eql c (code-char charms/ll:key_down))
     (setf *tui-scroll-offset* (max 0 (1- *tui-scroll-offset*)))
     nil)

    ;; Ctrl-R: history up (older) — standard readline-like binding
    ((eql c (code-char 18))  ;; Ctrl-R
     (tui-history-navigate :up)
     (tui-reset-completions)
     nil)

    ;; Ctrl-S: history down (newer)
    ((eql c (code-char 19))  ;; Ctrl-S
     (tui-history-navigate :down)
     (tui-reset-completions)
     nil)

    ;; Backspace
    ((or (eql c #\Backspace) (eql c #\Rubout) (eql c (code-char 127))
         (eql c (code-char charms/ll:key_backspace)))
     (when (> (length *tui-input-buffer*) 0)
       (setf *tui-input-buffer* (subseq *tui-input-buffer* 0 (1- (length *tui-input-buffer*)))))
     (tui-history-reset)
     (tui-on-input-change)
     nil)

    ;; Printable character
    ((and c (graphic-char-p c))
     (setf *tui-input-buffer* (concatenate 'string *tui-input-buffer* (string c)))
     (tui-history-reset)
     (tui-on-input-change)
     nil)

    (t nil)))
;;* main loop
(defun tui-process-command (cmd-name)
  "Execute a command by name within the TUI context.
   Suspends curses around the command so commands that spawn external
   programs (fzf, $EDITOR, etc.) can use the terminal safely."
  (multiple-value-bind (cmd args) (parse-command cmd-name)
    (if cmd
        (handler-case
            (let ((output-string nil))
              ;; Suspend curses so subprocesses can drive the terminal.
              (charms/ll:def-prog-mode)
              (charms/ll:endwin)
              (unwind-protect
                   (setf output-string
                         (with-output-to-string (*standard-output*)
                           (execute-command cmd args)))
                ;; Resume curses and force a full repaint.
                (charms/ll:reset-prog-mode)
                (charms:clear-window charms:*standard-window* :force-repaint t)
                (charms:refresh-window charms:*standard-window*))
              (when (and output-string (> (length output-string) 0))
                (dolist (line (str:lines output-string))
                  (tui-add-chat-line line :type :text))))
          (serious-condition (e)
            ;; Make sure curses is back even if something blew up.
            (ignore-errors (charms/ll:reset-prog-mode))
            (tui-add-chat-line (format nil "Command failed: ~A" e) :type :error)
            (tui-set-status (format nil "Command failed: ~A" e) :error)))
        (tui-add-chat-line (format nil "Unknown command: ~A" cmd-name) :type :error))))

(defun tui-stream-llm-response (prompt win)
  "Send PROMPT to the LLM with streaming, updating the TUI chat panel live.
   WIN is the curses window used for rendering updates between chunks."
  (unless *current-model*
    (tui-add-chat-line "Error: No model selected. Use /model to select a model." :type :error)
    (return-from tui-stream-llm-response nil))

  (let* ((messages-for-api (prepare-messages prompt))
         (provider-name (model-config-provider *current-model*))
         (provider-type (intern (string-upcase provider-name) :keyword))
         (sys-prompt (system-prompt))
         (full-response (make-string-output-stream))
         (reader-instance nil)
         ;; We'll build up the current assistant message as a single chat-line
         ;; and update it in-place as chunks arrive.
         )

    (handler-case
        (progn
          (multiple-value-bind (response-result initial-messages)
              (llm:complete
               provider-type
               messages-for-api
               :model (model-config-model-name *current-model*)
               :max-tokens (model-config-max-output-tokens *current-model*)
               :max-context (model-config-max-input-tokens *current-model*)
               :system-prompt sys-prompt
               :extra-headers (normalize-http-extra-headers
                               (when (model-config-extra-params *current-model*)
                                 (gethash "extra_headers" (model-config-extra-params *current-model*))))
               :stream t)
            (declare (ignore initial-messages))

            (setf reader-instance response-result)
            (setf *current-stream-reader* reader-instance)

            (unless (typep reader-instance 'llm:llm-stream-reader)
              (error "LLM complete did not return a stream reader when stream=t"))

            ;; Add an empty assistant line that we will update in-place
            (push (list :text "" :type :assistant :role "assistant") *tui-chat-lines*)

            (loop for chunk = (llm:read-next-chunk reader-instance)
                  while chunk
                  do (progn
                       (write-string chunk full-response)
                       ;; Update the in-place chat line with accumulated text
                       (let ((current-text (get-output-stream-string full-response)))
                         ;; We need to re-write the accumulated text (get-output-stream-string consumed it)
                         (write-string current-text full-response)
                         (setf (getf (first *tui-chat-lines*) :text) current-text))
                       ;; Re-render after each chunk for live streaming effect
                       (setf *tui-scroll-offset* 0)
                       (tui-refresh-sidebar-data)
                       (tui-render win)))

            (let ((assistant-response (get-output-stream-string full-response)))
              ;; Final update of the chat line text — apply markdown rendering
              (setf (getf (first *tui-chat-lines*) :text) (render-md-ansi assistant-response))
              ;; Add to chat history for persistence
              (add-to-chat-history "user" prompt)
              (add-to-chat-history "assistant" assistant-response)
              (nhooks:run-hook *process-history-hook* *chat-history*)
              assistant-response)))
      (error (e)
        (when (and reader-instance (not (llm:llm-stream-reader-closed-p reader-instance)))
          (ignore-errors (llm:close-reader reader-instance)))
        (tui-add-chat-line (format nil "Error: ~A" e) :type :error)
        (tui-set-status (format nil "Error: ~A" e) :error)
        nil))))

(defun tui-send-message (&optional (win charms:*standard-window*))
  "Send the current input buffer as a message to the LLM."
  (let ((input (string-trim '(#\Space #\Tab) *tui-input-buffer*)))
    (setf *tui-input-buffer* "")
    (setf *tui-scroll-offset* 0)
    (tui-reset-completions)
    (tui-history-reset)
    (when (string= input "") (return-from tui-send-message))
    ;; Save to input history
    (tui-history-push input)

    ;; Check if it's a command
    (multiple-value-bind (cmd args) (parse-command input)
      (if cmd
          (tui-process-command input)
          (progn
            ;; Regular message to LLM
            (tui-add-user-message input)
            (tui-set-status (format nil "Streaming from ~A..."
                                    (if *current-model* (model-config-name *current-model*) "LLM"))
                            :info)
            ;; Render immediately to show user message + status
            (tui-refresh-sidebar-data)
            (tui-render win)
            ;; Stream the LLM response with live TUI updates
            (tui-stream-llm-response input win)
            (tui-set-status (format nil "Model: ~A | C-p: Commands | C-r/s: History | C-c: Quit"
                                    (if *current-model* (model-config-name *current-model*) "None"))
                            :info))))))

(cffi:defcfun ("setlocale" %setlocale) :string
  (category :int)
  (locale :string))

(defun tui-setlocale ()
  "Set the locale to the user's default so ncurses handles UTF-8."
  (%setlocale 0 ""))  ;; LC_ALL = 0, "" = use environment

(defun run-tui ()
  "Run the 3-column TUI main loop."
  (tui-setlocale)
  (setf *tui-running* t)
  (setf *tui-chat-lines* '())
  (setf *tui-input-buffer* "")
  (setf *tui-scroll-offset* 0)
  (setf *tui-command-modal-open* nil)
  (setf *tui-command-query* "")
  (setf *tui-command-selected* 0)
  (setf *tui-command-tab* :system)
  (setf *tui-sidebar-tool-calls* nil)
  (unless *tui-sidebar-widgets*
    (setf *tui-sidebar-widgets* (make-default-sidebar-widgets)))
  (tui-reset-completions)
  ;; Preserve input history across TUI restarts, but reset navigation
  (tui-history-reset)
  (tui-modal-reset-completions)
  (tui-refresh-sidebar-data)
  (tui-set-status (format nil "Model: ~A | C-p: Commands | C-r/s: History | C-c: Quit"
                          (if *current-model* (model-config-name *current-model*) "None"))
                  :info)
  (tui-add-chat-line "Hactar AI Pair Programmer" :type :text)
  (tui-add-chat-line "Type /help for commands, or Ctrl-P to open the command palette." :type :text)
  (tui-add-chat-line "" :type :text)

  (unwind-protect
       (charms:with-curses ()
         (charms:disable-echoing)
         (charms:enable-raw-input :interpret-control-characters nil)
         (charms:enable-extra-keys charms:*standard-window*)
         (charms/ll:curs-set 0)
         (tui-apply-theme (get-or-create-theme))

         (let ((win charms:*standard-window*))
           (loop named tui-loop do
                 (tui-refresh-sidebar-data)
                 (tui-render win)

                 (let ((c (charms:get-char win)))
                   (when c
                     (if *tui-command-modal-open*
                         ;; Modal input handling
                         (let ((result (tui-handle-modal-input c)))
                           (cond
                             ((null result) nil) ;; just re-render
                             ((eq result :close) nil)
                             ((and (consp result) (eq (car result) :execute))
                              (tui-process-command (cdr result))
                              (tui-refresh-sidebar-data))))
                         ;; Main input handling
                         (let ((result (tui-handle-main-input c)))
                           (case result
                             (:quit (return-from tui-loop))
                             (:send (tui-send-message win))
                             (t nil))))))))
    (setf *tui-running* nil))))

;;* oauth login flow (TUI)
(defun tui-oauth-login (&optional provider-id)
  "Suspend curses and run the interactive OAuth login flow, then restore the TUI.
   This makes it trivial to drive the same login flow from the TUI as from the REPL."
  (charms/ll:def-prog-mode)
  (charms/ll:endwin)
  (unwind-protect
       (handler-case (run-oauth-login-flow provider-id)
         (serious-condition (e)
           (format t "~&Login failed: ~A~%" e))
         (:no-error (result)
           (when result
             (tui-add-chat-line "OAuth login completed." :type :text))
           result))
    (charms/ll:reset-prog-mode)
    (charms:clear-window charms:*standard-window* :force-repaint t)
    (charms:refresh-window charms:*standard-window*)))

;;* tui utils
(defun ansi (code)
  "Helper to generate ANSI escape sequences."
  (format nil "~C~A" #\Esc code))

(defun get-terminal-size ()
  "Tries to get terminal size using stty. Defaults to 24x80."
  (handler-case
      (multiple-value-bind (output error-output exit-code)
                           (uiop:run-program '("stty" "size") :output :string :ignore-error-status t :error-output :string)
        (if (and (zerop exit-code) (> (length output) 0))
          (let ((parts (str:split #\Space (string-trim '(#\Newline) output))))
            (values (parse-integer (first parts)) (parse-integer (second parts))))
          (progn
            (debug-log "stty size failed. Code:" exit-code "Error:" error-output "Output:" output)
            (values 24 80))))
    (error (e)
      (debug-log "Error getting terminal size:" e)
      (values 24 80))))

(defun flush-terminal-input ()
  "Flush any pending input from the terminal input buffer.
   This discards characters typed while waiting for LLM response."
  (handler-case
      #+sbcl
      (let ((fd 0)) ; stdin file descriptor
        ;; SB-POSIX:TCFLUSH expects 2 arguments: FD and QUEUE (e.g. SB-POSIX:TCIFLUSH).
        (sb-posix:tcflush fd sb-posix:tciflush))
      #-sbcl
      (handler-case
          (loop while (listen *standard-input*)
                do (read-char-no-hang *standard-input* nil nil))
        (error () nil))
    (error (e)
      (declare (ignore e))
      ;; Fallback: try to read and discard any available input
      (handler-case
          (loop while (listen *standard-input*)
                do (read-char-no-hang *standard-input* nil nil))
        (error () nil)))))

(defun disable-terminal-echo ()
  "Disable terminal echo so keystrokes typed during LLM wait are invisible."
  (handler-case
      #+sbcl
      (let* ((fd 0)
             (termios (sb-posix:tcgetattr fd)))
        (setf (sb-posix:termios-lflag termios)
              (logandc2 (sb-posix:termios-lflag termios)
                        sb-posix:echo))
        (sb-posix:tcsetattr fd sb-posix:tcsanow termios))
      #-sbcl
      (uiop:run-program '("stty" "-echo") :ignore-error-status t)
    (error () nil)))

(defun enable-terminal-echo ()
  "Re-enable terminal echo."
  (handler-case
      #+sbcl
      (let* ((fd 0)
             (termios (sb-posix:tcgetattr fd)))
        (setf (sb-posix:termios-lflag termios)
              (logior (sb-posix:termios-lflag termios)
                      sb-posix:echo))
        (sb-posix:tcsetattr fd sb-posix:tcsanow termios))
      #-sbcl
      (uiop:run-program '("stty" "echo") :ignore-error-status t)
    (error () nil)))

(defun enter-raw-mode ()
  "Uses stty to enter raw mode and disable echo."
  (handler-case (uiop:run-program '("stty" "raw" "-echo"))
    (error (e) (debug-log "Failed to enter raw mode with stty:" e))))

(defun exit-raw-mode ()
  "Uses stty to exit raw mode and enable echo."
  (handler-case (uiop:run-program '("stty" "-raw" "echo"))
    (error (e) (debug-log "Failed to exit raw mode with stty:" e))))

(defun parse-ansi-string (str)
  "Parse a string containing ANSI escape sequences into a list of (substring . style-plist) runs."
  (let ((runs '())
        (current-style (list :color nil :bold nil :dim nil))
        (len (length str))
        (i 0)
        (chunk-start 0))
    (labels ((add-run (end)
               (when (> end chunk-start)
                 (push (cons (subseq str chunk-start end) (copy-list current-style)) runs))))
      (loop while (< i len) do
        (cond
          ;; Check for ESC [
          ((and (< (1+ i) len)
                (char= (char str i) #\Esc)
                (char= (char str (1+ i)) #\[))
           ;; Add the current run before the escape sequence
           (add-run i)
           ;; Find the terminating 'm'
           (let ((m-pos (position #\m str :start (+ i 2))))
             (if m-pos
                 (let* ((codes-str (subseq str (+ i 2) m-pos))
                        ;; Split codes by semicolon
                        (codes (str:split #\; codes-str)))
                   ;; Update current-style based on codes.
                   ;; Handle simple SGR codes as well as 256-color
                   ;; (38;5;N) and truecolor (38;2;R;G;B) sequences.
                   (let* ((nums (mapcar (lambda (cs)
                                          (or (ignore-errors (parse-integer cs)) -1))
                                        codes))
                          (vec (coerce nums 'vector))
                          (n (length vec))
                          (k 0))
                     (flet ((idx->color (i)
                              (case (mod i 8)
                                (0 nil) ;; black -> default
                                (1 :red) (2 :green) (3 :yellow)
                                (4 :blue) (5 :magenta) (6 :cyan) (7 :white))))
                       (loop while (< k n) do
                         (let ((code (aref vec k)))
                           (cond
                             ((= code 0) (setf current-style (list :color nil :bold nil :dim nil)))
                             ((= code 1) (setf (getf current-style :bold) t))
                             ((= code 2) (setf (getf current-style :dim) t))
                             ((= code 22) (setf (getf current-style :bold) nil)
                                          (setf (getf current-style :dim) nil))
                             ((= code 39) (setf (getf current-style :color) nil))
                             ;; 256-color foreground: 38;5;N
                             ((and (= code 38) (< (+ k 2) n) (= (aref vec (+ k 1)) 5))
                              (setf (getf current-style :color)
                                    (idx->color (aref vec (+ k 2))))
                              (incf k 2))
                             ;; truecolor foreground: 38;2;R;G;B (approximate as default)
                             ((and (= code 38) (< (+ k 4) n) (= (aref vec (+ k 1)) 2))
                              (setf (getf current-style :color) nil)
                              (incf k 4))
                             ((member code '(30 90)) (setf (getf current-style :color) nil))
                             ((member code '(31 91)) (setf (getf current-style :color) :red))
                             ((member code '(32 92)) (setf (getf current-style :color) :green))
                             ((member code '(33 93)) (setf (getf current-style :color) :yellow))
                             ((member code '(34 94)) (setf (getf current-style :color) :blue))
                             ((member code '(35 95)) (setf (getf current-style :color) :magenta))
                             ((member code '(36 96)) (setf (getf current-style :color) :cyan))
                             ((member code '(37 97)) (setf (getf current-style :color) :white)))
                           (incf k)))))
                   (setf i (1+ m-pos))
                   (setf chunk-start i))
                 ;; If no 'm' is found, treat the ESC as normal text
                 (progn
                   (incf i 2)))))
          (t
           (incf i))))
      (add-run len)
      (nreverse runs))))

(defun tokenize-run (run)
  "Tokenize a run (text . style) into a list of (token-string . style) where token-string is either a word or a space sequence."
  (let* ((text (car run))
         (style (cdr run))
         (tokens '())
         (len (length text))
         (i 0))
    (loop while (< i len) do
      (let ((char (char text i)))
        (if (char= char #\Space)
            ;; Collect consecutive spaces
            (let ((start i))
              (loop while (and (< i len) (char= (char text i) #\Space))
                    do (incf i))
              (push (cons (subseq text start i) style) tokens))
            ;; Collect consecutive non-spaces
            (let ((start i))
              (loop while (and (< i len) (not (char= (char text i) #\Space)))
                    do (incf i))
              (push (cons (subseq text start i) style) tokens)))))
    (nreverse tokens)))

(defun wrap-ansi-line-tokens (tokens width)
  "Wrap a list of (token . style) tokens to the given width.
   Returns a list of lines, where each line is a list of (text . style) runs."
  (let ((lines '())
        (current-runs '())
        (current-len 0))
    (labels ((commit-line ()
               (when current-runs
                 ;; Remove trailing spaces from current-runs
                 (let ((trimmed-runs '())
                       (rev (nreverse current-runs))
                       (trimming t))
                   (dolist (run rev)
                     (let ((text (car run))
                           (style (cdr run)))
                       (if trimming
                           (let ((trimmed (string-right-trim " " text)))
                             (unless (string= trimmed "")
                               (setf trimming nil)
                               (push (cons trimmed style) trimmed-runs)))
                           (push run trimmed-runs))))
                   (when trimmed-runs
                     (push trimmed-runs lines)))
                 (setf current-runs '()
                       current-len 0)))
             (add-to-line (text style)
               (let ((len (length text)))
                 ;; If the last run in current-runs has the same style, merge them!
                 (if (and current-runs (equal (cdr (first current-runs)) style))
                     (setf (car (first current-runs)) (concatenate 'string (car (first current-runs)) text))
                     (push (cons text style) current-runs))
                 (incf current-len len))))
      (dolist (tok tokens)
        (let ((text (car tok))
              (style (cdr tok)))
          (cond
            ;; If token is a space
            ((char= (char text 0) #\Space)
             ;; Skip leading spaces on the line
             (when (> current-len 0)
               (add-to-line text style)))
            ;; If token is a word
            (t
             (let ((word-len (length text)))
               (cond
                 ;; If it fits on the current line
                 ((or (zerop current-len) (<= (+ current-len word-len) width))
                  (add-to-line text style))
                 ;; Otherwise, start a new line
                 (t
                  (commit-line)
                  (add-to-line text style))))))))
      (commit-line)
      (nreverse lines))))

(defun wrap-ansi-text (text width)
  "Wrap TEXT (which may contain ANSI escape sequences and explicit newlines) to the given WIDTH.
   Returns a list of lines, where each line is a list of (text . style) runs."
  (unless text
    (return-from wrap-ansi-text nil))
  (let ((all-wrapped-lines '())
        (raw-lines (str:lines text)))
    (unless raw-lines
      (setf raw-lines (list "")))
    (dolist (raw-line raw-lines)
      (if (string= raw-line "")
          (push (list (cons "" '(:color nil :bold nil :dim nil))) all-wrapped-lines)
          (let* ((runs (parse-ansi-string raw-line))
                 (tokens (mapcan #'tokenize-run runs))
                 (wrapped-lines (wrap-ansi-line-tokens tokens width)))
            (dolist (wl wrapped-lines)
              (push wl all-wrapped-lines)))))
    (nreverse all-wrapped-lines)))

(defun tui-write-runs (win runs col row max-width base-pair is-user)
  "Write a list of (text . style) runs to WIN at (COL, ROW), applying styles and base-pair."
  (let ((current-col col)
        (remaining-width max-width))
    (dolist (run runs)
      (when (<= remaining-width 0) (return))
      (let* ((text (car run))
             (style (cdr run))
             (color (getf style :color))
             (bold (getf style :bold))
             (dim (getf style :dim))
             ;; Determine the color pair to use
             (pair (cond
                     ;; If the run has an explicit ANSI color, use it!
                     (color (case color
                              (:red (tui-color-pair :ansi-red))
                              (:green (tui-color-pair :ansi-green))
                              (:yellow (tui-color-pair :ansi-yellow))
                              (:blue (tui-color-pair :ansi-blue))
                              (:magenta (tui-color-pair :ansi-magenta))
                              (:cyan (tui-color-pair :ansi-cyan))
                              (:white (tui-color-pair :ansi-white))
                              (:muted (tui-color-pair :muted))
                              (:tool-pending (tui-color-pair :tool-pending))
                              (:tool-running (tui-color-pair :tool-running))
                              (:tool-done (tui-color-pair :tool-done))
                              (:tool-failed (tui-color-pair :tool-failed))
                              (t base-pair)))
                     (t base-pair)))
             ;; Truncate the text to remaining width
             (write-text (if (> (length text) remaining-width)
                             (subseq text 0 remaining-width)
                             text)))
        (when (> (length write-text) 0)
          ;; Turn on attributes
          (when (> pair 0)
            (charms/ll:attron (charms/ll:color-pair pair)))
          (when (or bold is-user)
            (charms/ll:attron charms/ll:a_bold))
          (when dim
            (charms/ll:attron charms/ll:a_dim))

          ;; Write string safely
          (handler-case
              (charms:write-string-at-point win write-text current-col row)
            (error () nil))

          ;; Turn off attributes
          (when dim
            (charms/ll:attroff charms/ll:a_dim))
          (when (or bold is-user)
            (charms/ll:attroff charms/ll:a_bold))
          (when (> pair 0)
            (charms/ll:attroff (charms/ll:color-pair pair)))

          (incf current-col (length write-text))
          (decf remaining-width (length write-text)))))))

(defun wrap-text (text width)
  "Simple text wrapper. Respects existing newlines in TEXT, then wraps long lines."
  (when text
    (let ((result '()))
      ;; First split on newlines to preserve explicit line breaks
      (let ((lines (str:lines text)))
        (unless lines
          ;; str:lines on "" may return NIL; treat as a single empty line
          (setf lines (list "")))
      (dolist (input-line lines)
        (if (string= input-line "")
            (push input-line result)
            ;; Wrap by words (also normalizes whitespace)
            (let ((current-line (make-string-output-stream))
                  (current-length 0))
              (dolist (word (str:words input-line))
                (let ((word-length (length word)))
                  (when (and (not (zerop current-length))
                             (> (+ current-length word-length 1) width))
                    (push (get-output-stream-string current-line) result)
                    (setf current-line (make-string-output-stream))
                    (setf current-length 0))
                  (unless (zerop current-length)
                    (write-char #\Space current-line)
                    (incf current-length))
                  (write-string word current-line)
                  (incf current-length word-length)))
              (push (get-output-stream-string current-line) result)))))
      (nreverse result))))

;;** user inputs
(defmacro with-terminal-interaction (&body body)
  "Suspends the curses TUI and/or readline terminal state if active, runs BODY, and then restores them."
  (let ((was-tui (gensym "WAS-TUI"))
        (was-repl (gensym "WAS-REPL")))
    `(let ((,was-tui *tui-running*)
           (,was-repl *in-repl*))
       (if ,was-tui
           (progn
             (charms/ll:def-prog-mode)
             (charms/ll:endwin)
             (when ,was-repl
               (rl:deprep-terminal))
             (unwind-protect
                  (progn ,@body)
               (when ,was-repl
                 (rl:prep-terminal t))
               (charms/ll:reset-prog-mode)
               (charms:clear-window charms:*standard-window* :force-repaint t)
               (charms:refresh-window charms:*standard-window*)))
           (if ,was-repl
               (progn
                 (rl:deprep-terminal)
                 (unwind-protect
                      (progn ,@body)
                   (rl:prep-terminal t)))
               (progn ,@body))))))

(defun confirm-action-tui (prompt)
  "Render a nice native curses pop-up confirmation dialog and return T/NIL."
  (let ((win charms:*standard-window*))
    (tui-render win)
    (multiple-value-bind (win-width win-height)
        (charms:window-dimensions win)
      (let* ((box-width (min 70 (- win-width 4)))
             (box-height 8)
             (box-col (floor (- win-width box-width) 2))
             (box-row (floor (- win-height box-height) 2))
             (inner-width (- box-width 4))
             (prompt-lines (wrap-text prompt inner-width)))

        ;; Draw box background
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-bg)))
        (loop for y from box-row below (+ box-row box-height)
              do (tui-safe-write win (make-string box-width :initial-element #\Space)
                                 box-col y box-width))
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-bg)))

        ;; Draw border
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-border)))
        (tui-box win box-col box-row box-width box-height)
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-border)))

        ;; Draw Title
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-border)))
        (charms/ll:attron charms/ll:a_bold)
        (tui-safe-write win " Confirmation Required " (+ box-col 2) box-row (- box-width 4))
        (charms/ll:attroff charms/ll:a_bold)
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-border)))

        ;; Draw prompt lines
        (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-bg)))
        (loop for line in prompt-lines
              for r from (+ box-row 2)
              do (tui-safe-write win line (+ box-col 2) r inner-width))

        ;; Draw actions help
        (let ((action-str "[Y] Yes   [N/Esc] Cancel"))
          (tui-safe-write win action-str
                          (+ box-col (floor (- box-width (length action-str)) 2))
                          (+ box-row box-height -2)
                          inner-width))
        (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-bg)))

        (charms:refresh-window win)

        ;; Input reading loop
        (loop
          (let ((c (charms:get-char win :ignore-error t)))
            (when c
              (cond
                ((or (char-equal c #\y) (char-equal c #\Y))
                 (return t))
                ((or (char-equal c #\n) (char-equal c #\N)
                     (eql (char-code c) 27))
                 (return nil))))))))))

(defun confirm-action (prompt)
  "Prompts the user for confirmation (Y/N) and returns T for yes, NIL for no."
  (if *tui-running*
      (let ((was-suspended (not (eql (charms/ll:isendwin) 0))))
        (when was-suspended
          (charms/ll:reset-prog-mode)
          (charms:clear-window charms:*standard-window* :force-repaint t))
        (unwind-protect
             (confirm-action-tui prompt)
          (when was-suspended
            (charms/ll:def-prog-mode)
            (charms/ll:endwin))))
      (with-terminal-interaction
        (loop
          (format t "~A [Y/N]: " prompt)
          (finish-output) ; Ensure prompt is displayed before reading input
          (let ((response (read-line *standard-input* nil nil)))
            (cond
              ((string-equal response "y") (return t))
              ((string-equal response "n") (return nil))
              (t (format t "Please enter Y or N.~%"))))))))

(defun get-multiline-input ()
  "Get multiline input from the user using a temporary file and editor."
  (with-terminal-interaction
    (let ((temp-file (merge-pathnames (format nil "hactar-temp-~A.txt" (get-universal-time))
                                      (uiop:temporary-directory))))
      (unwind-protect
          (progn
            ;; Create empty file
            (with-open-file (stream temp-file :direction :output :if-exists :supersede)
              (format stream "# Enter your prompt here. Lines starting with # are comments.~%"))

            ;; Open editor
            (let ((editor (or (uiop:getenv "EDITOR") "nano")))
              (uiop:run-program (list editor (namestring temp-file))
                                :output :interactive
                                :error-output :interactive
                                :input :interactive))

            ;; Read the file content using UTF-8
            (with-open-file (stream temp-file :direction :input :external-format :utf-8)
              (let ((content (make-string (file-length stream))))
                (read-sequence content stream)
                ;; Remove comment lines
                (cl-ppcre:regex-replace-all "^#.*$" content ""))))

        ;; Clean up
        (when (probe-file temp-file)
          (delete-file temp-file))))))
