(in-package :hactar)
;;* tui state 
(defvar *tui-running* nil "T when the 3-column TUI is active.")
(defvar *tui-chat-lines* '() "List of chat line plists for the main panel.")
(defvar *tui-sidebar-model* nil "Current model name string for sidebar display.")
(defvar *tui-sidebar-files* '() "List of modified file strings for sidebar.")
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

;; Modal (command palette) completion state
(defvar *tui-modal-completions* '() "Completion candidates in the command modal.")
(defvar *tui-modal-completion-index* -1 "Selected index in modal completion list.")
(defvar *tui-modal-completion-prefix* "" "The query prefix when modal completion started.")

;; Color pair IDs are now managed dynamically by tui-theme.lisp.
;; Use (tui-color-pair :role) to get the ncurses pair ID for a semantic role.

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
        (bottom (+ row height -1)))
    ;; corners
    (handler-case (charms:write-char-at-point win #\┌ col row) (error () nil))
    (handler-case (charms:write-char-at-point win #\┐ right row) (error () nil))
    (handler-case (charms:write-char-at-point win #\└ col bottom) (error () nil))
    (handler-case (charms:write-char-at-point win #\┘ right bottom) (error () nil))
    ;; edges
    (tui-hline win (1+ col) row (- width 2) #\─)
    (tui-hline win (1+ col) bottom (- width 2) #\─)
    (tui-vline win col (1+ row) (- height 2) #\│)
    (tui-vline win right (1+ row) (- height 2) #\│)))

;;; --- Completion helpers ---

(defun tui-reset-completions ()
  "Clear any active completion state."
  (setf *tui-completions* '())
  (setf *tui-completion-index* -1)
  (setf *tui-completion-prefix* ""))

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
                         (completions (get-command-completions cmd-name partial args-without-partial)))
                    (when completions
                      (setf *tui-completion-prefix* input)
                      (setf *tui-completions* completions)
                      (setf *tui-completion-index* 0))))))))))

;;; --- Modal completion helpers ---

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

;;; --- Chat line management ---

(defun tui-add-chat-line (text &key (type :text) (role "system"))
  "Add a line to the chat panel. TYPE can be :text, :thought, :bash, :assistant, :user, :error."
  (push (list :text text :type type :role role) *tui-chat-lines*))

(defun tui-add-user-message (text)
  (tui-add-chat-line (format nil "> ~A" text) :type :user :role "user"))

(defun tui-add-assistant-message (text)
  (tui-add-chat-line text :type :assistant :role "assistant"))

(defun tui-add-thought (seconds)
  (tui-add-chat-line (format nil "Thought for ~As" seconds) :type :thought))

(defun tui-add-bash-output (task-name command output)
  (tui-add-chat-line (format nil "[ok] Bash task ~A" task-name) :type :bash)
  (when command
    (tui-add-chat-line (format nil "  task: ~A" command) :type :text))
  (when output
    (dolist (line (str:lines output))
      (tui-add-chat-line (format nil "    ~A" line) :type :text))))

;;; --- Tool call tracking ---

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

;;*Sidebar

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
                *files*)))
;;*Rendering
(defun tui-render-chat-panel (win col row width height)
  "Render the main chat area."
  ;; Wrap all chat lines to width, collect into display lines
  (let* ((content-width (- width 2))
         (all-display-lines '()))
    ;; Chat lines are newest-first in *tui-chat-lines*, reverse for display
    (dolist (chat-line (reverse *tui-chat-lines*))
      (let ((text (getf chat-line :text))
            (type (getf chat-line :type)))
        (let ((wrapped (wrap-text text content-width)))
          (dolist (wline (or wrapped '("")))
            (push (list :text wline :type type) all-display-lines)))))
    (setf all-display-lines (nreverse all-display-lines))
    (let* ((total-lines (length all-display-lines))
           (visible-lines height)
           (start (max 0 (- total-lines visible-lines *tui-scroll-offset*))))
      (loop for i from 0 below visible-lines
            for line-idx = (+ start i)
            for screen-row = (+ row i)
            do (if (and (>= line-idx 0) (< line-idx total-lines))
                   (let* ((dline (nth line-idx all-display-lines))
                          (text (getf dline :text))
                          (type (getf dline :type)))
                     ;; Set color based on type
                     (let ((pair (case type
                                   (:user (tui-color-pair :chat-user))
                                   (:assistant (tui-color-pair :chat-assistant))
                                   (:thought (tui-color-pair :chat-thought))
                                   (:bash (tui-color-pair :chat-bash))
                                   (:error (tui-color-pair :chat-error))
                                   (t 0))))
                       (when (> pair 0)
                         (charms/ll:attron (charms/ll:color-pair pair)))
                       (tui-safe-write win text col screen-row content-width)
                       (when (> pair 0)
                         (charms/ll:attroff (charms/ll:color-pair pair)))))
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

(defun tui-render-completions (win input-col input-row chat-width)
  "Render a completion dropdown above the input bar."
  (when (and *tui-completions* (>= *tui-completion-index* 0))
    (let* ((count (length *tui-completions*))
           (max-visible (min count 10))
           ;; Compute width: widest completion + 2 for padding
           (menu-width (min (- chat-width 4)
                            (+ 4 (reduce #'max *tui-completions*
                                         :key #'length :initial-value 0))))
           (menu-col (+ input-col 2))
           ;; Scroll offset so selected item is always visible
           (scroll (max 0 (min (- count max-visible)
                               (- *tui-completion-index* (1- max-visible)))))
           ;; Draw upward from the input row
           (base-row (1- input-row)))
      ;; Draw items from bottom to top (closest to input = first visible item)
      (loop for vi from 0 below max-visible
            for idx = (+ scroll vi)
            for screen-row = (- base-row (- max-visible vi 1))
            when (and (>= idx 0) (< idx count) (>= screen-row 0))
            do (let* ((item (nth idx *tui-completions*))
                      (selected-p (= idx *tui-completion-index*))
                      (display (str:shorten menu-width
                                            (format nil " ~A " item)
                                            :ellipsis "..")))
                 ;; Fill background
                 (if selected-p
                     (charms/ll:attron (charms/ll:color-pair (tui-color-pair :selected)))
                     (charms/ll:attron (charms/ll:color-pair (tui-color-pair :completion))))
                 (tui-safe-write win (make-string menu-width :initial-element #\Space)
                                 menu-col screen-row menu-width)
                 (tui-safe-write win display menu-col screen-row menu-width)
                 (if selected-p
                     (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :selected)))
                     (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :completion)))))))))

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

;;*Comman Palette

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
               (push (list :name name :description (second info)) cmds))
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
           (input-row (- total-height 3))
           (status-row (- total-height 1))
           (chat-height (- total-height 4)) ;; room for input + status + border
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

      ;; Render completion dropdown above input
      (when (and *tui-completions* (not *tui-command-modal-open*))
        (tui-render-completions win 1 input-row chat-width))

      ;; Render sidebar
      (tui-render-sidebar win sidebar-col 1 sidebar-width (- total-height 2))

      ;; Render status bar (full width, bottom)
      (tui-render-status-bar win 0 status-row total-width)

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

      ;; Enter - execute selected command
      ((or (eql c #\Return) (eql c #\Newline))
       (setf *tui-command-modal-open* nil)
       (tui-modal-reset-completions)
       (when (and filtered (< *tui-command-selected* (length filtered)))
         (let ((cmd (nth *tui-command-selected* filtered)))
           (cons :execute (getf cmd :name)))))

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

    ;; Tab: cycle to next completion
    ((eql c #\Tab)
     (setf *tui-completion-index*
           (mod (1+ *tui-completion-index*) (length *tui-completions*)))
     :handled)

    ;; Escape: dismiss completions
    ((eql c #\Esc)
     (tui-reset-completions)
     :handled)

    ;; Anything else: dismiss and pass through
    (t
     (tui-reset-completions)
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
     (tui-try-complete)
     nil)

    ;; Ctrl-P or Ctrl-K: open command palette
    ((or (eql c (code-char 16))   ;; Ctrl-P
         (eql c (code-char 11)))  ;; Ctrl-K
     (setf *tui-command-modal-open* t)
     (setf *tui-command-query* "")
     (setf *tui-command-selected* 0)
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

    ;; Backspace
    ((or (eql c #\Backspace) (eql c #\Rubout) (eql c (code-char 127))
         (eql c (code-char charms/ll:key_backspace)))
     (when (> (length *tui-input-buffer*) 0)
       (setf *tui-input-buffer* (subseq *tui-input-buffer* 0 (1- (length *tui-input-buffer*)))))
     nil)

    ;; Printable character
    ((and c (graphic-char-p c))
     (setf *tui-input-buffer* (concatenate 'string *tui-input-buffer* (string c)))
     nil)

    (t nil)))

;;; --- Main TUI loop ---

(defun tui-process-command (cmd-name)
  "Execute a command by name within the TUI context."
  (multiple-value-bind (cmd args) (parse-command cmd-name)
    (if cmd
        (let ((output (with-output-to-string (*standard-output*)
                        (execute-command cmd args))))
          (when (and output (> (length output) 0))
            (dolist (line (str:lines output))
              (tui-add-chat-line line :type :text))))
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
         (response-line-index nil))

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
            (setf response-line-index 0) ;; index 0 since we just pushed to front

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
              ;; Final update of the chat line text
              (setf (getf (first *tui-chat-lines*) :text) assistant-response)
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
    (when (string= input "") (return-from tui-send-message))

    ;; Check if it's a command
    (multiple-value-bind (cmd args) (parse-command input)
      (if cmd
          (let ((output (with-output-to-string (*standard-output*)
                          (execute-command cmd args))))
            (when (and output (> (length output) 0))
              (dolist (line (str:lines output))
                (tui-add-chat-line line :type :text))))
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
            (tui-set-status (format nil "Model: ~A | C-p: Commands | C-c: Quit"
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
  (setf *tui-sidebar-tool-calls* nil)
  (unless *tui-sidebar-widgets*
    (setf *tui-sidebar-widgets* (make-default-sidebar-widgets)))
  (tui-reset-completions)
  (tui-refresh-sidebar-data)
  (tui-set-status (format nil "Model: ~A | C-p: Commands | C-c: Quit"
                          (if *current-model* (model-config-name *current-model*) "None"))
                  :info)
  (tui-add-chat-line "Hactar AI Pair Programmer" :type :text)
  (tui-add-chat-line "Type /help for commands, or Ctrl-P to open the command palette." :type :text)
  (tui-add-chat-line "" :type :text)

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
                        (t nil)))))))))

  (setf *tui-running* nil))

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

(defun fuzzy-select (items)
  "Displays a simple fzf-like TUI selector using fuzzy-match and cl-charms.
   In editor mode, falls back to a simple numbered list selection.
   ITEMS is a list of plists, e.g., '(((:item . \"display\") (:preview \"preview text\")))'.
   Returns the selected plist or nil if cancelled (Esc)."
  (when (null items) (return-from fuzzy-select nil))
  ;; In editor mode, use simple numbered selection instead of curses TUI
  (when *in-editor*
    (return-from fuzzy-select (fuzzy-select-simple items)))
  (when *in-repl*
    (rl:deprep-terminal))
  (let ((result nil))
    (charms:with-curses ()
      (charms:disable-echoing)
      (charms:enable-raw-input :interpret-control-characters t)
      (charms:enable-extra-keys charms:*standard-window*)
      (charms/ll:curs-set 0) ; Hide cursor

      (let* ((query "")
             (selected-index 0)
             (display-offset 0)
             (filtered-items items)
             (item-map (make-hash-table :test 'equal)))

        (dolist (item items)
          (setf (gethash (cdr (assoc :item item)) item-map) item))

        (loop named select-loop do
              (multiple-value-bind (win-width win-height)
                  (charms:window-dimensions charms:*standard-window*)
                (let* ((input-row 0)          ; Row 0 for input (charms uses 0-based)
                       (top-border-row 1)
                       (list-start-row 2)
                       (status-row (1- win-height))
                       (bottom-border-row (max top-border-row (1- status-row)))
                       (list-height (max 1 (- win-height 3)))
                       (list-width (max 10 (floor (* win-width 0.5))))
                       (preview-border-col list-width)
                       (preview-content-col (1+ preview-border-col))
                       (preview-end-col (min (1- win-width) (1- win-width)))
                       (preview-content-width (max 1 (- preview-end-col preview-content-col))))

                  ;; Clear screen
                  (charms:clear-window charms:*standard-window* :force-repaint t)

                  ;; 1. Filter and sort items based on query using fuzzy-match
                  (setf filtered-items
                        (if (string= query "")
                            items
                            (let* ((display-strings (mapcar (lambda (item) (cdr (assoc :item item))) items))
                                   (matched-strings (fuzzy-match:fuzzy-match query display-strings)))
                              (mapcar (lambda (str) (gethash str item-map)) matched-strings))))

                  ;; 2. Clamp selected-index
                  (setf selected-index (if (null filtered-items)
                                           0
                                           (max 0 (min selected-index (1- (length filtered-items))))))

                  ;; 3. Adjust display-offset (scrolling)
                  (when (< selected-index display-offset)
                    (setf display-offset selected-index))
                  (when (>= selected-index (+ display-offset list-height))
                    (setf display-offset (1+ (- selected-index list-height))))
                  (setf display-offset (max 0 display-offset))

                  ;; 4. Render UI

                  ;; Render Input Line (Row 0)
                  (let ((input-str (format nil "> ~A" query)))
                    (charms:write-string-at-point charms:*standard-window*
                                                  (str:shorten (1- win-width) input-str :ellipsis "")
                                                  0 input-row))

                  ;; Render Preview Top Border (using ASCII for ncurses compatibility)
                  (when (< preview-border-col win-width)
                    (charms:write-char-at-point charms:*standard-window* #\+
                                                preview-border-col top-border-row))
                  (loop for col from preview-content-col
                        for count from 0 below preview-content-width
                        when (< col win-width)
                        do (charms:write-char-at-point charms:*standard-window* #\-
                                                       col top-border-row))
                  (when (< preview-end-col win-width)
                    (charms:write-char-at-point charms:*standard-window* #\+
                                                preview-end-col top-border-row))

                  ;; Prepare Preview Content (Selected Item)
                  (let* ((selected-item (when (> (length filtered-items) 0) (nth selected-index filtered-items)))
                         (preview-text (if selected-item (cdr (assoc :preview selected-item)) "(No selection)"))
                         (wrapped-preview-lines (wrap-text preview-text preview-content-width)))

                    ;; Render Item List (Left Column) and Preview (Right Column with Border)
                    (loop for list-row from 0 below list-height
                          for i = (+ list-row display-offset)
                          for screen-row = (+ list-row list-start-row)
                          do
                             ;; Render List Item
                             (when (< i (length filtered-items))
                               (let* ((item-plist (nth i filtered-items))
                                      (safe-list-width (min list-width (1- win-width)))
                                      (display-text (str:shorten safe-list-width (cdr (assoc :item item-plist)) :ellipsis "")))
                                 (when (= i selected-index)
                                   (charms/ll:attron charms/ll:a_reverse))
                                 (charms:write-string-at-point charms:*standard-window*
                                                               display-text
                                                               0 screen-row)
                                 ;; Pad with spaces to fill the highlight across the list width
                                 (when (= i selected-index)
                                   (loop for pad-col from (length display-text) below safe-list-width
                                         when (< pad-col win-width)
                                         do (charms:write-char-at-point charms:*standard-window* #\Space
                                                                        pad-col screen-row))
                                   (charms/ll:attroff charms/ll:a_reverse))))

                             ;; Draw Preview Left Border
                             (when (< preview-border-col win-width)
                               (charms:write-char-at-point charms:*standard-window* #\|
                                                           preview-border-col screen-row))

                             ;; Render Preview Content Line
                             (when (and (< list-row (length wrapped-preview-lines))
                                        (< preview-content-col win-width))
                               (let* ((avail-width (max 1 (- preview-end-col preview-content-col)))
                                      (line (str:shorten avail-width
                                                         (nth list-row wrapped-preview-lines)
                                                         :ellipsis "")))
                                 (charms:write-string-at-point charms:*standard-window*
                                                               line
                                                               preview-content-col screen-row)))

                             ;; Draw Preview Right Border
                             (when (< preview-end-col win-width)
                               (charms:write-char-at-point charms:*standard-window* #\|
                                                           preview-end-col screen-row)))

                    ;; Render Preview Bottom Border
                    (when (< preview-border-col win-width)
                      (charms:write-char-at-point charms:*standard-window* #\+
                                                  preview-border-col bottom-border-row))
                    (loop for col from preview-content-col
                          for count from 0 below preview-content-width
                          when (< col win-width)
                          do (charms:write-char-at-point charms:*standard-window* #\-
                                                         col bottom-border-row))
                    (when (< preview-end-col win-width)
                      (charms:write-char-at-point charms:*standard-window* #\+
                                                  preview-end-col bottom-border-row))

                    ;; Render Status Line
                    (let ((status-str (format nil "~A/~A Sel:~A Scr:~A"
                                             (length filtered-items) (length items)
                                             selected-index display-offset)))
                      (charms:write-string-at-point charms:*standard-window*
                                                    (str:shorten (1- win-width) status-str :ellipsis "")
                                                    0 status-row)))

                  ;; Refresh the window to display everything
                  (charms:refresh-window charms:*standard-window*)

                  ;; 5. Handle Input
                  (let ((c (charms:get-char charms:*standard-window*)))
                    (cond
                      ;; ESC key (value 27) - cancel selection
                      ((eql c #\Esc)
                       (setf result nil)
                       (return-from select-loop))

                      ;; Arrow keys via charms extra keys
                      ((eql c (code-char charms/ll:key_up))
                       (setf selected-index (max 0 (1- selected-index))))

                      ((eql c (code-char charms/ll:key_down))
                       (setf selected-index (if (null filtered-items) 0
                                                (min (1- (length filtered-items)) (1+ selected-index)))))

                      ;; Enter - return selection
                      ((or (eql c #\Return) (eql c #\Newline))
                       (setf result (when (> (length filtered-items) 0)
                                      (nth selected-index filtered-items)))
                       (return-from select-loop))

                      ;; Backspace - remove character from query
                      ((or (eql c #\Backspace) (eql c #\Rubout)
                           (eql c (code-char 127))    ; DEL
                           (eql c (code-char charms/ll:key_backspace)))
                       (when (> (length query) 0)
                         (setf query (subseq query 0 (1- (length query))))
                         (setf selected-index 0)
                         (setf display-offset 0)))

                      ;; Any other printable character - add to query
                      ((and c (graphic-char-p c))
                       (setf query (concatenate 'string query (string c)))
                       (setf selected-index 0)
                       (setf display-offset 0)))))))))
    (when *in-repl*
      (rl:prep-terminal t))
    result))
(defun fuzzy-select-simple (items)
  "Simple numbered list selector for editor mode (no curses).
   Returns the selected plist or nil."
  (format t "~%Select an item:~%")
  (loop for item in items
        for i from 1
        do (format t "  ~A) ~A~%" i (cdr (assoc :item item))))
  (format t "Enter number (or 0 to cancel): ")
  (force-output)
  (handler-case
      (let* ((input (read-line *standard-input* nil nil))
             (n (when input (parse-integer (string-trim '(#\Space #\Tab) input) :junk-allowed t))))
        (if (and n (> n 0) (<= n (length items)))
            (nth (1- n) items)
            nil))
    (error () nil)))

;; ** fzf
(defun select-with-fzf (items &key preview-command)
  "Presents a list of items to the user via fzf and returns the selected item.
   In editor mode, falls back to a simple numbered list.
   ITEMS should be a list of strings.
   PREVIEW-COMMAND is an optional shell command string used for the fzf preview."
  (when (and items *in-editor*)
    (return-from select-with-fzf (select-with-fzf-simple items)))
  (when items
    (uiop:with-temporary-file (:pathname input-pathname :stream input-stream :keep nil)
                              (uiop:with-temporary-file (:pathname output-pathname :keep nil)
                                                        ;; Write items to input file
                                                        (dolist (item items)
                                                          (format input-stream "~A~%" item))
                                                        (finish-output input-stream)
                                                        (close input-stream)

                                                        ;; Build fzf command
                                                        (let* ((fzf-base-command "fzf --exit-0 --bind 'enter:accept-non-empty'")
                                                               (fzf-preview-opts (if preview-command
                                                                                   (format nil "--preview '~A' --preview-window=right:60%:wrap" preview-command)
                                                                                   ""))
                                                               (fzf-command (format nil "~A ~A < ~S > ~S"
                                                                                    fzf-base-command
                                                                                    fzf-preview-opts
                                                                                    (uiop:native-namestring input-pathname)
                                                                                    (uiop:native-namestring output-pathname))))
                                                          (debug-log "Running fzf command:" fzf-command)
                                                          (rl:deprep-terminal)
                                                          (unwind-protect
                                                              (handler-case
                                                                  (let ((exit-code (uiop:run-program fzf-command
                                                                                                     :force-shell t
                                                                                                     :input :interactive
                                                                                                     :output :interactive
                                                                                                     :error-output :interactive
                                                                                                     :ignore-error-status t)))
                                                                    (cond
                                                                      ((or (not exit-code) (= 0 exit-code))
                                                                        (when (probe-file output-pathname)
                                                                          (let ((result (string-trim '(#\Newline #\Return #\Space #\Tab)
                                                                                                     (uiop:read-file-string output-pathname))))
									    (if (string= result "")
                                                                              nil
                                                                              result))))
                                                                      ((= exit-code 1) ; No match / user aborted with Esc/Ctrl-C before selection
                                                                        (debug-log "fzf exited with code 1 (no match or aborted)")
                                                                        nil)
                                                                      ((= exit-code 130) ; User cancelled with Ctrl-C after selection or Esc
                                                                        (debug-log "fzf cancelled by user (exit code 130)")
                                                                        nil)
                                                                      (t
                                                                        (debug-log "fzf exited with code:" exit-code)
                                                                        nil)))
                                                                (error (e)
                                                                  (debug-log "Error running fzf:" e)
                                                                 nil))
                                                            (rl:prep-terminal t)))))))
(defun select-with-fzf-simple (items)
  "Simple numbered list selector for editor mode (no fzf). ITEMS is a list of strings."
  (format t "~%Select an item:~%")
  (loop for item in items
        for i from 1
        do (format t "  ~A) ~A~%" i item))
  (format t "Enter number (or 0 to cancel): ")
  (force-output)
  (handler-case
      (let* ((input (read-line *standard-input* nil nil))
             (n (when input (parse-integer (string-trim '(#\Space #\Tab) input) :junk-allowed t))))
        (if (and n (> n 0) (<= n (length items)))
            (nth (1- n) items)
            nil))
    (error () nil)))

;;** user inputs
(defun confirm-action (prompt)
  "Prompts the user for confirmation (Y/N) and returns T for yes, NIL for no."
  (loop
    (format t "~A [Y/N]: " prompt)
    (finish-output) ; Ensure prompt is displayed before reading input
    (let ((response (read-line *standard-input* nil nil)))
      (cond
        ((string-equal response "y") (return t))
        ((string-equal response "n") (return nil))
        (t (format t "Please enter Y or N.~%"))))))

(defun get-multiline-input ()
  "Get multiline input from the user using a temporary file and editor."
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
        (delete-file temp-file)))))
;; * fzf
(defun select-doc-with-fzf (doc-list)
  "Uses fzf to select a document from a list of plists.
   In editor mode, falls back to a simple numbered list.
   Displays titles, previews full doc, returns the selected plist."
  (when (and doc-list *in-editor*)
    (return-from select-doc-with-fzf
      (let* ((items (loop for doc in doc-list
                          for title = (cdr (assoc :title doc))
                          collect `((:item . ,title) (:preview . ,(format-doc-for-fzf-preview doc)))))
             (selected (fuzzy-select-simple items)))
        (when selected
          (let ((idx (position selected items :test #'eq)))
            (when idx (nth idx doc-list)))))))
  (when doc-list
    (uiop:with-temporary-file (:pathname input-pathname :stream input-stream :keep nil)
                              (uiop:with-temporary-file (:pathname preview-pathname :stream preview-stream :keep nil)
                                                        (uiop:with-temporary-file (:pathname output-pathname :keep nil)
                                                                                  ;; Write Index<tab>Title to input file and full preview to preview file
                                                                                  (loop for doc in doc-list
                                                                                        for i from 0
                                                                                        for title = (cdr (assoc :title doc))
                                                                                        for preview-string = (format-doc-for-fzf-preview doc)
                                                                                        do (format input-stream "~A~C~A~%" i #\Tab title)
                                                                                        (format preview-stream "~A~%" preview-string))
                                                                                  (finish-output input-stream)
                                                                                  (close input-stream)
                                                                                  (finish-output preview-stream)
                                                                                  (close preview-stream)

                                                                                  ;; Build fzf command
                                                                                  (let* ((fzf-command (format nil "fzf --exit-0 --bind 'enter:accept-non-empty' --delimiter='\\t' --with-nth=2 --preview=\"head -n {n} '~A' | tail -n 1\" --preview-window=right:60%:wrap < ~S > ~S"
                                                                                                              (uiop:native-namestring preview-pathname)
                                                                                                              (uiop:native-namestring input-pathname)
                                                                                                              (uiop:native-namestring output-pathname))))
                                                                                    (debug-log "Running fzf command for docs:" fzf-command)
										    (when *in-repl*
                                                                                      (rl:deprep-terminal))
                                                                                    (unwind-protect
                                                                                         (handler-case
                                                                                             (let ((exit-code (uiop:run-program fzf-command
																:force-shell t
																:input :interactive
																:output :interactive
																:error-output :interactive
																:ignore-error-status t)))
                                                                                               (cond
												 ((or (not exit-code) (= 0 exit-code))
                                                                                                  (when (probe-file output-pathname)
                                                                                                    (let* ((selected-line (string-trim '(#\Newline #\Return) (uiop:read-file-string output-pathname)))
                                                                                                           (tab-pos (position #\Tab selected-line)))
                                                                                                      (when tab-pos
													(let ((selected-index-str (subseq selected-line 0 tab-pos)))
                                                                                                          (handler-case
                                                                                                              (let ((selected-index (parse-integer selected-index-str)))
                                                                                                                (nth selected-index doc-list))
                                                                                                            (error (e)
													      (debug-log "Error parsing selected index:" selected-index-str e)
													      nil)))))))
												 ((= exit-code 1)
                                                                                                  (debug-log "fzf (docs) exited with code 1 (no match or aborted)")
                                                                                                  nil)
												 ((= exit-code 130)
                                                                                                  (debug-log "fzf (docs) cancelled by user (exit code 130)")
                                                                                                  nil)
												 (t
                                                                                                  (debug-log "fzf (docs) exited with code:" exit-code)
                                                                                                  nil)))
                                                                                           (error (e)
											     (debug-log "Error running fzf for docs:" e)
											     nil))
										      (when *in-repl*
											(rl:prep-terminal t)))))))))
