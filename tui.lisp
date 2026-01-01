(in-package :hactar)
;; tui handling stuff

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
  "Simple text wrapper."
  (when text
    (let ((lines '())
          (current-line (make-string-output-stream))
          (current-length 0))
      (dolist (word (str:words text))
        (let ((word-length (length word)))
          (when (and (not (zerop current-length))
                     (> (+ current-length word-length 1) width))
            ;; Current line is full, push it and start a new one
            (push (get-output-stream-string current-line) lines)
            (setf current-line (make-string-output-stream))
            (setf current-length 0))
          ;; Add word to current line
          (unless (zerop current-length)
            (write-char #\Space current-line)
            (incf current-length))
          (write-string word current-line)
          (incf current-length word-length)))
      ;; Push the last line
      (push (get-output-stream-string current-line) lines)
      (nreverse lines))))

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
