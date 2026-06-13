;; fuzzy selection inspired by fzf
(in-package :hactar)

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

(defun fuzzy-select-raw (items &key multi)
  "Internal curses-based selector. ITEMS is a list of plists/alists."
  (when (null items) (return-from fuzzy-select-raw nil))
  (when *in-editor*
    (if multi
        (let ((sel (fuzzy-select-simple items)))
          (return-from fuzzy-select-raw (when sel (list sel))))
        (return-from fuzzy-select-raw (fuzzy-select-simple items))))
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
             (item-map (make-hash-table :test 'equal))
             (marked-items (make-hash-table :test 'eq)))

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
                                      (is-marked (and multi (gethash item-plist marked-items)))
                                      (marker (if is-marked "* " "  "))
                                      (safe-list-width (min list-width (1- win-width)))
                                      (display-text (str:shorten safe-list-width (format nil "~A~A" marker (cdr (assoc :item item-plist))) :ellipsis "")))
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
                    (let ((status-str (if multi
                                          (format nil "~A/~A [Selected: ~A] Sel:~A Scr:~A"
                                                  (length filtered-items) (length items)
                                                  (hash-table-count marked-items)
                                                  selected-index display-offset)
                                          (format nil "~A/~A Sel:~A Scr:~A"
                                                  (length filtered-items) (length items)
                                                  selected-index display-offset))))
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

                      ;; Tab key - toggle selection in multi-select mode
                      ((and multi (or (eql c #\Tab) (eql c (code-char 9))))
                       (let ((current-item (when (> (length filtered-items) 0) (nth selected-index filtered-items))))
                         (when current-item
                           (if (gethash current-item marked-items)
                               (remhash current-item marked-items)
                               (setf (gethash current-item marked-items) t))
                           ;; Move down to the next item
                           (setf selected-index (if (null filtered-items) 0
                                                    (min (1- (length filtered-items)) (1+ selected-index)))))))

                      ;; Enter - return selection
                      ((or (eql c #\Return) (eql c #\Newline))
                       (cond
                         (multi
                          (let ((selected-list '()))
                            (maphash (lambda (k v) (declare (ignore v)) (push k selected-list)) marked-items)
                            (if (null selected-list)
                                (let ((current-item (when (> (length filtered-items) 0) (nth selected-index filtered-items))))
                                  (setf result (if current-item (list current-item) nil)))
                                (setf result (nreverse selected-list)))))
                         (t
                          (setf result (when (> (length filtered-items) 0)
                                         (nth selected-index filtered-items)))))
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

(defun fuzzy-select (items &key multi)
  "Unified interactive selector. ITEMS can be a list of strings or a list of alists/plists.
   If MULTI is true, returns a list of selected items. Otherwise, returns a single selected item."
  (when (null items) (return-from fuzzy-select nil))
  (let* ((is-string-list (stringp (first items)))
         (formatted-items (if is-string-list
                              (mapcar (lambda (item)
                                        `((:item . ,item)
                                          (:preview . ,item)))
                                      items)
                              items))
         (selected-raw (fuzzy-select-raw formatted-items :multi multi)))
    (cond
      ((null selected-raw) nil)
      (multi
       (if is-string-list
           (mapcar (lambda (x) (cdr (assoc :item x))) selected-raw)
           selected-raw))
      (t
       (if is-string-list
           (cdr (assoc :item selected-raw))
           selected-raw)))))

(defun format-doc-for-fzf-preview (doc)
  "Formats a document alist into a string suitable for preview."
  (let* ((title (cdr (assoc :title doc)))
         (content (cdr (assoc :content doc)))
         (tags (cdr (assoc :tags doc)))
         (covers (cdr (assoc :covers doc)))
         (uri (cdr (assoc :uri doc)))
         (source (cdr (assoc :source doc))))
    (with-output-to-string (s)
      (when title
        (format s "Title: ~A~%" title))
      (when uri
        (format s "URI: ~A~%" uri))
      (when source
        (format s "Source: ~A~%" source))
      (when tags
        (format s "Tags: ~{~A~^, ~}~%" (uiop:ensure-list tags)))
      (when covers
        (format s "Covers: ~{~A~^, ~}~%" (uiop:ensure-list covers)))
      (format s "~%~A~%" (or content "")))))

(defun fuzzy-select-doc (doc-list)
  "Uses the internal selection system to select a document from a list of plists/alists.
   Displays titles, previews full doc, returns the selected plist."
  (when doc-list
    (let* ((items (loop for doc in doc-list
                        for title = (or (cdr (assoc :title doc))
                                        (getf doc :title)
                                        (cdr (assoc :item doc))
                                        (getf doc :item)
                                        "Untitled")
                        collect `((:item . ,title)
                                  (:preview . ,(format-doc-for-fzf-preview doc))
                                  (:doc . ,doc))))
           (selected-item (fuzzy-select items)))
      (when selected-item
        (or (cdr (assoc :doc selected-item))
            (getf selected-item :doc))))))
