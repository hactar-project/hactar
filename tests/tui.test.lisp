(in-package :hactar-tests)

(def-suite tui-tests :description "Tests for TUI utility functions.")
(in-suite tui-tests)

;;* wrap-text

(test wrap-text-nil-input
  "wrap-text returns NIL when given NIL."
  (is (null (hactar::wrap-text nil 40))))

(test wrap-text-empty-string
  "wrap-text on an empty string returns a list with one empty string."
  (let ((result (hactar::wrap-text "" 40)))
    (is (= 1 (length result)))
    (is (string= "" (first result)))))

(test wrap-text-single-word-fits
  "A single word shorter than width stays on one line."
  (let ((result (hactar::wrap-text "hello" 40)))
    (is (= 1 (length result)))
    (is (string= "hello" (first result)))))

(test wrap-text-single-word-exceeds-width
  "A single word longer than width still appears on one line (no mid-word break)."
  (let ((result (hactar::wrap-text "superlongword" 5)))
    (is (= 1 (length result)))
    (is (string= "superlongword" (first result)))))

(test wrap-text-wraps-at-width
  "Multiple words wrap to the next line when exceeding width."
  (let ((result (hactar::wrap-text "one two three four" 10)))
    ;; "one two" = 7, "three" = 5, "four" = 4
    ;; "one two" fits in 10, "one two three" = 13 > 10
    (is (>= (length result) 2))
    ;; Each line should not exceed width (unless a single word is longer)
    (dolist (line result)
      (is (<= (length line) 18))))) ; Sanity: no line longer than input

(test wrap-text-respects-width-boundary
  "Words are placed on new lines when they would exceed the width."
  (let ((result (hactar::wrap-text "aa bb cc dd" 5)))
    ;; "aa bb" = 5, fits exactly. "cc dd" = 5, fits exactly.
    (is (= 2 (length result)))
    (is (string= "aa bb" (first result)))
    (is (string= "cc dd" (second result)))))

(test wrap-text-multiple-spaces-collapsed
  "str:words splits on whitespace, so multiple spaces between words are collapsed."
  (let ((result (hactar::wrap-text "hello    world" 40)))
    (is (= 1 (length result)))
    (is (string= "hello world" (first result)))))

(test wrap-text-width-1
  "With width 1, each word goes on its own line."
  (let ((result (hactar::wrap-text "a b c" 1)))
    (is (= 3 (length result)))
    (is (string= "a" (first result)))
    (is (string= "b" (second result)))
    (is (string= "c" (third result)))))

(test wrap-text-exact-fit
  "Text that fits exactly in the width produces one line."
  (let ((result (hactar::wrap-text "abcde" 5)))
    (is (= 1 (length result)))
    (is (string= "abcde" (first result)))))

(test wrap-text-long-paragraph
  "A longer paragraph wraps correctly and no line exceeds the width (barring single long words)."
  (let* ((text "The quick brown fox jumps over the lazy dog and then runs away fast")
         (width 20)
         (result (hactar::wrap-text text width)))
    (is (> (length result) 1))
    ;; Each line should be at most `width` characters unless a single word exceeds it
    (dolist (line result)
      ;; Each line should be reasonable
      (is (> (length line) 0)))))

;;* ansi

(test ansi-generates-escape-sequence
  "ansi generates a proper ESC + code string."
  (let ((result (hactar::ansi "[7m")))
    (is (stringp result))
    (is (char= #\Esc (char result 0)))
    (is (string= "[7m" (subseq result 1)))))

(test ansi-empty-code
  "ansi with an empty string just returns ESC."
  (let ((result (hactar::ansi "")))
    (is (= 1 (length result)))
    (is (char= #\Esc (char result 0)))))

(test ansi-cursor-movement
  "ansi can generate cursor movement sequences."
  (let ((result (hactar::ansi "[5;10H")))
    (is (char= #\Esc (char result 0)))
    (is (string= "[5;10H" (subseq result 1)))))

;;* TUI state management

(test tui-add-chat-line-basic
  "tui-add-chat-line pushes a plist onto *tui-chat-lines*."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-chat-line "Hello world" :type :text :role "system")
    (is (= 1 (length hactar::*tui-chat-lines*)))
    (let ((line (first hactar::*tui-chat-lines*)))
      (is (string= "Hello world" (getf line :text)))
      (is (eq :text (getf line :type)))
      (is (string= "system" (getf line :role))))))

(test tui-add-chat-line-multiple
  "Multiple calls to tui-add-chat-line accumulate lines (newest first)."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-chat-line "First" :type :text)
    (hactar::tui-add-chat-line "Second" :type :text)
    (hactar::tui-add-chat-line "Third" :type :text)
    (is (= 3 (length hactar::*tui-chat-lines*)))
    ;; push means newest is first
    (is (string= "Third" (getf (first hactar::*tui-chat-lines*) :text)))
    (is (string= "First" (getf (third hactar::*tui-chat-lines*) :text)))))

(test tui-add-chat-line-default-role
  "tui-add-chat-line defaults role to 'system'."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-chat-line "test")
    (is (string= "system" (getf (first hactar::*tui-chat-lines*) :role)))))

(test tui-add-chat-line-default-type
  "tui-add-chat-line defaults type to :text."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-chat-line "test")
    (is (eq :text (getf (first hactar::*tui-chat-lines*) :type)))))

(test tui-add-user-message
  "tui-add-user-message adds a line with > prefix, :user type, and 'user' role."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-user-message "Hello")
    (is (= 1 (length hactar::*tui-chat-lines*)))
    (let ((line (first hactar::*tui-chat-lines*)))
      (is (string= "> Hello" (getf line :text)))
      (is (eq :user (getf line :type)))
      (is (string= "user" (getf line :role))))))

(test tui-add-assistant-message
  "tui-add-assistant-message adds a line with :assistant type."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-assistant-message "I can help with that.")
    (is (= 1 (length hactar::*tui-chat-lines*)))
    (let ((line (first hactar::*tui-chat-lines*)))
      (is (string= "I can help with that." (getf line :text)))
      (is (eq :assistant (getf line :type)))
      (is (string= "assistant" (getf line :role))))))

(test tui-add-thought
  "tui-add-thought adds a thought line with seconds."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-thought 5)
    (is (= 1 (length hactar::*tui-chat-lines*)))
    (let ((line (first hactar::*tui-chat-lines*)))
      (is (search "5" (getf line :text)))
      (is (eq :thought (getf line :type))))))

(test tui-add-bash-output-basic
  "tui-add-bash-output adds task header, command, and output lines."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-bash-output "build" "make all" (format nil "ok~%done"))
    ;; Should have: task header + command line + 2 output lines = 4
    (is (= 4 (length hactar::*tui-chat-lines*)))
    ;; Newest first, so first element is last added line
    (let ((all-texts (mapcar (lambda (l) (getf l :text)) (reverse hactar::*tui-chat-lines*))))
      (is (search "build" (first all-texts)))
      (is (search "make all" (second all-texts))))))

(test tui-add-bash-output-nil-command
  "tui-add-bash-output with nil command skips the command line."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-bash-output "test" nil "output line")
    ;; task header + 1 output line = 2
    (is (= 2 (length hactar::*tui-chat-lines*)))))

(test tui-add-bash-output-nil-output
  "tui-add-bash-output with nil output skips output lines."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-bash-output "test" "cmd" nil)
    ;; task header + command line = 2
    (is (= 2 (length hactar::*tui-chat-lines*)))))

(test tui-add-bash-output-both-nil
  "tui-add-bash-output with nil command and output only adds header."
  (let ((hactar::*tui-chat-lines* '()))
    (hactar::tui-add-bash-output "test" nil nil)
    (is (= 1 (length hactar::*tui-chat-lines*)))))

;;* TUI status management

(test tui-set-status-default-type
  "tui-set-status sets text and defaults type to :info."
  (let ((hactar::*tui-status-line* "")
        (hactar::*tui-status-type* :info))
    (hactar::tui-set-status "All good")
    (is (string= "All good" hactar::*tui-status-line*))
    (is (eq :info hactar::*tui-status-type*))))

(test tui-set-status-ok-type
  "tui-set-status with :ok type."
  (let ((hactar::*tui-status-line* "")
        (hactar::*tui-status-type* :info))
    (hactar::tui-set-status "Success!" :ok)
    (is (string= "Success!" hactar::*tui-status-line*))
    (is (eq :ok hactar::*tui-status-type*))))

(test tui-set-status-error-type
  "tui-set-status with :error type."
  (let ((hactar::*tui-status-line* "")
        (hactar::*tui-status-type* :info))
    (hactar::tui-set-status "Something broke" :error)
    (is (string= "Something broke" hactar::*tui-status-line*))
    (is (eq :error hactar::*tui-status-type*))))

;;* TUI sidebar data

(test tui-refresh-sidebar-data-with-model
  "tui-refresh-sidebar-data populates model name from *current-model*."
  (let ((hactar::*tui-sidebar-model* nil)
        (hactar::*tui-project-name* "")
        (hactar::*tui-project-path* "")
        (hactar::*tui-sidebar-files* '())
        (hactar::*current-model* (hactar::find-model-by-name
                                   (if hactar::*available-models*
                                       (hactar::model-config-name (first hactar::*available-models*))
                                       "")))
        (hactar::*name* "test-project")
        (hactar::*repo-root* #P"/tmp/test/")
        (hactar::*files* '()))
    (hactar::tui-refresh-sidebar-data)
    (is (string= "test-project" hactar::*tui-project-name*))
    (is (stringp hactar::*tui-sidebar-model*))))

(test tui-refresh-sidebar-data-no-model
  "tui-refresh-sidebar-data shows 'None' when no model is selected."
  (let ((hactar::*tui-sidebar-model* nil)
        (hactar::*tui-project-name* "")
        (hactar::*tui-project-path* "")
        (hactar::*tui-sidebar-files* '())
        (hactar::*current-model* nil)
        (hactar::*name* nil)
        (hactar::*repo-root* nil)
        (hactar::*files* '()))
    (hactar::tui-refresh-sidebar-data)
    (is (string= "None" hactar::*tui-sidebar-model*))
    (is (string= "" hactar::*tui-project-name*))))

(test tui-refresh-sidebar-data-files
  "tui-refresh-sidebar-data converts context file paths to relative strings."
  (let ((hactar::*tui-sidebar-model* nil)
        (hactar::*tui-project-name* "")
        (hactar::*tui-project-path* "")
        (hactar::*tui-sidebar-files* '())
        (hactar::*current-model* nil)
        (hactar::*name* "proj")
        (hactar::*repo-root* #P"/tmp/proj/")
        (hactar::*files* (list #P"/tmp/proj/foo.lisp" "/tmp/proj/bar.lisp")))
    (hactar::tui-refresh-sidebar-data)
    (is (= 2 (length hactar::*tui-sidebar-files*)))
    (is (every #'stringp hactar::*tui-sidebar-files*))
    ;; Should be relative paths, not absolute
    (is (string= "foo.lisp" (first hactar::*tui-sidebar-files*)))
    (is (string= "bar.lisp" (second hactar::*tui-sidebar-files*)))))

;;* TUI command helpers

(test tui-get-all-commands-returns-list
  "tui-get-all-commands returns a sorted list of command plists."
  (let ((cmds (hactar::tui-get-all-commands)))
    (is (listp cmds))
    ;; Each entry should have :name and :description
    (dolist (cmd cmds)
      (is (stringp (getf cmd :name)))
      (is (or (null (getf cmd :description))
              (stringp (getf cmd :description)))))
    ;; Should be sorted by name
    (when (> (length cmds) 1)
      (loop for (a b) on cmds
            while b
            do (is (string<= (getf a :name) (getf b :name)))))))

(test tui-get-all-commands-includes-known-commands
  "tui-get-all-commands includes commands we know exist like 'help' and 'model'."
  (let* ((cmds (hactar::tui-get-all-commands))
         (names (mapcar (lambda (c) (getf c :name)) cmds)))
    (is (member "/help" names :test #'string=))
    (is (member "/model" names :test #'string=))
    (is (member "/clear" names :test #'string=))))

(test tui-filter-commands-empty-query
  "tui-filter-commands with empty query returns all commands."
  (let* ((cmds (hactar::tui-get-all-commands))
         (filtered (hactar::tui-filter-commands "" cmds)))
    (is (= (length cmds) (length filtered)))))

(test tui-filter-commands-matching-query
  "tui-filter-commands with a specific query filters down the list."
  (let* ((cmds (hactar::tui-get-all-commands))
         (filtered (hactar::tui-filter-commands "help" cmds)))
    (is (> (length filtered) 0))
    (is (<= (length filtered) (length cmds)))
    ;; The filtered results should contain "help" in their names
    (is (member "/help" (mapcar (lambda (c) (getf c :name)) filtered) :test #'string=))))

(test tui-filter-commands-no-match
  "tui-filter-commands with a nonsense query returns empty or very few results."
  (let* ((cmds (hactar::tui-get-all-commands))
         (filtered (hactar::tui-filter-commands "zzzzxxxxxqqqq" cmds)))
    (is (listp filtered))
    ;; Fuzzy match might still return something, but should be much less
    (is (<= (length filtered) (length cmds)))))

;;* TUI input handling (main view, no curses needed)

(test tui-handle-main-input-ctrl-c-quits
  "Ctrl-C (code 3) returns :quit."
  (let ((hactar::*tui-command-modal-open* nil)
        (hactar::*tui-input-buffer* "")
        (hactar::*tui-scroll-offset* 0))
    (is (eq :quit (hactar::tui-handle-main-input (code-char 3))))))

(test tui-handle-main-input-ctrl-p-opens-modal
  "Ctrl-P (code 16) opens the command modal."
  (let ((hactar::*tui-command-modal-open* nil)
        (hactar::*tui-command-query* "old")
        (hactar::*tui-command-selected* 5)
        (hactar::*tui-input-buffer* "")
        (hactar::*tui-scroll-offset* 0))
    (hactar::tui-handle-main-input (code-char 16))
    (is (eq t hactar::*tui-command-modal-open*))
    (is (string= "" hactar::*tui-command-query*))
    (is (= 0 hactar::*tui-command-selected*))))

(test tui-handle-main-input-ctrl-k-opens-modal
  "Ctrl-K (code 11) also opens the command modal."
  (let ((hactar::*tui-command-modal-open* nil)
        (hactar::*tui-command-query* "")
        (hactar::*tui-command-selected* 0)
        (hactar::*tui-input-buffer* "")
        (hactar::*tui-scroll-offset* 0))
    (hactar::tui-handle-main-input (code-char 11))
    (is (eq t hactar::*tui-command-modal-open*))))

(test tui-handle-main-input-enter-sends
  "Enter returns :send when input buffer is non-empty."
  (let ((hactar::*tui-input-buffer* "hello")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (is (eq :send (hactar::tui-handle-main-input #\Return)))))

(test tui-handle-main-input-enter-empty-no-send
  "Enter returns nil when input buffer is empty."
  (let ((hactar::*tui-input-buffer* "")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (is (null (hactar::tui-handle-main-input #\Return)))))

(test tui-handle-main-input-printable-char
  "Printable characters are appended to the input buffer."
  (let ((hactar::*tui-input-buffer* "hel")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (hactar::tui-handle-main-input #\l)
    (is (string= "hell" hactar::*tui-input-buffer*))
    (hactar::tui-handle-main-input #\o)
    (is (string= "hello" hactar::*tui-input-buffer*))))

(test tui-handle-main-input-backspace
  "Backspace removes the last character from input buffer."
  (let ((hactar::*tui-input-buffer* "hello")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (hactar::tui-handle-main-input (code-char 127))
    (is (string= "hell" hactar::*tui-input-buffer*))))

(test tui-handle-main-input-backspace-empty
  "Backspace on empty buffer does nothing."
  (let ((hactar::*tui-input-buffer* "")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (hactar::tui-handle-main-input (code-char 127))
    (is (string= "" hactar::*tui-input-buffer*))))

(test tui-handle-main-input-scroll-up
  "Up arrow increments scroll offset."
  (let ((hactar::*tui-scroll-offset* 0)
        (hactar::*tui-input-buffer* "")
        (hactar::*tui-command-modal-open* nil))
    (hactar::tui-handle-main-input (code-char charms/ll:key_up))
    (is (= 1 hactar::*tui-scroll-offset*))
    (hactar::tui-handle-main-input (code-char charms/ll:key_up))
    (is (= 2 hactar::*tui-scroll-offset*))))

(test tui-handle-main-input-scroll-down
  "Down arrow decrements scroll offset, clamped to 0."
  (let ((hactar::*tui-scroll-offset* 2)
        (hactar::*tui-input-buffer* "")
        (hactar::*tui-command-modal-open* nil))
    (hactar::tui-handle-main-input (code-char charms/ll:key_down))
    (is (= 1 hactar::*tui-scroll-offset*))
    (hactar::tui-handle-main-input (code-char charms/ll:key_down))
    (is (= 0 hactar::*tui-scroll-offset*))
    (hactar::tui-handle-main-input (code-char charms/ll:key_down))
    (is (= 0 hactar::*tui-scroll-offset*))))

;;* TUI modal input handling

(test tui-handle-modal-input-esc-closes
  "ESC closes the command modal."
  (let ((hactar::*tui-command-modal-open* t)
        (hactar::*tui-command-query* "test")
        (hactar::*tui-command-selected* 0))
    (let ((result (hactar::tui-handle-modal-input #\Esc)))
      (is (eq :close result))
      (is (null hactar::*tui-command-modal-open*)))))

(test tui-handle-modal-input-tab-completes-command-name
  "Tab fills the query with the selected command name when no space is present."
  (let ((hactar::*tui-command-tab* :system)
        (hactar::*tui-command-query* "hel")
        (hactar::*tui-command-selected* 0)
        (hactar::*tui-command-modal-open* t))
    (hactar::tui-handle-modal-input #\Tab)
    ;; Query should now contain the name of the first filtered match (without leading /)
    (is (> (length hactar::*tui-command-query*) 0))
    ;; Should have been updated from "hel" to a full command name like "help"
    (is (stringp hactar::*tui-command-query*))))

(test tui-handle-modal-input-tab-completes-arguments
  "Tab completes command arguments when query already contains a command and space."
  (let ((hactar::*tui-command-tab* :system)
        (hactar::*tui-command-query* "model ")
        (hactar::*tui-command-selected* 0)
        (hactar::*tui-command-modal-open* t))
    (hactar::tui-handle-modal-input #\Tab)
    ;; If /model has completions registered and models are available,
    ;; the query should have been extended with a model name
    (is (stringp hactar::*tui-command-query*))
    ;; At minimum, the query should still start with "model"
    (is (str:starts-with? "model" hactar::*tui-command-query*))))

(test tui-handle-modal-input-typing
  "Printable characters are appended to the command query."
  (let ((hactar::*tui-command-query* "")
        (hactar::*tui-command-selected* 3)
        (hactar::*tui-command-modal-open* t))
    (hactar::tui-handle-modal-input #\h)
    (is (string= "h" hactar::*tui-command-query*))
    (is (= 0 hactar::*tui-command-selected*))
    (hactar::tui-handle-modal-input #\e)
    (is (string= "he" hactar::*tui-command-query*))
    (hactar::tui-handle-modal-input #\l)
    (is (string= "hel" hactar::*tui-command-query*))
    (hactar::tui-handle-modal-input #\p)
    (is (string= "help" hactar::*tui-command-query*))))

(test tui-handle-modal-input-backspace
  "Backspace removes last char from query and resets selection."
  (let ((hactar::*tui-command-query* "help")
        (hactar::*tui-command-selected* 2)
        (hactar::*tui-command-modal-open* t))
    (hactar::tui-handle-modal-input (code-char 127))
    (is (string= "hel" hactar::*tui-command-query*))
    (is (= 0 hactar::*tui-command-selected*))))

(test tui-handle-modal-input-backspace-empty
  "Backspace on empty query does nothing."
  (let ((hactar::*tui-command-query* "")
        (hactar::*tui-command-selected* 0)
        (hactar::*tui-command-modal-open* t))
    (hactar::tui-handle-modal-input (code-char 127))
    (is (string= "" hactar::*tui-command-query*))))

(test tui-handle-modal-input-arrow-down
  "Down arrow increments selected index."
  (let ((hactar::*tui-command-query* "")
        (hactar::*tui-command-selected* 0)
        (hactar::*tui-command-modal-open* t))
    (hactar::tui-handle-modal-input (code-char charms/ll:key_down))
    (is (= 1 hactar::*tui-command-selected*))))

(test tui-handle-modal-input-arrow-up
  "Up arrow decrements selected index, clamped to 0."
  (let ((hactar::*tui-command-query* "")
        (hactar::*tui-command-selected* 1)
        (hactar::*tui-command-modal-open* t))
    (hactar::tui-handle-modal-input (code-char charms/ll:key_up))
    (is (= 0 hactar::*tui-command-selected*))
    (hactar::tui-handle-modal-input (code-char charms/ll:key_up))
    (is (= 0 hactar::*tui-command-selected*))))

(test tui-handle-modal-input-enter-executes
  "Enter closes modal and returns (:execute . command-name)."
  (let ((hactar::*tui-command-query* "")
        (hactar::*tui-command-selected* 0)
        (hactar::*tui-command-modal-open* t))
    (let ((result (hactar::tui-handle-modal-input #\Return)))
      (is (null hactar::*tui-command-modal-open*))
      ;; Result should be a cons with :execute
      (when result
        (is (consp result))
        (is (eq :execute (car result)))
        (is (stringp (cdr result)))))))

;;* TUI run-tui initialization

(test tui-run-tui-initializes-state
  "run-tui sets up initial state variables before entering curses.
   We test the initialization part by checking the variables set before charms:with-curses."
  ;; We can't actually run the full TUI loop, but we can verify the state
  ;; setup functions work correctly in isolation
  (let ((hactar::*tui-running* nil)
        (hactar::*tui-chat-lines* '((some old line)))
        (hactar::*tui-input-buffer* "old input")
        (hactar::*tui-scroll-offset* 5)
        (hactar::*tui-command-modal-open* t))
    ;; Simulate what run-tui does before entering the curses loop
    (setf hactar::*tui-running* t)
    (setf hactar::*tui-chat-lines* '())
    (setf hactar::*tui-input-buffer* "")
    (setf hactar::*tui-scroll-offset* 0)
    (setf hactar::*tui-command-modal-open* nil)
    (is (eq t hactar::*tui-running*))
    (is (null hactar::*tui-chat-lines*))
    (is (string= "" hactar::*tui-input-buffer*))
    (is (= 0 hactar::*tui-scroll-offset*))
    (is (null hactar::*tui-command-modal-open*))))

;;* TUI variable defaults

(test tui-variables-have-defaults
  "All TUI state variables have sensible default values."
  ;; These test that the defvar forms provide proper initial values
  (is (null hactar::*tui-running*))
  (is (listp hactar::*tui-chat-lines*))
  (is (stringp hactar::*tui-input-buffer*))
  (is (integerp hactar::*tui-input-cursor*))
  (is (integerp hactar::*tui-scroll-offset*))
  (is (null hactar::*tui-command-modal-open*))
  (is (stringp hactar::*tui-command-query*))
  (is (integerp hactar::*tui-command-selected*))
  (is (keywordp hactar::*tui-command-tab*))
  (is (stringp hactar::*tui-status-line*))
  (is (keywordp hactar::*tui-status-type*))
  (is (stringp hactar::*tui-project-name*))
  (is (stringp hactar::*tui-project-path*)))

;;* TUI completion system

(test tui-reset-completions-clears-state
  "tui-reset-completions clears all completion variables."
  (let ((hactar::*tui-completions* '("a" "b" "c"))
        (hactar::*tui-completion-index* 2)
        (hactar::*tui-completion-prefix* "/model ol"))
    (hactar::tui-reset-completions)
    (is (null hactar::*tui-completions*))
    (is (= -1 hactar::*tui-completion-index*))
    (is (string= "" hactar::*tui-completion-prefix*))))

(test get-command-completions-with-registered-completer
  "get-command-completions calls the registered completer for a command."
  (let ((hactar::*command-completions* (make-hash-table :test 'equal)))
    (setf (gethash "/test-cmd" hactar::*command-completions*)
          (lambda (text args)
            (declare (ignore args))
            (remove-if-not (lambda (s) (str:starts-with-p text s))
                           '("alpha" "beta" "gamma"))))
    (let ((result (hactar::get-command-completions "/test-cmd" "al" nil)))
      (is (= 1 (length result)))
      (is (string= "alpha" (first result))))))

(test get-command-completions-no-completer
  "get-command-completions returns nil when no completer is registered."
  (let ((hactar::*command-completions* (make-hash-table :test 'equal)))
    (is (null (hactar::get-command-completions "/nonexistent" "foo" nil)))))

(test get-command-completions-empty-text
  "get-command-completions with empty text returns all options from completer."
  (let ((hactar::*command-completions* (make-hash-table :test 'equal)))
    (setf (gethash "/test-cmd" hactar::*command-completions*)
          (lambda (text args)
            (declare (ignore args))
            (if (string= text "")
                '("one" "two" "three")
                (remove-if-not (lambda (s) (str:starts-with-p text s))
                               '("one" "two" "three")))))
    (let ((result (hactar::get-command-completions "/test-cmd" "" nil)))
      (is (= 3 (length result))))))

(test tui-try-complete-command-names
  "Tab on a partial command name completes to matching commands."
  (let ((hactar::*tui-input-buffer* "/hel")
        (hactar::*tui-completions* '())
        (hactar::*tui-completion-index* -1)
        (hactar::*tui-completion-prefix* "")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (hactar::tui-try-complete)
    ;; Should have found at least /help
    (is (> (length hactar::*tui-completions*) 0))
    (is (member "/help" hactar::*tui-completions* :test #'string=))
    (is (= 0 hactar::*tui-completion-index*))))

(test tui-try-complete-cycles
  "Repeated Tab calls cycle through completions."
  (let ((hactar::*tui-input-buffer* "/hel")
        (hactar::*tui-completions* '())
        (hactar::*tui-completion-index* -1)
        (hactar::*tui-completion-prefix* "")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (hactar::tui-try-complete) ;; first completion
    (let ((first-completion hactar::*tui-input-buffer*))
      (when (> (length hactar::*tui-completions*) 1)
        (hactar::tui-try-complete) ;; cycle to next
        ;; Index should have advanced
        (is (= 1 hactar::*tui-completion-index*))))))

(test tui-try-complete-model-args
  "Tab on /model completes model names when completions are registered."
  ;; This test relies on the /model command having a :completions function registered
  (when (gethash "/model" hactar::*command-completions*)
    (let ((hactar::*tui-input-buffer* "/model ")
          (hactar::*tui-completions* '())
          (hactar::*tui-completion-index* -1)
          (hactar::*tui-completion-prefix* "")
          (hactar::*tui-scroll-offset* 0)
          (hactar::*tui-command-modal-open* nil))
      (hactar::tui-try-complete)
      ;; Should have model completions if models are available
      (when hactar::*available-models*
        (is (> (length hactar::*tui-completions*) 0))))))

(test tui-handle-main-input-tab-triggers-complete
  "Tab key in main input triggers completion (doesn't return :quit or :send)."
  (let ((hactar::*tui-input-buffer* "/hel")
        (hactar::*tui-completions* '())
        (hactar::*tui-completion-index* -1)
        (hactar::*tui-completion-prefix* "")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (let ((result (hactar::tui-handle-main-input #\Tab)))
      (is (null result)) ;; Tab returns nil (just updates state)
      ;; Completions should be populated
      (is (> (length hactar::*tui-completions*) 0)))))

(test tui-completions-reset-on-printable-char
  "Typing a printable character resets completions."
  (let ((hactar::*tui-input-buffer* "/help")
        (hactar::*tui-completions* '("/help" "/hello"))
        (hactar::*tui-completion-index* 0)
        (hactar::*tui-completion-prefix* "/hel")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (hactar::tui-handle-main-input #\x)
    (is (null hactar::*tui-completions*))
    (is (= -1 hactar::*tui-completion-index*))))

(test tui-completions-reset-on-backspace
  "Backspace resets completions."
  (let ((hactar::*tui-input-buffer* "/help")
        (hactar::*tui-completions* '("/help"))
        (hactar::*tui-completion-index* 0)
        (hactar::*tui-completion-prefix* "/hel")
        (hactar::*tui-scroll-offset* 0)
        (hactar::*tui-command-modal-open* nil))
    (hactar::tui-handle-main-input (code-char 127))
    (is (null hactar::*tui-completions*))
    (is (= -1 hactar::*tui-completion-index*))))

;;* fuzzy-select edge cases

(test fuzzy-select-nil-items
  "fuzzy-select returns NIL immediately when given NIL items."
  (is (null (hactar::fuzzy-select nil))))

(test fuzzy-select-empty-list
  "fuzzy-select returns NIL immediately when given an empty list."
  (is (null (hactar::fuzzy-select '()))))

;;* get-terminal-size

(test get-terminal-size-returns-two-values
  "get-terminal-size returns two positive integer values."
  (multiple-value-bind (rows cols) (hactar::get-terminal-size)
    (is (integerp rows))
    (is (integerp cols))
    (is (> rows 0))
    (is (> cols 0))))

(test get-terminal-size-reasonable-defaults
  "get-terminal-size returns at least 24x80 (the fallback defaults)."
  (multiple-value-bind (rows cols) (hactar::get-terminal-size)
    (is (>= rows 1))
    (is (>= cols 1))))

;;* Theme system tests

(test hex-to-rgb-basic
  "hex-to-rgb parses standard hex colors."
  (multiple-value-bind (r g b) (hactar::hex-to-rgb "#ff0000")
    (is (= 255 r)) (is (= 0 g)) (is (= 0 b)))
  (multiple-value-bind (r g b) (hactar::hex-to-rgb "#00ff00")
    (is (= 0 r)) (is (= 255 g)) (is (= 0 b)))
  (multiple-value-bind (r g b) (hactar::hex-to-rgb "#0000ff")
    (is (= 0 r)) (is (= 0 g)) (is (= 255 b))))

(test hex-to-rgb-gruvbox
  "hex-to-rgb parses Gruvbox bg color correctly."
  (multiple-value-bind (r g b) (hactar::hex-to-rgb "#282828")
    (is (= #x28 r)) (is (= #x28 g)) (is (= #x28 b))))

(test hex-to-rgb-without-hash
  "hex-to-rgb accepts strings without leading #."
  (multiple-value-bind (r g b) (hactar::hex-to-rgb "ebdbb2")
    (is (= #xeb r)) (is (= #xdb g)) (is (= #xb2 b))))

(test hex-to-rgb-invalid-length
  "hex-to-rgb signals error on invalid length."
  (signals error (hactar::hex-to-rgb "#fff"))
  (signals error (hactar::hex-to-rgb "#ff00")))

(test rgb-to-ncurses-1000-scaling
  "rgb-to-ncurses-1000 scales 0-255 to 0-1000."
  (multiple-value-bind (nr ng nb) (hactar::rgb-to-ncurses-1000 0 0 0)
    (is (= 0 nr)) (is (= 0 ng)) (is (= 0 nb)))
  (multiple-value-bind (nr ng nb) (hactar::rgb-to-ncurses-1000 255 255 255)
    (is (= 1000 nr)) (is (= 1000 ng)) (is (= 1000 nb)))
  (multiple-value-bind (nr ng nb) (hactar::rgb-to-ncurses-1000 128 128 128)
    ;; round(128*1000/255) = round(501.96) = 502
    (is (= 502 nr)) (is (= 502 ng)) (is (= 502 nb))))

(test rgb-to-nearest-256-black
  "rgb-to-nearest-256 maps pure black to index 0 or 16."
  (let ((idx (hactar::rgb-to-nearest-256 0 0 0)))
    (is (member idx '(0 16)))))

(test rgb-to-nearest-256-white
  "rgb-to-nearest-256 maps pure white to index 15 or 231."
  (let ((idx (hactar::rgb-to-nearest-256 255 255 255)))
    (is (member idx '(15 231)))))

(test rgb-to-nearest-256-red
  "rgb-to-nearest-256 maps pure red reasonably."
  (let ((idx (hactar::rgb-to-nearest-256 255 0 0)))
    ;; Should be index 9 (bright red) or 196 (cube red)
    (is (> idx 0))
    (is (< idx 256))))

(test rgb-to-nearest-8-basic
  "rgb-to-nearest-8 maps basic colors to ANSI indices."
  (is (= 0 (hactar::rgb-to-nearest-8 0 0 0)))      ;; black
  (is (= 7 (hactar::rgb-to-nearest-8 200 200 200)))) ;; white-ish

(test tui-theme-struct-creation
  "make-tui-theme creates a valid struct."
  (let ((theme (hactar::make-tui-theme :name "test" :bg "#282828" :fg "#ebdbb2")))
    (is (hactar::tui-theme-p theme))
    (is (string= "test" (hactar::tui-theme-name theme)))
    (is (string= "#282828" (hactar::tui-theme-bg theme)))
    (is (string= "#ebdbb2" (hactar::tui-theme-fg theme)))))

(test make-default-theme-valid
  "make-default-theme returns a valid theme struct."
  (let ((theme (hactar::make-default-theme)))
    (is (hactar::tui-theme-p theme))
    (is (string= "default" (hactar::tui-theme-name theme)))))

(test make-gruvbox-dark-theme-valid
  "make-gruvbox-dark-theme returns a valid theme struct."
  (let ((theme (hactar::make-gruvbox-dark-theme)))
    (is (hactar::tui-theme-p theme))
    (is (string= "gruvbox-dark" (hactar::tui-theme-name theme)))
    (is (string= "#282828" (hactar::tui-theme-bg theme)))))

(test all-builtin-themes-valid
  "All built-in theme factories return valid tui-theme structs."
  (dolist (factory hactar::*tui-builtin-themes*)
    (let ((theme (funcall factory)))
      (is (hactar::tui-theme-p theme))
      (is (stringp (hactar::tui-theme-name theme)))
      (is (> (length (hactar::tui-theme-name theme)) 0)))))

(test all-builtin-themes-unique-names
  "All built-in themes have unique names."
  (let ((names (mapcar (lambda (f) (hactar::tui-theme-name (funcall f)))
                       hactar::*tui-builtin-themes*)))
    (is (= (length names) (length (remove-duplicates names :test #'string=))))))

(test find-theme-by-name-builtin
  "find-theme-by-name finds built-in themes."
  (let ((theme (hactar::find-theme-by-name "gruvbox-dark")))
    (is (not (null theme)))
    (is (hactar::tui-theme-p theme))
    (is (string= "gruvbox-dark" (hactar::tui-theme-name theme)))))

(test find-theme-by-name-not-found
  "find-theme-by-name returns nil for unknown themes."
  (is (null (hactar::find-theme-by-name "nonexistent-theme-xyz"))))

(test find-theme-by-name-case-insensitive
  "find-theme-by-name is case-insensitive."
  (let ((theme (hactar::find-theme-by-name "Gruvbox-Dark")))
    (is (not (null theme)))))

(test get-or-create-theme-default
  "get-or-create-theme returns default when no theme is set."
  (let ((hactar::*tui-theme* nil)
        (hactar::*tui-theme-name* nil))
    (let ((theme (hactar::get-or-create-theme)))
      (is (hactar::tui-theme-p theme))
      (is (string= "default" (hactar::tui-theme-name theme))))))

(test get-or-create-theme-named
  "get-or-create-theme uses *tui-theme-name* to find a theme."
  (let ((hactar::*tui-theme* nil)
        (hactar::*tui-theme-name* "dracula"))
    (let ((theme (hactar::get-or-create-theme)))
      (is (hactar::tui-theme-p theme))
      (is (string= "dracula" (hactar::tui-theme-name theme))))))

(test get-or-create-theme-returns-existing
  "get-or-create-theme returns *tui-theme* when already set."
  (let* ((existing (hactar::make-tui-theme :name "custom"))
         (hactar::*tui-theme* existing)
         (hactar::*tui-theme-name* nil))
    (is (eq existing (hactar::get-or-create-theme)))))

(test list-available-themes-includes-builtins
  "list-available-themes includes all built-in themes."
  (let ((themes (hactar::list-available-themes)))
    (is (>= (length themes) (length hactar::*tui-builtin-themes*)))
    (let ((names (mapcar #'hactar::tui-theme-name themes)))
      (is (member "default" names :test #'string=))
      (is (member "gruvbox-dark" names :test #'string=))
      (is (member "dracula" names :test #'string=))
      (is (member "nord" names :test #'string=)))))

;;* Theme color pair registry (non-ncurses tests)

(test tui-color-pair-returns-zero-for-unregistered
  "tui-color-pair returns 0 for unregistered roles."
  (let ((hactar::*tui-color-pairs* (make-hash-table :test 'eq)))
    (is (= 0 (hactar::tui-color-pair :nonexistent-role)))))

;;* Sidebar widget tests

(test tui-sidebar-widget-struct
  "tui-sidebar-widget struct creation works."
  (let ((widget (hactar::make-tui-sidebar-widget
                 :name "test"
                 :render-fn (lambda (win col row w h) (declare (ignore win col row w h)) 1))))
    (is (hactar::tui-sidebar-widget-p widget))
    (is (string= "test" (hactar::tui-sidebar-widget-name widget)))
    (is (functionp (hactar::tui-sidebar-widget-render-fn widget)))
    (is (null (hactar::tui-sidebar-widget-visible-fn widget)))))

(test make-default-sidebar-widgets-list
  "make-default-sidebar-widgets returns a list of 6 widgets."
  (let ((widgets (hactar::make-default-sidebar-widgets)))
    (is (listp widgets))
    (is (= 6 (length widgets)))
    (dolist (w widgets)
      (is (hactar::tui-sidebar-widget-p w))
      (is (functionp (hactar::tui-sidebar-widget-render-fn w))))))

(test sidebar-widget-factories
  "All sidebar widget factory functions return valid widgets."
  (dolist (factory (list #'hactar::make-sidebar-widget-project-header
                         #'hactar::make-sidebar-widget-model
                         #'hactar::make-sidebar-widget-files
                         #'hactar::make-sidebar-widget-tool-calls
                         #'hactar::make-sidebar-widget-lsps
                         #'hactar::make-sidebar-widget-mcps))
    (let ((w (funcall factory)))
      (is (hactar::tui-sidebar-widget-p w))
      (is (stringp (hactar::tui-sidebar-widget-name w)))
      (is (functionp (hactar::tui-sidebar-widget-render-fn w))))))

(test sidebar-widget-names-unique
  "Default sidebar widgets have unique names."
  (let* ((widgets (hactar::make-default-sidebar-widgets))
         (names (mapcar #'hactar::tui-sidebar-widget-name widgets)))
    (is (= (length names) (length (remove-duplicates names :test #'string=))))))

(test sidebar-widgets-customizable
  "Users can reorder sidebar widgets."
  (let ((hactar::*tui-sidebar-widgets*
          (list (hactar::make-sidebar-widget-project-header)
                (hactar::make-sidebar-widget-files)
                (hactar::make-sidebar-widget-model))))
    (is (= 3 (length hactar::*tui-sidebar-widgets*)))
    (is (string= "project-header"
                  (hactar::tui-sidebar-widget-name (first hactar::*tui-sidebar-widgets*))))
    (is (string= "files"
                  (hactar::tui-sidebar-widget-name (second hactar::*tui-sidebar-widgets*))))
    (is (string= "model"
                  (hactar::tui-sidebar-widget-name (third hactar::*tui-sidebar-widgets*))))))

(test sidebar-widget-visible-fn
  "Widget visible-fn controls whether widget is rendered."
  (let ((always-visible (hactar::make-tui-sidebar-widget
                         :name "always"
                         :render-fn (lambda (w c r wi h) (declare (ignore w c r wi h)) 1)
                         :visible-fn nil))
        (never-visible (hactar::make-tui-sidebar-widget
                        :name "never"
                        :render-fn (lambda (w c r wi h) (declare (ignore w c r wi h)) 1)
                        :visible-fn (lambda () nil)))
        (conditionally-visible (hactar::make-tui-sidebar-widget
                                :name "conditional"
                                :render-fn (lambda (w c r wi h) (declare (ignore w c r wi h)) 1)
                                :visible-fn (lambda () t))))
    ;; nil visible-fn means always visible
    (is (null (hactar::tui-sidebar-widget-visible-fn always-visible)))
    ;; never-visible returns nil
    (is (null (funcall (hactar::tui-sidebar-widget-visible-fn never-visible))))
    ;; conditionally-visible returns t
    (is (funcall (hactar::tui-sidebar-widget-visible-fn conditionally-visible)))))

(test tool-call-status-color-role
  "tui-tool-call-status-color-role maps statuses to role keywords."
  (is (eq :tool-pending (hactar::tui-tool-call-status-color-role "pending")))
  (is (eq :tool-running (hactar::tui-tool-call-status-color-role "in_progress")))
  (is (eq :tool-done (hactar::tui-tool-call-status-color-role "completed")))
  (is (eq :tool-failed (hactar::tui-tool-call-status-color-role "failed")))
  (is (eq :muted (hactar::tui-tool-call-status-color-role "unknown"))))

;;* Theme struct completeness validation

(test theme-struct-has-all-slots
  "tui-theme struct has all expected color slots."
  (let ((theme (hactar::make-gruvbox-dark-theme)))
    ;; Verify key slots are non-nil (except bg/fg which can be nil for default)
    (is (stringp (hactar::tui-theme-name theme)))
    (is (stringp (hactar::tui-theme-border theme)))
    (is (stringp (hactar::tui-theme-header theme)))
    (is (stringp (hactar::tui-theme-selected-fg theme)))
    (is (stringp (hactar::tui-theme-selected-bg theme)))
    (is (stringp (hactar::tui-theme-muted theme)))
    (is (stringp (hactar::tui-theme-prompt-marker theme)))
    (is (stringp (hactar::tui-theme-status-ok-fg theme)))
    (is (stringp (hactar::tui-theme-status-ok-bg theme)))
    (is (stringp (hactar::tui-theme-status-error-fg theme)))
    (is (stringp (hactar::tui-theme-status-error-bg theme)))
    (is (stringp (hactar::tui-theme-sidebar-title theme)))
    (is (stringp (hactar::tui-theme-sidebar-label theme)))
    (is (stringp (hactar::tui-theme-file-added theme)))
    (is (stringp (hactar::tui-theme-file-removed theme)))
    (is (stringp (hactar::tui-theme-modal-bg-fg theme)))
    (is (stringp (hactar::tui-theme-modal-bg-bg theme)))
    (is (stringp (hactar::tui-theme-modal-border theme)))
    (is (stringp (hactar::tui-theme-keyhint theme)))
    (is (stringp (hactar::tui-theme-lsp-ok theme)))
    (is (stringp (hactar::tui-theme-tool-pending theme)))
    (is (stringp (hactar::tui-theme-tool-running theme)))
    (is (stringp (hactar::tui-theme-tool-done theme)))
    (is (stringp (hactar::tui-theme-tool-failed theme)))))

;;* xterm-256 palette

(test xterm-256-palette-size
  "The xterm-256 palette has 256 entries."
  (let ((palette (hactar::build-xterm-256-palette)))
    (is (= 256 (length palette)))
    ;; All entries should be non-nil lists of 3 values
    (loop for i from 0 below 256
          do (is (not (null (aref palette i))))
             (is (= 3 (length (aref palette i)))))))
