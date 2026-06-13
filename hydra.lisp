;; hydras inspired by the Emacs
(in-package :hactar)

(defvar *hydras* (make-hash-table :test 'equal)
  "Registry of defined hydras.")

(defvar *hydra-input* nil
  "An optional input stream for hydra, primarily used in tests to mock terminal input.")

(defvar *hydra-output* nil
  "An optional output stream for hydra, primarily used in tests to mock terminal output.")

(defstruct hydra
  name
  options
  docstring
  heads)

(defstruct hydra-head
  key
  cmd
  hint
  exit)

(defun get-head-exit (head default-exit)
  (let ((head-exit (hydra-head-exit head)))
    (if (eq head-exit :default)
        default-exit
        head-exit)))

(defun print-hydra-menu (hydra stream)
  (let* ((options (hydra-options hydra))
         (title (or (getf options :title)
                    (string-capitalize (string-downcase (symbol-name (hydra-name hydra))))))
         (docstring (hydra-docstring hydra))
         (heads (hydra-heads hydra))
         (border (make-string 60 :initial-element #\-)))
    (format stream "~%~A~%" (colorize border :dim))
    (format stream "  ~A~%" (colorize title :bold-blue))
    (when docstring
      (format stream "  ~A~%" (colorize docstring :dim)))
    (format stream "~A~%" (colorize border :dim))
    (dolist (head heads)
      (let ((key (hydra-head-key head))
            (hint (hydra-head-hint head)))
        (format stream "  [~A] ~A~%"
                (colorize (string key) :bold-cyan)
                hint)))
    ;; Always add standard quit option if not overridden
    (unless (find "q" heads :key #'hydra-head-key :test #'string-equal)
      (format stream "  [~A] quit~%" (colorize "q" :bold-red)))
    (format stream "~A~%" (colorize border :dim))
    (format stream "Enter key: ")
    (force-output stream)))

(defun run-subcommand (cmd)
  "Run a subcommand. It can be a string, symbol, or function/lambda/form."
  (cond
   ((functionp cmd)
     (funcall cmd))
   ((and (symbolp cmd) (gethash (string-downcase (symbol-name cmd)) *hydras*))
     (run-hydra (gethash (string-downcase (symbol-name cmd)) *hydras*)))
   ((and (symbolp cmd) (gethash (format nil "/~A" (string-downcase (symbol-name cmd))) *commands*))
     (execute-command (format nil "/~A" (string-downcase (symbol-name cmd))) nil))
   ((and (symbolp cmd) (fboundp cmd))
     (funcall cmd))
   ((stringp cmd)
     (let ((normalized (if (str:starts-with? "/" cmd) cmd (format nil "/~A" cmd))))
       (let ((hydra-name (string-downcase (string-trim '(#\/) normalized))))
         (if (gethash hydra-name *hydras*)
             (run-hydra (gethash hydra-name *hydras*))
             (execute-command normalized nil)))))
    (t
     (error "Unknown subcommand type: ~A" cmd))))

(defun cmd-is-hydra-p (cmd)
  "Return T if CMD refers to a registered hydra."
  (cond
    ((null cmd) nil)
    ((and (symbolp cmd) (gethash (string-downcase (symbol-name cmd)) *hydras*))
     t)
    ((stringp cmd)
     (let* ((normalized (if (str:starts-with? "/" cmd) cmd (format nil "/~A" cmd)))
            (hydra-name (string-downcase (string-trim '(#\/) normalized))))
       (if (gethash hydra-name *hydras*) t nil)))
    (t nil)))

(defun run-subcommand-tui (cmd)
  "Execute a hydra subcommand in the TUI context, suspending curses if it is not a nested hydra."
  (if (cmd-is-hydra-p cmd)
      (run-subcommand cmd)
      (let ((output-string nil))
        ;; Suspend curses so subprocesses can drive the terminal.
        (charms/ll:def-prog-mode)
        (charms/ll:endwin)
        (unwind-protect
             (setf output-string
                   (with-output-to-string (*standard-output*)
                     (run-subcommand cmd)))
          ;; Resume curses and force a full repaint.
          (charms/ll:reset-prog-mode)
          (charms:clear-window charms:*standard-window* :force-repaint t)
          (charms:refresh-window charms:*standard-window*))
        (when (and output-string (> (length output-string) 0))
          (dolist (line (str:lines output-string))
            (tui-add-chat-line line :type :text))))))

(defun run-hydra-tui (hydra)
  "Run the interactive loop for a hydra in TUI mode using charms popup."
  (let* ((options (hydra-options hydra))
         (default-exit (getf options :exit nil))
         (title (or (getf options :title)
                    (string-capitalize (string-downcase (symbol-name (hydra-name hydra))))))
         (docstring (hydra-docstring hydra))
         (heads (hydra-heads hydra))
         (has-quit-override (find "q" heads :key #'hydra-head-key :test #'string-equal))
         (win charms:*standard-window*)
         (exit-requested nil))
    (loop until exit-requested do
      ;; 1. Draw the standard TUI background first
      (tui-render win)

      ;; 2. Render the popup dialog
      (multiple-value-bind (win-width win-height)
          (charms:window-dimensions win)
        (let* ((win-width-avail (- win-width 4))
               (box-width (min 60 win-width-avail))
               (inner-width (- box-width 4))
               (title-str (format nil " ~A " title))
               (docstring-lines (if docstring (wrap-text docstring inner-width) nil))
               (doc-len (if docstring-lines (+ (length docstring-lines) 1) 0))
               (heads-count (length heads))
               (show-quit (not has-quit-override))
               (quit-height (if show-quit 1 0))
               (box-height (min (- win-height 2)
                                (+ 4 doc-len heads-count quit-height)))
               (box-col (floor (- win-width box-width) 2))
               (box-row (floor (- win-height box-height) 2)))

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
          (tui-safe-write win title-str (+ box-col 2) box-row (- box-width 4))
          (charms/ll:attroff charms/ll:a_bold)
          (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-border)))

          ;; Draw Docstring if present
          (let ((current-row (+ box-row 2)))
            (when docstring-lines
              (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-bg)))
              (dolist (line docstring-lines)
                (tui-safe-write win line (+ box-col 2) current-row inner-width)
                (incf current-row))
              (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-bg)))
              ;; Draw a line separator after docstring
              (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-border)))
              (tui-hline win (1+ box-col) current-row (- box-width 2) #\─)
              (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-border)))
              (incf current-row))

            ;; Draw Heads
            (charms/ll:attron (charms/ll:color-pair (tui-color-pair :modal-bg)))
            (dolist (head heads)
              (let ((key (hydra-head-key head))
                    (hint (hydra-head-hint head)))
                (tui-safe-write win "  [" (+ box-col 2) current-row inner-width)
                (charms/ll:attron charms/ll:a_bold)
                (tui-safe-write win key (+ box-col 5) current-row (length key))
                (charms/ll:attroff charms/ll:a_bold)
                (tui-safe-write win (format nil "] ~A" hint) (+ box-col 5 (length key)) current-row inner-width)
                (incf current-row)))

            ;; Draw Quit option if applicable
            (when show-quit
              (tui-safe-write win "  [" (+ box-col 2) current-row inner-width)
              (charms/ll:attron charms/ll:a_bold)
              (tui-safe-write win "q" (+ box-col 5) current-row 1)
              (charms/ll:attroff charms/ll:a_bold)
              (tui-safe-write win "] quit" (+ box-col 6) current-row inner-width)
              (incf current-row))
            (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :modal-bg))))))

      ;; 3. Refresh window
      (charms:refresh-window win)

      ;; 4. Get input char
      (let ((c (charms:get-char win :ignore-error t)))
        (when c
          (cond
            ;; Escape key or 'q' / 'Q' (unless 'q' is overridden as a head key)
            ((or (eql (char-code c) 27)
                 (and (not has-quit-override)
                      (or (char= c #\q) (char= c #\Q))))
             (setf exit-requested t))
            (t
             ;; Look for matching head key
             (let ((head (find (string c) heads
                               :key #'hydra-head-key
                               :test #'string-equal)))
               (when head
                 (handler-case
                     (run-subcommand-tui (hydra-head-cmd head))
                   (error (e)
                     (tui-add-chat-line (format nil "Error executing command: ~A" e) :type :error)
                     (tui-set-status (format nil "Error: ~A" e) :error)))
                 (let ((exit-p (get-head-exit head default-exit)))
                   (when exit-p
                     (setf exit-requested t))))))))))))

(defun run-hydra (hydra)
  "Run the interactive loop for a hydra."
  (if (and (boundp '*tui-running*)
           (symbol-value '*tui-running*)
           (null *hydra-input*))
      (run-hydra-tui hydra)
      (let* ((options (hydra-options hydra))
             (default-exit (getf options :exit nil))
             (stream (if (and (boundp '*tui-running*) (symbol-value '*tui-running*))
                         *terminal-io*
                         *standard-output*))
             (in-stream (if (and (boundp '*tui-running*) (symbol-value '*tui-running*))
                            *terminal-io*
                            *standard-input*)))
        (let ((out (or *hydra-output* stream))
              (in (or *hydra-input* in-stream))
              (exit-requested nil))
          (loop until exit-requested do
            (print-hydra-menu hydra out)
            (let ((input (read-line in nil nil)))
              (if (null input)
                  (setf exit-requested t)
                  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) input)))
                    (cond
                      ((string= trimmed "")
                       nil)
                      ((string-equal trimmed "q")
                       (setf exit-requested t))
                      (t
                       (let ((head (find trimmed (hydra-heads hydra)
                                         :key #'hydra-head-key
                                         :test #'string-equal)))
                         (if head
                             (progn
                               (handler-case
                                   (run-subcommand (hydra-head-cmd head))
                                 (error (e)
                                   (format out "~&Error executing command: ~A~%" e)
                                   (force-output out)))
                               (let ((exit-p (get-head-exit head default-exit)))
                                 (when exit-p
                                   (setf exit-requested t))))
                             (progn
                               (format out "Invalid key: ~A. Press 'q' to quit.~%" trimmed)
                               (force-output out))))))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-hydra-head-spec (head-spec)
    (let ((key (first head-spec))
          (cmd (second head-spec))
          (hint (third head-spec))
          (exit-val :default))
      (let ((plist (cdddr head-spec)))
        (when plist
          (if (keywordp (first plist))
              (setf exit-val (getf plist :exit :default))
              (setf exit-val (first plist)))))
      (let ((cmd-form (if (and (consp cmd) (not (eq (car cmd) 'lambda)))
                          `(lambda () ,cmd)
                          cmd)))
        `(make-hydra-head
          :key ,(cond
                  ((characterp key) (string key))
                  ((symbolp key) (string-downcase (symbol-name key)))
                  ((stringp key) key)
                  (t (error "Invalid key in hydra head: ~A" key)))
          :cmd ,(if (or (functionp cmd-form) (and (consp cmd-form) (eq (car cmd-form) 'lambda)))
                    cmd-form
                    `',cmd-form)
          :hint ,(or hint (and (symbolp cmd) (string-downcase (symbol-name cmd))) "")
          :exit ',exit-val)))))

(defmacro defhydra (name options &body body)
  "Define a transient/hydra menu command and subcommands.
   Syntax:
   (defhydra name (options)
     \"optional-docstring\"
     (key command hint &key exit)
     ...)"
  (let* ((docstring (when (stringp (first body)) (first body)))
         (heads-spec (if (stringp (first body)) (rest body) body))
         (hydra-name-str (string-downcase (symbol-name name))))
    `(progn
       (setf (gethash ,hydra-name-str *hydras*)
             (make-hydra
              :name ',name
              :options ',options
              :docstring ,docstring
              :heads (list ,@(mapcar #'%parse-hydra-head-spec heads-spec))))
       (define-command ,name (args)
         ,(or docstring "Transient/Hydra menu")
         (declare (ignore args))
         (run-hydra (gethash ,hydra-name-str *hydras*))
         :slash t :sub t))))
