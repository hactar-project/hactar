;;;; Main Entry Point for Hactar Editor
;;;; Ties together all editor components.

(defpackage :hactar-editor
  (:use :cl
        :hactar-editor/gap-buffer
        :hactar-editor/buffer
        :hactar-editor/keymap)
  (:export #:start-editor
           #:*editor-running*
           ;; Re-export commonly used symbols
           #:current-buffer
           #:find-file
           #:save-buffer
           #:define-key
           #:*global-keymap*))

(in-package :hactar-editor)

;;; Editor State
(defvar *editor-running* nil
  "Whether the editor is currently running.")

(defvar *kill-ring* nil
  "The kill ring (clipboard history).")

(defvar *kill-ring-max* 60
  "Maximum number of entries in the kill ring.")

(defvar *last-command* nil
  "The last command executed.")

(defvar *this-command* nil
  "The command currently being executed.")

(defvar *prefix-arg* nil
  "The prefix argument for the current command.")

(defvar *key-sequence* nil
  "The current key sequence being built.")

;;; Command Registration
(defvar *command-table* (make-hash-table :test 'eq)
  "Table mapping command symbols to their functions.")

(defmacro defcommand (name args docstring &body body)
  "Define an editor command.
   Commands are functions that can be bound to keys and called interactively."
  `(progn
     (defun ,name ,args
       ,docstring
       ,@body)
     (setf (gethash ',name *command-table*) #',name)
     ',name))

(defun command-function (command)
  "Get the function for COMMAND."
  (gethash command *command-table*))

(defun call-command (command &rest args)
  "Call COMMAND with ARGS."
  (let ((fn (command-function command)))
    (if fn
        (apply fn args)
        (message "Unknown command: ~A" command))))

;;; Message System
(defvar *messages* nil
  "List of recent messages.")

(defun message (format-string &rest args)
  "Display a message to the user."
  (let ((msg (apply #'format nil format-string args)))
    (push msg *messages*)
    (when (> (length *messages*) 100)
      (setf *messages* (subseq *messages* 0 100)))
    ;; For now, just print to stdout
    (format t "~&[Message] ~A~%" msg)
    msg))

;;; Basic Commands
(defcommand self-insert-command ()
  "Insert the character that invoked this command."
  (when (and *key-sequence* 
             (= (length *key-sequence*) 1))
    (let* ((key (first *key-sequence*))
           (char (key-event-char key)))
      (when (characterp char)
        (buffer-insert (current-buffer) char)))))

(defcommand forward-char (&optional (n 1))
  "Move forward N characters."
  (buffer-forward-char (current-buffer) n))

(defcommand backward-char (&optional (n 1))
  "Move backward N characters."
  (buffer-backward-char (current-buffer) n))

(defcommand next-line (&optional (n 1))
  "Move to the next line."
  (buffer-forward-line (current-buffer) n))

(defcommand previous-line (&optional (n 1))
  "Move to the previous line."
  (buffer-backward-line (current-buffer) n))

(defcommand beginning-of-line ()
  "Move to the beginning of the current line."
  (buffer-beginning-of-line (current-buffer)))

(defcommand end-of-line ()
  "Move to the end of the current line."
  (buffer-end-of-line (current-buffer)))

(defcommand delete-char (&optional (n 1))
  "Delete N characters forward."
  (let ((deleted (buffer-delete-forward (current-buffer) n)))
    (when deleted
      (message "Deleted: ~A" deleted))))

(defcommand delete-backward-char (&optional (n 1))
  "Delete N characters backward."
  (buffer-delete-backward (current-buffer) n))

(defcommand set-mark ()
  "Set the mark at point."
  (buffer-set-mark (current-buffer))
  (message "Mark set"))

(defcommand kill-region ()
  "Kill (cut) the region."
  (let ((text (buffer-kill-region (current-buffer))))
    (when text
      (push text *kill-ring*)
      (when (> (length *kill-ring*) *kill-ring-max*)
        (setf *kill-ring* (subseq *kill-ring* 0 *kill-ring-max*)))
      (message "Killed ~A characters" (length text)))))

(defcommand yank ()
  "Yank (paste) the most recent kill."
  (when *kill-ring*
    (buffer-insert-string (current-buffer) (first *kill-ring*))))

(defcommand undo ()
  "Undo the last change."
  (if (buffer-undo (current-buffer))
      (message "Undo!")
      (message "No more undo information")))

(defcommand find-file (filename)
  "Open a file in a new buffer."
  ;; In a real implementation, this would prompt for filename
  (let ((buffer (find-file-buffer filename)))
    (switch-to-buffer buffer)
    (message "Opened: ~A" filename)))

(defcommand save-buffer-command ()
  "Save the current buffer."
  (let ((path (save-buffer (current-buffer))))
    (message "Saved: ~A" path)))

(defcommand keyboard-quit ()
  "Cancel the current operation."
  (setf *key-sequence* nil)
  (setf *prefix-arg* nil)
  (message "Quit"))

;;; AI Commands (stubs for now)
(defcommand ai-edit (prompt)
  "Send PROMPT to LLM and apply resulting changes."
  (message "AI Edit: ~A" prompt))

(defcommand ai-complete ()
  "Get AI completion at point."
  (message "AI Complete - not yet implemented"))

(defcommand ai-explain ()
  "Explain selected code or symbol at point."
  (message "AI Explain - not yet implemented"))

(defcommand ai-refactor (instruction)
  "Refactor selected code according to INSTRUCTION."
  (message "AI Refactor: ~A" instruction))

;;; Hactar Integration Commands
(defcommand hactar-add-file ()
  "Add current buffer's file to Hactar context."
  (let ((path (buffer-file-path (current-buffer))))
    (if path
        (progn
          ;; Call into Hactar
          (when (find-package :hactar)
            (funcall (intern "ADD-FILE-TO-CONTEXT" :hactar) path))
          (message "Added to context: ~A" path))
        (message "Buffer has no file"))))

(defcommand hactar-drop-file ()
  "Drop current buffer's file from Hactar context."
  (let ((path (buffer-file-path (current-buffer))))
    (if path
        (progn
          (when (find-package :hactar)
            (funcall (intern "DROP-FILE-FROM-CONTEXT" :hactar) path))
          (message "Dropped from context: ~A" path))
        (message "Buffer has no file"))))

(defcommand hactar-context ()
  "Show current Hactar context."
  (if (find-package :hactar)
      (let ((context (funcall (intern "GENERATE-CONTEXT" :hactar))))
        (message "Context: ~A chars" (length context)))
      (message "Hactar not loaded")))

;;; Command Loop
(defun process-key-event (key-event)
  "Process a single key event."
  (push key-event *key-sequence*)
  (let ((binding (key-binding *key-sequence* *ai-keymap* *global-keymap*)))
    (cond
      ;; Found a binding
      (binding
       (setf *this-command* binding)
       (call-command binding)
       (setf *last-command* binding)
       (setf *key-sequence* nil))
      ;; Might be a prefix
      ((or (prefix-key-p *global-keymap* *key-sequence*)
           (prefix-key-p *ai-keymap* *key-sequence*))
       ;; Wait for more keys
       (message "~A-" (key-sequence-to-string *key-sequence*)))
      ;; Single printable character - self-insert
      ((and (= (length *key-sequence*) 1)
            (null (key-event-modifiers key-event))
            (characterp (key-event-char key-event))
            (graphic-char-p (key-event-char key-event)))
       (setf *this-command* 'self-insert-command)
       (call-command 'self-insert-command)
       (setf *last-command* 'self-insert-command)
       (setf *key-sequence* nil))
      ;; Unknown binding
      (t
       (message "~A is undefined" (key-sequence-to-string *key-sequence*))
       (setf *key-sequence* nil)))))

;;; Editor Entry Point
(defun start-editor (&optional filename)
  "Start the Hactar editor."
  (setf *editor-running* t)
  (message "Welcome to Hactar Editor")
  
  ;; Create initial buffer
  (if filename
      (find-file filename)
      (make-buffer "*scratch*"))
  
  ;; For now, just return - full terminal UI will be added later
  (message "Editor initialized. Buffer: ~A" (buffer-name (current-buffer)))
  
  ;; In a real implementation, this would enter the main event loop
  ;; For now, return the current buffer for testing
  (current-buffer))

(defun stop-editor ()
  "Stop the editor."
  (setf *editor-running* nil)
  (message "Goodbye!"))

;;; Provide a simple REPL-based interface for testing
(defun editor-repl ()
  "A simple REPL for testing the editor."
  (start-editor)
  (loop
    (format t "~&hactar-ed> ")
    (force-output)
    (let ((input (read-line *standard-input* nil)))
      (when (null input)
        (return))
      (cond
        ((string= input "quit") (return (stop-editor)))
        ((string= input "buffer") 
         (format t "~A~%" (buffer-to-string (current-buffer))))
        ((string= input "point")
         (format t "Point: ~A, Line: ~A, Col: ~A~%"
                 (buffer-point (current-buffer))
                 (buffer-line-number (current-buffer))
                 (buffer-column (current-buffer))))
        ((string= input "undo")
         (call-command 'undo))
        ((uiop:string-prefix-p "insert " input)
         (buffer-insert-string (current-buffer) 
                               (subseq input 7)))
        ((uiop:string-prefix-p "open " input)
         (find-file (subseq input 5)))
        ((string= input "save")
         (call-command 'save-buffer-command))
        (t
         (format t "Commands: quit, buffer, point, undo, insert <text>, open <file>, save~%"))))))
