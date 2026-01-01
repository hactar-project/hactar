;;;; Keymap System for Hactar Editor
;;;; Inspired by Emacs keymap system with support for key sequences.

(defpackage :hactar-editor/keymap
  (:use :cl)
  (:export #:keymap
           #:make-keymap
           #:keymap-name
           #:keymap-parent
           #:define-key
           #:lookup-key
           #:key-binding
           ;; Key parsing
           #:parse-key
           #:parse-key-sequence
           #:key-to-string
           ;; Predefined keymaps
           #:*global-keymap*
           #:*ai-keymap*
           #:*minibuffer-keymap*
           ;; Key event
           #:key-event
           #:key-event-char
           #:key-event-modifiers
           #:make-key-event))

(in-package :hactar-editor/keymap)

;;; Key Events
(defstruct key-event
  "Represents a single key press with modifiers."
  char          ; The character or special key symbol
  (modifiers nil :type list))  ; List of :control, :meta, :shift, :super

(defun modifier-p (mod)
  "Check if MOD is a valid modifier."
  (member mod '(:control :meta :shift :super)))

(defun key-event-equal (a b)
  "Test if two key events are equal."
  (and (equal (key-event-char a) (key-event-char b))
       (null (set-exclusive-or (key-event-modifiers a)
                               (key-event-modifiers b)))))

(defun parse-key (key-string)
  "Parse a key string like 'C-x', 'M-f', 'C-M-s' into a key-event."
  (let ((modifiers nil)
        (remaining key-string))
    ;; Parse modifiers
    (loop
      (cond
        ((and (>= (length remaining) 2)
              (char= (char remaining 1) #\-))
         (let ((mod-char (char remaining 0)))
           (case mod-char
             ((#\C #\c) (push :control modifiers))
             ((#\M #\m) (push :meta modifiers))
             ((#\S #\s) (push :shift modifiers))
             ((#\H #\h) (push :super modifiers))
             (t (return))))
         (setf remaining (subseq remaining 2)))
        (t (return))))
    ;; Parse the key itself
    (let ((key-char
            (cond
              ((string= remaining "RET") #\Return)
              ((string= remaining "TAB") #\Tab)
              ((string= remaining "SPC") #\Space)
              ((string= remaining "ESC") #\Escape)
              ((string= remaining "DEL") #\Backspace)
              ((string= remaining "UP") :up)
              ((string= remaining "DOWN") :down)
              ((string= remaining "LEFT") :left)
              ((string= remaining "RIGHT") :right)
              ((string= remaining "HOME") :home)
              ((string= remaining "END") :end)
              ((string= remaining "PGUP") :page-up)
              ((string= remaining "PGDN") :page-down)
              ((= (length remaining) 1) (char remaining 0))
              (t (intern (string-upcase remaining) :keyword)))))
      (make-key-event :char key-char :modifiers (nreverse modifiers)))))

(defun parse-key-sequence (key-string)
  "Parse a key sequence string like 'C-x C-f' into a list of key-events."
  (mapcar #'parse-key (uiop:split-string key-string :separator " ")))

(defun key-to-string (key-event)
  "Convert a key-event back to a string representation."
  (with-output-to-string (s)
    (dolist (mod (key-event-modifiers key-event))
      (format s "~A-" (case mod
                        (:control "C")
                        (:meta "M")
                        (:shift "S")
                        (:super "H"))))
    (let ((char (key-event-char key-event)))
      (format s "~A"
              (case char
                (#\Return "RET")
                (#\Tab "TAB")
                (#\Space "SPC")
                (#\Escape "ESC")
                (#\Backspace "DEL")
                (:up "UP")
                (:down "DOWN")
                (:left "LEFT")
                (:right "RIGHT")
                (:home "HOME")
                (:end "END")
                (:page-up "PGUP")
                (:page-down "PGDN")
                (t (if (characterp char)
                       (string char)
                       (symbol-name char))))))))

(defun key-sequence-to-string (key-events)
  "Convert a list of key-events to a key sequence string."
  (format nil "~{~A~^ ~}" (mapcar #'key-to-string key-events)))

;;; Keymap
(defclass keymap ()
  ((name :accessor keymap-name 
         :initarg :name
         :type string
         :documentation "Name of this keymap for debugging")
   (parent :accessor keymap-parent
           :initarg :parent
           :initform nil
           :documentation "Parent keymap to inherit from")
   (bindings :accessor keymap-bindings
             :initform (make-hash-table :test 'equal)
             :documentation "Hash table mapping key sequences to commands"))
  (:documentation "A keymap maps key sequences to commands."))

(defun make-keymap (&key name parent)
  "Create a new keymap."
  (make-instance 'keymap :name name :parent parent))

(defun key-hash (key-event)
  "Create a hashable representation of a key-event."
  (cons (key-event-char key-event)
        (sort (copy-list (key-event-modifiers key-event)) #'string<)))

(defun sequence-hash (key-events)
  "Create a hashable representation of a key sequence."
  (mapcar #'key-hash key-events))

(defun define-key (keymap key-sequence command)
  "Bind KEY-SEQUENCE to COMMAND in KEYMAP.
   KEY-SEQUENCE can be a string like 'C-x C-f' or a list of key-events."
  (let* ((events (if (stringp key-sequence)
                     (parse-key-sequence key-sequence)
                     key-sequence))
         (hash (sequence-hash events)))
    (setf (gethash hash (keymap-bindings keymap)) command)))

(defun lookup-key (keymap key-sequence &key (inherit t))
  "Look up KEY-SEQUENCE in KEYMAP.
   Returns the command or NIL if not found.
   If INHERIT is true, also check parent keymaps."
  (let* ((events (if (stringp key-sequence)
                     (parse-key-sequence key-sequence)
                     key-sequence))
         (hash (sequence-hash events))
         (binding (gethash hash (keymap-bindings keymap))))
    (or binding
        (and inherit
             (keymap-parent keymap)
             (lookup-key (keymap-parent keymap) key-sequence :inherit t)))))

(defun key-binding (key-sequence &rest keymaps)
  "Look up KEY-SEQUENCE in KEYMAPS (checked in order).
   Returns the first binding found or NIL."
  (dolist (keymap keymaps)
    (when keymap
      (let ((binding (lookup-key keymap key-sequence)))
        (when binding
          (return-from key-binding binding)))))
  nil)

;;; Prefix Keys
(defun prefix-key-p (keymap key-sequence)
  "Check if KEY-SEQUENCE is a prefix of any binding in KEYMAP."
  (let* ((events (if (stringp key-sequence)
                     (parse-key-sequence key-sequence)
                     key-sequence))
         (prefix-hash (sequence-hash events))
         (prefix-len (length prefix-hash)))
    (block found
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (when (and (> (length key) prefix-len)
                            (equal (subseq key 0 prefix-len) prefix-hash))
                   (return-from found t)))
               (keymap-bindings keymap))
      nil)))

;;; Predefined Keymaps
(defvar *global-keymap* (make-keymap :name "global")
  "The global keymap, used for all buffers.")

(defvar *ai-keymap* (make-keymap :name "ai" :parent *global-keymap*)
  "Keymap for AI-related commands.")

(defvar *minibuffer-keymap* (make-keymap :name "minibuffer" :parent *global-keymap*)
  "Keymap for the minibuffer.")

;;; Setup default global bindings
(defun setup-default-bindings ()
  "Set up default key bindings."
  ;; Movement
  (define-key *global-keymap* "C-f" 'forward-char)
  (define-key *global-keymap* "C-b" 'backward-char)
  (define-key *global-keymap* "C-n" 'next-line)
  (define-key *global-keymap* "C-p" 'previous-line)
  (define-key *global-keymap* "C-a" 'beginning-of-line)
  (define-key *global-keymap* "C-e" 'end-of-line)
  (define-key *global-keymap* "M-f" 'forward-word)
  (define-key *global-keymap* "M-b" 'backward-word)
  (define-key *global-keymap* "C-v" 'scroll-down)
  (define-key *global-keymap* "M-v" 'scroll-up)
  (define-key *global-keymap* "M-<" 'beginning-of-buffer)
  (define-key *global-keymap* "M->" 'end-of-buffer)
  
  ;; Editing
  (define-key *global-keymap* "C-d" 'delete-char)
  (define-key *global-keymap* "DEL" 'delete-backward-char)
  (define-key *global-keymap* "C-k" 'kill-line)
  (define-key *global-keymap* "C-w" 'kill-region)
  (define-key *global-keymap* "M-w" 'copy-region)
  (define-key *global-keymap* "C-y" 'yank)
  (define-key *global-keymap* "M-y" 'yank-pop)
  (define-key *global-keymap* "C-/" 'undo)
  (define-key *global-keymap* "C-_" 'undo)
  
  ;; Mark
  (define-key *global-keymap* "C-SPC" 'set-mark)
  (define-key *global-keymap* "C-x C-x" 'exchange-point-and-mark)
  
  ;; Files
  (define-key *global-keymap* "C-x C-f" 'find-file)
  (define-key *global-keymap* "C-x C-s" 'save-buffer)
  (define-key *global-keymap* "C-x C-w" 'write-file)
  (define-key *global-keymap* "C-x C-c" 'save-buffers-kill-editor)
  
  ;; Buffers
  (define-key *global-keymap* "C-x b" 'switch-to-buffer)
  (define-key *global-keymap* "C-x k" 'kill-buffer)
  (define-key *global-keymap* "C-x C-b" 'list-buffers)
  
  ;; Windows
  (define-key *global-keymap* "C-x 2" 'split-window-below)
  (define-key *global-keymap* "C-x 3" 'split-window-right)
  (define-key *global-keymap* "C-x 0" 'delete-window)
  (define-key *global-keymap* "C-x 1" 'delete-other-windows)
  (define-key *global-keymap* "C-x o" 'other-window)
  
  ;; Search
  (define-key *global-keymap* "C-s" 'isearch-forward)
  (define-key *global-keymap* "C-r" 'isearch-backward)
  (define-key *global-keymap* "M-%" 'query-replace)
  
  ;; Help
  (define-key *global-keymap* "C-h k" 'describe-key)
  (define-key *global-keymap* "C-h f" 'describe-function)
  (define-key *global-keymap* "C-h v" 'describe-variable)
  (define-key *global-keymap* "C-h b" 'describe-bindings)
  
  ;; AI commands (C-c a prefix)
  (define-key *ai-keymap* "C-c a e" 'ai-edit)
  (define-key *ai-keymap* "C-c a c" 'ai-complete)
  (define-key *ai-keymap* "C-c a x" 'ai-explain)
  (define-key *ai-keymap* "C-c a r" 'ai-refactor)
  (define-key *ai-keymap* "C-c a f" 'ai-fix)
  (define-key *ai-keymap* "C-c a d" 'ai-document)
  (define-key *ai-keymap* "C-c a t" 'ai-test)
  
  ;; Hactar commands (C-c h prefix)
  (define-key *global-keymap* "C-c h a" 'hactar-add-file)
  (define-key *global-keymap* "C-c h d" 'hactar-drop-file)
  (define-key *global-keymap* "C-c h c" 'hactar-context)
  (define-key *global-keymap* "C-c h m" 'hactar-model)
  (define-key *global-keymap* "C-c h /" 'hactar-command)
  
  ;; Code-value commands (C-c v prefix)
  (define-key *global-keymap* "C-c v s" 'code-select)
  (define-key *global-keymap* "C-c v t" 'code-transform)
  (define-key *global-keymap* "C-c v a" 'code-apply-staged)
  (define-key *global-keymap* "C-c v u" 'code-undo)
  (define-key *global-keymap* "C-c v h" 'code-history)
  
  ;; Minibuffer
  (define-key *minibuffer-keymap* "RET" 'minibuffer-exit)
  (define-key *minibuffer-keymap* "C-g" 'minibuffer-abort)
  (define-key *minibuffer-keymap* "TAB" 'minibuffer-complete))

;; Initialize default bindings
(setup-default-bindings)
