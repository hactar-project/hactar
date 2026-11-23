(in-package #:hactar-tui)

(defvar *input* *standard-input*)
(defvar *output* *standard-output*)
(defvar *error-output* cl:*error-output*)
(defvar *use-readline* nil
  "When non-nil and *INPUT* is *STANDARD-INPUT*, READ-INPUT uses cl-readline.")

(defmacro with-tui ((&key input output error-output theme) &body body)
  `(let ((*input* (or ,input *input*))
         (*output* (or ,output *output*))
         (*error-output* (or ,error-output *error-output*))
         (*theme* (or ,theme *theme*)))
     ,@body))

(defun read-input (&key prompt)
  "Read one input line. The owning program decides how to dispatch it."
  (let ((prompt-str (or prompt (theme-prompt *theme*))))
    (if (and *use-readline* (eq *input* *standard-input*))
        (rl:readline :prompt prompt-str :add-history t)
        (progn
          (write-string prompt-str *output*)
          (force-output *output*)
          (read-line *input* nil nil)))))

(defun render (value &key (format :text))
  "Render plain data without dispatching or inspecting engine state."
  (let ((text (typecase value
                (string value)
                (condition (format nil "~A~A"
                                   (theme-error-prefix *theme*) value))
                (t (princ-to-string value)))))
    (write-string (ecase format
                    (:text text)
                    (:markdown (hactar-md:render-terminal text))
                    (:ansi (render-markdown-ansi text :theme *theme*)))
                  *output*)
    (force-output *output*)
    value))

(defun pick (query choices &key (limit 10))
  "Return fuzzy-ranked CHOICES; interaction belongs to a program loop."
  (hactar-fzf:select query choices :limit limit))

(defun pick-interactive (choices &key (prompt "Select> ") (filter-prompt "Filter> ")
                                      (limit 10))
  "Stream-based interactive fuzzy picker over *INPUT* and *OUTPUT*.
Reads an optional filter query, displays numbered matches, and returns the
selected choice string or NIL."
  (let* ((query (or (read-input :prompt filter-prompt) ""))
         (matches (pick query choices :limit limit)))
    (when (null matches)
      (return-from pick-interactive nil))
    (when (= 1 (length matches))
      (return-from pick-interactive (first matches)))
    (loop for choice in matches
          for i from 1
          do (format *output* "~D) ~A~%" i choice))
    (force-output *output*)
    (let* ((raw (read-input :prompt prompt))
           (idx (and raw (ignore-errors (parse-integer (string-trim '(#\Space #\Tab) raw))))))
      (when (and idx (<= 1 idx (length matches)))
        (nth (1- idx) matches)))))

(defun spinner (label thunk)
  "Render LABEL, call THUNK, and return all of its values."
  (format *output* "~A... " label)
  (force-output *output*)
  (multiple-value-prog1 (funcall thunk)
    (format *output* "done~%")
    (force-output *output*)))

(defstruct hydra name prompt bindings)
(defvar *hydras* (make-hash-table :test 'equal))

(defun define-hydra (name prompt bindings)
  "Register a named key map. BINDINGS is an alist of key to function."
  (setf (gethash (string-downcase (string name)) *hydras*)
        (make-hydra :name name :prompt prompt :bindings bindings)))

(defun find-hydra (name)
  (gethash (string-downcase (string name)) *hydras*))

(defun run-hydra (hydra &key key)
  "Run one selected HYDRA binding. Programs own any repeated input loop."
  (let* ((map (if (hydra-p hydra) hydra (find-hydra hydra)))
         (binding (and map (assoc key (hydra-bindings map) :test #'equal))))
    (unless map (error "Unknown hydra: ~A" hydra))
    (unless binding (error "Unknown hydra key: ~A" key))
    (funcall (cdr binding))))

(defun repl-loop (handler &key prompt (quit-commands '("quit" "exit" "/quit" "/exit")))
  "Read lines via READ-INPUT and pass each non-empty line to HANDLER.
HANDLER returns text or printable data to RENDER. Stops on EOF or QUIT-COMMANDS."
  (loop for raw = (read-input :prompt prompt)
        while raw
        for line = (string-trim '(#\Space #\Tab #\Return #\Newline) raw)
        until (member line quit-commands :test #'string-equal)
        unless (string= line "")
          do (let ((out (funcall handler line)))
               (when out
                 (render out)
                 (terpri *output*)
                 (force-output *output*)))))
