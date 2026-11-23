(in-package #:hactar-rebuild)

;;; Bidirectional interfaces: editable views over Hactar state/context.
;;; Each interface has a :path, a :render function (state -> string) and a
;;; :parse function (string -> effect). definterface (surface.lisp) registers
;;; them; the helpers below drive read/write/sync and provide the three
;;; built-ins: files.dir, org-interface, and shell.

;;; ---------------------------------------------------------------------------
;;; .hactar/ context directory
;;; ---------------------------------------------------------------------------

(defun context-dir (&optional (root (or *repo-root* (uiop:getcwd))))
  (uiop:ensure-directory-pathname
   (merge-pathnames ".hactar/" (uiop:ensure-directory-pathname root))))

(defun interface-path (relative &optional (root (or *repo-root* (uiop:getcwd))))
  (merge-pathnames relative (context-dir root)))

;;; ---------------------------------------------------------------------------
;;; Generic interface drive (render to disk / parse from disk)
;;; ---------------------------------------------------------------------------

(defun resolve-fn (designator)
  "Resolve a function DESIGNATOR (symbol or function) to a callable, or NIL."
  (cond ((functionp designator) designator)
        ((and (symbolp designator) (fboundp designator))
         (symbol-function designator))
        (t nil)))

(defun render-interface (name)
  "Return the rendered string for interface NAME, or NIL if it has no renderer."
  (let* ((iface (gethash (key-string name) *interfaces*))
         (fn (and iface (resolve-fn (interface-def-render iface)))))
    (and fn (funcall fn))))

(defun parse-interface (name content)
  "Feed CONTENT to interface NAME's parser. Returns the parser's value."
  (let* ((iface (gethash (key-string name) *interfaces*))
         (fn (and iface (resolve-fn (interface-def-parse iface)))))
    (and fn (funcall fn content))))

(defun write-interface (name &optional (root (or *repo-root* (uiop:getcwd))))
  "Render interface NAME and write it to its :path under the repo."
  (let ((iface (gethash (key-string name) *interfaces*)))
    (unless iface (return-from write-interface (err (format nil "No interface: ~A" name))))
    (let ((content (render-interface name))
          (path (merge-pathnames (interface-def-path iface)
                                 (uiop:ensure-directory-pathname root))))
      (ensure-directories-exist path)
      (atomic-write path (or content ""))
      (ok (format nil "wrote ~A" (namestring path)) :data (list :path path)))))

(defun read-interface (name &optional (root (or *repo-root* (uiop:getcwd))))
  "Read interface NAME's file from disk and feed it to its parser."
  (let ((iface (gethash (key-string name) *interfaces*)))
    (unless iface (return-from read-interface (err (format nil "No interface: ~A" name))))
    (let ((path (merge-pathnames (interface-def-path iface)
                                 (uiop:ensure-directory-pathname root))))
      (if (probe-file path)
          (parse-interface name (uiop:read-file-string path))
          (err (format nil "Interface file missing: ~A" (namestring path)))))))

(defun sync-interfaces (&optional (root (or *repo-root* (uiop:getcwd))))
  "Render every registered interface that has a renderer out to disk.
This is the state -> interface-file synchronization direction."
  (let ((written '()))
    (maphash (lambda (k iface)
               (when (resolve-fn (interface-def-render iface))
                 (write-interface k root)
                 (push k written)))
             *interfaces*)
    (nreverse written)))

;;; ---------------------------------------------------------------------------
;;; files.dir — a directory-like context manifest
;;; ---------------------------------------------------------------------------

(defun context-add-file (path &optional annotation)
  "Add PATH to the context selection (idempotent, order-preserving)."
  (unless (assoc path *context-files* :test #'string=)
    (setf *context-files* (append *context-files* (list (cons path annotation)))))
  *context-files*)

(defun context-drop-file (path)
  "Remove PATH from the context selection."
  (setf *context-files*
        (remove path *context-files* :key #'car :test #'string=))
  *context-files*)

(defun render-context-files ()
  "Render the files.dir manifest: one 'path  # annotation' line per entry."
  (with-output-to-string (s)
    (format s "# Hactar context files. Edit freely; one path per line.~%")
    (format s "# Optional trailing '  # note' annotates an entry.~%")
    (dolist (entry *context-files*)
      (if (cdr entry)
          (format s "~A  # ~A~%" (car entry) (cdr entry))
          (format s "~A~%" (car entry))))))

(defun parse-context-files (content)
  "Parse a files.dir manifest back into *context-files*, preserving order."
  (let ((entries '()))
    (dolist (raw (str:split #\Newline content))
      (let ((line (str:trim raw)))
        (unless (or (string= line "") (str:starts-with-p "#" line))
          (let* ((hash (search "  #" line))
                 (path (str:trim (if hash (subseq line 0 hash) line)))
                 (note (and hash (str:trim (subseq line (+ hash 3))))))
            (unless (string= path "")
              (push (cons path (and note (plusp (length note)) note)) entries))))))
    (setf *context-files* (nreverse entries))))

(defun render-context-payload (&optional (root (or *repo-root* (uiop:getcwd))))
  "Concatenate the selected files' contents for agent context."
  (with-output-to-string (s)
    (dolist (entry *context-files*)
      (let ((p (merge-pathnames (car entry)
                                (uiop:ensure-directory-pathname root))))
        (format s "===== ~A =====~%" (car entry))
        (format s "~A~%~%" (if (probe-file p) (uiop:read-file-string p)
                               "[missing]"))))))

;;; ---------------------------------------------------------------------------
;;; org-interface — editable org view of context, project map, and harness
;;; ---------------------------------------------------------------------------

(defun render-context-org ()
  "Render context, project map, and the generated harness as an org document."
  (with-output-to-string (s)
    (format s "#+title: Hactar Context~%")
    (format s "#+startup: overview~%~%")
    (format s "* Project~%~%")
    (format s "- stack :: ~{~A~^ ~}~%" *stack*)
    (when *project-map*
      (format s "- scripts :: ~{~A~^ ~}~%" (getf *project-map* :scripts))
      (format s "- docs :: ~{~A~^ ~}~%" (getf *project-map* :docs)))
    (format s "~%* Context Files~%~%")
    (if *context-files*
        (dolist (e *context-files*)
          (format s "- ~A~@[ :: ~A~]~%" (car e) (cdr e)))
        (format s "- (none selected)~%"))
    (format s "~%* Update Status~%~%")
    (format s "- ~A~%" (if *update-status* (getf *update-status* :status) :none))
    (format s "~%* Generated Harness~%~%")
    (format s "Editable DSL forms. Changes inside the block are parsed back and~%")
    (format s "validated against the closed surface.~%~%")
    (format s "#+begin_src lisp~%")
    (let ((path (hactar-file-path *repo-root*)))
      (when (probe-file path)
        (dolist (form (current-generated-region path))
          (let ((*print-case* :downcase) (*package* (find-package :hactar-user)))
            (format s "~S~%" (rehome-tree form))))))
    (format s "#+end_src~%")))

(defun extract-org-src-blocks (content &optional (lang "lisp"))
  "Return the concatenated bodies of all #+begin_src LANG blocks.
Delegates to the hactar-org library."
  (hactar-org:extract-src-blocks content lang))

(defun parse-context-org (content)
  "Parse the org interface: extract lisp src blocks and validate them against
the closed DSL surface. Returns (values ok-p forms-or-errors)."
  (let* ((src (extract-org-src-blocks content "lisp"))
         (forms (with-input-from-string (s src)
                  (let ((*read-eval* nil) (*package* (find-package :hactar-rebuild)))
                    (loop for f = (read s nil :eof) until (eq f :eof) collect f)))))
    (multiple-value-bind (ok errs) (validate-forms forms)
      (if ok (values t forms) (values nil errs)))))

;;; ---------------------------------------------------------------------------
;;; shell — sandboxed command execution that feeds the self-update ledger
;;; ---------------------------------------------------------------------------

(defun shell-exec (cmd &key (sandbox "project"))
  "Run CMD through sandbox policy and record the activity on the update ledger.
This is the shell interface: all shell activity is observable by self-update."
  (let ((result (shell-cmd cmd :sandbox sandbox)))
    (push (list :kind :shell :cmd cmd
                :status (result-status result)
                :at (get-universal-time))
          *update-ledger*)
    result))

(defun install-builtin-interfaces ()
  "Register the engine-owned interfaces. Called at load and after every
reset-registries so built-ins survive .hactar.lisp reloads (user definterface
forms are layered on top afterward)."
  (definterface files.dir
    :path ".hactar/context.files"
    :format :text
    :render render-context-files
    :parse parse-context-files)
  (definterface org-interface
    :path ".hactar/context.org"
    :format :org
    :render render-context-org
    :parse parse-context-org)
  (definterface shell
    :path ".hactar/shell.log"
    :format :text
    :render nil
    :parse nil))

(install-builtin-interfaces)

(defun install-interface-defaults ()
  "Reinstall built-in interfaces and the state->interface sync hook."
  (install-builtin-interfaces)
  (when (gethash "turn.after" *hooks*)
    (add-handler 'turn.after
      (lambda (turn activity)
        (declare (ignore turn activity))
        (ignore-errors (sync-interfaces))))))

(register-post-reset 'install-interface-defaults)

