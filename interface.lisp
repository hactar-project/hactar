;;* Interface layer — two-way files mediating Hactar state
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(definterface
            instance-id instance-dir
            interface-abs-path
            materialize-interface materialize-all-interfaces
            sync-interface-out sync-interface-in
            with-interface-suppressed
            render-dir-list parse-dir-list
            render-org-aggregate parse-org-aggregate)))

(defstruct iface
  "A two-way file binding state to a file under the instance dir."
  (name nil :type (or null keyword))
  (path nil :type (or null string))
  (format :text :type keyword)
  (render nil :type (or null function))
  (parse nil :type (or null function))
  (on-change nil :type (or null function))
  (hooks '() :type list)
  (watch t :type boolean)
  (installed-handlers '() :type list)
  (title nil :type (or null string))
  (sections '() :type list)
  (self-hashes '() :type list)
  (last-hash nil)
  (abs-path nil))

(defstruct org-section
  "A headline-keyed sub-interface within an :org aggregate interface."
  (id nil :type (or null string))
  (title nil :type (or null string))
  (render nil :type (or null function))
  (parse nil :type (or null function)))

(defun %heading-body-string (heading)
  "Serialize HEADING's non-property children to a string."
  (with-output-to-string (s)
    (dolist (child (org-mode-parser:node-children heading))
      (unless (typep child 'org-mode-parser:org-properties-drawer)
        (write-string (org-mode-parser:org-to-string child) s)))))

(defun render-org-aggregate (iface)
  "Render an :org aggregate IFACE: one headline per section, keyed by :ID."
  (with-output-to-string (s)
    (when (iface-title iface)
      (format s "#+TITLE: ~A~%~%" (iface-title iface)))
    (dolist (sec (iface-sections iface))
      (let ((body (and (org-section-render sec)
                       (funcall (org-section-render sec)))))
        (format s "* ~A~%:PROPERTIES:~%:ID: ~A~%:END:~%~A~%~%"
                (or (org-section-title sec) (org-section-id sec))
                (org-section-id sec)
                (if (and body (> (length body) 0))
                    (string-right-trim '(#\Newline) body)
                    ""))))))

(defun parse-org-aggregate (iface text)
  "Parse an :org aggregate IFACE: dispatch each headline (by :ID) to its parse."
  (let ((doc (org-mode-parser:parse-org-string (or text ""))))
    (dolist (sec (iface-sections iface))
      (when (org-section-parse sec)
        (let ((h (org-mode-parser:find-heading-by-custom-id doc (org-section-id sec))))
          (when h (funcall (org-section-parse sec) h)))))))

(defun instance-id ()
  "Return (and memoize) the unique instance id (PID-based by default)."
  (or *instance-id*
      (setf *instance-id* (format nil "~A" (current-pid)))))

(defun instance-dir ()
  "Return (and ensure) the instance directory ./.hactar/<id>/."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil ".hactar/~A/" (instance-id))
                               (or *repo-root* (uiop:getcwd))))))
    (ensure-directories-exist dir)
    (setf *instance-dir* dir)
    dir))

(defmacro definterface (name path &key (format :text) render parse on-change
                                       hooks (watch t) sections title)
  "Define a two-way file interface NAME mapped to PATH under the instance dir.
   :render (lambda () -> string) produces file contents from state.
   :parse  (lambda (string) -> t) applies file contents back into state.
   :on-change is used when :parse is absent.
   :hooks is a list of (HOOK-VAR DIRECTION) entries. DIRECTION is:
     :out — re-render the file from state when the hook fires.
     :in  — re-parse the file into state when the hook fires.
   :watch (default T) installs a file watcher that re-parses on external edits.
   :sections (for :format :org) is a list of (:id :title :render :parse) plists;
   each becomes a headline-keyed sub-interface.
   :title is the optional #+TITLE for org-aggregate interfaces."
  `(setf (gethash ,name *interfaces*)
         (make-iface :name ,name
                     :path ,path
                     :format ,format
                     :render ,render
                     :parse ,parse
                     :on-change ,on-change
                     :hooks ',hooks
                     :watch ,watch
                     :title ,title
                     :sections (list ,@(mapcar
                                        (lambda (sec)
                                          `(make-org-section
                                            :id ,(getf sec :id)
                                            :title ,(getf sec :title)
                                            :render ,(getf sec :render)
                                            :parse ,(getf sec :parse)))
                                        sections)))))

(defun interface-abs-path (iface)
  "Absolute path of an interface's file under the instance dir."
  (merge-pathnames (iface-path iface) (instance-dir)))

(defun %iface-note-self-write (iface hash)
  "Record HASH as content Hactar itself wrote, so the watcher can ignore the
   resulting filesystem event(s) instead of mistaking them for external edits.
   Keeps a bounded ring of recent self-write hashes so several writes can be
   in flight at once (the FS watcher delivers events asynchronously, and a
   single write can produce more than one event)."
  (setf (iface-last-hash iface) hash)
  (push hash (iface-self-hashes iface))
  (when (> (length (iface-self-hashes iface)) 32)
    (setf (iface-self-hashes iface)
          (subseq (iface-self-hashes iface) 0 32)))
  hash)

(defun %iface-echo-p (iface hash)
  "Return non-nil (and consume the matching entry) when HASH corresponds to a
   recent self-write, i.e. an echo of Hactar's own write rather than a user edit."
  (when (member hash (iface-self-hashes iface))
    (setf (iface-self-hashes iface)
          (remove hash (iface-self-hashes iface) :count 1))
    t))

(defun %iface-write (iface content)
  "Write CONTENT to IFACE's file and record it as a self-write (echo-guard)."
  (let ((path (interface-abs-path iface)))
    (setf (iface-abs-path iface) path)
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string content s))
    (%iface-note-self-write iface (sxhash content))
    path))

(defun %iface-render-content (iface)
  "Compute file content for IFACE, dispatching on org-aggregate sections."
  (cond
    ((and (eq (iface-format iface) :org) (iface-sections iface))
     (render-org-aggregate iface))
    ((iface-render iface) (funcall (iface-render iface)))
    (t "")))

(defun materialize-interface (iface)
  "Write the initial file for IFACE and install its watcher and declared hooks."
  (let ((content (%iface-render-content iface)))
    (%iface-write iface content)
    (pushnew iface *active-interfaces*)
    (when (iface-watch iface)
      (install-interface-watcher iface))
    (install-interface-hooks iface)
    iface))

(defun sync-interface-out (name)
  "State -> file: re-render NAME and write it."
  (let ((iface (gethash name *interfaces*)))
    (when iface
      (%iface-write iface (%iface-render-content iface)))))

(defun %iface-apply-in (iface content)
  "Apply CONTENT to state via IFACE's org sections, :parse, or :on-change."
  (cond
    ((and (eq (iface-format iface) :org) (iface-sections iface))
     (parse-org-aggregate iface content))
    ((iface-parse iface) (funcall (iface-parse iface) content))
    ((iface-on-change iface) (funcall (iface-on-change iface) content))))

(defun sync-interface-in (name)
  "File -> state: read NAME's file and apply via :parse/:on-change."
  (let ((iface (gethash name *interfaces*)))
    (when (and iface (probe-file (interface-abs-path iface)))
      (%iface-apply-in iface (uiop:read-file-string (interface-abs-path iface))))))

(defmacro with-interface-suppressed ((name) &body body)
  "Run BODY, then re-sync NAME's echo-guard hash to ignore our own write."
  (let ((g (gensym "IFACE")))
    `(let ((,g (gethash ,name *interfaces*)))
       (prog1 (progn ,@body)
         (when ,g
           (let ((path (interface-abs-path ,g)))
             (when (probe-file path)
               (%iface-note-self-write
                ,g (sxhash (uiop:read-file-string path))))))))))

(defun install-interface-watcher (iface)
  "Install a *raw-file-event-hook* handler that fires sync-interface-in on edits.
   We use the raw hook (not *file-event-hook*) because interface files live under
   the git-ignored instance dir and would otherwise be filtered out before dispatch."
  (nhooks:add-hook
   *raw-file-event-hook*
   (make-instance 'nhooks:handler
                  :fn (lambda (pathname event-type)
                        (declare (ignore event-type))
                        (let ((apath (or (iface-abs-path iface)
                                         (interface-abs-path iface))))
                          (when (and pathname apath
                                     (ignore-errors (probe-file pathname))
                                     (equal (ignore-errors (truename pathname))
                                            (ignore-errors (truename apath))))
                            (let ((content (ignore-errors (uiop:read-file-string apath))))
                              (when content
                                (let ((h (sxhash content)))
                                  ;; Echo-guard: ignore events caused by our own
                                  ;; writes; only genuine external edits sync in.
                                  (unless (%iface-echo-p iface h)
                                    (setf (iface-last-hash iface) h)
                                    (handler-case
                                        (%iface-apply-in iface content)
                                      (error (e)
                                        (debug-log "Interface sync-in failed for"
                                                   (iface-name iface) ":" e))))))))))
                  :name (intern (format nil "IFACE-WATCH-~A" (iface-name iface))
                                :keyword))))

(defun uninstall-interface-hooks (iface)
  "Remove any hook handlers previously installed for IFACE."
  (dolist (pair (iface-installed-handlers iface))
    (ignore-errors (nhooks:remove-hook (car pair) (cdr pair))))
  (setf (iface-installed-handlers iface) '()))

(defun install-interface-hooks (iface)
  "Install the hooks declared by IFACE.
   Each entry is (HOOK-VAR DIRECTION): :out re-renders the file from state when
   the hook fires; :in re-parses the file into state. Idempotent across calls."
  (uninstall-interface-hooks iface)
  (loop for entry in (iface-hooks iface)
        for hook-sym = (first entry)
        for direction = (second entry)
        when (boundp hook-sym)
          do (let* ((iname (iface-name iface))
                    (dir direction)
                    (hook (symbol-value hook-sym))
                    (handler (make-instance 'nhooks:handler
                                            :fn (lambda (&rest ignored)
                                                  (declare (ignore ignored))
                                                  (ecase dir
                                                    (:out (sync-interface-out iname))
                                                    (:in (sync-interface-in iname))))
                                            :name (intern (format nil "IFACE-~A-~A-~A"
                                                                  iname hook-sym dir)
                                                          :keyword))))
               (nhooks:add-hook hook handler)
               (push (cons hook handler) (iface-installed-handlers iface)))))

(defun materialize-all-interfaces ()
  "Materialize all registered interfaces into the instance dir."
  (setf *active-interfaces* '())
  (when (and (boundp '*file-watcher*) *file-watcher*)
    (ignore-errors (add-directory-to-watch *file-watcher* (instance-dir))))
  (maphash (lambda (name iface)
             (declare (ignore name))
             (handler-case (materialize-interface iface)
               (error (e) (debug-log "Failed to materialize interface:" e))))
           *interfaces*))

;;** Format helpers (§7.2)

(defun render-dir-list (paths &key (base *repo-root*)
                                   (header "# hactar files  (edit: delete a line to drop, add a path to add)"))
  "Render PATHS as a dired-like list relative to BASE."
  (with-output-to-string (s)
    (format s "~A~%" header)
    (dolist (p paths)
      (let ((rel (if base
                     (uiop:native-namestring (uiop:enough-pathname p base))
                     (uiop:native-namestring p))))
        (format s "~A~%" rel)))))

(defun parse-dir-list (text)
  "Parse a dired-like list, ignoring blank and comment (#) lines."
  (remove-if (lambda (line)
               (or (string= line "")
                   (str:starts-with? "#" line)))
             (mapcar (lambda (l) (string-trim '(#\Space #\Tab #\Return) l))
                     (str:lines text))))

;;** Built-in interfaces
(definterface :files "context/files.dir"
  :format :dir
  :hooks ((*context-file-added-hook* :out)
          (*context-file-dropped-hook* :out))
  :render (lambda () (render-dir-list *files*))
  :parse (lambda (text)
           (let* ((wanted (parse-dir-list text))
                  (abs-wanted (mapcar (lambda (p)
                                        (uiop:native-namestring
                                         (merge-pathnames p (or *repo-root* (uiop:getcwd)))))
                                      wanted))
                  (current (mapcar #'uiop:native-namestring *files*)))
             ;; additions
             (dolist (p abs-wanted)
               (unless (member p current :test #'string=)
                 (when (probe-file p) (add-file-to-context p))))
             ;; removals
             (dolist (f (copy-list current))
               (unless (member f abs-wanted :test #'string=)
                 (drop-file-from-context f))))))

(definterface :state "state.lisp"
  :format :lisp
  :render (lambda ()
            (format nil ";;; Project state (edit and save to apply)~%(in-package :hactar)~%~%~
                         (setf *name* ~S)~%(setf *author* ~S)~%~
                         (setf *language* ~S)~%(setf *stack* '~S)~%"
                    *name* *author* *language* *stack*))
  :parse (lambda (text)
           (declare (ignore text))
           (let ((iface (gethash :state *interfaces*)))
             (when iface
               (ignore-errors (load (interface-abs-path iface)))))))

(definterface :prompt "prompt.org"
  :format :org
  :render (lambda () (or *pending-prompt* ""))
  :parse (lambda (text) (setf *pending-prompt* text)))

;;** Org-aggregate context interface
(defun %org-list-items (body)
  "Collect '- item' list entries from an org BODY string."
  (loop for line in (str:lines body)
        for l = (string-trim '(#\Space #\Tab #\Return) line)
        when (str:starts-with? "- " l)
          collect (subseq l 2)))

(defun render-project-details-body ()
  (format nil "- Name: ~A~%- Author: ~A~%- Language: ~A~%- Shell: ~A~%- Stack: ~{~A~^, ~}~%"
          (or *name* "") (or *author* "") (or *language* "") (or *shell* "")
          (or *stack* '())))

(defun parse-project-details-heading (heading)
  (dolist (item (%org-list-items (%heading-body-string heading)))
    (let ((cpos (position #\: item)))
      (when cpos
        (let ((key (string-downcase (string-trim '(#\Space) (subseq item 0 cpos))))
              (val (string-trim '(#\Space) (subseq item (1+ cpos)))))
          (cond
            ((string= key "name") (set-context-variable :name val))
            ((string= key "author") (set-context-variable :author val))
            ((string= key "language") (set-context-variable :language val))
            ((string= key "shell") (set-context-variable :shell val))
            ((string= key "stack")
             (set-context-variable
              :stack (remove "" (mapcar (lambda (x) (string-trim '(#\Space) x))
                                        (str:split "," val))
                             :test #'string=)))))))))

(defun render-ctx-files-body ()
  (if *files*
      (format nil "~{- ~A~%~}"
              (mapcar (lambda (f)
                        (uiop:native-namestring
                         (uiop:enough-pathname f (or *repo-root* (uiop:getcwd)))))
                      *files*))
      ""))

(defun parse-ctx-files-heading (heading)
  (let* ((rels (%org-list-items (%heading-body-string heading)))
         (abs-wanted (mapcar (lambda (p)
                               (uiop:native-namestring
                                (merge-pathnames p (or *repo-root* (uiop:getcwd)))))
                             rels))
         (current (mapcar #'uiop:native-namestring *files*)))
    (dolist (p abs-wanted)
      (unless (member p current :test #'string=)
        (when (probe-file p) (add-file-to-context p))))
    (dolist (f (copy-list current))
      (unless (member f abs-wanted :test #'string=)
        (drop-file-from-context f)))))

(defun render-ctx-docs-body ()
  (if *docs-context*
      (format nil "~{- ~A~%~}"
              (mapcar (lambda (d) (cdr (assoc :title d))) *docs-context*))
      ""))

(defun parse-ctx-docs-heading (heading)
  (let ((titles (%org-list-items (%heading-body-string heading))))
    (dolist (d (copy-list *docs-context*))
      (unless (member (cdr (assoc :title d)) titles :test #'string=)
        (remove-doc-from-context (cdr (assoc :id d)))))))

(defun render-ctx-errors-body ()
  (if *errors-context*
      (format nil "~{- ~A~%~}"
              (mapcar (lambda (e)
                        (format nil "~A: ~A" (cdr (assoc :code e)) (cdr (assoc :title e))))
                      *errors-context*))
      ""))

(defun parse-ctx-errors-heading (heading)
  (let ((codes (mapcar (lambda (item)
                         (let ((cpos (position #\: item)))
                           (if cpos (string-trim '(#\Space) (subseq item 0 cpos)) item)))
                       (%org-list-items (%heading-body-string heading)))))
    (dolist (e (copy-list *errors-context*))
      (unless (member (cdr (assoc :code e)) codes :test #'string=)
        (remove-error-from-context (cdr (assoc :code e)))))))

(definterface :context-org ".hactar.context.org"
  :format :org
  :title "Hactar Context"
  :hooks ((*context-file-added-hook* :out)
          (*context-file-dropped-hook* :out)
          (*context-variable-changed-hook* :out)
          (*context-doc-added-hook* :out)
          (*context-doc-dropped-hook* :out))
  :sections
  ((:id "project-details" :title "Project Details"
    :render #'render-project-details-body
    :parse #'parse-project-details-heading)
   (:id "ctx-files" :title "Files"
    :render #'render-ctx-files-body
    :parse #'parse-ctx-files-heading)
   (:id "ctx-docs" :title "Documentation"
    :render #'render-ctx-docs-body
    :parse #'parse-ctx-docs-heading)
   (:id "ctx-errors" :title "Errors"
    :render #'render-ctx-errors-body
    :parse #'parse-ctx-errors-heading)))

(defun render-docs-list (docs)
  "Render the list of documents currently in context to a string."
  (with-output-to-string (s)
    (format s "# Documentation in context (edit and save to apply)~%")
    (dolist (doc docs)
      (let ((uri (or (cdr (assoc :uri doc))
                     (cdr (assoc :source doc))
                     (cdr (assoc :id doc)))))
        (when uri
          (format s "~A~%" uri))))))

(defun parse-docs-list (text)
  "Parse a list of document URIs, import them if necessary, and update *docs-context*."
  (let* ((wanted (parse-dir-list text))
         ;; Additions
         (current-docs (copy-list *docs-context*)))
    (dolist (uri wanted)
      (let ((doc (find uri *docs* :key (lambda (d) (cdr (assoc :uri d))) :test #'string=)))
        (unless doc
          (setf doc (find uri *docs* :key (lambda (d) (cdr (assoc :source d))) :test #'string=)))
        (unless doc
          ;; Try importing it
          (let ((*silent* t))
            (execute-import uri))
          ;; Try finding again
          (setf doc (find uri *docs* :key (lambda (d) (cdr (assoc :uri d))) :test #'string=))
          (unless doc
            (setf doc (find uri *docs* :key (lambda (d) (cdr (assoc :source d))) :test #'string=))))
        (when doc
          (add-doc-to-context doc))))
    ;; Removals
    (dolist (doc current-docs)
      (let ((doc-uri (cdr (assoc :uri doc)))
            (doc-source (cdr (assoc :source doc))))
        (unless (or (and doc-uri (member doc-uri wanted :test #'string=))
                    (and doc-source (member doc-source wanted :test #'string=)))
          (remove-doc-from-context (cdr (assoc :id doc))))))))

(definterface :docs "context/docs.org"
  :format :text
  :hooks ((*context-doc-added-hook* :out)
          (*context-doc-dropped-hook* :out))
  :render (lambda () (render-docs-list *docs-context*))
  :parse #'parse-docs-list)

;;** Mold interface
(defun render-active-mold-org ()
  (if (and (boundp '*active-mold*) *active-mold*)
      (mold-export-as-org *active-mold*)
      ""))

(defun parse-mold-org (text)
  "Re-parse the Rules section of a mold org doc into *active-rules* for the mold."
  (when (and (boundp '*active-mold*) *active-mold*)
    (let ((in-rules nil)
          (rules '()))
      (dolist (line (str:lines text))
        (let ((l (string-trim '(#\Space #\Tab #\Return) line)))
          (cond
            ((and (str:starts-with? "* " l)
                  (string-equal (string-trim '(#\Space) (subseq l 2)) "Rules"))
             (setf in-rules t))
            ((str:starts-with? "* " l) (setf in-rules nil))
            ((and in-rules (str:starts-with? "- " l))
             (push (subseq l 2) rules)))))
      (setf rules (nreverse rules))
      ;; Replace existing generic rule keys for this mold.
      (let ((prefix (format nil "mold/~A/rule/" (mold-definition-name *active-mold*)))
            (to-remove '()))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (when (str:starts-with-p prefix k) (push k to-remove)))
                 *active-rules*)
        (dolist (k to-remove) (remhash k *active-rules*)))
      (dolist (r rules)
        (let ((rule-text (cl-ppcre:regex-replace "^\\[[^\\]]+\\]\\s*" r "")))
          (setf (gethash (format nil "mold/~A/rule/~A"
                                 (mold-definition-name *active-mold*)
                                 (sxhash rule-text))
                         *active-rules*)
                rule-text))))))

(definterface :mold "mold.org"
  :format :text
  :render #'render-active-mold-org
  :parse #'parse-mold-org)
