;;* Context Preset System
(in-package :hactar)

;;** Data Structures

(defstruct preset
  "A reusable context configuration."
  (name nil :type (or null string symbol))
  (description nil :type (or null string))
  ;; Files to add (globs and explicit paths)
  (files '() :type list)
  ;; Documentation to load (by name or ID)
  (docs '() :type list)
  ;; Rules to activate (symbols)
  (rules '() :type list)
  ;; Skills to load (names)
  (skills '() :type list)
  ;; Stack requirements (will warn if not met)
  (requires-stack '() :type list)
  ;; Custom setup function
  (setup-fn nil :type (or null function))
  ;; Custom teardown function
  (teardown-fn nil :type (or null function))
  ;; Parent preset name (for composition)
  (extends nil :type (or null string symbol))
  ;; Whether this preset is currently active
  (active-p nil :type boolean)
  ;; Source file path (if loaded from file)
  (source-file nil :type (or null pathname string)))

(defstruct context-snapshot
  "A saved state of the context that can be restored."
  (name nil :type (or null string))
  (timestamp nil :type (or null integer))
  (description nil :type (or null string))
  ;; Captured state
  (files '() :type list)
  (images '() :type list)
  (docs-context '() :type list)
  (errors-context '() :type list)
  (active-rules-text nil :type (or null list))  ; List of (name . text) pairs
  (active-presets '() :type list)
  ;; Optional - can be large
  (chat-history-p nil :type boolean)
  (chat-history '() :type list))

;;** Preset Registry

(defun register-preset (preset)
  "Register a preset in the global registry."
  (let ((name (preset-name-string preset)))
    (setf (gethash name *presets*) preset)
    preset))

(defun get-preset (name)
  "Get a preset by name."
  (gethash (if (stringp name) name (string-downcase (symbol-name name))) 
           *presets*))

(defun preset-name-string (preset)
  "Get the string name of a preset."
  (let ((name (preset-name preset)))
    (if (stringp name) 
        name 
        (string-downcase (symbol-name name)))))

(defun list-presets ()
  "Return list of all registered preset names."
  (let ((names '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *presets*)
    (sort names #'string<)))

(defun clear-presets ()
  "Clear all registered presets."
  (clrhash *presets*)
  (setf *active-presets* '()))

;;** defpreset Macro

(defmacro defpreset (name description &body body)
  "Define a reusable context preset.
   
   Usage:
   (defpreset auth
     \"Authentication feature development\"
     :extends core
     :files (\"src/auth/*.ts\" \"src/api/auth.ts\")
     :docs (\"jwt\" \"oauth2\")
     :skills (\"auth-patterns\")
     :rules (auth-security)
     :requires-stack (\"react\" \"typescript\")
     :setup (lambda () (format t \"Auth preset loaded.~%\"))
     :teardown (lambda () (format t \"Auth preset unloaded.~%\")))"
  (let ((files (getf body :files))
        (docs (getf body :docs))
        (rules (getf body :rules))
        (skills (getf body :skills))
        (requires-stack (getf body :requires-stack))
        (extends (getf body :extends))
        (setup (getf body :setup))
        (teardown (getf body :teardown)))
    `(register-preset
      (make-preset
       :name ',name
       :description ,description
       :files ',files
       :docs ',docs
       :rules ',rules
       :skills ',skills
       :requires-stack ',requires-stack
       :extends ',extends
       :setup-fn ,setup
       :teardown-fn ,teardown))))

;;** Glob Expansion

(defun expand-file-globs (patterns &optional (base-dir *repo-root*))
  "Expand a list of file patterns (including globs) to actual file paths."
  (let ((files '()))
    (dolist (pattern patterns)
      (let ((expanded (expand-single-glob pattern base-dir)))
        (setf files (append files expanded))))
    (remove-duplicates files :test #'string=)))

(defun expand-single-glob (pattern base-dir)
  "Expand a single glob pattern to matching file paths."
  (let ((full-pattern (merge-pathnames pattern base-dir)))
    (if (or (search "*" pattern) (search "?" pattern))
        ;; It's a glob pattern
        (let ((matches '()))
          (handler-case
              (uiop:collect-sub*directories
               base-dir
               (constantly t)
               (constantly t)
               (lambda (dir)
                 (dolist (file (uiop:directory-files dir))
                   (when (glob-matches-p (uiop:native-namestring file)
                                         (uiop:native-namestring full-pattern))
                     (push (uiop:native-namestring file) matches)))))
            (error (e)
              (debug-log "Error expanding glob ~A: ~A" pattern e)))
          matches)
        ;; Literal path
        (let ((path (uiop:native-namestring full-pattern)))
          (if (probe-file path)
              (list path)
              '())))))

;;** Preset Resolution (Inheritance)

(defun resolve-preset-chain (preset)
  "Resolve the inheritance chain of a preset, returning list from base to derived."
  (let ((chain (list preset))
        (current preset))
    (loop while (preset-extends current)
          do (let ((parent (get-preset (preset-extends current))))
               (if parent
                   (progn
                     (push parent chain)
                     (setf current parent))
                   (progn
                     (format t "~&Warning: Parent preset '~A' not found~%" 
                             (preset-extends current))
                     (return)))))
    chain))

(defun merge-preset-files (chain)
  "Merge files from a preset inheritance chain."
  (let ((files '()))
    (dolist (preset chain)
      (setf files (append files (preset-files preset))))
    (remove-duplicates files :test #'string=)))

(defun merge-preset-docs (chain)
  "Merge docs from a preset inheritance chain."
  (let ((docs '()))
    (dolist (preset chain)
      (setf docs (append docs (preset-docs preset))))
    (remove-duplicates docs :test #'string=)))

(defun merge-preset-skills (chain)
  "Merge skills from a preset inheritance chain."
  (let ((skills '()))
    (dolist (preset chain)
      (setf skills (append skills (preset-skills preset))))
    (remove-duplicates skills :test #'string=)))

(defun merge-preset-rules (chain)
  "Merge rules from a preset inheritance chain."
  (let ((rules '()))
    (dolist (preset chain)
      (setf rules (append rules (preset-rules preset))))
    (remove-duplicates rules)))

(defun merge-preset-requires-stack (chain)
  "Merge stack requirements from a preset inheritance chain."
  (let ((reqs '()))
    (dolist (preset chain)
      (setf reqs (append reqs (preset-requires-stack preset))))
    (remove-duplicates reqs :test #'string-equal)))

;;** Preset Loading

(defun preset/load (name &key quiet)
  "Load a preset by name, adding its files, docs, skills to context."
  (let ((preset (get-preset name)))
    (unless preset
      (format t "~&Preset not found: ~A~%" name)
      (return-from preset/load nil))
    
    (when (member (preset-name-string preset) *active-presets* :test #'string=)
      (unless quiet
        (format t "~&Preset already active: ~A~%" name))
      (return-from preset/load preset))
    
    (let* ((chain (resolve-preset-chain preset))
           (files (merge-preset-files chain))
           (docs (merge-preset-docs chain))
           (skills (merge-preset-skills chain))
           (rules (merge-preset-rules chain))
           (requires (merge-preset-requires-stack chain)))
      
      (dolist (req requires)
        (unless (member req *stack* :test #'string-equal)
          (format t "~&Warning: Preset '~A' requires '~A' but it's not in stack~%"
                  name req)))
      
      (unless quiet
        (format t "~&Loading preset: ~A~%" name)
        (when (preset-extends preset)
          (format t "  (extends: ~A)~%" (preset-extends preset))))
      
      (let ((expanded-files (expand-file-globs files)))
        (unless quiet
          (format t "  Adding ~A files to context...~%" (length expanded-files)))
        (dolist (file expanded-files)
          (add-file-to-context file)))
      
      (when docs
        (unless quiet
          (format t "  Loading docs: ~{~A~^, ~}~%" docs))
        (dolist (doc-name docs)
          ;; Try to find and load the doc
          (let ((doc (find doc-name *docs* 
                           :key (lambda (d) (cdr (assoc :title d)))
                           :test #'string-equal)))
            (when doc
              (add-doc-to-context doc)))))

      (when skills
        (unless quiet
          (format t "  Loading skills: ~{~A~^, ~}~%" skills))
        (dolist (skill-name skills)
          (run-skills-load (list :subcommand (list skill-name)))))
      
      (when rules
        (unless quiet
          (format t "  Rules specified: ~{~A~^, ~}~%" rules)))
      
      (dolist (p chain)
        (when (preset-setup-fn p)
          (funcall (preset-setup-fn p))))
      
      (setf (preset-active-p preset) t)
      (push (preset-name-string preset) *active-presets*)
      
      preset)))

(defun preset/unload (name &key quiet)
  "Unload a preset, removing its files from context."
  (let ((preset (get-preset name)))
    (unless preset
      (format t "~&Preset not found: ~A~%" name)
      (return-from preset/unload nil))
    
    (unless (preset-active-p preset)
      (unless quiet
        (format t "~&Preset not active: ~A~%" name))
      (return-from preset/unload nil))
    
    (unless quiet
      (format t "~&Unloading preset: ~A~%" name))
    
    (let ((chain (reverse (resolve-preset-chain preset))))
      (dolist (p chain)
        (when (preset-teardown-fn p)
          (funcall (preset-teardown-fn p)))))
    
    (let ((files (expand-file-globs (preset-files preset))))
      (dolist (file files)
        (drop-file-from-context file)))
    
    (setf (preset-active-p preset) nil)
    (setf *active-presets* 
          (remove (preset-name-string preset) *active-presets* :test #'string=))
    
    preset))

(defun preset/unload-all ()
  "Unload all active presets."
  (dolist (name (reverse *active-presets*))
    (preset/unload name :quiet t))
  (format t "~&All presets unloaded.~%"))

(defun preset/reload (name)
  "Reload a preset (unload then load)."
  (preset/unload name :quiet t)
  (preset/load name))

;;** Preset File Discovery & Loading

(defun preset-search-directories ()
  "Return list of directories to search for preset files."
  (remove-if-not 
   #'uiop:directory-exists-p
   (append *preset-search-paths*
           (when *repo-root*
             (list (merge-pathnames ".hactar/presets/" *repo-root*)))
           (list (uiop:subpathname *hactar-config-path* "presets/")))))

(defun discover-preset-files ()
  "Find all preset files in search directories."
  (let ((files '()))
    (dolist (dir (preset-search-directories))
      (when (uiop:directory-exists-p dir)
        (dolist (file (uiop:directory-files dir))
          (when (and (string= (pathname-type file) "lisp")
                     (search ".preset" (pathname-name file)))
            (push file files)))))
    files))

(defun load-preset-file (path)
  "Load presets from a file."
  (handler-case
      (progn
        (load path)
        (unless *silent*
          (format t "~&Loaded preset file: ~A~%" (uiop:native-namestring path))))
    (error (e)
      (format t "~&Error loading preset file ~A: ~A~%" 
              (uiop:native-namestring path) e))))

(defun load-discovered-presets ()
  "Load all discovered preset files."
  (dolist (file (discover-preset-files))
    (load-preset-file file)))

;;** Snapshot System

(defun snapshot/save (name &key description include-history)
  "Save the current context state as a snapshot."
  (let* ((rules-list '())
         (_ (maphash (lambda (k v) (push (cons k v) rules-list)) *active-rules*))
         (snapshot (make-context-snapshot
                    :name name
                    :timestamp (get-universal-time)
                    :description description
                    :files (copy-list *files*)
                    :images (copy-list *images*)
                    :docs-context (copy-list *docs-context*)
                    :errors-context (copy-list *errors-context*)
                    :active-rules-text rules-list
                    :active-presets (copy-list *active-presets*)
                    :chat-history-p include-history
                    :chat-history (when include-history (copy-list *chat-history*)))))
    (declare (ignore _))
    (setf (gethash name *snapshots*) snapshot)
    (save-snapshot-to-disk snapshot)
    (format t "~&Saved snapshot: ~A~%" name)
    snapshot))

(defun snapshot/restore (name)
  "Restore context from a snapshot."
  (let ((snapshot (or (gethash name *snapshots*)
                      (load-snapshot-from-disk name))))
    (unless snapshot
      (format t "~&Snapshot not found: ~A~%" name)
      (return-from snapshot/restore nil))
    
    (setf *files* '())
    (setf *images* '())
    (setf *docs-context* '())
    (setf *errors-context* '())
    (clrhash *active-rules*)
    
    (setf *files* (copy-list (context-snapshot-files snapshot)))
    (setf *images* (copy-list (context-snapshot-images snapshot)))
    (setf *docs-context* (copy-list (context-snapshot-docs-context snapshot)))
    (setf *errors-context* (copy-list (context-snapshot-errors-context snapshot)))
    
    (dolist (rule-pair (context-snapshot-active-rules-text snapshot))
      (setf (gethash (car rule-pair) *active-rules*) (cdr rule-pair)))
    
    (when (context-snapshot-chat-history-p snapshot)
      (setf *chat-history* (copy-list (context-snapshot-chat-history snapshot))))
    
    (when *exposed-context-file*
      (context-expose-upsert-files-section)
      (context-expose-upsert-docs-section)
      (context-expose-upsert-errors-section))
    
    (format t "~&Restored snapshot: ~A (from ~A)~%" 
            name 
            (format-timestamp (context-snapshot-timestamp snapshot)))
    snapshot))

(defun snapshot/delete (name)
  "Delete a snapshot."
  (remhash name *snapshots*)
  (let ((path (snapshot-file-path name)))
    (when (probe-file path)
      (delete-file path)))
  (format t "~&Deleted snapshot: ~A~%" name))

(defun snapshot/list ()
  "List all snapshots."
  (let ((snapshots '()))
    (maphash (lambda (k v) 
               (push (list :name k 
                           :timestamp (context-snapshot-timestamp v)
                           :description (context-snapshot-description v))
                     snapshots))
             *snapshots*)
    (ensure-directories-exist *snapshots-dir*)
    (dolist (file (uiop:directory-files *snapshots-dir*))
      (when (string= (pathname-type file) "json")
        (let ((name (pathname-name file)))
          (unless (gethash name *snapshots*)
            (push (list :name name :timestamp nil :description "(on disk)") 
                  snapshots)))))
    (sort snapshots #'string< :key (lambda (s) (getf s :name)))))

(defun format-timestamp (universal-time)
  "Format a universal time for display."
  (if universal-time
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time universal-time)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour min sec))
      "unknown"))

;;** Snapshot Persistence

(defun snapshot-file-path (name)
  "Get the file path for a snapshot."
  (merge-pathnames (format nil "~A.json" name) *snapshots-dir*))

(defun save-snapshot-to-disk (snapshot)
  "Save a snapshot to disk as JSON."
  (ensure-directories-exist *snapshots-dir*)
  (let ((path (snapshot-file-path (context-snapshot-name snapshot))))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (cl-json:encode-json
       `((:name . ,(context-snapshot-name snapshot))
         (:timestamp . ,(context-snapshot-timestamp snapshot))
         (:description . ,(context-snapshot-description snapshot))
         (:files . ,(coerce (context-snapshot-files snapshot) 'vector))
         (:images . ,(coerce (context-snapshot-images snapshot) 'vector))
         (:docs-context . ,(coerce (context-snapshot-docs-context snapshot) 'vector))
         (:errors-context . ,(coerce (context-snapshot-errors-context snapshot) 'vector))
         (:active-rules-text . ,(context-snapshot-active-rules-text snapshot))
         (:active-presets . ,(coerce (context-snapshot-active-presets snapshot) 'vector))
         (:chat-history-p . ,(context-snapshot-chat-history-p snapshot))
         (:chat-history . ,(when (context-snapshot-chat-history-p snapshot)
                             (coerce (context-snapshot-chat-history snapshot) 'vector))))
       stream))))

(defun load-snapshot-from-disk (name)
  "Load a snapshot from disk."
  (let ((path (snapshot-file-path name)))
    (when (probe-file path)
      (handler-case
          (with-open-file (stream path :direction :input :external-format :utf-8)
            (let ((data (cl-json:decode-json stream)))
              (make-context-snapshot
               :name (cdr (assoc :name data))
               :timestamp (cdr (assoc :timestamp data))
               :description (cdr (assoc :description data))
               :files (coerce (cdr (assoc :files data)) 'list)
               :images (coerce (cdr (assoc :images data)) 'list)
               :docs-context (coerce (cdr (assoc :docs-context data)) 'list)
               :errors-context (coerce (cdr (assoc :errors-context data)) 'list)
               :active-rules-text (cdr (assoc :active-rules-text data))
               :active-presets (coerce (cdr (assoc :active-presets data)) 'list)
               :chat-history-p (cdr (assoc :chat-history-p data))
               :chat-history (coerce (or (cdr (assoc :chat-history data)) #()) 'list))))
        (error (e)
          (format t "~&Error loading snapshot ~A: ~A~%" name e)
          nil)))))

;;** REPL Commands - Presets

(define-command preset (args)
  "Load a preset or list available presets.
Usage: /preset              - List available presets
       /preset <name>       - Load a preset"
  (if args
      (preset/load (first args))
      (let ((presets (list-presets)))
        (if presets
            (progn
              (format t "~&Available presets:~%")
              (dolist (name presets)
                (let* ((preset (get-preset name))
                       (active (preset-active-p preset)))
                  (format t "  ~A~A - ~A~%"
                          (if active "* " "  ")
                          name
                          (or (preset-description preset) "(no description)")))))
            (format t "~&No presets defined.~%")))))

(define-command preset.load (args)
  "Load a preset by name.
Usage: /preset.load <name>"
  (if args
      (preset/load (first args))
      (format t "~&Usage: /preset.load <name>~%")))

(define-command preset.unload (args)
  "Unload a preset or all presets.
Usage: /preset.unload [name]  - Unload specific or all presets"
  (if args
      (preset/unload (first args))
      (preset/unload-all)))

(define-command preset.list (args)
  "List available presets with details."
  (declare (ignore args))
  (let ((presets (list-presets)))
    (if presets
        (progn
          (format t "~&Presets:~%")
          (dolist (name presets)
            (let ((preset (get-preset name)))
              (format t "~%~A~A~%"
                      name
                      (if (preset-active-p preset) " [ACTIVE]" ""))
              (format t "  ~A~%" (or (preset-description preset) "(no description)"))
              (when (preset-extends preset)
                (format t "  Extends: ~A~%" (preset-extends preset)))
              (when (preset-files preset)
                (format t "  Files: ~{~A~^, ~}~%" (preset-files preset)))
              (when (preset-docs preset)
                (format t "  Docs: ~{~A~^, ~}~%" (preset-docs preset)))
              (when (preset-skills preset)
                (format t "  Skills: ~{~A~^, ~}~%" (preset-skills preset)))
              (when (preset-requires-stack preset)
                (format t "  Requires: ~{~A~^, ~}~%" (preset-requires-stack preset))))))
        (format t "~&No presets defined.~%"))))

(define-command preset.show (args)
  "Show details of a specific preset.
Usage: /preset.show <name>"
  (if args
      (let ((preset (get-preset (first args))))
        (if preset
            (progn
              (format t "~&Preset: ~A~%" (preset-name preset))
              (format t "Description: ~A~%" (or (preset-description preset) "(none)"))
              (format t "Active: ~A~%" (if (preset-active-p preset) "yes" "no"))
              (when (preset-extends preset)
                (format t "Extends: ~A~%" (preset-extends preset)))
              (format t "~%Files:~%")
              (dolist (f (preset-files preset))
                (format t "  ~A~%" f))
              (when (preset-docs preset)
                (format t "~%Docs: ~{~A~^, ~}~%" (preset-docs preset)))
              (when (preset-skills preset)
                (format t "~%Skills: ~{~A~^, ~}~%" (preset-skills preset)))
              (when (preset-rules preset)
                (format t "~%Rules: ~{~A~^, ~}~%" (preset-rules preset)))
              (when (preset-requires-stack preset)
                (format t "~%Requires stack: ~{~A~^, ~}~%" (preset-requires-stack preset))))
            (format t "~&Preset not found: ~A~%" (first args))))
      (format t "~&Usage: /preset.show <name>~%")))

(define-command preset.active (args)
  "Show currently active presets."
  (declare (ignore args))
  (if *active-presets*
      (progn
        (format t "~&Active presets:~%")
        (loop for name in *active-presets*
              for i from 1
              do (let ((preset (get-preset name)))
                   (format t "  ~A. ~A~A~%"
                           i name
                           (if (preset-extends preset)
                               (format nil " (extends ~A)" (preset-extends preset))
                               "")))))
      (format t "~&No active presets.~%")))

(define-command preset.save (args)
  "Save current context as a new preset.
Usage: /preset.save <name> [description]"
  (if args
      (let* ((name (first args))
             (description (when (rest args)
                            (str:join " " (rest args))))
             (preset (make-preset
                      :name name
                      :description (or description "Saved from current context")
                      :files (copy-list *files*))))
        (register-preset preset)
        (let* ((preset-dir (merge-pathnames ".hactar/presets/" *repo-root*))
               (preset-file (merge-pathnames (format nil "~A.preset.lisp" name) preset-dir)))
          (ensure-directories-exist preset-file)
          (with-open-file (stream preset-file
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (format stream ";;; Preset: ~A~%" name)
            (format stream ";;; Generated: ~A~%~%" (format-timestamp (get-universal-time)))
            (format stream "(defpreset ~A~%" name)
            (format stream "  ~S~%" (or description "Saved from current context"))
            (format stream "  :files (~{~%    ~S~}))~%" *files*))
          (format t "~&Saved preset '~A' to ~A~%" name (uiop:native-namestring preset-file))))
      (format t "~&Usage: /preset.save <name> [description]~%")))

(define-command preset.reload (args)
  "Reload a preset or all active presets.
Usage: /preset.reload [name]"
  (if args
      (preset/reload (first args))
      (let ((active (copy-list *active-presets*)))
        (dolist (name active)
          (preset/reload name)))))

;;** REPL Commands - Snapshots

(define-command snapshot (args)
  "List snapshots or save current context.
Usage: /snapshot           - List snapshots
       /snapshot <name>    - Save snapshot with name"
  (if args
      (snapshot/save (first args))
      (let ((snapshots (snapshot/list)))
        (if snapshots
            (progn
              (format t "~&Snapshots:~%")
              (dolist (s snapshots)
                (format t "  ~A~@[ - ~A~]~@[ (~A)~]~%"
                        (getf s :name)
                        (when (getf s :timestamp)
                          (format-timestamp (getf s :timestamp)))
                        (getf s :description))))
            (format t "~&No snapshots.~%")))))

(define-command snapshot.save (args)
  "Save current context as a snapshot.
Usage: /snapshot.save <name> [--history] [--description \"desc\"]"
  (if args
      (let* ((name (first args))
             (rest-args (rest args))
             (include-history (member "--history" rest-args :test #'string=))
             (desc-pos (position "--description" rest-args :test #'string=))
             (description (when desc-pos (nth (1+ desc-pos) rest-args))))
        (snapshot/save name :description description :include-history include-history))
      (format t "~&Usage: /snapshot.save <name> [--history] [--description \"desc\"]~%")))

(define-command snapshot.restore (args)
  "Restore context from a snapshot.
Usage: /snapshot.restore <name>"
  (if args
      (snapshot/restore (first args))
      (format t "~&Usage: /snapshot.restore <name>~%")))

(define-command snapshot.delete (args)
  "Delete a snapshot.
Usage: /snapshot.delete <name>"
  (if args
      (when (confirm-action (format nil "Delete snapshot '~A'?" (first args)))
        (snapshot/delete (first args)))
      (format t "~&Usage: /snapshot.delete <name>~%")))

(define-command snapshot.list (args)
  "List all snapshots with details."
  (declare (ignore args))
  (let ((snapshots (snapshot/list)))
    (if snapshots
        (progn
          (format t "~&Snapshots:~%")
          (dolist (s snapshots)
            (format t "  ~A~%"  (getf s :name))
            (when (getf s :timestamp)
              (format t "    Time: ~A~%" (format-timestamp (getf s :timestamp))))
            (when (getf s :description)
              (format t "    Description: ~A~%" (getf s :description)))))
        (format t "~&No snapshots.~%"))))

;;** CLI Sub-commands

(define-sub-command preset (args)
  "Manage context presets.
Usage: hactar preset list
       hactar preset load <name>
       hactar preset show <name>"
  (let ((subcommand (first args))
        (rest-args (rest args)))
    (cond
      ((or (null subcommand) (string= subcommand "list"))
       (funcall (first (gethash "preset.list" *commands*)) nil))
      ((string= subcommand "load")
       (when rest-args
         (preset/load (first rest-args))))
      ((string= subcommand "show")
       (when rest-args
         (funcall (first (gethash "preset.show" *commands*)) rest-args)))
      (t
       (format t "~&Unknown preset command: ~A~%" subcommand)))))

;;** Built-in Presets

(defpreset core
  "Hactar core development files"
  :files ("globals.lisp"
          "hactar.lisp"
          "context.lisp"
          "commands.lisp")
  :docs ("hactar-internals"))

(defpreset minimal
  "Minimal context - just essential files"
  :files ()
  :setup (lambda ()
           (format t "~&Minimal preset loaded. Context is empty.~%")))
