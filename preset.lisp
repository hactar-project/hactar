;; presets are a bunch of state that gets loaded.
;; you can use to for example keep a list of files for one feature and then load all the files to e.g work on authentication
(in-package :hactar)

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

;;* Registry
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
;;* utils
(defun expand-file-globs (patterns &optional (base-dir *repo-root*))
  "Expand a list of file patterns (including globs) to actual file paths.
   Reuses the single glob engine via EXPAND-FILE-PATTERN (context.lisp/utils.lisp)."
  (remove-duplicates
   (loop for pattern in patterns
         append (expand-file-pattern pattern base-dir))
   :test #'string=))

;;* core

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

;;* Files

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

;;* commands
(defun preset-to-alist (preset)
  "Convert a preset struct to an association list for serialization."
  (let ((alist `(("name" . ,(preset-name-string preset))
                 ("active" . ,(if (preset-active-p preset) t :false))
                 ("description" . ,(or (preset-description preset) "")))))
    (when (preset-extends preset)
      (push (cons "extends" (let ((ext (preset-extends preset)))
                              (if (symbolp ext) (string-downcase (symbol-name ext)) ext)))
            alist))
    (when (preset-files preset)
      (push (cons "files" (coerce (preset-files preset) 'vector)) alist))
    (when (preset-docs preset)
      (push (cons "docs" (coerce (preset-docs preset) 'vector)) alist))
    (when (preset-skills preset)
      (push (cons "skills" (coerce (preset-skills preset) 'vector)) alist))
    (when (preset-rules preset)
      (push (cons "rules" (coerce (mapcar (lambda (r) (if (symbolp r) (string-downcase (symbol-name r)) r))
                                          (preset-rules preset))
                                  'vector))
            alist))
    (when (preset-requires-stack preset)
      (push (cons "requires" (coerce (preset-requires-stack preset) 'vector)) alist))
    (nreverse alist)))

(defun presets-to-json (presets)
  "Convert a list of presets to a JSON string."
  (to-json (coerce (mapcar #'preset-to-alist presets) 'vector)))

(defun presets-to-yaml (presets)
  "Convert a list of presets to a YAML string."
  (with-output-to-string (s)
    (dolist (p presets)
      (format s "- name: ~A~%  active: ~A~%  description: ~A~%"
              (preset-name-string p)
              (if (preset-active-p p) "true" "false")
              (or (preset-description p) ""))
      (when (preset-extends p)
        (format s "  extends: ~A~%" (let ((ext (preset-extends p)))
                                      (if (symbolp ext) (string-downcase (symbol-name ext)) ext))))
      (when (preset-files p)
        (format s "  files:~%")
        (dolist (f (preset-files p))
          (format s "    - ~A~%" f)))
      (when (preset-docs p)
        (format s "  docs:~%")
        (dolist (d (preset-docs p))
          (format s "    - ~A~%" d)))
      (when (preset-skills p)
        (format s "  skills:~%")
        (dolist (sk (preset-skills p))
          (format s "    - ~A~%" sk)))
      (when (preset-rules p)
        (format s "  rules:~%")
        (dolist (r (preset-rules p))
          (format s "    - ~A~%" (if (symbolp r) (string-downcase (symbol-name r)) r))))
      (when (preset-requires-stack p)
        (format s "  requires:~%")
        (dolist (req (preset-requires-stack p))
          (format s "    - ~A~%" req))))))

(defun preset-to-yaml-string (p)
  "Convert a single preset to a YAML string."
  (with-output-to-string (s)
    (format s "name: ~A~%active: ~A~%description: ~A~%"
            (preset-name-string p)
            (if (preset-active-p p) "true" "false")
            (or (preset-description p) ""))
    (when (preset-extends p)
      (format s "extends: ~A~%" (let ((ext (preset-extends p)))
                                  (if (symbolp ext) (string-downcase (symbol-name ext)) ext))))
    (when (preset-files p)
      (format s "files:~%")
      (dolist (f (preset-files p))
        (format s "  - ~A~%" f)))
    (when (preset-docs p)
      (format s "docs:~%")
      (dolist (d (preset-docs p))
        (format s "  - ~A~%" d)))
    (when (preset-skills p)
      (format s "skills:~%")
      (dolist (sk (preset-skills p))
        (format s "  - ~A~%" sk)))
    (when (preset-rules p)
      (format s "rules:~%")
      (dolist (r (preset-rules p))
        (format s "  - ~A~%" (if (symbolp r) (string-downcase (symbol-name r)) r))))
    (when (preset-requires-stack p)
      (format s "requires:~%")
      (dolist (req (preset-requires-stack p))
        (format s "  - ~A~%" req)))))

(defun presets-to-xml (presets)
  "Convert a list of presets to an XML string."
  (with-output-to-string (s)
    (format s "<presets>~%")
    (dolist (p presets)
      (format s "  <preset>~%")
      (format s "    <name>~A</name>~%" (preset-name-string p))
      (format s "    <active>~A</active>~%" (if (preset-active-p p) "true" "false"))
      (format s "    <description>~A</description>~%" (or (preset-description p) ""))
      (when (preset-extends p)
        (format s "    <extends>~A</extends>~%" (let ((ext (preset-extends p)))
                                                  (if (symbolp ext) (string-downcase (symbol-name ext)) ext))))
      (when (preset-files p)
        (format s "    <files>~%")
        (dolist (f (preset-files p))
          (format s "      <file>~A</file>~%" f))
        (format s "    </files>~%"))
      (when (preset-docs p)
        (format s "    <docs>~%")
        (dolist (d (preset-docs p))
          (format s "      <doc>~A</doc>~%" d))
        (format s "    </docs>~%"))
      (when (preset-skills p)
        (format s "    <skills>~%")
        (dolist (sk (preset-skills p))
          (format s "      <skill>~A</skill>~%" sk))
        (format s "    </skills>~%"))
      (when (preset-rules p)
        (format s "    <rules>~%")
        (dolist (r (preset-rules p))
          (format s "      <rule>~A</rule>~%" (if (symbolp r) (string-downcase (symbol-name r)) r)))
        (format s "    </rules>~%"))
      (when (preset-requires-stack p)
        (format s "    <requires>~%")
        (dolist (req (preset-requires-stack p))
          (format s "      <require>~A</require>~%" req))
        (format s "    </requires>~%"))
      (format s "  </preset>~%"))
    (format s "</presets>~%")))

(defun preset-to-xml-string (p)
  "Convert a single preset to an XML string."
  (with-output-to-string (s)
    (format s "<preset>~%")
    (format s "  <name>~A</name>~%" (preset-name-string p))
    (format s "  <active>~A</active>~%" (if (preset-active-p p) "true" "false"))
    (format s "  <description>~A</description>~%" (or (preset-description p) ""))
    (when (preset-extends p)
      (format s "  <extends>~A</extends>~%" (let ((ext (preset-extends p)))
                                                (if (symbolp ext) (string-downcase (symbol-name ext)) ext))))
    (when (preset-files p)
      (format s "  <files>~%")
      (dolist (f (preset-files p))
        (format s "    <file>~A</file>~%" f))
      (format s "  </files>~%"))
    (when (preset-docs p)
      (format s "  <docs>~%")
      (dolist (d (preset-docs p))
        (format s "    <doc>~A</doc>~%" d))
      (format s "  </docs>~%"))
    (when (preset-skills p)
      (format s "  <skills>~%")
      (dolist (sk (preset-skills p))
        (format s "    <skill>~A</skill>~%" sk))
      (format s "  </skills>~%"))
    (when (preset-rules p)
      (format s "  <rules>~%")
      (dolist (r (preset-rules p))
        (format s "    <rule>~A</rule>~%" (if (symbolp r) (string-downcase (symbol-name r)) r)))
      (format s "  </rules>~%"))
    (when (preset-requires-stack p)
      (format s "  <requires>~%")
      (dolist (req (preset-requires-stack p))
        (format s "    <require>~A</require>~%" req))
      (format s "  </requires>~%"))
    (format s "</preset>~%")))

(defun presets-to-markdown (presets)
  "Convert a list of presets to a Markdown string."
  (with-output-to-string (s)
    (dolist (p presets)
      (format s "## ~A~A~%~%"
              (preset-name-string p)
              (if (preset-active-p p) " [ACTIVE]" ""))
      (format s "*Description:* ~A~%~%" (or (preset-description p) "(no description)"))
      (when (preset-extends p)
        (format s "*Extends:* ~A~%~%" (let ((ext (preset-extends p)))
                                        (if (symbolp ext) (string-downcase (symbol-name ext)) ext))))
      (when (preset-files p)
        (format s "*Files:*~%")
        (dolist (f (preset-files p))
          (format s "  - ~A~%" f))
        (terpri s))
      (when (preset-docs p)
        (format s "*Docs:*~%")
        (dolist (d (preset-docs p))
          (format s "  - ~A~%" d))
        (terpri s))
      (when (preset-skills p)
        (format s "*Skills:*~%")
        (dolist (sk (preset-skills p))
          (format s "  - ~A~%" sk))
        (terpri s))
      (when (preset-rules p)
        (format s "*Rules:*~%")
        (dolist (r (preset-rules p))
          (format s "  - ~A~%" (if (symbolp r) (string-downcase (symbol-name r)) r)))
        (terpri s))
      (when (preset-requires-stack p)
        (format s "*Requires stack:*~%")
        (dolist (req (preset-requires-stack p))
          (format s "  - ~A~%" req))
        (terpri s)))))

(defun presets-to-org-mode (presets)
  "Convert a list of presets to an Org-mode string."
  (with-output-to-string (s)
    (dolist (p presets)
      (format s "* ~A~A~%~%"
              (preset-name-string p)
              (if (preset-active-p p) " [ACTIVE]" ""))
      (format s "Description: ~A~%~%" (or (preset-description p) "(no description)"))
      (when (preset-extends p)
        (format s "Extends: ~A~%~%" (let ((ext (preset-extends p)))
                                      (if (symbolp ext) (string-downcase (symbol-name ext)) ext))))
      (when (preset-files p)
        (format s "Files:~%")
        (dolist (f (preset-files p))
          (format s "  - ~A~%" f))
        (terpri s))
      (when (preset-docs p)
        (format s "Docs:~%")
        (dolist (d (preset-docs p))
          (format s "  - ~A~%" d))
        (terpri s))
      (when (preset-skills p)
        (format s "Skills:~%")
        (dolist (sk (preset-skills p))
          (format s "  - ~A~%" sk))
        (terpri s))
      (when (preset-rules p)
        (format s "Rules:~%")
        (dolist (r (preset-rules p))
          (format s "  - ~A~%" (if (symbolp r) (string-downcase (symbol-name r)) r)))
        (terpri s))
      (when (preset-requires-stack p)
        (format s "Requires stack:~%")
        (dolist (req (preset-requires-stack p))
          (format s "  - ~A~%" req))
        (terpri s)))))

(defun get-show-preset (args)
  "Extract the preset object from arguments, or nil."
  (let* ((clean-args (remove-if (lambda (x) (str:starts-with-p "-" x)) args))
         (name (first clean-args)))
    (when name
      (get-preset name))))

(define-command preset (args)
  "Load, manage, or list context presets. A preset is a reusable context configuration containing files, rules, documentation, and skills.
Usage: /preset              - List available presets
       /preset <name>       - Load a preset by name
       /preset.load <name>  - Load a preset
       /preset.unload <name>- Unload an active preset
       /preset.active       - List active presets
       /preset.show <name>  - Show details of a preset"
  (cond
    (args (preset/load (first args)))
    ((and *tui-running* (list-presets))
     (let ((selected (fuzzy-select (list-presets))))
       (when selected (preset/load selected))))
    (t
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
  :json (lambda (args)
          (declare (ignore args))
          (to-json (coerce (loop for name in (list-presets)
                                 collect (let ((p (get-preset name)))
                                           `(("name" . ,name)
                                             ("active" . ,(if (preset-active-p p) t :false))
                                             ("description" . ,(or (preset-description p) "")))))
                           'vector)))
  :yaml (lambda (args)
          (declare (ignore args))
          (with-output-to-string (s)
            (dolist (name (list-presets))
              (let ((p (get-preset name)))
                (format s "- name: ~A~%  active: ~A~%  description: ~A~%"
                        name
                        (if (preset-active-p p) "true" "false")
                        (or (preset-description p) ""))))))
  :xml (lambda (args)
         (declare (ignore args))
         (with-output-to-string (s)
           (format s "<presets>~%")
           (dolist (name (list-presets))
             (let ((p (get-preset name)))
               (format s "  <preset>~%    <name>~A</name>~%    <active>~A</active>~%    <description>~A</description>~%  </preset>~%"
                       name
                       (if (preset-active-p p) "true" "false")
                       (or (preset-description p) ""))))
           (format s "</presets>~%")))
  :markdown (lambda (args)
              (declare (ignore args))
              (with-output-to-string (s)
                (format s "| Name | Active | Description |~%|---|---|---|~%")
                (dolist (name (list-presets))
                  (let ((p (get-preset name)))
                    (format s "| ~A | ~A | ~A |~%"
                            name
                            (if (preset-active-p p) "yes" "no")
                            (or (preset-description p) ""))))))
  :org-mode (lambda (args)
              (declare (ignore args))
              (with-output-to-string (s)
                (format s "| Name | Active | Description |~%|---|---|---|~%")
                (dolist (name (list-presets))
                  (let ((p (get-preset name)))
                    (format s "| ~A | ~A | ~A |~%"
                            name
                            (if (preset-active-p p) "yes" "no")
                            (or (preset-description p) "")))))))

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
        (format t "~&No presets defined.~%")))
  :json (lambda (args)
          (declare (ignore args))
          (presets-to-json (mapcar #'get-preset (list-presets))))
  :yaml (lambda (args)
          (declare (ignore args))
          (presets-to-yaml (mapcar #'get-preset (list-presets))))
  :xml (lambda (args)
         (declare (ignore args))
         (presets-to-xml (mapcar #'get-preset (list-presets))))
  :markdown (lambda (args)
              (declare (ignore args))
              (presets-to-markdown (mapcar #'get-preset (list-presets))))
  :org-mode (lambda (args)
              (declare (ignore args))
              (presets-to-org-mode (mapcar #'get-preset (list-presets)))))

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
      (format t "~&Usage: /preset.show <name>~%"))
  :json (lambda (args)
          (let ((p (get-show-preset args)))
            (if p
                (to-json (preset-to-alist p))
                "{}")))
  :yaml (lambda (args)
          (let ((p (get-show-preset args)))
            (if p
                (preset-to-yaml-string p)
                "")))
  :xml (lambda (args)
         (let ((p (get-show-preset args)))
           (if p
               (preset-to-xml-string p)
               "")))
  :markdown (lambda (args)
              (let ((p (get-show-preset args)))
                (if p
                    (presets-to-markdown (list p))
                    "")))
  :org-mode (lambda (args)
              (let ((p (get-show-preset args)))
                (if p
                    (presets-to-org-mode (list p))
                    ""))))

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
      (format t "~&No active presets.~%"))
  :json (lambda (args)
          (declare (ignore args))
          (presets-to-json (mapcar #'get-preset *active-presets*)))
  :yaml (lambda (args)
          (declare (ignore args))
          (presets-to-yaml (mapcar #'get-preset *active-presets*)))
  :xml (lambda (args)
         (declare (ignore args))
         (presets-to-xml (mapcar #'get-preset *active-presets*)))
  :markdown (lambda (args)
              (declare (ignore args))
              (presets-to-markdown (mapcar #'get-preset *active-presets*)))
  :org-mode (lambda (args)
              (declare (ignore args))
              (presets-to-org-mode (mapcar #'get-preset *active-presets*))))

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

;;* cli Sub-commands

(define-sub-command preset (args)
  "Manage context presets.
Usage: hactar preset list
       hactar preset load <name>
       hactar preset show <name>"
  (let ((subcommand (first args))
        (rest-args (rest args)))
    (cond
      ((or (null subcommand) (string= subcommand "list"))
       (funcall (first (gethash "/preset.list" *commands*)) nil))
      ((string= subcommand "load")
       (when rest-args
         (preset/load (first rest-args))))
      ((string= subcommand "show")
       (when rest-args
         (funcall (first (gethash "/preset.show" *commands*)) rest-args)))
      (t
       (format t "~&Unknown preset command: ~A~%" subcommand)))))

;;* Hactar Presets
;; TODO move to hactar-mode
(defpreset core
  "Hactar core development files"
  :files ("state.lisp"
          "hactar.lisp"
          "context.lisp"
          "commands.lisp")
  :docs ("hactar-internals"))

(defpreset minimal
  "Minimal context - just essential files"
  :files ()
  :setup (lambda ()
           (format t "~&Minimal preset loaded. Context is empty.~%")))
