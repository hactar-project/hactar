;;* Feature System — highest-level composition unit in Hactar
(in-package :hactar)

;;** Exports

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(deffeature
            activate-feature
            deactivate-feature
            get-feature
            list-features
            list-active-features-summary
            *feature-registry*
            *active-features*)))

;;** Structs

(defstruct feature-definition
  "A feature bundles skills, docs, rules, entities, generators, and patterns."
  (name nil :type (or null symbol))
  (description nil :type (or null string))
  (variants (make-hash-table :test 'equal) :type hash-table)
  (active-p nil :type boolean)
  (active-variant nil :type (or null keyword)))

(defstruct feature-variant
  "A specific variant/configuration of a feature."
  (name nil :type (or null keyword))
  (description nil :type (or null string))
  (inherits nil :type (or null keyword))
  (skills '() :type list)
  (docs '() :type list)
  (rules nil :type (or null string))
  (entities '() :type list)
  (generators '() :type list)
  (patterns '() :type list)
  (packages '() :type list)
  (tags '() :type list)
  (env '() :type list)
  (on-activate nil :type (or null function))
  (on-deactivate nil :type (or null function)))

;;** Registry and State

(defvar *feature-registry* (make-hash-table :test 'equal)
  "All defined features, keyed by lowercase name string.")

(defvar *active-features* (make-hash-table :test 'equal)
  "Currently active features. Keys are name strings, values are plists with
   :variant (keyword), :resolved (merged feature-variant), :entity-ids (list),
   :rule-keys (list), :generator-names (list), :pattern-names (list).")

;;** Variant Resolution

(defun %resolve-feature-variant (feature-def variant-key)
  "Resolve a variant by following :inherits chains and merging fields.
   Child overrides scalar fields; lists are appended (child after parent)."
  (let* ((variants (feature-definition-variants feature-def))
         (variant (gethash variant-key variants)))
    (unless variant
      (return-from %resolve-feature-variant nil))
    (if (feature-variant-inherits variant)
        (let ((parent (%resolve-feature-variant feature-def (feature-variant-inherits variant))))
          (if parent
              (%merge-variants parent variant)
              variant))
        variant)))

(defun %merge-variants (parent child)
  "Merge two feature-variants. Child overrides scalars; lists are appended."
  (make-feature-variant
   :name (feature-variant-name child)
   :description (or (feature-variant-description child)
                    (feature-variant-description parent))
   :inherits nil ; Already resolved
   :skills (append (feature-variant-skills parent)
                   (remove-if (lambda (s) (member s (feature-variant-skills parent)))
                              (feature-variant-skills child)))
   :docs (append (feature-variant-docs parent)
                 (remove-if (lambda (d) (member d (feature-variant-docs parent) :test #'equal))
                            (feature-variant-docs child)))
   :rules (let ((pr (feature-variant-rules parent))
                (cr (feature-variant-rules child)))
            (cond
              ((and pr cr) (format nil "~A~%~%~A" pr cr))
              (cr cr)
              (t pr)))
   :entities (append (feature-variant-entities parent)
                     (feature-variant-entities child))
   :generators (append (feature-variant-generators parent)
                       (remove-if (lambda (g) (member g (feature-variant-generators parent)))
                                  (feature-variant-generators child)))
   :patterns (append (feature-variant-patterns parent)
                     (remove-if (lambda (p) (member p (feature-variant-patterns parent)))
                                (feature-variant-patterns child)))
   :packages (append (feature-variant-packages parent)
                     (remove-if (lambda (p) (member p (feature-variant-packages parent) :test #'string=))
                                (feature-variant-packages child)))
   :tags (append (feature-variant-tags parent)
                 (remove-if (lambda (tag) (member tag (feature-variant-tags parent)))
                            (feature-variant-tags child)))
   :env (append (feature-variant-env parent)
                (feature-variant-env child))
   :on-activate (or (feature-variant-on-activate child)
                    (feature-variant-on-activate parent))
   :on-deactivate (or (feature-variant-on-deactivate child)
                      (feature-variant-on-deactivate parent))))

;;** deffeature macro

(defmacro deffeature (name description &body body)
  "Define a feature with variants.

   Usage:
   (deffeature auth
     \"Authentication and authorization\"
     :variants
     ((:default
       :description \"JWT auth\"
       :skills (:auth-patterns)
       :rules \"Always hash passwords...\"
       :entities ((:type frameworkroute :name \"/login\" ...))
       :generators (:auth-middleware)
       :patterns (:auth-guard)
       :packages (\"bcrypt\" \"jsonwebtoken\")
       :tags (:authenticated))
      (:github
       :description \"GitHub OAuth\"
       :inherits :default
       :skills (:oauth-patterns)
       :packages (\"passport-github2\")
       :env ((\"GITHUB_CLIENT_ID\" . \"...\")
             (\"GITHUB_CLIENT_SECRET\" . \"...\")))))"
  (let* ((variant-specs (getf body :variants))
         (name-str (string-downcase (symbol-name name))))
    `(progn
       (let ((fdef (make-feature-definition
                    :name ',name
                    :description ,description)))
         ,@(mapcar (lambda (vspec)
                     (let ((vname (first vspec))
                           (vbody (rest vspec)))
                       `(setf (gethash ,vname (feature-definition-variants fdef))
                              (make-feature-variant
                               :name ,vname
                               :description ,(getf vbody :description)
                               :inherits ,(getf vbody :inherits)
                               :skills ',(getf vbody :skills)
                               :docs ',(getf vbody :docs)
                               :rules ,(getf vbody :rules)
                               :entities ',(getf vbody :entities)
                               :generators ',(getf vbody :generators)
                               :patterns ',(getf vbody :patterns)
                               :packages ',(getf vbody :packages)
                               :tags ',(getf vbody :tags)
                               :env ',(getf vbody :env)
                               :on-activate ,(getf vbody :on-activate)
                               :on-deactivate ,(getf vbody :on-deactivate)))))
                   variant-specs)
         (setf (gethash ,name-str *feature-registry*) fdef)
         ',name))))

;;** Activation

(defun activate-feature (name &key variant dry-run no-install)
  "Activate a feature by name with an optional variant.
   NAME: string or symbol.
   VARIANT: keyword (default :default).
   DRY-RUN: if T, only preview what would happen.
   NO-INSTALL: if T, skip package installation prompt."
  (let* ((name-str (if (stringp name) (string-downcase name)
                       (string-downcase (symbol-name name))))
         (variant-key (or variant :default))
         (fdef (gethash name-str *feature-registry*)))

    (unless fdef
      (format t "~&Unknown feature: ~A~%" name-str)
      (return-from activate-feature nil))

    ;; Deactivate if already active
    (when (gethash name-str *active-features*)
      (unless dry-run
        (deactivate-feature name-str)))

    (let ((resolved (%resolve-feature-variant fdef variant-key)))
      (unless resolved
        (format t "~&Unknown variant ~A for feature ~A~%" variant-key name-str)
        (format t "  Available variants: ~{~A~^, ~}~%"
                (let ((keys '()))
                  (maphash (lambda (k v) (declare (ignore v)) (push k keys))
                           (feature-definition-variants fdef))
                  (nreverse keys)))
        (return-from activate-feature nil))

      (if dry-run
          (%feature-dry-run name-str variant-key resolved)
          (%feature-activate name-str variant-key resolved fdef no-install)))))

(defun %feature-dry-run (name-str variant-key resolved)
  "Preview what feature activation would do."
  (format t "~&[DRY RUN] Would activate feature: ~A (variant: ~A)~%" name-str variant-key)
  (when (feature-variant-skills resolved)
    (format t "  Would load skills: ~{~A~^, ~}~%"
            (mapcar (lambda (s) (string-downcase (symbol-name s)))
                    (feature-variant-skills resolved))))
  (when (feature-variant-rules resolved)
    (format t "  Would add rules to system prompt (~A chars)~%"
            (length (feature-variant-rules resolved))))
  (when (feature-variant-entities resolved)
    (format t "  Would create ~A entities:~%" (length (feature-variant-entities resolved)))
    (dolist (espec (feature-variant-entities resolved))
      (let ((etype (getf espec :type))
            (ename (getf espec :name))
            (emethod (getf espec :method))
            (etags (getf espec :tags)))
        (format t "    - ~A: ~A~@[ (~A)~]~@[ [~{~A~^, ~}]~]~%"
                etype ename emethod etags))))
  (when (feature-variant-generators resolved)
    (format t "  Would activate generators: ~{~A~^, ~}~%"
            (mapcar (lambda (g) (string-downcase (symbol-name g)))
                    (feature-variant-generators resolved))))
  (when (feature-variant-patterns resolved)
    (format t "  Would activate patterns: ~{~A~^, ~}~%"
            (mapcar (lambda (p) (string-downcase (symbol-name p)))
                    (feature-variant-patterns resolved))))
  (when (feature-variant-packages resolved)
    (format t "  Would need packages: ~{~A~^, ~}~%"
            (feature-variant-packages resolved)))
  (when (feature-variant-env resolved)
    (format t "  Required env vars: ~{~A~^, ~}~%"
            (mapcar #'car (feature-variant-env resolved))))
  t)

(defun %feature-activate (name-str variant-key resolved fdef no-install)
  "Actually activate a feature."
  (format t "~&Activating feature: ~A (variant: ~A)~%" name-str variant-key)

  (let ((entity-ids '())
        (rule-keys '())
        (activated-generators '())
        (activated-patterns '()))

    ;; 1. Load skills
    (when (feature-variant-skills resolved)
      (dolist (skill-name (feature-variant-skills resolved))
        (let ((skill (get-skill skill-name)))
          (if skill
              (progn
                (load-skill-into-context skill)
                (format t "  ✓ Loaded skill: ~A~%"
                        (string-downcase (symbol-name skill-name))))
              (format t "  ⚠ Skill not found: ~A~%"
                      (string-downcase (symbol-name skill-name)))))))

    ;; 2. Add docs to context
    (when (feature-variant-docs resolved)
      (dolist (doc-ref (feature-variant-docs resolved))
        (when (stringp doc-ref)
          (handler-case
              (progn
                (add-file-to-context doc-ref)
                (format t "  ✓ Added doc: ~A~%" doc-ref))
            (error (e)
              (format t "  ⚠ Could not add doc ~A: ~A~%" doc-ref e))))))

    ;; 3. Inject rules into *active-rules*
    (when (feature-variant-rules resolved)
      (let ((rule-key (format nil "feature/~A/rules" name-str)))
        (setf (gethash rule-key *active-rules*)
              (feature-variant-rules resolved))
        (push rule-key rule-keys)
        (format t "  ✓ Added rules to system prompt~%")))

    ;; 4. Create entity instances
    (when (feature-variant-entities resolved)
      (format t "  ✓ Created entities:~%")
      (dolist (espec (feature-variant-entities resolved))
        (let* ((etype (getf espec :type))
               (ename (getf espec :name))
               (emethod (getf espec :method))
               (etags (getf espec :tags))
               (efields (getf espec :fields))
               (create-args (append (list :tags etags)
                                    (when emethod (list :method emethod))
                                    (when efields (list :fields efields)))))
          (handler-case
              (let ((instance (apply #'entity/create etype ename create-args)))
                (when instance
                  (push (entity-instance-id instance) entity-ids)
                  (format t "    - ~A: ~A~@[ (~A)~]~%" etype ename emethod)))
            (error (e)
              (format t "    ⚠ Failed to create ~A ~A: ~A~%" etype ename e))))))

    ;; 5. Activate generators
    (when (feature-variant-generators resolved)
      (dolist (gen-name (feature-variant-generators resolved))
        (let* ((gen-name-str (string-downcase (symbol-name gen-name)))
               (gen-list (gethash gen-name-str *generators*)))
          (if gen-list
              (dolist (gen gen-list)
                (when (or (null (generator-when-fn gen))
                          (funcall (generator-when-fn gen)))
                  (activate-generator gen)
                  (push gen-name-str activated-generators)))
              (format t "  ⚠ Generator not found: ~A~%" gen-name-str))))
      (when activated-generators
        (format t "  ✓ Activated generators: ~{~A~^, ~}~%"
                (nreverse (copy-list activated-generators)))))

    ;; 6. Activate patterns
    (when (feature-variant-patterns resolved)
      (dolist (pat-name (feature-variant-patterns resolved))
        (let* ((pat-name-str (string-downcase (symbol-name pat-name)))
               (pat-list (gethash pat-name-str *patterns*)))
          (if pat-list
              (dolist (pat pat-list)
                (when (or (null (pattern-when-fn pat))
                          (funcall (pattern-when-fn pat)))
                  (activate-pattern pat)
                  (push pat-name-str activated-patterns)))
              (format t "  ⚠ Pattern not found: ~A~%" pat-name-str))))
      (when activated-patterns
        (format t "  ✓ Activated patterns: ~{~A~^, ~}~%"
                (nreverse (copy-list activated-patterns)))))

    ;; 7. Report env vars
    (when (feature-variant-env resolved)
      (format t "  ⚠ Required environment variables:~%")
      (dolist (env-pair (feature-variant-env resolved))
        (format t "    - ~A: ~A~%" (car env-pair) (cdr env-pair))))

    ;; 8. Handle packages
    (when (and (feature-variant-packages resolved) (not no-install))
      (%feature-handle-packages (feature-variant-packages resolved)))

    ;; 9. Run on-activate hook
    (when (feature-variant-on-activate resolved)
      (handler-case (funcall (feature-variant-on-activate resolved))
        (error (e) (format t "  ⚠ on-activate hook error: ~A~%" e))))

    ;; 10. Store activation state
    (setf (feature-definition-active-p fdef) t)
    (setf (feature-definition-active-variant fdef) variant-key)
    (setf (gethash name-str *active-features*)
          (list :variant variant-key
                :resolved resolved
                :entity-ids entity-ids
                :rule-keys rule-keys
                :generator-names activated-generators
                :pattern-names activated-patterns))

    (format t "~&Feature '~A' is now active.~%" name-str)
    t))

(defun %feature-handle-packages (packages)
  "Check for missing packages and offer to install them."
  (when (and packages *repo-root*)
    ;; Try to detect which packages are already installed via package.json
    (let* ((pkg-json-path (merge-pathnames "package.json" *repo-root*))
           (installed-packages
             (when (probe-file pkg-json-path)
               (handler-case
                   (let* ((content (uiop:read-file-string pkg-json-path))
                          (parsed (cl-json:decode-json-from-string content))
                          (deps (mapcar #'car (cdr (assoc :dependencies parsed))))
                          (dev-deps (mapcar #'car (cdr (assoc :dev-dependencies parsed)))))
                     (append (mapcar (lambda (s) (string-downcase (string s))) deps)
                             (mapcar (lambda (s) (string-downcase (string s))) dev-deps)))
                 (error () nil))))
           (missing (remove-if (lambda (pkg)
                                 (member pkg installed-packages :test #'string=))
                               packages)))
      (cond
        ((null missing)
         (format t "  ✓ All required packages already installed~%"))
        (t
         (format t "  ⚠ Missing packages: ~{~A~^, ~}~%" missing)
         (when (confirm-action "    Install now?")
           (let* ((pm (infer-package-manager))
                  (install-cmd (case pm
                                 (:bun "bun add")
                                 (:pnpm "pnpm add")
                                 (:yarn "yarn add")
                                 (t "npm install")))
                  (cmd (format nil "~A ~{~A~^ ~}" install-cmd missing)))
             (format t "  Installing: ~A~%" cmd)
             (multiple-value-bind (output error-output exit-code)
                 (uiop:run-program cmd
                                   :directory *repo-root*
                                   :output :string
                                   :error-output :string
                                   :ignore-error-status t)
               (declare (ignore output))
               (if (zerop exit-code)
                   (format t "  ✓ Installed ~A package~:P~%" (length missing))
                   (format t "  ✗ Installation failed: ~A~%" error-output))))))))))

;;** Deactivation

(defun deactivate-feature (name)
  "Deactivate an active feature, cleaning up all its effects."
  (let* ((name-str (if (stringp name) (string-downcase name)
                       (string-downcase (symbol-name name))))
         (fdef (gethash name-str *feature-registry*))
         (state (gethash name-str *active-features*)))

    (unless state
      (format t "~&Feature '~A' is not active.~%" name-str)
      (return-from deactivate-feature nil))

    (format t "~&Deactivating feature: ~A~%" name-str)

    (let ((resolved (getf state :resolved))
          (entity-ids (getf state :entity-ids))
          (rule-keys (getf state :rule-keys))
          (generator-names (getf state :generator-names))
          (pattern-names (getf state :pattern-names)))

      ;; 1. Remove skills from active rules
      (when resolved
        (dolist (skill-name (feature-variant-skills resolved))
          (remhash skill-name *active-rules*)))
      (format t "  ✓ Removed skills from context~%")

      ;; 2. Remove rules
      (dolist (rk rule-keys)
        (remhash rk *active-rules*))
      (format t "  ✓ Removed rules from system prompt~%")

      ;; 3. Deactivate generators
      (dolist (gen-name-str generator-names)
        (let ((gen-list (gethash gen-name-str *active-generators*)))
          (dolist (gen gen-list)
            (deactivate-generator gen))))
      (when generator-names
        (format t "  ✓ Deactivated generators~%"))

      ;; 4. Deactivate patterns
      (dolist (pat-name-str pattern-names)
        (let ((pat-list (gethash pat-name-str *active-patterns*)))
          (dolist (pat pat-list)
            (deactivate-pattern pat))))
      (when pattern-names
        (format t "  ✓ Deactivated patterns~%"))

      ;; 5. Remove entity instances
      (when (and resolved (feature-variant-entities resolved))
        (dolist (espec (feature-variant-entities resolved))
          (let ((etype (getf espec :type)))
            ;; Remove entities whose IDs we tracked
            (dolist (eid entity-ids)
              (delete-entity-instance etype eid)))))
      (when entity-ids
        (format t "  ✓ Cleared entity instances~%"))

      ;; 6. Run on-deactivate hook
      (when (and resolved (feature-variant-on-deactivate resolved))
        (handler-case (funcall (feature-variant-on-deactivate resolved))
          (error (e) (format t "  ⚠ on-deactivate hook error: ~A~%" e))))

      ;; 7. Clean up state
      (when fdef
        (setf (feature-definition-active-p fdef) nil)
        (setf (feature-definition-active-variant fdef) nil))
      (remhash name-str *active-features*)

      (format t "Feature '~A' deactivated.~%" name-str)
      t)))

;;** Query functions

(defun get-feature (name)
  "Get a feature definition by name."
  (gethash (if (stringp name) (string-downcase name)
               (string-downcase (symbol-name name)))
           *feature-registry*))

(defun list-features ()
  "Return a list of all defined features as plists."
  (let ((result '()))
    (maphash (lambda (name-str fdef)
               (let ((variants '()))
                 (maphash (lambda (k v)
                            (declare (ignore v))
                            (push k variants))
                          (feature-definition-variants fdef))
                 (push (list :name name-str
                             :description (feature-definition-description fdef)
                             :active (feature-definition-active-p fdef)
                             :active-variant (feature-definition-active-variant fdef)
                             :variants (nreverse variants))
                       result)))
             *feature-registry*)
    (sort result #'string< :key (lambda (p) (getf p :name)))))

(defun list-active-features-summary ()
  "Return a summary string of active features for display."
  (let ((summaries '()))
    (maphash (lambda (name-str state)
               (push (format nil "~A (~A)" name-str (getf state :variant))
                     summaries))
             *active-features*)
    (nreverse summaries)))

;;** Active feature rules for system prompt

(defun get-active-feature-rules ()
  "Collect all active feature rules into a string for the system prompt."
  (let ((rules '()))
    (maphash (lambda (name-str state)
               (declare (ignore name-str))
               (let* ((resolved (getf state :resolved))
                      (rule-text (when resolved (feature-variant-rules resolved))))
                 (when (and rule-text (> (length rule-text) 0))
                   (push rule-text rules))))
             *active-features*)
    (if rules
        (format nil "~%## Active Feature Rules~%~{~A~%~}" (nreverse rules))
        "")))

;;** Slash command

(define-command feature (args)
  "Activate or manage features.
Usage:
  /feature <name>              - Activate feature with default variant
  /feature <name> --<variant>  - Activate feature with specific variant
  /feature list                - List all available features
  /feature active              - List currently active features
  /feature off <name>          - Deactivate a feature
  /feature show <name>         - Show feature details
  /feature --dry-run <name>    - Preview what activation would do"
  (let ((dry-run (member "--dry-run" args :test #'string=))
        (no-install (member "--no-install" args :test #'string=))
        (clean-args (remove-if (lambda (a) (member a '("--dry-run" "--no-install") :test #'string=)) args)))
    (cond
      ;; /feature list
      ((and clean-args (string-equal (first clean-args) "list"))
       (%feature-cmd-list))

      ;; /feature active
      ((and clean-args (string-equal (first clean-args) "active"))
       (%feature-cmd-active))

      ;; /feature off <name>
      ((and clean-args (string-equal (first clean-args) "off"))
       (if (second clean-args)
           (deactivate-feature (second clean-args))
           (format t "Usage: /feature off <name>~%")))

      ;; /feature show <name>
      ((and clean-args (string-equal (first clean-args) "show"))
       (if (second clean-args)
           (%feature-cmd-show (second clean-args))
           (format t "Usage: /feature show <name>~%")))

      ;; /feature <name> [--variant]
      (clean-args
       (let* ((feature-name (first clean-args))
              (variant-arg (find-if (lambda (a) (and (str:starts-with? "--" a)
                                                     (not (string= a "--dry-run"))
                                                     (not (string= a "--no-install"))))
                                    args))
              (variant-key (when variant-arg
                             (intern (string-upcase (subseq variant-arg 2)) :keyword))))
         (activate-feature feature-name
                           :variant variant-key
                           :dry-run dry-run
                           :no-install no-install)))

      ;; No args
      (t
       (format t "Usage: /feature <name> [--<variant>] [--dry-run] [--no-install]~%")
       (format t "       /feature list | active | off <name> | show <name>~%"))))
  :slash t :sub t
  :acp (lambda (cmd-args)
         (cond
           ((null cmd-args)
            `(("text" . "Usage: /feature <name> [--<variant>]")
              ("data" . (("features" . ,(coerce
                                         (mapcar (lambda (f)
                                                   `(("name" . ,(getf f :name))
                                                     ("description" . ,(getf f :description))
                                                     ("active" . ,(if (getf f :active) t :false))
                                                     ("activeVariant" . ,(or (getf f :active-variant) :null))
                                                     ("variants" . ,(coerce (mapcar
                                                                             (lambda (v) (string-downcase (symbol-name v)))
                                                                             (getf f :variants))
                                                                            'vector))))
                                                 (list-features))
                                         'vector))))))
           ((string-equal (first cmd-args) "list")
            (let ((features (list-features)))
              `(("text" . ,(format nil "~A feature~:P available." (length features)))
                ("data" . ,(coerce
                            (mapcar (lambda (f)
                                      `(("name" . ,(getf f :name))
                                        ("description" . ,(getf f :description))
                                        ("active" . ,(if (getf f :active) t :false))
                                        ("variants" . ,(coerce (mapcar
                                                                (lambda (v) (string-downcase (symbol-name v)))
                                                                (getf f :variants))
                                                               'vector))))
                                    features)
                            'vector)))))
           ((string-equal (first cmd-args) "active")
            (let ((active (list-active-features-summary)))
              `(("text" . ,(if active
                               (format nil "Active features: ~{~A~^, ~}" active)
                               "No active features."))
                ("data" . ,(coerce active 'vector)))))
           ((string-equal (first cmd-args) "off")
            (if (second cmd-args)
                (progn
                  (deactivate-feature (second cmd-args))
                  `(("text" . ,(format nil "Feature '~A' deactivated." (second cmd-args)))))
                `(("text" . "Usage: /feature off <name>"))))
           (t
            (let* ((feature-name (first cmd-args))
                   (variant-arg (find-if (lambda (a) (str:starts-with? "--" a)) (rest cmd-args)))
                   (variant-key (when variant-arg
                                  (intern (string-upcase (subseq variant-arg 2)) :keyword))))
              (let ((output (with-output-to-string (*standard-output*)
                              (activate-feature feature-name :variant variant-key))))
                `(("text" . ,output))))))))

(defun %feature-cmd-list ()
  "Implementation of /feature list."
  (let ((features (list-features)))
    (if features
        (progn
          (format t "~&Available features:~%")
          (dolist (f features)
            (format t "  ~A~A — ~A~%"
                    (getf f :name)
                    (if (getf f :active)
                        (format nil " [ACTIVE: ~A]"
                                (string-downcase (symbol-name (getf f :active-variant))))
                        "")
                    (getf f :description))
            (format t "    Variants: ~{~A~^, ~}~%"
                    (mapcar (lambda (v) (string-downcase (symbol-name v)))
                            (getf f :variants)))))
        (format t "~&No features defined.~%"))))

(defun %feature-cmd-active ()
  "Implementation of /feature active."
  (let ((active (list-active-features-summary)))
    (if active
        (progn
          (format t "~&Active features:~%")
          (dolist (s active)
            (format t "  ~A~%" s)))
        (format t "~&No active features.~%"))))

(defun %feature-cmd-show (name)
  "Implementation of /feature show <name>."
  (let* ((name-str (string-downcase name))
         (fdef (gethash name-str *feature-registry*))
         (state (gethash name-str *active-features*)))
    (unless fdef
      (format t "~&Feature '~A' not found.~%" name-str)
      (return-from %feature-cmd-show nil))
    (format t "~&Feature: ~A~%" name-str)
    (format t "  Description: ~A~%" (feature-definition-description fdef))
    (format t "  Active: ~A~A~%"
            (if (feature-definition-active-p fdef) "yes" "no")
            (if (feature-definition-active-variant fdef)
                (format nil " (variant: ~A)"
                        (string-downcase (symbol-name (feature-definition-active-variant fdef))))
                ""))
    (let ((variants '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k variants))
               (feature-definition-variants fdef))
      (format t "  Variants: ~{~A~^, ~}~%"
              (mapcar (lambda (v) (string-downcase (symbol-name v)))
                      (nreverse variants))))
    (when state
      (let ((resolved (getf state :resolved)))
        (when resolved
          (when (feature-variant-skills resolved)
            (format t "  Skills: ~{~A~^, ~}~%"
                    (mapcar (lambda (s) (string-downcase (symbol-name s)))
                            (feature-variant-skills resolved))))
          (when (feature-variant-entities resolved)
            (format t "  Entities: ~A~%" (length (feature-variant-entities resolved))))
          (when (feature-variant-packages resolved)
            (format t "  Packages: ~{~A~^, ~}~%"
                    (feature-variant-packages resolved)))
          (when (feature-variant-rules resolved)
            (format t "  Rules: ~A active rule block~:P~%"
                    (length (getf state :rule-keys)))))))))
