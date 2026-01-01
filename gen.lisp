;;* Generative commands — Rails-like scaffolding for the LLM era
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defgenerator
            defpattern
            *generators*
            *patterns*
            *generator-activated-hook*
            *pattern-activated-hook*
            list-active-generators
            list-active-patterns
            find-generator
            find-pattern
            activate-generator
            deactivate-generator
            activate-pattern
            deactivate-pattern
            define-framework-analyzer
            get-analyzer
            framework-analyzer-queries)))

(defvar *generators* (make-hash-table :test 'equal)
  "Hash table of all defined generators, keyed by name.")

(defvar *active-generators* (make-hash-table :test 'equal)
  "Hash table of currently active generators, keyed by name.
   Multiple generators with same name can be active; highest priority wins.")

(defvar *patterns* (make-hash-table :test 'equal)
  "Hash table of all defined patterns, keyed by name.")

(defvar *active-patterns* (make-hash-table :test 'equal)
  "Hash table of currently active patterns, keyed by name.")

(nhooks:define-hook-type generator-activated (function (generator) t)
  "Hook run when a generator becomes active.")
(defvar *generator-activated-hook* (make-instance 'hook-generator-activated))

(nhooks:define-hook-type generator-deactivated (function (generator) t)
  "Hook run when a generator becomes inactive.")
(defvar *generator-deactivated-hook* (make-instance 'hook-generator-deactivated))

(nhooks:define-hook-type pattern-activated (function (pattern) t)
  "Hook run when a pattern becomes active.")
(defvar *pattern-activated-hook* (make-instance 'hook-pattern-activated))

(nhooks:define-hook-type pattern-deactivated (function (pattern) t)
  "Hook run when a pattern becomes inactive.")
(defvar *pattern-deactivated-hook* (make-instance 'hook-pattern-deactivated))

(defvar *registries* (make-hash-table :test 'equal)
  "Hash table of defined registries, keyed by type.")

(defvar *recipes* (make-hash-table :test 'equal)
  "Hash table of defined recipes, keyed by name.")

(defvar *gen-history* '()
  "Stack of executed generation operations for undo support.")

(defvar *gen-history-limit* 50
  "Maximum number of generation operations to keep in history.")

(defstruct gen-operation
  "Represents a single atomic generation operation."
  (type nil :type keyword)           ; :create, :modify, :delete, :rename, :install, etc.
  (file nil :type (or null string pathname))
  (content nil :type (or null string))
  (search nil :type (or null string))
  (replace nil :type (or null string))
  (package nil :type (or null string))
  (source nil :type (or null string pathname))
  (target nil :type (or null string pathname))
  (command nil :type (or null string))
  (registry-type nil :type (or null keyword)) ; For :register/:unregister
  (entry nil)                          ; Registry entry data
  (condition nil)                      ; When to execute (:when clause)
  (executed-p nil :type boolean)
  (rollback-data nil)                  ; Data needed to undo this operation
  (timestamp nil :type (or null integer)))

(defstruct gen-result
  "Result of executing a generation plan."
  (success-p nil :type boolean)
  (operations '() :type list)
  (errors '() :type list)
  (files-created '() :type list)
  (files-modified '() :type list)
  (files-deleted '() :type list)
  (message nil :type (or null string)))

(defstruct generator
  "A generator definition that produces operations."
  (name nil :type (or null string symbol))
  (id nil :type (or null string))              ; Unique ID for this generator instance
  (description nil :type (or null string))
  (priority 10 :type integer)                   ; Higher priority = checked first
  (args '() :type list)                         ; Argument specification
  (infer-from-context '() :type list)           ; Things to infer from project
  (operations '() :type list)                   ; Operation templates
  (templates '() :type list)                    ; Associated templates
  (compose '() :type list)                      ; Other generators to call
  ;; Activation control
  (when-fn nil :type (or null function))        ; Predicate: is this generator applicable?
  (hooks nil :type list)                        ; List of (hook-var . handler) to install
  (active-p nil :type boolean))                 ; Is this generator currently active?

(defvar *generator-counter* 0)

(defun generate-generator-id (name)
  "Generate a unique ID for a generator."
  (incf *generator-counter*)
  (format nil "gen-~A-~A"
          (if (stringp name) name (string-downcase (symbol-name name)))
          *generator-counter*))

(defmacro defgenerator (name description &body body)
  "Define a generator for code scaffolding.

   Generators are activated based on :when predicates and :hooks.

   :when - A predicate form evaluated to determine if generator is active
   :hooks - List of (hook-var . handler-fn) that activate this generator
   :priority - Higher priority generators shadow lower priority ones with same name

   Usage:
   ;; Base generator (always active)
   (defgenerator component
     \"Generate a component\"
     :args (name &key props style test)
     :operations (...))

   ;; Conditional generator (active when React detected)
   (defgenerator component
     \"Generate a React component\"
     :when (member \"react\" *stack* :test #'string-equal)
     :priority 10
     :args (name &key props)
     :operations (...))

   ;; Hook-activated generator
   (defgenerator route
     \"Generate a React Router route\"
     :hooks ((*stack-changed-hook*
              (lambda (stack) (member \"react-router\" stack :test #'string-equal))))
     :priority 10
     :operations (...))"
  (let ((when-clause (getf body :when))
        (hooks (getf body :hooks))
        (priority (getf body :priority 10))
        (args (getf body :args))
        (infer (getf body :infer-from-context))
        (ops (getf body :operations))
        (templates (getf body :templates))
        (compose (getf body :compose)))
    (let ((name-str (if (stringp name) name (string-downcase (symbol-name name))))
          (gen-id-var (gensym "GEN-ID")))
      `(let ((,gen-id-var (generate-generator-id ,name-str)))
         (let ((gen (make-generator
                     :name ',name
                     :id ,gen-id-var
                     :description ,description
                     :priority ,priority
                     :args ',args
                     :infer-from-context ',infer
                     :operations ',ops
                     :templates ',templates
                     :compose ',compose
                     :when-fn ,(when when-clause `(lambda () ,when-clause))
                     :hooks ',hooks
                     :active-p nil)))
           (push gen (gethash ,name-str *generators*))
           ,@(when hooks
               (mapcar (lambda (hook-spec)
                         (let ((hook-var (first hook-spec))
                               (handler-form (second hook-spec)))
                           `(install-generator-hook gen ,hook-var ,handler-form)))
                       hooks))
           ,(when when-clause
              `(when (funcall (generator-when-fn gen))
                 (activate-generator gen)))
           ,(unless (or when-clause hooks)
              `(activate-generator gen))
           gen)))))

(defun install-generator-hook (gen hook handler-fn)
  "Install a hook that controls generator activation."
  (let ((wrapper-fn (lambda (&rest args)
                      (when (apply handler-fn args)
                        (activate-generator gen)))))
    (nhooks:add-hook hook
                     (make-instance 'nhooks:handler
                                    :fn wrapper-fn
                                    :name (intern (generator-id gen) :keyword)))))

(defun activate-generator (gen)
  "Mark a generator as active and add to active registry."
  (unless (generator-active-p gen)
    (setf (generator-active-p gen) t)
    (let ((name-str (if (stringp (generator-name gen))
                        (generator-name gen)
                        (string-downcase (symbol-name (generator-name gen))))))
      (push gen (gethash name-str *active-generators*)))
    (nhooks:run-hook *generator-activated-hook* gen)))

(defun deactivate-generator (gen)
  "Mark a generator as inactive and remove from active registry."
  (when (generator-active-p gen)
    (setf (generator-active-p gen) nil)
    (let ((name-str (if (stringp (generator-name gen))
                        (generator-name gen)
                        (string-downcase (symbol-name (generator-name gen))))))
      (setf (gethash name-str *active-generators*)
            (remove gen (gethash name-str *active-generators*) :test #'eq)))
    (nhooks:run-hook *generator-deactivated-hook* gen)))

;;** Pattern Definition

(defstruct pattern
  "A reusable code pattern that can be applied to files."
  (name nil :type (or null string symbol))
  (id nil :type (or null string))              ; Unique ID for this pattern instance
  (description nil :type (or null string))
  (priority 10 :type integer)                   ; Higher priority = checked first
  (requires '() :type list)                     ; Required technologies
  (package nil :type (or null string))          ; Package to install
  (config-schema nil)                           ; Configuration options
  (operations '() :type list)
  ;; Activation control
  (when-fn nil :type (or null function))        ; Predicate: is this pattern applicable?
  (hooks nil :type list)                        ; List of (hook-var . handler) to install
  (active-p nil :type boolean))                 ; Is this pattern currently active?

(defvar *pattern-counter* 0)

(defun generate-pattern-id (name)
  "Generate a unique ID for a pattern."
  (incf *pattern-counter*)
  (format nil "pat-~A-~A"
          (if (stringp name) name (string-downcase (symbol-name name)))
          *pattern-counter*))

(defmacro defpattern (name description &body body)
  "Define a reusable pattern.

   Patterns are activated based on :when predicates and :hooks.

   :when - A predicate form evaluated to determine if pattern is active
   :hooks - List of (hook-var . handler-fn) that activate this pattern
   :priority - Higher priority patterns shadow lower priority ones with same name

   Usage:
   ;; Base pattern (always active)
   (defpattern error-boundary
     \"Add error boundary\"
     :operations (...))

   ;; Conditional pattern
   (defpattern error-boundary
     \"React Error Boundary component\"
     :when (member \"react\" *stack* :test #'string-equal)
     :priority 10
     :package \"react-error-boundary\"
     :operations (...))

   ;; Hook-activated pattern
   (defpattern route-error-boundary
     \"React Router error element\"
     :hooks ((*stack-changed-hook*
              (lambda (stack) (member \"react-router\" stack :test #'string-equal))))
     :priority 15
     :operations (...))"
  (let ((when-clause (getf body :when))
        (hooks (getf body :hooks))
        (priority (getf body :priority 10))
        (requires (getf body :requires))
        (package (getf body :package))
        (config-schema (getf body :config-schema))
        (ops (getf body :operations)))
    (let ((name-str (if (stringp name) name (string-downcase (symbol-name name))))
          (pat-id-var (gensym "PAT-ID")))
      `(let ((,pat-id-var (generate-pattern-id ,name-str)))
         (let ((pat (make-pattern
                     :name ',name
                     :id ,pat-id-var
                     :description ,description
                     :priority ,priority
                     :requires ',requires
                     :package ,package
                     :config-schema ',config-schema
                     :operations ',ops
                     :when-fn ,(when when-clause `(lambda () ,when-clause))
                     :hooks ',hooks
                     :active-p nil)))
           (push pat (gethash ,name-str *patterns*))
           ,@(when hooks
               (mapcar (lambda (hook-spec)
                         (let ((hook-var (first hook-spec))
                               (handler-form (second hook-spec)))
                           `(install-pattern-hook pat ,hook-var ,handler-form)))
                       hooks))
           ,(when when-clause
              `(when (funcall (pattern-when-fn pat))
                 (activate-pattern pat)))
           ,(unless (or when-clause hooks)
              `(activate-pattern pat))
           pat)))))

(defun install-pattern-hook (pat hook handler-fn)
  "Install a hook that controls pattern activation."
  (let ((wrapper-fn (lambda (&rest args)
                      (when (apply handler-fn args)
                        (activate-pattern pat)))))
    (nhooks:add-hook hook
                     (make-instance 'nhooks:handler
                                    :fn wrapper-fn
                                    :name (intern (pattern-id pat) :keyword)))))

(defun activate-pattern (pat)
  "Mark a pattern as active and add to active registry."
  (unless (pattern-active-p pat)
    (setf (pattern-active-p pat) t)
    (let ((name-str (if (stringp (pattern-name pat))
                        (pattern-name pat)
                        (string-downcase (symbol-name (pattern-name pat))))))
      (push pat (gethash name-str *active-patterns*)))
    (nhooks:run-hook *pattern-activated-hook* pat)))

(defun deactivate-pattern (pat)
  "Mark a pattern as inactive and remove from active registry."
  (when (pattern-active-p pat)
    (setf (pattern-active-p pat) nil)
    (let ((name-str (if (stringp (pattern-name pat))
                        (pattern-name pat)
                        (string-downcase (symbol-name (pattern-name pat))))))
      (setf (gethash name-str *active-patterns*)
            (remove pat (gethash name-str *active-patterns*) :test #'eq)))
    (nhooks:run-hook *pattern-deactivated-hook* pat)))

(defstruct registry
  "A registry for tracking things like routes, exports, etc."
  (type nil :type (or null keyword))
  (description nil :type (or null string))
  (file-pattern nil :type (or null string))
  (entry-pattern nil :type (or null string))
  (detect-from-framework-p nil :type boolean)
  (variants '() :type list)
  (add-fn nil :type (or null function))
  (remove-fn nil :type (or null function)))

(defmacro defregistry (type description &body body)
  "Define a registry type for tracking code artifacts."
  (let ((file-pattern (getf body :file-pattern))
        (entry-pattern (getf body :entry-pattern))
        (detect-from-framework (getf body :detect-from-framework))
        (variants (getf body :variants)))
    `(setf (gethash ,type *registries*)
           (make-registry
            :type ,type
            :description ,description
            :file-pattern ,file-pattern
            :entry-pattern ,entry-pattern
            :detect-from-framework-p ,detect-from-framework
            :variants ',variants))))

(defmacro define-framework-analyzer (name description &key detect queries)
  "Define a framework analyzer."
  `(let ((analyzer (make-framework-analyzer
                    :name ',name
                    :description ,description
                    :detect-fn (lambda () ,detect)
                    :queries ,queries)))
     (setf (gethash (if (stringp ',name) ',name (string-downcase (symbol-name ',name)))
                    *framework-analyzers*)
           analyzer)))

(defun find-generator (name)
  "Find the highest-priority active generator for NAME.
   Returns NIL if no active generator exists for that name."
  (let* ((name-str (if (stringp name) name (string-downcase (symbol-name name))))
         (active-list (gethash name-str *active-generators*)))
    (when active-list
      (first (sort (copy-list active-list) #'> :key #'generator-priority)))))

(defun find-pattern (name)
  "Find the highest-priority active pattern for NAME.
   Returns NIL if no active pattern exists for that name."
  (let* ((name-str (if (stringp name) name (string-downcase (symbol-name name))))
         (active-list (gethash name-str *active-patterns*)))
    (when active-list
      (first (sort (copy-list active-list) #'> :key #'pattern-priority)))))

(defun list-active-generators ()
  "Return list of all currently active generators.
   For each unique name, shows the highest-priority active generator."
  (let ((result '()))
    (maphash (lambda (name generators)
               (let ((best (first (sort (copy-list generators) #'>
                                        :key #'generator-priority))))
                 (when best
                   (push (list :name name
                               :id (generator-id best)
                               :description (generator-description best)
                               :priority (generator-priority best))
                         result))))
             *active-generators*)
    (nreverse result)))

(defun list-active-patterns ()
  "Return list of all currently active patterns.
   For each unique name, shows the highest-priority active pattern."
  (let ((result '()))
    (maphash (lambda (name patterns)
               (let ((best (first (sort (copy-list patterns) #'>
                                        :key #'pattern-priority))))
                 (when best
                   (push (list :name name
                               :id (pattern-id best)
                               :description (pattern-description best)
                               :priority (pattern-priority best)
                               :package (pattern-package best))
                         result))))
             *active-patterns*)
    (nreverse result)))

(defun list-all-generators ()
  "Return list of ALL defined generators (active and inactive)."
  (let ((result '()))
    (maphash (lambda (name generators)
               (dolist (gen generators)
                 (push (list :name name
                             :id (generator-id gen)
                             :description (generator-description gen)
                             :priority (generator-priority gen)
                             :active (generator-active-p gen))
                       result)))
             *generators*)
    (nreverse result)))

(defun list-all-patterns ()
  "Return list of ALL defined patterns (active and inactive)."
  (let ((result '()))
    (maphash (lambda (name patterns)
               (dolist (pat patterns)
                 (push (list :name name
                             :id (pattern-id pat)
                             :description (pattern-description pat)
                             :priority (pattern-priority pat)
                             :active (pattern-active-p pat)
                             :package (pattern-package pat))
                       result)))
             *patterns*)
    (nreverse result)))

(defun refresh-generators ()
  "Re-evaluate :when predicates for all generators and update activation status."
  (maphash (lambda (name generators)
             (declare (ignore name))
             (dolist (gen generators)
               (when (generator-when-fn gen)
                 (if (funcall (generator-when-fn gen))
                     (activate-generator gen)
                     (deactivate-generator gen)))))
           *generators*))

(defun refresh-patterns ()
  "Re-evaluate :when predicates for all patterns and update activation status."
  (maphash (lambda (name patterns)
             (declare (ignore name))
             (dolist (pat patterns)
               (when (pattern-when-fn pat)
                 (if (funcall (pattern-when-fn pat))
                     (activate-pattern pat)
                     (deactivate-pattern pat)))))
           *patterns*))

(defun get-template-path (template-name)
  "Find a template by name, searching mode directories and data paths.
   Returns the full path to the template or NIL if not found."
  (let ((search-paths (append *template-search-paths*
                              (list (uiop:subpathname *hactar-data-path* "templates/")
                                    (uiop:subpathname *hactar-config-path* "templates/")))))
    (dolist (base-path search-paths)
      (let ((full-path (merge-pathnames template-name base-path)))
        (when (probe-file full-path)
          (return-from get-template-path full-path))))
    nil))

(defun load-template (template-name)
  "Load and return template content, or NIL if not found."
  (let ((path (get-template-path template-name)))
    (when path
      (uiop:read-file-string path))))

(defun expand-template-string (template vars)
  "Expand a mustache-style template string with VARS.
   VARS is a plist or alist of variable bindings."
  (let ((alist-vars (if (and vars (listp vars) (consp (first vars)))
                        vars  ; Already an alist
                        ;; Convert plist to alist for mustache
                        (loop for (k v) on vars by #'cddr
                              collect (cons k v)))))
    (mustache:render* template alist-vars)))

(defun expand-path-template (path-template vars)
  "Expand ${var} style placeholders in a path template."
  (let ((result path-template))
    (loop for (k v) on vars by #'cddr
          do (let ((placeholder (format nil "${~A}"
                                        (string-downcase
                                         (if (keywordp k)
                                             (symbol-name k)
                                             (string k))))))
               (setf result (cl-ppcre:regex-replace-all
                             (cl-ppcre:quote-meta-chars placeholder)
                             result
                             (or v "")))))
    result))

(defun infer-framework ()
  "Detect the framework being used in the current project."
  (cond
    ;; Check meta-frameworks first (they imply the base framework)
    ((member "nextjs" *stack* :test #'string-equal) :nextjs)
    ((member "remix" *stack* :test #'string-equal) :remix)
    ;; Then check base frameworks
    ((member "react" *stack* :test #'string-equal) :react)
    ((member "vue" *stack* :test #'string-equal) :vue)
    ((member "angular" *stack* :test #'string-equal) :angular)
    ((member "express" *stack* :test #'string-equal) :express)
    ((member "fastify" *stack* :test #'string-equal) :fastify)
    ((member "hono" *stack* :test #'string-equal) :hono)
    (t nil)))

(defun infer-style-system ()
  "Detect the styling system in use."
  (cond
    ((member "tailwind" *stack* :test #'string-equal) :tailwind)
    ((probe-file (merge-pathnames "tailwind.config.js" *repo-root*)) :tailwind)
    ((probe-file (merge-pathnames "tailwind.config.ts" *repo-root*)) :tailwind)
    ((member "styled-components" *stack* :test #'string-equal) :styled-components)
    ((member "css-modules" *stack* :test #'string-equal) :css-modules)
    (t :css)))

(defun infer-test-framework ()
  "Detect the testing framework in use."
  (cond
    ((member "vitest" *stack* :test #'string-equal) :vitest)
    ((member "jest" *stack* :test #'string-equal) :jest)
    ((member "mocha" *stack* :test #'string-equal) :mocha)
    ((probe-file (merge-pathnames "vitest.config.ts" *repo-root*)) :vitest)
    ((probe-file (merge-pathnames "jest.config.js" *repo-root*)) :jest)
    (t nil)))

(defun infer-package-manager ()
  "Detect the package manager in use."
  (cond
    ((probe-file (merge-pathnames "bun.lockb" *repo-root*)) :bun)
    ((probe-file (merge-pathnames "pnpm-lock.yaml" *repo-root*)) :pnpm)
    ((probe-file (merge-pathnames "yarn.lock" *repo-root*)) :yarn)
    ((probe-file (merge-pathnames "package-lock.json" *repo-root*)) :npm)
    (t :npm)))

(defun infer-context-value (key)
  "Infer a context value by key."
  (case key
    (:framework (infer-framework))
    (:style-system (infer-style-system))
    (:test-framework (infer-test-framework))
    (:package-manager (infer-package-manager))
    (:language *language*)
    (t nil)))

(defun build-generator-context (generator args)
  "Build the context variables for a generator execution.
   Combines explicit args with inferred context values."
  (let* ((name (if (and args (stringp (first args))) (first args) "unnamed"))
         (rest-args (if (and args (stringp (first args))) (rest args) args))
         (context (list :name name
                        :name-kebab (kebab-case name)
                        :name-pascal (pascal-case name)
                        :name-camel (camel-case name)
                        :framework (infer-framework)
                        :style-system (infer-style-system)
                        :test-framework (infer-test-framework)
                        :package-manager (infer-package-manager)
                        :language (or *language* "typescript")
                        :stack *stack*
                        :author (or *author* "")
                        :project-name (or *name* ""))))
    (loop for (k v) on rest-args by #'cddr
          do (setf (getf context k) v))
    (dolist (infer-key (generator-infer-from-context generator))
      (unless (getf context infer-key)
        (let ((inferred (infer-context-value infer-key)))
          (when inferred
            (setf (getf context infer-key) inferred)))))
    context))

(defun compile-generator-operations (generator args)
  "Compile a generator's operations into executable gen-operations.
   Expands templates and path variables using the context."
  (let* ((context (build-generator-context generator args))
         (operations '()))
    (dolist (op-spec (generator-operations generator))
      (let ((op-type (first op-spec))
            (op-props (rest op-spec)))
        (case op-type
          (:create
           (let* ((file-template (getf op-props :file))
                  (template-name (getf op-props :template))
                  (file-path (expand-path-template file-template context))
                  (template-content (when template-name (load-template template-name)))
                  (content (if template-content
                               (expand-template-string template-content context)
                               (format nil "// Generated: ~A~%" file-path))))
             (push (make-gen-operation :type :create
                                       :file file-path
                                       :content content)
                   operations)))
          (:modify
           (let* ((file-template (getf op-props :file))
                  (file-path (expand-path-template file-template context))
                  (search-template (getf op-props :search))
                  (replace-template (getf op-props :replace))
                  (search-text (expand-template-string search-template context))
                  (replace-text (expand-template-string replace-template context)))
             (push (make-gen-operation :type :modify
                                       :file file-path
                                       :search search-text
                                       :replace replace-text)
                   operations)))
          (:delete
           (let* ((file-template (getf op-props :file))
                  (file-path (expand-path-template file-template context)))
             (push (make-gen-operation :type :delete
                                       :file file-path)
                   operations)))
          (:install
           (let ((package (getf op-props :package)))
             (push (make-gen-operation :type :install
                                       :package (if (stringp package)
                                                    package
                                                    (expand-template-string package context)))
                   operations)))
          (:run
           (let ((command (getf op-props :command)))
             (push (make-gen-operation :type :run
                                       :command (expand-template-string command context))
                   operations))))))
    (nreverse operations)))

(defun execute-generator (generator args &key dry-run confirm)
  "Execute a generator with the given arguments.
   Returns a gen-result struct."
  (let ((operations (compile-generator-operations generator args)))
    (if operations
        (execute-gen-plan operations :dry-run dry-run :confirm confirm)
        (make-gen-result :success-p nil
                         :message "No operations generated"))))

(defun execute-gen-operation (op &key dry-run)
  "Execute a single generation operation.
   If DRY-RUN is t, just preview without making changes."
  (unless (or (null (gen-operation-condition op))
              (eval (gen-operation-condition op)))
    (return-from execute-gen-operation nil))

  (let ((result nil))
    (case (gen-operation-type op)
      (:create
       (setf result (execute-create-op op :dry-run dry-run)))
      (:modify
       (setf result (execute-modify-op op :dry-run dry-run)))
      (:delete
       (setf result (execute-delete-op op :dry-run dry-run)))
      (:rename
       (setf result (execute-rename-op op :dry-run dry-run)))
      (:install
       (setf result (execute-install-op op :dry-run dry-run)))
      (:uninstall
       (setf result (execute-uninstall-op op :dry-run dry-run)))
      (:run
       (setf result (execute-run-op op :dry-run dry-run)))
      (t
       (format t "~&Unknown operation type: ~A~%" (gen-operation-type op))
       (setf result nil)))

    (when (and result (not dry-run))
      (setf (gen-operation-executed-p op) t)
      (setf (gen-operation-timestamp op) (get-universal-time)))

    result))

(defun execute-create-op (op &key dry-run)
  "Execute a :create operation."
  (let* ((file-path (gen-operation-file op))
         (content (gen-operation-content op))
         (full-path (merge-pathnames file-path *repo-root*)))
    (if dry-run
        (progn
          (format t "~&  [CREATE] ~A~%" file-path)
          (format t "           ~A chars~%" (length content))
          t)
        (progn
          (ensure-directories-exist full-path)
          (setf (gen-operation-rollback-data op)
                (list :type :delete :file file-path))
          (write-file-content (uiop:native-namestring full-path) content)
          (format t "~&  ✓ Created: ~A~%" file-path)
          t))))

(defun execute-modify-op (op &key dry-run)
  "Execute a :modify operation (SEARCH/REPLACE)."
  (let* ((file-path (gen-operation-file op))
         (search-text (gen-operation-search op))
         (replace-text (gen-operation-replace op))
         (full-path (merge-pathnames file-path *repo-root*)))
    (unless (probe-file full-path)
      (format t "~&  ✗ File not found: ~A~%" file-path)
      (return-from execute-modify-op nil))

    (if dry-run
        (progn
          (format t "~&  [MODIFY] ~A~%" file-path)
          (format t "           Search: ~A chars, Replace: ~A chars~%"
                  (length search-text) (length replace-text))
          t)
        (let* ((content (uiop:read-file-string full-path))
               (search-pos (search search-text content)))
          (unless search-pos
            (format t "~&  ✗ Search block not found in: ~A~%" file-path)
            (return-from execute-modify-op nil))

          (setf (gen-operation-rollback-data op)
                (list :type :modify
                      :file file-path
                      :search replace-text
                      :replace search-text))

          (let ((new-content (concatenate 'string
                                          (subseq content 0 search-pos)
                                          replace-text
                                          (subseq content (+ search-pos (length search-text))))))
            (write-file-content (uiop:native-namestring full-path) new-content)
            (format t "~&  ✓ Modified: ~A~%" file-path)
            t)))))

(defun execute-delete-op (op &key dry-run)
  "Execute a :delete operation."
  (let* ((file-path (gen-operation-file op))
         (full-path (merge-pathnames file-path *repo-root*)))
    (unless (probe-file full-path)
      (format t "~&  ✗ File not found: ~A~%" file-path)
      (return-from execute-delete-op nil))

    (if dry-run
        (progn
          (format t "~&  [DELETE] ~A~%" file-path)
          t)
        (progn
          (setf (gen-operation-rollback-data op)
                (list :type :create
                      :file file-path
                      :content (uiop:read-file-string full-path)))
          (delete-file full-path)
          (format t "~&  ✓ Deleted: ~A~%" file-path)
          t))))

(defun execute-rename-op (op &key dry-run)
  "Execute a :rename operation."
  (let* ((source (gen-operation-source op))
         (target (gen-operation-target op))
         (source-path (merge-pathnames source *repo-root*))
         (target-path (merge-pathnames target *repo-root*)))
    (unless (probe-file source-path)
      (format t "~&  ✗ Source not found: ~A~%" source)
      (return-from execute-rename-op nil))

    (if dry-run
        (progn
          (format t "~&  [RENAME] ~A -> ~A~%" source target)
          t)
        (progn
          (setf (gen-operation-rollback-data op)
                (list :type :rename :source target :target source))
          (ensure-directories-exist target-path)
          (rename-file source-path target-path)
          (format t "~&  ✓ Renamed: ~A -> ~A~%" source target)
          t))))

(defun execute-install-op (op &key dry-run)
  "Execute an :install operation (package installation)."
  (let* ((package (gen-operation-package op))
         (pm (infer-package-manager))
         (cmd (case pm
                (:bun (format nil "bun add ~A" package))
                (:pnpm (format nil "pnpm add ~A" package))
                (:yarn (format nil "yarn add ~A" package))
                (t (format nil "npm install ~A" package)))))
    (if dry-run
        (progn
          (format t "~&  [INSTALL] ~A~%" package)
          (format t "            Command: ~A~%" cmd)
          t)
        (progn
          (setf (gen-operation-rollback-data op)
                (list :type :uninstall :package package))
          (format t "~&  Installing ~A...~%" package)
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program cmd
                                :directory *repo-root*
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore output))
            (if (zerop exit-code)
                (progn
                  (format t "~&  ✓ Installed: ~A~%" package)
                  t)
                (progn
                  (format t "~&  ✗ Failed to install ~A: ~A~%" package error-output)
                  nil)))))))

(defun execute-uninstall-op (op &key dry-run)
  "Execute an :uninstall operation."
  (let* ((package (gen-operation-package op))
         (pm (infer-package-manager))
         (cmd (case pm
                (:bun (format nil "bun remove ~A" package))
                (:pnpm (format nil "pnpm remove ~A" package))
                (:yarn (format nil "yarn remove ~A" package))
                (t (format nil "npm uninstall ~A" package)))))
    (if dry-run
        (progn
          (format t "~&  [UNINSTALL] ~A~%" package)
          t)
        (progn
          (format t "~&  Uninstalling ~A...~%" package)
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program cmd
                                :directory *repo-root*
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore output error-output))
            (if (zerop exit-code)
                (progn
                  (format t "~&  ✓ Uninstalled: ~A~%" package)
                  t)
                (progn
                  (format t "~&  ✗ Failed to uninstall ~A~%" package)
                  nil)))))))

(defun execute-run-op (op &key dry-run)
  "Execute a :run operation (shell command)."
  (let ((cmd (gen-operation-command op)))
    (if dry-run
        (progn
          (format t "~&  [RUN] ~A~%" cmd)
          t)
        (progn
          (format t "~&  Running: ~A~%" cmd)
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program cmd
                                :directory *repo-root*
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore error-output))
            (when (and output (> (length output) 0))
              (format t "~A~%" output))
            (zerop exit-code))))))

(defun generate-plan-with-llm (command context)
  "Use the LLM to generate a plan of operations from a command."
  (let* ((system-prompt (uiop:read-file-string (get-prompt-path "code-gen-planner-system.org")))
         (prompt (format nil "Generate a plan for: ~A

Project context:
- Framework: ~A
- Language: ~A
- Stack: ~{~A~^, ~}

~A"
                         command
                         (or (infer-framework) "unknown")
                         (or *language* "unknown")
                         *stack*
                         context)))
    (get-llm-response prompt
                      :stream nil
                      :add-to-history nil
                      :custom-system-prompt system-prompt)))

(defun parse-plan-response (response)
  "Parse the LLM response into a list of gen-operations."
  (let ((operations '())
        (lines (str:lines response))
        (current-op nil)
        (current-content (make-string-output-stream))
        (in-content nil))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ((string= trimmed "---")
           (when current-op
             (when in-content
               (let ((content (get-output-stream-string current-content)))
                 (setf (gen-operation-content current-op)
                       (string-trim '(#\Space #\Tab #\Newline) content))))
             (push current-op operations)
             (setf current-op nil)
             (setf current-content (make-string-output-stream))
             (setf in-content nil)))

          ((str:starts-with? "OPERATION:" trimmed)
           (let ((type-str (string-trim '(#\Space) (subseq trimmed 10))))
             (setf current-op (make-gen-operation
                               :type (intern (string-upcase type-str) :keyword)))))

          ((and current-op (str:starts-with? "FILE:" trimmed))
           (setf (gen-operation-file current-op)
                 (string-trim '(#\Space) (subseq trimmed 5))))

          ((and current-op (str:starts-with? "PACKAGE:" trimmed))
           (setf (gen-operation-package current-op)
                 (string-trim '(#\Space) (subseq trimmed 8))))

          ((and current-op (str:starts-with? "COMMAND:" trimmed))
           (setf (gen-operation-command current-op)
                 (string-trim '(#\Space) (subseq trimmed 8))))

          ((and current-op (str:starts-with? "SEARCH:" trimmed))
           (setf (gen-operation-search current-op)
                 (string-trim '(#\Space) (subseq trimmed 7))))

          ((and current-op (str:starts-with? "REPLACE:" trimmed))
           (setf (gen-operation-replace current-op)
                 (string-trim '(#\Space) (subseq trimmed 8))))

          ((and current-op (str:starts-with? "CONTENT:" trimmed))
           (setf in-content t)
           (let ((rest (subseq trimmed 8)))
             (when (> (length rest) 0)
               (write-string (string-trim '(#\Space) rest) current-content)
               (write-char #\Newline current-content))))

          ((and current-op in-content)
           (write-string line current-content)
           (write-char #\Newline current-content)))))

    (when current-op
      (when in-content
        (setf (gen-operation-content current-op)
              (string-trim '(#\Space #\Tab #\Newline)
                           (get-output-stream-string current-content))))
      (push current-op operations))

    (nreverse operations)))

;;** Plan Execution

(defun execute-gen-plan (operations &key dry-run confirm)
  "Execute a list of operations. Returns a gen-result struct."
  (when confirm
    (format t "~&This will:~%")
    (dolist (op operations)
      (execute-gen-operation op :dry-run t))
    (unless (confirm-action "Proceed?")
      (return-from execute-gen-plan
        (make-gen-result :success-p nil :message "Cancelled by user"))))

  (let ((result (make-gen-result :success-p t))
        (executed '()))
    (dolist (op operations)
      (handler-case
          (if (execute-gen-operation op :dry-run dry-run)
              (progn
                (push op executed)
                (case (gen-operation-type op)
                  (:create (push (gen-operation-file op)
                                 (gen-result-files-created result)))
                  (:modify (push (gen-operation-file op)
                                 (gen-result-files-modified result)))
                  (:delete (push (gen-operation-file op)
                                 (gen-result-files-deleted result)))))
              (progn
                (push (format nil "Failed: ~A ~A"
                              (gen-operation-type op)
                              (or (gen-operation-file op)
                                  (gen-operation-package op)))
                      (gen-result-errors result))
                (setf (gen-result-success-p result) nil)))
        (error (e)
          (push (format nil "Error: ~A" e) (gen-result-errors result))
          (setf (gen-result-success-p result) nil))))

    (setf (gen-result-operations result) (nreverse executed))

    (unless dry-run
      (push (list :operations executed :timestamp (get-universal-time))
            *gen-history*)
      (when (> (length *gen-history*) *gen-history-limit*)
        (setf *gen-history* (subseq *gen-history* 0 *gen-history-limit*))))

    result))

;;** Undo Support

(defun gen-undo ()
  "Undo the last generation operation."
  (unless *gen-history*
    (format t "~&No generation history to undo.~%")
    (return-from gen-undo nil))

  (let* ((last-gen (pop *gen-history*))
         (operations (getf last-gen :operations)))
    (format t "~&Undoing ~A operation(s)...~%" (length operations))

    (dolist (op (reverse operations))
      (let ((rollback (gen-operation-rollback-data op)))
        (when rollback
          (let ((rollback-op (make-gen-operation
                              :type (getf rollback :type)
                              :file (getf rollback :file)
                              :content (getf rollback :content)
                              :search (getf rollback :search)
                              :replace (getf rollback :replace)
                              :source (getf rollback :source)
                              :target (getf rollback :target)
                              :package (getf rollback :package))))
            (execute-gen-operation rollback-op)))))

    (format t "~&Undo complete.~%")
    t))

;;** High-Level Generation Commands

(defun run-generator (name args)
  "Run a generator by name with the given arguments.
   Finds the best generator for the current stack.
   Uses template-based generation when templates are defined,
   falls back to LLM generation otherwise."
  (let ((gen (find-generator name)))
    (unless gen
      (format t "~&Unknown generator: ~A~%" name)
      (return-from run-generator nil))

    (format t "~&Running generator: ~A~%" (generator-name gen))
    (format t "~&Priority: ~A~%" (generator-priority gen))
    (format t "~&Args: ~A~%" args)

    ;; Check if generator has template-based operations
    (if (generator-operations gen)
        ;; Template-based execution (Zero LLM)
        (progn
          (format t "~&Using template-based generation (Zero LLM)~%")
          (execute-generator gen args :confirm t))
        ;; Fall back to LLM-based generation
        (progn
          (format t "~&Using LLM-based generation~%")
          (let* ((command (format nil "~A ~{~A~^ ~}. ~A"
                                  (generator-name gen) args
                                  (generator-description gen)))
                 (context (generate-context))
                 (response (generate-plan-with-llm command context))
                 (operations (parse-plan-response response)))
            (if operations
                (execute-gen-plan operations :confirm t)
                (format t "~&Could not generate plan.~%")))))))

(defun apply-pattern (name target &key config)
  "Apply a pattern to target files.
   Finds the best pattern for the current stack."
  (let ((pattern (find-pattern name)))
    (unless pattern
      (format t "~&Unknown pattern: ~A~%" name)
      (return-from apply-pattern nil))

    (format t "~&Applying pattern: ~A to ~A~%" (pattern-name pattern) target)
    (when (pattern-package pattern)
      (format t "~&Package: ~A~%" (pattern-package pattern)))

    (let* ((package-op (when (pattern-package pattern)
                         (list (make-gen-operation
                                :type :install
                                :package (pattern-package pattern)))))
           (command (format nil "Apply ~A pattern to ~A~@[ with config ~A~]. ~A"
                            (pattern-name pattern) target config
                            (pattern-description pattern)))
           (context (generate-context))
           (response (generate-plan-with-llm command context))
           (llm-operations (parse-plan-response response))
           (final-operations (append package-op llm-operations)))
      (if final-operations
          (execute-gen-plan final-operations :confirm t)
          (format t "~&Could not generate plan.~%")))))

;;** Built-in Generators

(defgenerator component
  "Generate a React/Vue/Angular component with TypeScript"
  :args (name &key props style test))

(defgenerator apiroute
  "Generate an API route handler"
  :args (path &key method handler middleware))

(defgenerator model
  "Generate a data model with ORM integration"
  :args (name &key fields))

(defgenerator hook
  "Generate a React hook"
  :args (name &key args return-type))

(defgenerator page
  "Generate a page/view component"
  :args (path &key layout))

(defgenerator test
  "Generate tests for existing code"
  :args (target &key coverage))

(defpattern rate-limiting
  "Add rate limiting to API endpoints"
  :detect-framework t
  :variants
  ((:express (:package "express-rate-limit"))
   (:fastify (:package "@fastify/rate-limit"))
   (:hono (:package "hono-rate-limiter"))))

(defpattern error-boundary
  "Wrap React components in error boundaries"
  :requires (:react))

(defpattern logging
  "Add structured logging"
  :detect-framework t
  :variants
  ((:node (:package "pino"))
   (:python (:package "structlog"))))

(defpattern validation
  "Add input validation"
  :detect-framework t
  :variants
  ((:node (:package "zod"))
   (:python (:package "pydantic"))))

(define-command gen (args)
  "Generate code using a generator or natural language.
Usage: /gen <generator> [name] [options...]
       /gen \"natural language description\"
Examples:
  /gen component Button --props \"label:string\"
  /gen route /api/users --method GET,POST
  /gen \"a form component for user registration\""
  (if (and args (gethash (string-downcase (first args)) *generators*))
      (run-generator (first args) (rest args))
      (let* ((command (format nil "~{~A~^ ~}" args))
             (context (generate-context))
             (response (generate-plan-with-llm command context))
             (operations (parse-plan-response response)))
        (if operations
            (execute-gen-plan operations :confirm t)
            (format t "~&Could not generate a plan from: ~A~%" command)))))

(define-command gen/add (args)
  "Add a pattern to files.
Usage: /gen/add <pattern> --to <glob>
Examples:
  /gen/add rate-limiting --to \"src/api/**/*.ts\"
  /gen/add error-boundary --wrap \"src/components/*.tsx\""
  (let* ((pattern-name (first args))
         (rest-args (rest args))
         (target (or (getf-string rest-args "--to")
                     (getf-string rest-args "--wrap")))
         (config (getf-string rest-args "--config")))
    (if (and pattern-name target)
        (apply-pattern pattern-name target :config config)
        (format t "Usage: /gen/add <pattern> --to <glob>~%"))))

(defun getf-string (plist key)
  "Get value after KEY in a list of strings."
  (loop for (k v) on plist by #'cddr
        when (string= k key)
        return v))

(define-command gen/rm (args)
  "Remove generated code.
Usage: /gen/rm <type> <name>"
  (if (>= (length args) 2)
      (let ((type (first args))
            (name (second args)))
        (format t "~&Removing ~A: ~A~%" type name)
        (let* ((command (format nil "Remove the ~A named ~A and clean up all references" type name))
               (context (generate-context))
               (response (generate-plan-with-llm command context))
               (operations (parse-plan-response response)))
          (if operations
              (execute-gen-plan operations :confirm t)
              (format t "~&Could not generate removal plan.~%"))))
      (format t "Usage: /gen/rm <type> <name>~%")))

(define-command gen/undo (args)
  "Undo the last generation operation."
  (declare (ignore args))
  (gen-undo))

(define-command gen/history (args)
  "Show generation history."
  (declare (ignore args))
  (if *gen-history*
      (loop for entry in *gen-history*
            for i from 1
            do (format t "~&[~A] ~A operation(s) at ~A~%"
                       i
                       (length (getf entry :operations))
                       (getf entry :timestamp)))
      (format t "~&No generation history.~%")))

(define-command gen/list (args)
  "List available generators and patterns.
   Use --all to show inactive ones too."
  (let ((show-all (member "--all" args :test #'string-equal)))
    (format t "~&Current stack: ~{~A~^, ~}~%" (or *stack* '("(none)")))
    (format t "~%Active Generators:~%")
    (let ((active-gens (list-active-generators)))
      (if active-gens
          (dolist (gen-info active-gens)
            (format t "  ~A - ~A (priority: ~A)~%"
                    (getf gen-info :name)
                    (getf gen-info :description)
                    (getf gen-info :priority)))
          (format t "  (none)~%")))
    (when show-all
      (format t "~%All Generators (including inactive):~%")
      (dolist (gen-info (list-all-generators))
        (format t "  ~A~A - ~A~%"
                (getf gen-info :name)
                (if (getf gen-info :active) " [ACTIVE]" " [inactive]")
                (getf gen-info :description))))
    (format t "~%Active Patterns:~%")
    (let ((active-pats (list-active-patterns)))
      (if active-pats
          (dolist (pat-info active-pats)
            (format t "  ~A - ~A~@[ (package: ~A)~]~%"
                    (getf pat-info :name)
                    (getf pat-info :description)
                    (getf pat-info :package)))
          (format t "  (none)~%")))
    (when show-all
      (format t "~%All Patterns (including inactive):~%")
      (dolist (pat-info (list-all-patterns))
        (format t "  ~A~A - ~A~%"
                (getf pat-info :name)
                (if (getf pat-info :active) " [ACTIVE]" " [inactive]")
                (getf pat-info :description))))))


(define-sub-command gen (args)
  "Generate code using Hactar's generative layer.
Usage: hactar gen <generator|description> [args...]"
  (let ((command (format nil "~{~A~^ ~}" args)))
    (if (string= command "")
        (progn
          (format t "Usage: hactar gen <generator|description> [args...]~%")
          (format t "~%Available generators:~%")
          (maphash (lambda (name gens)
                     (let ((gen (if (listp gens) (first gens) gens)))
                       (when gen
                         (format t "  ~A - ~A~%" name (generator-description gen)))))
                   *generators*))
        (if (gethash (string-downcase (first args)) *generators*)
            (run-generator (first args) (rest args))
            (let* ((context (generate-context))
                   (response (generate-plan-with-llm command context))
                   (operations (parse-plan-response response)))
              (if operations
                  (execute-gen-plan operations :confirm (not *silent*))
                  (format t "~&Could not generate a plan from: ~A~%" command)))))))

(defstruct recipe
  "A recipe composes queries, transforms, and file operations."
  (name nil :type (or null string symbol))
  (description nil :type (or null string))
  (requires nil :type list)           ; List of predicates that must be true
  (conflicts nil :type list)          ; List of predicates that must be false
  (context-spec nil :type list)       ; Alist of (var . finder-fn)
  (steps nil :type list))             ; List of step specifications

(defmacro defrecipe (name description &body body)
  "Define a recipe for multi-step code generation.

   Usage:
   (defrecipe add-authentication
     \"Add JWT authentication to a React application\"
     :requires ((react?) (react-router?))
     :conflicts ((has-auth-context?))
     :context
     ((:app-file (find-app-entry))
      (:routes-file (find-routes-config)))
     :steps
     ((:create \"src/auth/AuthContext.tsx\" :template \"react/auth-context.tsx\")
      (:transform (ctx :app-file) :with (wrap-with-provider :provider \"AuthProvider\"))
      (:install \"jwt-decode\")))"
  (let ((requires (getf body :requires))
        (conflicts (getf body :conflicts))
        (context-spec (getf body :context))
        (steps (getf body :steps)))
    `(setf (gethash ,(if (stringp name) name (string-downcase (symbol-name name)))
                    *recipes*)
           (make-recipe
            :name ',name
            :description ,description
            :requires ',requires
            :conflicts ',conflicts
            :context-spec ',context-spec
            :steps ',steps))))

(defun get-recipe (name)
  "Get a recipe by name."
  (gethash (if (stringp name) name (string-downcase (symbol-name name)))
           *recipes*))

(defun list-recipes ()
  "Return list of all defined recipe names."
  (let ((names '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *recipes*)
    (nreverse names)))

(defun recipe/check-requirements (recipe)
  "Check if recipe requirements are met. Returns (values ok-p error-messages)."
  (let ((errors '()))
    (dolist (req (recipe-requires recipe))
      (unless (eval req)
        (push (format nil "Requirement not met: ~S" req) errors)))
    (dolist (conflict (recipe-conflicts recipe))
      (when (eval conflict)
        (push (format nil "Conflict detected: ~S" conflict) errors)))
    (values (null errors) (nreverse errors))))

(defun recipe/build-context (recipe)
  "Build the context for a recipe by evaluating context-spec."
  (let ((ctx (make-hash-table :test 'equal)))
    (dolist (spec (recipe-context-spec recipe))
      (let ((var (first spec))
            (finder (second spec)))
        (setf (gethash var ctx) (eval finder))))
    ctx))

(defun recipe/compile-step (step context)
  "Compile a recipe step into gen-operations."
  (let ((step-type (first step))
        (step-args (rest step)))
    (case step-type
      (:create
       (let* ((file-path (first step-args))
              (rest-args (rest step-args))
              (template-name (getf rest-args :template))
              (template-content (when template-name (load-template template-name)))
              (content (or template-content
                           (format nil "// Generated by recipe~%"))))
         (list (make-gen-operation :type :create
                                   :file file-path
                                   :content content))))
      (:modify
       (let* ((file-path (first step-args))
              (rest-args (rest step-args))
              (search-text (getf rest-args :search))
              (replace-text (getf rest-args :replace)))
         (list (make-gen-operation :type :modify
                                   :file file-path
                                   :search search-text
                                   :replace replace-text))))
      (:transform
       (let* ((target-spec (first step-args))
              (rest-args (rest step-args))
              (transform-spec (getf rest-args :with))
              (target-file (if (and (listp target-spec) (eq (first target-spec) 'ctx))
                               (gethash (second target-spec) context)
                               target-spec)))
         (when target-file
           (let* ((cv (code/select target-file))
                  (transform-name (if (listp transform-spec) (first transform-spec) transform-spec))
                  (transform-args (if (listp transform-spec) (rest transform-spec) nil))
                  (transformed (apply #'code/transform cv transform-name transform-args)))
             (list (make-gen-operation :type :modify
                                       :file target-file
                                       :search (code-value-source-text cv)
                                       :replace (code-value-source-text transformed)))))))
      (:install
       (let ((package (first step-args)))
         (list (make-gen-operation :type :install
                                   :package package))))
      (:uninstall
       (let ((package (first step-args)))
         (list (make-gen-operation :type :uninstall
                                   :package package))))
      (:run
       (let ((command (first step-args)))
         (list (make-gen-operation :type :run
                                   :command command))))
      (t
       (format t "~&Unknown recipe step type: ~A~%" step-type)
       nil))))

(defun recipe/plan (name)
  "Generate a plan for a recipe without executing it."
  (let ((recipe (get-recipe name)))
    (unless recipe
      (format t "~&Unknown recipe: ~A~%" name)
      (return-from recipe/plan nil))

    (format t "~&Recipe: ~A~%" (recipe-name recipe))
    (format t "~A~%~%" (recipe-description recipe))

    ;; Check requirements
    (multiple-value-bind (ok-p errors) (recipe/check-requirements recipe)
      (unless ok-p
        (format t "~&Requirements not met:~%")
        (dolist (err errors)
          (format t "  - ~A~%" err))
        (return-from recipe/plan nil)))

    (format t "~&Requirements: OK~%")

    ;; Build context
    (let ((context (recipe/build-context recipe)))
      (format t "~&Context:~%")
      (maphash (lambda (k v)
                 (format t "  ~A: ~A~%" k v))
               context)

      ;; Compile steps
      (format t "~%Steps:~%")
      (let ((all-ops '())
            (step-num 0))
        (dolist (step (recipe-steps recipe))
          (incf step-num)
          (format t "~&~A. ~A~%" step-num step)
          (let ((ops (recipe/compile-step step context)))
            (setf all-ops (append all-ops ops))))

        (format t "~%Operations (~A total):~%" (length all-ops))
        (dolist (op all-ops)
          (execute-gen-operation op :dry-run t))

        all-ops))))

(defun recipe/execute (name &key dry-run)
  "Execute a recipe."
  (let ((recipe (get-recipe name)))
    (unless recipe
      (format t "~&Unknown recipe: ~A~%" name)
      (return-from recipe/execute nil))

    ;; Check requirements
    (multiple-value-bind (ok-p errors) (recipe/check-requirements recipe)
      (unless ok-p
        (format t "~&Cannot execute recipe. Requirements not met:~%")
        (dolist (err errors)
          (format t "  - ~A~%" err))
        (return-from recipe/execute nil)))

    ;; Build context and compile steps
    (let* ((context (recipe/build-context recipe))
           (all-ops '()))
      (dolist (step (recipe-steps recipe))
        (let ((ops (recipe/compile-step step context)))
          (setf all-ops (append all-ops ops))))

      ;; Execute
      (execute-gen-plan all-ops :dry-run dry-run :confirm (not dry-run)))))

;;** Recipe REPL Commands

(define-command recipe/list (args)
  "List all defined recipes."
  (declare (ignore args))
  (let ((recipes (list-recipes)))
    (if recipes
        (progn
          (format t "~&Available recipes:~%")
          (dolist (name recipes)
            (let ((recipe (get-recipe name)))
              (format t "  ~A - ~A~%" name (recipe-description recipe)))))
        (format t "~&No recipes defined.~%"))))

(define-command recipe/plan (args)
  "Preview a recipe without executing.
Usage: /recipe/plan <name>"
  (let ((name (first args)))
    (if name
        (recipe/plan name)
        (format t "~&Usage: /recipe/plan <name>~%"))))

(define-command recipe/execute (args)
  "Execute a recipe.
Usage: /recipe/execute <name>"
  (let ((name (first args)))
    (if name
        (recipe/execute name)
        (format t "~&Usage: /recipe/execute <name>~%"))))

;;** Built-in Recipes

(defrecipe add-error-handling
  "Add comprehensive error handling to the application"
  :requires ((react?))
  :steps
  ((:create "src/components/ErrorBoundary.tsx"
    :template "react/error-boundary.tsx.mustache")
   (:create "src/hooks/useErrorHandler.ts"
    :template "react/use-error-handler.ts.mustache")))

(defrecipe add-authentication
  "Add JWT authentication to a React application"
  :requires ((react?) (react-router?))
  :steps
  ((:create "src/auth/AuthContext.tsx"
    :template "react/auth-context.tsx.mustache")
   (:create "src/auth/useAuth.ts"
    :template "react/use-auth.ts.mustache")
   (:create "src/auth/ProtectedRoute.tsx"
    :template "react/protected-route.tsx.mustache")
   (:install "jwt-decode")))

(defrecipe add-api-layer
  "Add a structured API layer with fetch wrapper"
  :requires ((typescript?))
  :steps
  ((:create "src/api/client.ts"
    :template "api/client.ts.mustache")
   (:create "src/api/types.ts"
    :template "api/types.ts.mustache")))

(defun react? ()
  "Check if React is in the stack."
  (member "react" *stack* :test #'string-equal))

(defun react-router? ()
  "Check if React Router is in the stack."
  (member "react-router" *stack* :test #'string-equal))

(defun express? ()
  "Check if Express is in the stack."
  (member "express" *stack* :test #'string-equal))

(defun typescript? ()
  "Check if TypeScript is being used."
  (or (member "typescript" *stack* :test #'string-equal)
      (probe-file (merge-pathnames "tsconfig.json" *repo-root*))))

(defun find-app-entry ()
  "Find the main app entry file."
  (or (probe-file (merge-pathnames "src/App.tsx" *repo-root*))
      (probe-file (merge-pathnames "src/App.jsx" *repo-root*))
      (probe-file (merge-pathnames "src/app.tsx" *repo-root*))
      (probe-file (merge-pathnames "app/page.tsx" *repo-root*))))

(defun find-routes-config ()
  "Find the routes configuration file."
  (or (probe-file (merge-pathnames "src/routes.tsx" *repo-root*))
      (probe-file (merge-pathnames "src/router.tsx" *repo-root*))
      (probe-file (merge-pathnames "src/App.tsx" *repo-root*))))

;;** Original Gen Commands

(define-command gen.project.config (args)
  "Generate a .hactar.toml for the current project using the LLM, based on repository context and files added."
  (declare (ignore args))
  (if (null *repo-root*)
      (format t "Error: Repository root is not set. Cannot write configuration.~%")
      (let* ((config-path (merge-pathnames #P".hactar.toml" *repo-root*))
             (config-native (uiop:native-namestring config-path))
             (toml (generate-project-config-toml)))
        (if (and (probe-file config-path)
                 (not (confirm-action (format nil "Overwrite existing ~A?" config-native))))
            (format t "Skipped writing ~A~%" config-native)
            (if (write-file-content config-path toml)
                (progn
                  (format t "Wrote project config to ~A~%" config-native)
                  (when *git-autocommit*
                    (git-add (list config-path))
                    (git-commit "Generate .hactar.toml for project")))
                (format t "Failed to write project config to ~A~%" config-native))))))
(defun generate-project-config-toml ()
  "Generate a TOML configuration string for the current project using the LLM and repository context."
  (let* ((config-spec
          "Project-specific config file: .hactar.toml

[project]
author = \"...\"                  # String (optional)
language = \"...\"                # String (primary language)
stack = [\"...\"]                 # Array of Strings (frameworks/tools)
guide_extension = \"org\"         # String (default \"org\")
guide_exclude_tags = [\"nocontext\"]  # Array of Strings

# Optional legacy compatibility for watcher:
# test_command = \"...\"          # String (command for test watcher)

[project.commands]
test = \"...\"                    # String
lint = \"...\"                    # String
typecheck = \"...\"               # String

[auto]
lint = true|false
test = true|false
typecheck = true|false
docs = true|false
suggest_commands = true|false
cmds = true|false
all = true|false
limits = 10                      # Integer

# Analyzer toggles (array of tables):
# [[analyzers]]
# name = \"package-json\"
# enable = true

# Paths are machine-specific. Do NOT guess them unless explicitly inferable:
# [paths]
# pro = \"...\"
# hactar = \"...\"
# database = \"...\"
# piper_model = \"...\"
# templates = [\"...\"]")
         (context (generate-context))
         (system-prompt (uiop:read-file-string (get-prompt-path "project-analyzer-system.org")))
         (prompt (format nil "Using the following .hactar.toml specification, infer values from the repository context.
- Include only fields you can justify from the codebase.
- Prefer commands found in scripts, make targets, cargo, npm/yarn/pnpm, pytest, tox, etc.
- Do NOT include [paths] or API keys.
- If uncertain, leave fields out or use empty arrays/strings rather than guessing.
- Optionally include 'test_command' under [project] if a watcher command is evident.

SPEC:
```
~A
```

REPOSITORY CONTEXT:
```
~A
```" config-spec context))
         (response (get-llm-response prompt :custom-system-prompt system-prompt :stream nil :add-to-history t))
         (toml nil))
    (when response
      (let ((block (extract-md-fenced-code-block response)))
        (setf toml (if block
                       (cdr (assoc :contents block))
                       (string-trim '(#\Space #\Tab #\Newline #\Return #\`) response)))))
    (if (and toml (not (string= (string-trim '(#\Space #\Tab #\Newline #\Return) toml) "")))
        (progn
          ;; Ensure trailing newline
          (when (or (zerop (length toml))
                    (char/= (char toml (1- (length toml))) #\Newline))
            (setf toml (concatenate 'string toml (string #\Newline))))
          toml)
        "[project]
language = \"unknown\"
stack = []

[project.commands]
test = \"\"
lint = \"\"
typecheck = \"\"
")))

(defun generate-rule-code (instruction)
  "Generate a defrule form based on the instruction."
  (let* ((prompt-template (uiop:read-file-string (get-prompt-path "generate-rule.mustache")))
         (prompt (mustache:render* prompt-template `((:instruction . ,instruction))))
         (response (get-llm-response prompt :stream nil :add-to-history t)))
    (when response
      (let ((block (extract-md-fenced-code-block response)))
        (if block
            (cdr (assoc :contents block))
            (string-trim '(#\Space #\Tab #\Newline #\Return #\`) response))))))

(define-command gen.rule (args)
  "Generate a new Hactar rule (defrule) based on instructions.
   Usage: /gen.rule <instructions>"
  (let* ((instruction (format nil "~{~A~^ ~}" args))
         (rule-code (generate-rule-code instruction)))
    (if rule-code
        (progn
          (format t "Generated Rule:~%~A~%" rule-code)
          (if (confirm-action "Append this rule to .hactar.user.lisp?")
              (let ((user-file (merge-pathnames ".hactar.user.lisp" *repo-root*)))
                (with-open-file (stream user-file :direction :output :if-exists :append :if-does-not-exist :create)
                  (format stream "~%~A~%" rule-code))
                (format t "Rule appended to ~A~%" (uiop:native-namestring user-file)))
              (format t "Rule not saved.~%")))
        (format t "Failed to generate rule code.~%"))))
