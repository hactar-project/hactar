(in-package :hactar)

(defvar *redwood-has-css-import* nil
  "Set to T during compilation when a CSS import is detected.")

(defvar *redwood-env-bindings* nil
  "List of env binding keywords detected from env-call usage during compilation.")

(defvar *redwood-wrangler-sections* nil
  "List of wrangler section keywords explicitly configured via defwrangler.")

(defvar *redwood-default-import-sources*
  '("@tailwindcss/vite")
  "Module sources that should use default imports in generated Redwood files.")

(deftarget :redwood
  :parent :typescript
  :file-extension "tsx"
  :setup ((setf *redwood-has-css-import* nil)
          (setf *redwood-env-bindings* nil)
          (setf *redwood-wrangler-sections* nil))
  :finalize (redwood-finalize))

(register-json-emitters :redwood)
(register-toml-emitters :redwood)
(register-css-emitters :redwood)

(defjsmacro :redwood defcss (name &rest rules)
  (declare (ignore name))
  `(css-stylesheet
     ,@(loop for rule in rules
             append (css-expand-rule rule))))

;;* Default Configs 
(defun redwood-emit-default-package-json ()
  "Emit a default package.json if not already present."
  (unless (member "package.json" (vfs-all-paths *current-vfs*) :test #'string=)
    (compile-form '(defconfig))))

(defun redwood-emit-default-tsconfig ()
  "Emit a default tsconfig.json if not already present."
  (unless (member "tsconfig.json" (vfs-all-paths *current-vfs*) :test #'string=)
    (compile-form '(deftsconfig))))

(defun redwood-emit-default-vite-config ()
  "Emit a default vite.config.mts if not already present."
  (unless (member "vite.config.mts" (vfs-all-paths *current-vfs*) :test #'string=)
    (compile-form '(defviteconfig))))

(defun redwood-emit-default-wrangler ()
  "Emit a default wrangler.toml if not already present."
  (unless (member "wrangler.toml" (vfs-all-paths *current-vfs*) :test #'string=)
    (compile-form '(defwrangler :name "my-app"
                    :compatibility_date "2025-08-21"
		    :main "src/worker.tsx"
                    :compatibility_flags ("nodejs_compat")))))

(defun redwood-emit-auto-bindings ()
  "Append auto-detected env bindings to wrangler.toml for bindings not explicitly configured."
  (when *redwood-env-bindings*
    (let ((prev-file (vfs-current-file *current-vfs*)))
      (vfs-select-file *current-vfs* "wrangler.toml")
      (dolist (binding *redwood-env-bindings*)
        (unless (member binding *redwood-wrangler-sections*)
          (let ((section-name (string-downcase (symbol-name binding)))
                (binding-value (string-capitalize (string-downcase (symbol-name binding)))))
            (compiler-emitln "")
            (compile-form `(toml-table ,section-name "binding" ,binding-value)))))
      (when prev-file
        (vfs-select-file *current-vfs* prev-file)))))

(defun redwood-finalize ()
  "Emit default config files for any not explicitly defined."
  (redwood-emit-default-package-json)
  (redwood-emit-default-tsconfig)
  (redwood-emit-default-vite-config)
  (redwood-emit-default-wrangler)
  (redwood-emit-auto-bindings))

(defemit :redwood import (what &key from)
  (when (and from (stringp from) (search ".css" from))
    (setf *redwood-has-css-import* t))
  ;; Delegate to parent (typescript -> javascript) import emitter
  (let ((parent-emitter (find-emitter (gethash :typescript *compiler-targets*) 'import)))
    (funcall parent-emitter what :from from)))

(defjsmacro :redwood env-call (binding method &rest args)
  (pushnew binding *redwood-env-bindings*)
  (let* ((binding-name (string-capitalize (string-downcase (symbol-name binding))))
         (binding-sym (intern (concatenate 'string "*" binding-name) :hactar))
         (method-sym (intern (concatenate 'string "." (symbol-name method)) :hactar)))
    `(chain (@ env ,binding-sym) (,method-sym ,@args))))

;;* Helpers 
(defun jsx-element-p (symbol)
  "Check if a symbol should be emitted as a JSX element."
  (when (symbolp symbol)
    (let ((name (symbol-name symbol)))
      (and (> (length name) 0)
           (or (lower-case-p (char name 0))
               (char= (char name 0) #\*))))))
;;* Emitters 
(defemit :redwood pragma (text)
  (compiler-emit (format nil "\"~A\"" text)))

(defemit :redwood prepend-pragma (text)
  (compiler-prepend (format nil "\"~A\";~%" text)))

(defun emit-jsx-tag (tag)
  "Emit a JSX tag name."
  (let ((name (symbol-name tag)))
    (if (char= (char name 0) #\*)
        ;; Component: *Chat -> Chat
        (compiler-emit (string-capitalize (kebab-to-camel (string-downcase (subseq name 1)))))
        ;; HTML element: lowercase
        (compiler-emit (string-downcase name)))))

(defun emit-jsx-attr-name (key)
  "Emit a JSX attribute name from a keyword."
  (compiler-emit (kebab-to-camel (string-downcase (symbol-name key)))))

(defun parse-jsx-attrs-and-children (args)
  "Parse JSX args into (values attrs children). Keywords followed by values are attrs."
  (let ((attrs nil)
        (children nil)
        (remaining args))
    (loop while remaining
          do (if (keywordp (car remaining))
                 (progn
                   (push (cons (car remaining) (cadr remaining)) attrs)
                   (setf remaining (cddr remaining)))
                 (progn
                   (setf children remaining)
                   (return))))
    (values (nreverse attrs) children)))

(defun emit-jsx-child (child &optional parent-tag)
  (cond
    ((stringp child) (compiler-emit child))
    ((and parent-tag
          (eq parent-tag 'script)
          (listp child)
          (symbolp (car child))
          (eq (car child) 'import)
          (stringp (second child))
          (null (cddr child)))
     (compile-form child))
    ((and (listp child) (symbolp (car child)) (eq (car child) 'jsx))
     (compile-form child))
    ((and (listp child)
          (symbolp (car child))
          (find-macro *current-target* (car child)))
     (let ((expanded (apply (find-macro *current-target* (car child)) (cdr child))))
       (if (and (listp expanded) (symbolp (car expanded)) (eq (car expanded) 'jsx))
           (compile-form expanded)
           (progn
             (compiler-emit "{")
             (compile-form expanded)
             (compiler-emit "}")))))
    (t
     (compiler-emit "{")
     (compile-form child)
     (compiler-emit "}"))))

(defemit :redwood jsx (tag attrs &rest children)
  (compiler-emit "<")
  (emit-jsx-tag tag)
  (loop for (key value . rest) on attrs by #'cddr
        do (compiler-emit " ")
           (emit-jsx-attr-name key)
           (compiler-emit "=")
           (if (stringp value)
               (compiler-emit (format nil "\"~A\"" value))
               (progn
                 (compiler-emit "{")
                 (compile-form value)
                 (compiler-emit "}"))))
  (if children
      (progn
        (compiler-emit ">")
        (dolist (child children)
          (emit-jsx-child child tag))
        (compiler-emit "</")
        (emit-jsx-tag tag)
        (compiler-emit ">"))
      (compiler-emit " />")))

;; Register macros for all the jsx tags
(dolist (tag '(html head body div span p a h1 h2 h3 h4 h5 h6
               ul ol li table tr td th form input button textarea
               select option label img link meta title script style
               nav header footer main section article aside
               br hr pre code blockquote strong em i b u
               iframe video audio source canvas svg))
  (let ((captured-tag tag))
    (setf (gethash tag (target-definition-macros (gethash :redwood *compiler-targets*)))
          (lambda (&rest args)
            (multiple-value-bind (attrs children)
                (parse-jsx-attrs-and-children args)
              (let ((attr-plist
                      (loop for (key . value) in attrs
                            append (list key value))))
                `(jsx ,captured-tag ,attr-plist ,@children)))))))

(defjsmacro :redwood defcomponent (name props &rest body)
  (labels ((prop-name (prop)
             (if (listp prop) (first prop) prop))
           (prop-type-string (prop)
             (multiple-value-bind (pname ptype) (extract-type-annotation prop)
               (declare (ignore pname))
               ptype))
           (props-type-string (props)
             (if props
                 (format nil "{ ~{~A~^, ~} }"
                         (loop for prop in props
                               collect (format nil "~A~@[~A~]~A"
                                               (kebab-to-camel (string-downcase (symbol-name (prop-name prop))))
                                               nil
                                               (if (prop-type-string prop)
                                                   (format nil ": ~A" (prop-type-string prop))
                                                   ""))))
                 "{}"))
           (prop-names (props)
             (mapcar #'prop-name props))
           (jsx-like-form-p (form)
             (and (listp form)
                  (symbolp (car form))
                  (or (jsx-element-p (car form))
                      (eq (car form) 'jsx)))))
    `(export
       (const-typed ,name ,(format nil "React.FC<~A>" (props-type-string props))
         (lambda (,@(if props `((:destructure ,@(prop-names props))) nil))
           ,@(if (and (= 1 (length body))
                      (jsx-like-form-p (first body)))
                 `((return ,(first body)))
                 body))))))

(defjsmacro :redwood defaction (name args &rest body)
  `(progn
     (pragma "use server")
     (export (async (defun ,name ,args ,@body)))))

(defjsmacro :redwood defclient (name props &rest body)
  `(progn
     (export (defun ,name ,props ,@body))
     (prepend-pragma "use client")))

(defjsmacro :redwood let-state (&rest bindings-and-body)
  (let ((bindings nil)
        (body nil)
        (remaining bindings-and-body))
    (loop while remaining
          do (let ((item (car remaining)))
               (if (and (listp item) (listp (first item)) (= 2 (length (first item))))
                   (progn
                     (push item bindings)
                     (setf remaining (cdr remaining)))
                   (progn
                     (setf body remaining)
                     (return)))))
    (setf bindings (nreverse bindings))
    `(progn
       ,@(loop for (names init) in bindings
               collect `(destructuring-const ,names (use-state ,init)))
       ,@body)))

(defun plist->package-pairs-with-defaults (defaults overrides)
  "Merge DEFAULTS with OVERRIDES, with later OVERRIDES replacing matching package versions."
  (let ((merged (copy-list defaults)))
    (loop for (pkg ver) on overrides by #'cddr
          for pkg-name = (string-downcase (symbol-name pkg))
          do (let ((existing (member pkg-name merged :test #'string=)))
               (if existing
                   (setf (second existing) ver)
                   (setf merged (append merged (list pkg-name ver))))))
    merged))

(defjsmacro :redwood defconfig (&key name dependencies dev-dependencies (package-manager "pnpm"))
  (let ((default-dependencies '("react" "19.2.5"
                                "react-dom" "19.2.5"
                                "react-server-dom-webpack" "19.2.5"
                                "rwsdk" "1.2.3"))
        (default-dev-dependencies `("@cloudflare/vite-plugin" "1.31.0"
                                    "@cloudflare/workers-types" "4.20260405.1"
                                    "@types/node" "~25.3.5"
                                    "@types/react" "19.2.14"
                                    "@types/react-dom" "19.2.3"
				    ,@(when *redwood-has-css-import*
                                        '("@tailwindcss/vite" "^4.2.2"
                                          "tailwindcss" "^4.2.2"))
                                    "typescript" "6.0.2"
                                    "vite" "~7.3.2"
                                    "wrangler" "4.80.0")))
    `(progn
       (in-file "package.json")
       (json-object
         "name" ,(or name "my-app")
         "private" :true
         "packageManager" ,package-manager
         "scripts" (json-object
                     "dev" "vite dev"
                     "build" "vite build"
                     "preview" "vite preview")
         "dependencies" (json-object
                          ,@(plist->package-pairs-with-defaults default-dependencies dependencies))
         "devDependencies" (json-object
                             ,@(plist->package-pairs-with-defaults default-dev-dependencies dev-dependencies))))))

(defjsmacro :redwood deftsconfig (&rest pairs)
  `(progn
     (in-file "tsconfig.json")
     (json-object
       "compilerOptions" (json-object
                           ,@(if pairs
                                 (loop for (k v) on pairs by #'cddr
                                       collect (kebab-to-camel (string-downcase (symbol-name k)))
                                       collect (cond ((eq v t) :true)
                                                     ((null v) :false)
                                                     ((listp v) `(json-array ,@v))
                                                     (t v)))
                                 '("target" "es2021"
                                   "lib" (json-array "DOM" "DOM.Iterable" "ESNext" "ES2022")
                                   "jsx" "react-jsx"
                                   "module" "es2022"
                                   "moduleResolution" "bundler"
                                   "types" (json-array "@cloudflare/workers-types"
                                                       "./worker-configuration.d.ts"
                                                       "./types/rw.d.ts"
                                                       "./types/vite.d.ts")
                                   "paths" (json-object "@/*" (json-array "./src/*"))
                                   "resolveJsonModule" :true
                                   "checkJs" :false
                                   "noEmit" :true
                                   "isolatedModules" :true
                                   "allowSyntheticDefaultImports" :true
                                   "forceConsistentCasingInFileNames" :true
                                   "strict" :true
                                   "skipLibCheck" :true))
       "exclude" (json-array "node_modules" ".tmp")))))

(defjsmacro :redwood defviteconfig (&key plugins)
  (let ((all-plugins (append '((cloudflare "@cloudflare/vite-plugin")
                               (redwood "rwsdk/vite"))
                             (when *redwood-has-css-import*
                               '((tailwindcss "@tailwindcss/vite")))
                             plugins)))
    `(progn
       (in-file "vite.config.mts")
       (import (define-config) :from "vite")
       ,@(loop for plugin in all-plugins
               when (listp plugin)
               collect (redwood-import-form (first plugin) (second plugin)))
       (export-default (define-config (create
         :plugins (array
                    (cloudflare (create :vite-environment (create :name "worker")))
                    ,@(loop for plugin in all-plugins
                            unless (eq (if (listp plugin) (first plugin) plugin) 'cloudflare)
                            collect (let ((name (if (listp plugin) (first plugin) plugin)))
                                      `(,name))))))))))

(defjsmacro :redwood defwrangler (&rest pairs)
  (loop for (key val) on pairs by #'cddr
        when (and (listp val) (keywordp (first val)))
        do (pushnew key *redwood-wrangler-sections*))
  `(progn
     (in-file "wrangler.toml")
     ,@(loop for (key val) on pairs by #'cddr
             for key-name = (string-downcase (symbol-name key))
             collect (cond
                       ((and (listp val) (keywordp (first val)))
                        `(toml-table ,key-name
                           ,@(loop for (sk sv) on val by #'cddr
                                   collect (string-downcase (symbol-name sk))
                                   collect sv)))
                       ((listp val)
                        `(toml-pairs ,key-name (toml-array ,@val)))
                       (t
                        `(toml-pairs ,key-name ,val))))))

(defun normalize-routes (routes)
  "Normalize routes: if flat list like (\"path\" comp ...), group into ((\"path\" comp) ...)."
  (if (and routes (stringp (first routes)))
      ;; Flat pairs: group by twos
      (loop for (path comp) on routes by #'cddr
            collect (list path comp))
      ;; Already nested
      routes))

(defun redwood-default-import-source-p (source)
  "Return T when SOURCE should be emitted as a default import."
  (member source *redwood-default-import-sources* :test #'string=))

(defun redwood-import-form (symbol source &key default)
  "Build an import form for SYMBOL from SOURCE.
Uses a default import when DEFAULT is true or SOURCE is configured for default imports."
  (if (or default (redwood-default-import-source-p source))
      `(import ,symbol :from ,source)
      `(import (,symbol) :from ,source)))

(defun redwood-component-import-path (component &optional explicit-path)
  "Return the worker import path for COMPONENT, using EXPLICIT-PATH when provided."
  (or explicit-path
      (format nil "./app/pages/~A/~A"
              (let ((name (symbol-name component)))
                (if (and (symbolp component) (char= (char name 0) #\*))
                    (string-capitalize (kebab-to-camel (string-downcase (subseq name 1))))
                    (string-capitalize (kebab-to-camel (string-downcase name)))))
              (let ((name (symbol-name component)))
                (if (and (symbolp component) (char= (char name 0) #\*))
                    (string-capitalize (kebab-to-camel (string-downcase (subseq name 1))))
                    (string-capitalize (kebab-to-camel (string-downcase name))))))))

(defjsmacro :redwood defapp (&key wrapper routes)
  (labels ((component-name (sym)
             (if (and (symbolp sym) (char= (char (symbol-name sym) 0) #\*))
                 (string-capitalize (kebab-to-camel (string-downcase (subseq (symbol-name sym) 1))))
                 (string-capitalize (kebab-to-camel (string-downcase (symbol-name sym)))))))
    (let* ((wrapper-name (component-name wrapper))
           (normalized-routes (normalize-routes routes)))
      `(progn
         (in-file "src/worker.tsx")
         (import (define-app) :from "rwsdk/worker")
         (import (render route) :from "rwsdk/router")
         (import (,wrapper) :from ,(format nil "./app/~A" wrapper-name))
         ,@(loop for route-spec in normalized-routes
                 collect (let* ((component (second route-spec))
                                (explicit-path (third route-spec)))
                           (redwood-import-form component
                                                (redwood-component-import-path component explicit-path))))
         (export (defconst app-context (create)))
         (export-default
           (define-app
             (array
               (render ,wrapper
                       (array
                         ,@(loop for route-spec in normalized-routes
                                 collect `(route ,(first route-spec) ,(second route-spec))))))))
         (in-file "src/client.tsx")
         (import (init-client) :from "rwsdk/client")
         (init-client)))))

;;* Checkers 
(defcheck :redwood jsx-anchor-valid-href
  "Check that anchor (a) elements have a valid href attribute."
  (when (and (listp form)
             (symbolp (car form))
             (eq (car form) 'a))
    (multiple-value-bind (attrs children)
        (parse-jsx-attrs-and-children (cdr form))
      (declare (ignore children))
      (let ((href-entry (assoc :href attrs)))
        (cond
          ((null href-entry)
           (make-check-result :level :warning
                              :message "Anchor <a> element is missing href attribute"
                              :form form))
          ((let ((val (cdr href-entry)))
             (and (stringp val)
                  (or (string= val "")
                      (string= val "#")
                      (string= val "javascript:void(0)")
                      (string= val "javascript:void(0);"))))
           (make-check-result :level :warning
                              :message (format nil "Anchor <a> has invalid href: ~S"
                                               (cdr href-entry))
                              :form form)))))))
