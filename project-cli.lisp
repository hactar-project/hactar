;;* Project CLI (data-map architecture)
;;
;; A single, per-project Lisp file at CWD/.hactar.lisp that holds a
;; high-level, human-readable, human-editable *data map* of the project:
;; its routes, models, and stack. The file is generated (partly by
;; hard-coded extraction, partly by an LLM/diffusion model), loaded by
;; Hactar on startup, and kept in sync by a fast diffusion model.
;;
;; The generated file contains DATA, not behaviour:
;;
;;   (defproject "tal" :stack (redwood react) :root *repo-root*)
;;   (defroutes
;;     (:file "src/routes/login.tsx"   :path "/login"  :name login  :tags (auth))
;;     (:file "src/worker.tsx:222-555" :path "/logout" :name logout :tags (auth)))
;;   (defmodels
;;     (:name user :file "prisma/schema.prisma:1-6" :fields (id email)))
;;
;; Loading the file BUILDS the in-memory model. Commands are higher-order
;; functions defined ONCE in the engine that query that model.
;;
;; User-facing surface:  hactar <project> <command> [args/flags]
;;   e.g.  hactar tal routes.list --md
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defproject
            defroutes
            defmodels
            defscripts
            defcomponents
            defdeps
            defimports
            defenvs
            register-project-cli
            define-project-command
            generate-project-cli-file
            load-project-cli-file
            project-routes
            project-models
            find-route
            find-model
            add-route!
            route-source
            parse-loc
            *project-clis*)))

;;** Structs (the in-memory model)

(defstruct route-entry
  "One route in the project map. FILE is the source location (LOC string,
possibly path:START-END). PATH is the URL path. NAME/COMPONENT are symbols.
TAGS is a list of symbols. NOTE is a free-form preserved annotation."
  (file nil)
  (path nil)
  (name nil)
  (tags '())
  (component nil)
  (note nil)
  (line-start nil)
  (line-end nil))

(defstruct model-entry
  "One model/table in the project map."
  (name nil)
  (file nil)
  (fields '())
  (note nil)
  (line-start nil)
  (line-end nil))

(defstruct project-cli
  "A generated per-project CLI / data map."
  (name nil :type (or null string))
  (stack '() :type list)
  (root nil)
  (routes '())
  (models '())
  (commands (make-hash-table :test 'equal)))

;;** Registry

(defun project-cli-key (name)
  (string-downcase (string name)))

(defun get-project-cli (name)
  "Return the project-cli registered under NAME, or NIL."
  (gethash (project-cli-key name) *project-clis*))

(defun ensure-project-cli (name)
  "Return the project-cli for NAME, creating and registering one if needed."
  (let ((key (project-cli-key name)))
    (or (gethash key *project-clis*)
        (setf (gethash key *project-clis*)
              (make-project-cli :name key)))))

(defun register-project-cli (name &key stack root)
  "Register (or update) a project CLI named NAME. Installs a top-level
sub-command named NAME so `hactar <name> <command>` dispatches into this
project, and installs the default engine command set (unless overridden)."
  (let* ((key (project-cli-key name))
         (pcli (ensure-project-cli key)))
    (setf (project-cli-name pcli) key)
    (when stack (setf (project-cli-stack pcli) (if (listp stack) stack (list stack))))
    (setf (project-cli-root pcli) (or root *repo-root*))
    (pcli-install-default-commands pcli)
    ;; The sub-command table stores
    ;; (fn docstring cli-options positional-args examples syntax); FN is
    ;; funcalled with the remaining args.
    (setf (gethash key *sub-commands*)
          (list (lambda (args) (dispatch-project-command key args))
                (format nil "Project CLI for ~A. Usage: hactar ~A <command> [args]" key key)
                nil nil nil nil))
    pcli))

;;** Command table + dispatch

(defun register-project-command (project name fn &optional docstring)
  "Register command NAME (a string) with handler FN in PROJECT's command
table. FN is a function of one argument: the list of command arguments.
A USER-REGION `define-project-command` uses this to OVERRIDE an engine
default of the same name."
  (let* ((pcli (ensure-project-cli project))
         (cmd-name (string-downcase (string name))))
    (setf (gethash cmd-name (project-cli-commands pcli))
          (list :fn fn :doc (or docstring "")))
    cmd-name))

(defmacro define-project-command (project name args &body body)
  "Define/override a command NAME in PROJECT (a string). ARGS is a
one-element lambda list bound to the list of command arguments. Intended
for the USER REGION of CWD/.hactar.lisp to override an engine default."
  (let ((doc (when (stringp (first body)) (first body))))
    `(register-project-command ,project ',(string-downcase (string name))
                               (lambda ,args ,@body)
                               ,doc)))

(defun dispatch-project-command (project args)
  "Dispatch ARGS to a command in PROJECT. Resolution order: project-specific
override (USER REGION), then the engine default registered on the table.
With no args, or `commands`, list the available commands."
  (let* ((pcli (get-project-cli project)))
    (unless pcli
      (format t "Unknown project: ~A~%" project)
      (return-from dispatch-project-command nil))
    (let ((cmd-name (first args)))
      (cond
        ((or (null cmd-name)
             (string-equal cmd-name "commands")
             (string-equal cmd-name "help")
             (string-equal cmd-name "--help")
             (string-equal cmd-name "-h"))
         (print-project-commands pcli))
        (t
         (let ((entry (gethash (string-downcase cmd-name)
                               (project-cli-commands pcli))))
           (if entry
               (handler-case
                   (funcall (getf entry :fn) (rest args))
                 (error (e)
                   (format t "Error running ~A ~A: ~A~%" project cmd-name e)
                   nil))
               (progn
                 (format t "Unknown command: ~A ~A~%" project cmd-name)
                 (print-project-commands pcli)
                 nil))))))))

(defun print-project-commands (pcli)
  "Print the command catalogue of PCLI."
  (let ((names '()))
    (maphash (lambda (k v) (push (cons k (getf v :doc)) names))
             (project-cli-commands pcli))
    (setf names (sort names #'string< :key #'car))
    (format t "~A commands:~%" (project-cli-name pcli))
    (when (project-cli-stack pcli)
      (format t "  stack: ~{~A~^, ~}~%" (project-cli-stack pcli)))
    (format t "  routes: ~A, models: ~A~%"
            (length (project-cli-routes pcli))
            (length (project-cli-models pcli)))
    (if names
        (dolist (n names)
          (let ((doc (cdr n)))
            (format t "  ~A~@[  — ~A~]~%"
                    (colorize (car n) :bold-cyan)
                    (when (and doc (> (length doc) 0))
                      (first (cl-ppcre:split "\\n" doc))))))
        (format t "  (no commands defined)~%"))))

;;** Data DSL: defproject / defroutes / defmodels
;;
;; These are the forms written into CWD/.hactar.lisp. Evaluating them
;; builds (or replaces) the in-memory model for the "current" project.

(defvar *pcli-current-project* nil
  "Key of the project most recently established by `defproject`. `defroutes`
and `defmodels` target this project.")

(defun parse-loc (loc)
  "Parse a LOC string of the form \"path\" or \"path:START-END\".
Returns (values relative-path start end), where START/END are 1-based
inclusive integers or NIL."
  (if (null loc)
      (values nil nil nil)
      (let* ((s (string loc))
             (colon (position #\: s :from-end t)))
        (if (and colon
                 (cl-ppcre:scan "^[0-9]+-[0-9]+$" (subseq s (1+ colon))))
            (let* ((range (subseq s (1+ colon)))
                   (dash (position #\- range)))
              (values (subseq s 0 colon)
                      (parse-integer range :end dash)
                      (parse-integer range :start (1+ dash))))
            (values s nil nil)))))

(defun pcli-as-symbol (x)
  "Coerce X to a symbol interned in :hactar, or NIL."
  (cond
    ((null x) nil)
    ((symbolp x) x)
    ((stringp x) (intern (string-upcase x) :hactar))
    (t (intern (string-upcase (princ-to-string x)) :hactar))))

(defun pcli-as-symbols (xs)
  (remove nil (mapcar #'pcli-as-symbol (if (listp xs) xs (list xs)))))

(defun plist->route-entry (plist)
  "Build a route-entry from a PLIST as written in `defroutes`."
  (multiple-value-bind (path start end) (parse-loc (getf plist :file))
    (declare (ignore path))
    (make-route-entry
     :file (getf plist :file)
     :path (getf plist :path)
     :name (pcli-as-symbol (getf plist :name))
     :tags (pcli-as-symbols (getf plist :tags))
     :component (pcli-as-symbol (getf plist :component))
     :note (getf plist :note)
     :line-start start
     :line-end end)))

(defun plist->model-entry (plist)
  "Build a model-entry from a PLIST as written in `defmodels`."
  (multiple-value-bind (path start end) (parse-loc (getf plist :file))
    (declare (ignore path))
    (make-model-entry
     :name (pcli-as-symbol (getf plist :name))
     :file (getf plist :file)
     :fields (pcli-as-symbols (getf plist :fields))
     :note (getf plist :note)
     :line-start start
     :line-end end)))

(defun pcli-define-project (name &key stack root)
  "Runtime backing of `defproject`. Registers the project and marks it
current for subsequent `defroutes`/`defmodels`."
  (let ((pcli (register-project-cli name :stack stack :root root)))
    (setf *pcli-current-project* (project-cli-name pcli))
    pcli))

(defun pcli-set-routes (entries)
  "Replace the current project's route list with ENTRIES (route-entry list).
Replacement (not append) keeps reload idempotent."
  (let ((pcli (and *pcli-current-project*
                   (get-project-cli *pcli-current-project*))))
    (when pcli (setf (project-cli-routes pcli) entries))
    entries))

(defun pcli-set-models (entries)
  "Replace the current project's model list with ENTRIES."
  (let ((pcli (and *pcli-current-project*
                   (get-project-cli *pcli-current-project*))))
    (when pcli (setf (project-cli-models pcli) entries))
    entries))

(defmacro defproject (name &key stack root)
  "Declare the project this file describes. STACK is a literal list of stack
symbols; ROOT is an evaluated form (e.g. *repo-root*)."
  `(pcli-define-project ,name :stack ',stack :root ,root))

(defmacro defroutes (&rest entries)
  "Declare the project's routes. Each ENTRY is a plist:
  (:file LOC :path STRING :name SYMBOL :tags (SYMBOL...) :component SYMBOL :note STRING)"
  `(pcli-set-routes (list ,@(mapcar (lambda (e) `(plist->route-entry ',e)) entries))))

(defmacro defmodels (&rest entries)
  "Declare the project's models. Each ENTRY is a plist:
  (:name SYMBOL :file LOC :fields (SYMBOL...) :note STRING)"
  `(pcli-set-models (list ,@(mapcar (lambda (e) `(plist->model-entry ',e)) entries))))

(defvar *project-scripts* (make-hash-table :test 'equal))
(defvar *project-components* (make-hash-table :test 'equal))
(defvar *project-deps* (make-hash-table :test 'equal))
(defvar *project-imports* (make-hash-table :test 'equal))

(defmacro defscripts (&rest entries)
  `(setf (gethash *pcli-current-project* *project-scripts*) ',entries))

(defmacro defcomponents (&rest entries)
  `(setf (gethash *pcli-current-project* *project-components*) ',entries))

(defmacro defdeps (&rest entries)
  `(setf (gethash *pcli-current-project* *project-deps*) ',entries))

(defmacro defimports (&rest entries)
  `(setf (gethash *pcli-current-project* *project-imports*) ',entries))

(defvar *project-envs* (make-hash-table :test 'equal))

(defmacro defenvs (&rest entries)
  `(setf (gethash *pcli-current-project* *project-envs*) ',entries))


;;** Query / mutation API (over the in-memory model)

(defun project-routes (project)
  "Return the route-entry list for PROJECT."
  (let ((pcli (get-project-cli project)))
    (when pcli (project-cli-routes pcli))))

(defun project-models (project)
  "Return the model-entry list for PROJECT."
  (let ((pcli (get-project-cli project)))
    (when pcli (project-cli-models pcli))))

(defun find-route (project &key path name tag)
  "Return route-entries in PROJECT matching PATH, NAME and/or TAG.
PATH/NAME match case-insensitively (string or symbol); TAG matches
membership in the entry's tags."
  (let ((routes (project-routes project))
        (name-sym (and name (pcli-as-symbol name)))
        (tag-sym (and tag (pcli-as-symbol tag))))
    (remove-if-not
     (lambda (r)
       (and (or (null path)
                (and (route-entry-path r)
                     (string-equal (route-entry-path r) (string path))))
            (or (null name-sym) (eql (route-entry-name r) name-sym))
            (or (null tag-sym) (member tag-sym (route-entry-tags r)))))
     routes)))

(defun find-model (project name)
  "Return the model-entry in PROJECT named NAME (case-insensitive), or NIL."
  (let ((name-sym (pcli-as-symbol name)))
    (find name-sym (project-models project) :key #'model-entry-name)))

(defun add-route! (project entry)
  "Add ENTRY (a route-entry) to PROJECT's in-memory route list. Replaces an
existing route with the same :name. Returns ENTRY."
  (let ((pcli (ensure-project-cli project)))
    (setf (project-cli-routes pcli)
          (append (remove (route-entry-name entry)
                          (project-cli-routes pcli)
                          :key #'route-entry-name)
                  (list entry)))
    entry))

(defun pcli-read-span (loc &optional (root *repo-root*))
  "Read source for LOC. Honours a path:START-END range, returning only those
1-based inclusive lines; otherwise returns the whole file. NIL on failure."
  (multiple-value-bind (rel start end) (parse-loc loc)
    (when rel
      (let ((full (merge-pathnames rel root)))
        (handler-case
            (let ((content (uiop:read-file-string full)))
              (if (and start end)
                  (let ((lines (str:lines content)))
                    (str:join (string #\Newline)
                              (subseq lines
                                      (max 0 (1- start))
                                      (min (length lines) end))))
                  content))
          (error () nil))))))

(defun route-source (entry &optional (root *repo-root*))
  "Read the source of route ENTRY, honouring its line range."
  (pcli-read-span (route-entry-file entry) root))

(defun model-source (entry &optional (root *repo-root*))
  "Read the source of model ENTRY, honouring its line range."
  (pcli-read-span (model-entry-file entry) root))

;;** Default engine commands (registered once per project)

(defun pcli-flag-present-p (args &rest names)
  (some (lambda (n) (member n args :test #'string=)) names))

(defun pcli-flag-value (args flag)
  "Return the value following FLAG in ARGS, or NIL."
  (let ((pos (position flag args :test #'string=)))
    (when (and pos (< (1+ pos) (length args)))
      (nth (1+ pos) args))))

(defun pcli-positional-args (args)
  "Return ARGS with flags (and their values) removed."
  (let ((result '())
        (skip nil))
    (loop for a in args
          do (cond
               (skip (setf skip nil))
               ((str:starts-with? "--" a)
                (unless (member a '("--md" "--org") :test #'string=)
                  (setf skip t)))
               ((string= a "tag") (setf skip t))
               (t (push a result))))
    (nreverse result)))

(defun pcli-route-label (r)
  "Human label for a route entry: path (name) [tags] -> file."
  (format nil "~A~@[  (~A)~]~@[  [~{~A~^, ~}]~]  -> ~A"
          (or (route-entry-path r) "?")
          (and (route-entry-name r) (string-downcase (symbol-name (route-entry-name r))))
          (mapcar (lambda (s) (string-downcase (symbol-name s))) (route-entry-tags r))
          (route-entry-file r)))

(defun pcli-cmd-routes-list (project args)
  "List routes from the map. Supports `tag <t>`, `--md`, `--org`."
  (let* ((tag (pcli-flag-value args "tag"))
         (routes (if tag (find-route project :tag tag) (project-routes project)))
         (md (pcli-flag-present-p args "--md"))
         (org (pcli-flag-present-p args "--org"))
         (root (or (project-cli-root (get-project-cli project)) *repo-root*)))
    (cond
      (org (pcli-emit-routes-org routes root))
      (md  (pcli-emit-routes-md routes root))
      (t   (if routes
               (dolist (r routes) (format t "~A~%" (pcli-route-label r)))
               (format t "No routes in the map. Run `hactar project.update` to (re)scan.~%"))))
    routes))

(defun pcli-emit-routes-md (routes root)
  "Emit a markdown bundle of all ROUTES, reading each entry's source span."
  (format t "# Routes~%~%")
  (dolist (r routes)
    (format t "## ~A~@[ (~A)~]~%~%```~%~A~%```~%~%"
            (or (route-entry-path r) (route-entry-file r))
            (and (route-entry-name r) (string-downcase (symbol-name (route-entry-name r))))
            (or (route-source r root) ""))))

(defun pcli-emit-routes-org (routes root)
  "Emit a literate org-mode document of all ROUTES."
  (format t "#+TITLE: Routes~%~%")
  (dolist (r routes)
    (format t "* ~A~@[ (~A)~]~%~@[  :PROPERTIES:~%  :FILE: ~A~%  :END:~%~]#+begin_src tsx~%~A~%#+end_src~%~%"
            (or (route-entry-path r) (route-entry-file r))
            (and (route-entry-name r) (string-downcase (symbol-name (route-entry-name r))))
            (route-entry-file r)
            (or (route-source r root) ""))))

(defun pcli-cmd-routes-search (project args)
  "Search routes by substring across path, name, and tags."
  (let* ((query (string-downcase (str:join " " (pcli-positional-args args))))
         (routes (project-routes project)))
    (if (string= query "")
        (format t "Usage: routes.search <query>~%")
        (let ((hits (remove-if-not
                     (lambda (r)
                       (or (and (route-entry-path r)
                                (search query (string-downcase (route-entry-path r))))
                           (and (route-entry-name r)
                                (search query (string-downcase (symbol-name (route-entry-name r)))))
                           (some (lambda (tg) (search query (string-downcase (symbol-name tg))))
                                 (route-entry-tags r))
                           (and (route-entry-file r)
                                (search query (string-downcase (route-entry-file r))))))
                     routes)))
          (if hits
              (dolist (r hits) (format t "~A~%" (pcli-route-label r)))
              (format t "No routes match '~A'.~%" query))
          hits))))

(defun pcli-cmd-routes-add (project args)
  "Scaffold a new route component, add it to the map, and re-serialize.
Usage: routes.add <name> --path <p> [--tags a,b]."
  (let* ((positionals (pcli-positional-args args))
         (name (first positionals))
         (path (or (pcli-flag-value args "--path")
                   (and name (format nil "/~A" (string-downcase (kebab-case name))))))
         (tags-raw (pcli-flag-value args "--tags")))
    (if (null name)
        (format t "Usage: routes.add <name> --path <path> [--tags a,b]~%")
        (let* ((class-name (pascal-case name))
               (rel (format nil "src/app/pages/~A/~A.tsx"
                            (string-downcase (kebab-case name)) class-name))
               (content (format nil "import { RequestInfo } from 'rwsdk/worker';~%~%export function ~A({ request }: RequestInfo) {~%  return (~%    <div>~%      <h1>~A</h1>~%    </div>~%  );~%}~%"
                                class-name class-name))
               (tags (when tags-raw
                       (pcli-as-symbols (cl-ppcre:split "[,\\s]+" tags-raw))))
               (entry (make-route-entry
                       :file rel
                       :path path
                       :name (pcli-as-symbol name)
                       :tags tags
                       :component (pcli-as-symbol class-name))))
          (scaffold-create-file rel content)
          (add-route! project entry)
          (reserialize-region project :routes)
          (format t "Route '~A' created at ~A (~A) and added to the map.~%"
                  name path rel)
          entry))))

(defun pcli-cmd-routes-gen (project args)
  "Generate a route from a natural-language description using the LLM."
  (let* ((pcli (get-project-cli project))
         (stack (and pcli (project-cli-stack pcli)))
         (instructions (str:join " " args)))
    (if (or (null args) (string= (string-trim " " instructions) ""))
        (format t "Usage: routes.gen \"<instructions>\"~%")
        (let ((prompt (format nil "You are generating a RedwoodSDK route for this project (stack: ~{~A~^, ~}).~%Produce a single TSX route component and describe its URL path, name, and any tags.~%Follow RedwoodSDK conventions (rwsdk/worker, route()/render()).~%~%Instructions: ~A"
                              stack instructions)))
          (if *current-model*
              (get-llm-response prompt :stream t :add-to-history nil)
              (format t "No model selected. Use /model to select one before routes.gen.~%"))))))

(defun pcli-cmd-models-list (project args)
  "List models from the map."
  (declare (ignore args))
  (let ((models (project-models project)))
    (if models
        (dolist (m models)
          (format t "~A~@[  (~{~A~^, ~})~]~%"
                  (string-downcase (symbol-name (model-entry-name m)))
                  (mapcar (lambda (s) (string-downcase (symbol-name s))) (model-entry-fields m))))
        (format t "No models in the map. Run `hactar project.update` to (re)scan.~%"))
    models))

(defun pcli-cmd-schema (project args)
  "Print the schema, or a single model's source. Usage: schema [<table>]."
  (let* ((table (first args))
         (root (or (project-cli-root (get-project-cli project)) *repo-root*))
         (models (project-models project)))
    (cond
      ((null models) (format t "No models in the map.~%"))
      ((null table)
       (dolist (m models)
         (let ((src (model-source m root)))
           (when src (format t "~A~%~%" src)))))
      (t
       (let ((m (find-model project table)))
         (if m
             (format t "~A~%" (or (model-source m root)
                                  (format nil "model ~A" (model-entry-name m))))
             (format t "No model named ~A.~%" table)))))))

(defun pcli-cmd-feature (project args)
  "List/summarize code dealing with a concern. Cross-references the map's
route tags and falls back to git grep."
  (let* ((feature (str:join " " args))
         (root (or (project-cli-root (get-project-cli project)) *repo-root*)))
    (if (or (null args) (string= (string-trim " " feature) ""))
        (format t "Usage: feature <name>~%")
        (let ((tagged (find-route project :tag feature)))
          (when tagged
            (format t "Routes tagged '~A':~%" feature)
            (dolist (r tagged) (format t "  ~A~%" (pcli-route-label r))))
          (let ((matches
                  (handler-case
                      (multiple-value-bind (out err code)
                          (uiop:run-program
                           (list "git" "-C" (namestring root) "grep" "-l" "-i" feature)
                           :output :string :error-output :string :ignore-error-status t)
                        (declare (ignore err))
                        (when (zerop code)
                          (remove-if (lambda (s) (string= s "")) (str:lines out))))
                    (error () nil))))
            (cond
              (matches
               (format t "~@[~%~]Files related to '~A':~%" (when tagged t) feature)
               (dolist (f matches) (format t "  ~A~%" f))
               (when *current-model*
                 (format t "~%Summary:~%")
                 (get-llm-response
                  (format nil "Briefly summarize how the concern '~A' is implemented across these files: ~{~A~^, ~}. Do not propose edits."
                          feature matches)
                  :stream t :add-to-history nil)))
              ((null tagged)
               (format t "No code found related to '~A'.~%" feature))))))))

(defun pcli-package-scripts (&optional (root *repo-root*))
  "Return the package.json scripts as an alist of (name . command), or NIL."
  (let ((json (pcli-read-package-json root)))
    (when json
      (mapcar (lambda (pair)
                (cons (string-downcase (string (car pair))) (cdr pair)))
              (cdr (assoc :scripts json))))))

(defun pcli-run-script (script-names &key (fallback nil) (root *repo-root*))
  "Run the first matching package.json script from SCRIPT-NAMES, or FALLBACK."
  (let* ((scripts (pcli-package-scripts root))
         (chosen (loop for n in script-names
                       when (assoc n scripts :test #'string-equal)
                         return n))
         (cmd (cond
                (chosen (format nil "npm run ~A" chosen))
                (fallback fallback)
                (t nil))))
    (if (null cmd)
        (format t "No suitable script found (looked for: ~{~A~^, ~}).~%" script-names)
        (progn
          (format t "~&$ ~A~%" cmd)
          (uiop:run-program cmd :directory root
                                :output t :error-output t
                                :ignore-error-status t)))))

(defun pcli-cmd-build (project args)
  "Build the project."
  (declare (ignore args))
  (pcli-run-script '("build") :fallback "npm run build"
                              :root (or (project-cli-root (get-project-cli project)) *repo-root*)))

(defun pcli-cmd-release (project args)
  "Release/deploy the project."
  (declare (ignore args))
  (pcli-run-script '("release" "deploy") :fallback "npm run deploy"
                                         :root (or (project-cli-root (get-project-cli project)) *repo-root*)))

(defun pcli-install-default-commands (pcli)
  "Install the engine's default command set on PCLI, unless a command of the
same name has already been registered (USER-REGION overrides win because
the file loads register-project-cli before any custom define-project-command;
defaults therefore only fill gaps)."
  (let ((project (project-cli-name pcli))
        (table (project-cli-commands pcli)))
    (flet ((def (name fn doc)
             (unless (gethash name table)
               (setf (gethash name table) (list :fn fn :doc doc)))))
      (def "routes.list"   (lambda (a) (pcli-cmd-routes-list project a))
        "List routes. Flags: tag <t>, --md, --org.")
      (def "routes.search" (lambda (a) (pcli-cmd-routes-search project a))
        "Search routes by path/name/tag substring.")
      (def "routes.add"    (lambda (a) (pcli-cmd-routes-add project a))
        "Scaffold a route, add it to the map. Usage: routes.add <name> --path <p>.")
      (def "routes.gen"    (lambda (a) (pcli-cmd-routes-gen project a))
        "Generate a route from a natural-language description (LLM).")
      (def "models.list"   (lambda (a) (pcli-cmd-models-list project a))
        "List all models from the map.")
      (def "schema"        (lambda (a) (pcli-cmd-schema project a))
        "Print the schema. Usage: schema [<table>].")
      (def "feature"       (lambda (a) (pcli-cmd-feature project a))
        "List/summarize code dealing with a concern. Usage: feature <name>.")
      (def "build"         (lambda (a) (pcli-cmd-build project a))
        "Build the project.")
      (def "release"       (lambda (a) (pcli-cmd-release project a))
        "Release/deploy the project."))))

;;** Loading CWD/.hactar.lisp

(defun project-cli-pathname (&optional (root *repo-root*))
  "Return the pathname of the project CLI file under ROOT."
  (when root (merge-pathnames ".hactar.lisp" root)))

(defun load-project-cli-file (&optional (root *repo-root*))
  "Load CWD/.hactar.lisp if present. Idempotent. Returns the pathname or NIL."
  (let ((file (project-cli-pathname root)))
    (when (and file (probe-file file))
      (handler-case
          (progn
            (unless *silent*
              (format t "~&Loading project CLI from: ~A~%" file))
            (let ((*package* (find-package :hactar)))
              (load file))
            (setf *project-cli-file* file)
            file)
        (error (e)
          (log-warning "Error loading project CLI ~A: ~A" file e)
          nil)))))

;;** Stack / project detection (hard-coded)

(defun pcli-read-package-json (&optional (root *repo-root*))
  "Read package.json under ROOT and return its decoded alist, or NIL."
  (let ((pj (and root (merge-pathnames "package.json" root))))
    (when (and pj (probe-file pj))
      (handler-case
          (cl-json:decode-json-from-string (uiop:read-file-string pj))
        (error () nil)))))

(defun pcli-detect-stack (&optional (root *repo-root*))
  "Detect the project's stack from package.json deps, unioned with *stack*."
  (let* ((json (pcli-read-package-json root))
         (deps (when json
                 (append (mapcar #'car (cdr (assoc :dependencies json)))
                         (mapcar #'car (cdr (assoc :dev-dependencies json)))
                         (mapcar #'car (cdr (assoc :dev--dependencies json))))))
         (dep-names (mapcar (lambda (d) (string-downcase (string d))) deps))
         (stack (copy-list *stack*)))
    (flet ((add (tech) (pushnew tech stack :test #'string-equal)))
      (dolist (d dep-names)
        (cond
          ((or (string= d "rwsdk") (string= d "@redwoodjs/sdk")
               (search "redwood" d)) (add "redwood"))
          ((string= d "react") (add "react"))
          ((or (string= d "react-router") (string= d "react-router-dom"))
           (add "react-router"))
          ((string= d "prisma") (add "prisma"))
          ((string= d "@prisma/client") (add "prisma"))
          ((string= d "typescript") (add "typescript"))
          ((string= d "vite") (add "vite"))
          ((string= d "wrangler") (add "cloudflare"))))
      (when (probe-file (merge-pathnames "prisma/schema.prisma" root))
        (add "prisma")))
    (nreverse stack)))

(defun pcli-project-name (&optional (root *repo-root*))
  "Determine the project name from *name*, package.json, or the directory."
  (or (and *name* (string-downcase *name*))
      (let ((json (pcli-read-package-json root)))
        (let ((n (and json (cdr (assoc :name json)))))
          (when (and n (stringp n) (> (length n) 0))
            (let ((slash (position #\/ n)))
              (string-downcase (if slash (subseq n (1+ slash)) n))))))
      (and root (string-downcase (car (last (pathname-directory root)))))
      "project"))

;;** Extraction (the only place regex/heuristics run)

(defun pcli-list-files (&optional (root *repo-root*))
  "List repo-relative file paths under ROOT (git, then fd, then walk)."
  (or (when (ignore-errors (git-repo-present-p root))
        (ignore-errors (list-git-tracked-files root)))
      (ignore-errors (list-files-with-fd root))
      (ignore-errors
        (let ((results '()))
          (uiop:collect-sub*directories
           (uiop:ensure-directory-pathname root)
           (lambda (d)
             (let ((name (car (last (pathname-directory d)))))
               (not (member name '(".git" "node_modules" "dist" "build"
                                   ".hactar" "straight" ".cache")
                            :test #'equal))))
           (constantly t)
           (lambda (d)
             (dolist (f (uiop:directory-files d))
               (push (uiop:native-namestring (uiop:enough-pathname f root))
                     results))))
          (nreverse results)))
      '()))

(defun pcli-route-files (&optional (root *repo-root*))
  "Heuristically collect RedwoodSDK/React route source files relative to ROOT."
  (let ((files (pcli-list-files root)))
    (remove-if-not
     (lambda (f)
       (let ((lf (string-downcase f)))
         (and (or (str:ends-with? ".tsx" lf) (str:ends-with? ".ts" lf)
                  (str:ends-with? ".jsx" lf) (str:ends-with? ".js" lf))
              (or (search "route" lf)
                  (search "/pages/" lf)
                  (search "/app/" lf)
                  (search "worker" lf)))))
     files)))

(defun pcli-file-tags (full-path)
  "Extract tags (symbols) from `// @tag: a, b` style comments in a file."
  (handler-case
      (let* ((content (uiop:read-file-string full-path))
             (tags '()))
        (cl-ppcre:do-register-groups (grp)
            ("(?i)(?:@tags?|hactar-tags?):\\s*([^\\n]+)" content)
          (dolist (tag (cl-ppcre:split "[,\\s]+" (string-trim " " grp)))
            (when (> (length tag) 0) (push (pcli-as-symbol (string-downcase tag)) tags))))
        (nreverse tags))
    (error () nil)))

(defun pcli-file->name (rel)
  "Derive a route name symbol from a relative file path (basename, no ext)."
  (let* ((base (file-namestring rel))
         (dot (position #\. base))
         (stem (if dot (subseq base 0 dot) base)))
    (pcli-as-symbol (string-downcase (kebab-case stem)))))

(defun pcli-extract-route-paths (content)
  "Return a list of URL paths from RedwoodSDK `route('/x', ...)` calls in
CONTENT, in order of appearance. Empty when none are found."
  (let ((paths '()))
    (cl-ppcre:do-register-groups (p)
        ("route\\(\\s*[\"'`]([^\"'`]+)[\"'`]" content)
      (push p paths))
    (nreverse paths)))

(defun pcli-route-match-lines (content)
  "Return the 1-based line numbers of `route(` calls in CONTENT."
  (let ((lines '())
        (n 0))
    (dolist (line (str:lines content) (nreverse lines))
      (incf n)
      (when (cl-ppcre:scan "route\\(\\s*[\"'`]" line)
        (push n lines)))))

(defun pcli-path->name (path)
  "Derive a route name symbol from a URL PATH (last non-empty segment)."
  (let* ((segs (remove "" (cl-ppcre:split "/" path) :test #'string=))
         (last (car (last segs))))
    (when last
      (pcli-as-symbol (string-downcase (cl-ppcre:regex-replace-all "[^a-zA-Z0-9]+" last "-"))))))

(defun extract-routes-in-file (rel root)
  "Extract route-entry list from a single file REL (relative to ROOT).
A file with multiple `route()` calls yields one entry per call with a
line-range LOC (path:START-END); otherwise a single whole-file entry."
  (let* ((full (merge-pathnames rel root))
         (content (handler-case (uiop:read-file-string full) (error () nil))))
    (if (null content)
        '()
        (let* ((tags (pcli-file-tags full))
               (paths (pcli-extract-route-paths content))
               (lines (pcli-route-match-lines content)))
          (cond
            ;; Multiple embedded route() handlers: per-call entries w/ ranges.
            ((and (> (length paths) 1) (= (length paths) (length lines)))
             (loop for p in paths
                   for ls in lines
                   for le in (append (mapcar #'1- (rest lines))
                                     (list (length (str:lines content))))
                   collect (make-route-entry
                            :file (format nil "~A:~A-~A" rel ls (max ls le))
                            :path p
                            :name (or (pcli-path->name p) (pcli-file->name rel))
                            :tags tags
                            :line-start ls
                            :line-end (max ls le))))
            ;; Single route() call: whole-file entry with its declared path.
            ((= (length paths) 1)
             (list (make-route-entry
                    :file rel
                    :path (first paths)
                    :name (or (pcli-path->name (first paths)) (pcli-file->name rel))
                    :tags tags)))
            ;; No route() calls: file-based route; infer path from filename.
            (t
             (let ((name (pcli-file->name rel)))
               (list (make-route-entry
                      :file rel
                      :path (format nil "/~A" (string-downcase (symbol-name name)))
                      :name name
                      :tags tags)))))))))

(defun extract-routes (&optional (root *repo-root*))
  "Extract the full route-entry list for the project under ROOT."
  (let ((entries '()))
    (dolist (rel (pcli-route-files root))
      (setf entries (append entries (extract-routes-in-file rel root))))
    entries))

(defun pcli-schema-file (&optional (root *repo-root*))
  "Locate the Prisma schema file under ROOT, or NIL. Returns (values full rel)."
  (dolist (rel '("prisma/schema.prisma" "db/schema.prisma" "schema.prisma"))
    (let ((full (merge-pathnames rel root)))      (when (probe-file full)
        (return-from pcli-schema-file (values full rel)))))
  (values nil nil))

(defun extract-models (&optional (root *repo-root*))
  "Extract model-entry list from the Prisma schema, capturing line spans and
field names."
  (multiple-value-bind (full rel) (pcli-schema-file root)
    (when full
      (let* ((content (handler-case (uiop:read-file-string full) (error () "")))
             (lines (coerce (str:lines content) 'vector))
             (entries '()))
        (loop for i from 0 below (length lines)
              for line = (aref lines i)
              do (multiple-value-bind (m grps)
                     (cl-ppcre:scan-to-strings "^\\s*model\\s+(\\w+)\\s*\\{" line)
                   (when m
                     (let* ((name (aref grps 0))
                            (start (1+ i))
                            (end start)
                            (fields '()))
                       ;; walk to the closing brace
                       (loop for j from (1+ i) below (length lines)
                             for body = (aref lines j)
                             do (setf end (1+ j))
                                (let ((fld (nth-value 1 (cl-ppcre:scan-to-strings
                                                         "^\\s*(\\w+)\\s+\\w" body))))
                                  (when fld (push (pcli-as-symbol (aref fld 0)) fields)))
                             until (cl-ppcre:scan "^\\s*\\}" body))
                       (push (make-model-entry
                              :name (pcli-as-symbol name)
                              :file (format nil "~A:~A-~A" rel start end)
                              :fields (nreverse fields)
                              :line-start start
                              :line-end end)
                             entries)))))
        (nreverse entries)))))

;;** Serialization (structs -> data DSL forms)

(defparameter *pcli-user-region-marker*
  ";; --- USER REGION (never auto-overwritten) ---"
  "Everything from this marker to end-of-file is preserved across regeneration.")

(defun pcli-region-begin (tag) (format nil ";; <<<BEGIN GENERATED: ~A>>>" tag))
(defun pcli-region-end (tag) (format nil ";; <<<END GENERATED: ~A>>>" tag))

(defun pcli-sym (s)
  "Render symbol S in lowercase, or NIL for nil."
  (when s (string-downcase (symbol-name s))))

(defun render-defproject (project stack root-form)
  "Render the (defproject ...) form for PROJECT with STACK."
  (with-output-to-string (s)
    (format s "~A~%" (pcli-region-begin "meta"))
    (format s "(defproject ~S~%  :stack (~{~A~^ ~})~%  :root ~A)~%"
            project (mapcar #'string-downcase stack) root-form)
    (format s "~A" (pcli-region-end "meta"))))

(defun render-route-entry (r)
  "Render a single route-entry as a plist line."
  (format nil "  (:file ~S~@[ :path ~S~]~@[ :name ~A~]~@[ :tags (~{~A~^ ~})~]~@[ :component ~A~]~@[ :note ~S~])"
          (route-entry-file r)
          (route-entry-path r)
          (pcli-sym (route-entry-name r))
          (mapcar #'pcli-sym (route-entry-tags r))
          (pcli-sym (route-entry-component r))
          (route-entry-note r)))

(defun render-defroutes (routes)
  "Render the (defroutes ...) form from ROUTES (route-entry list)."
  (with-output-to-string (s)
    (format s "~A~%" (pcli-region-begin "routes"))
    (if routes
        (progn
          (format s "(defroutes~%")
          (loop for (r . more) on routes
                do (format s "~A~A~%" (render-route-entry r) (if more "" ")"))))
        (format s "(defroutes)~%"))
    (format s "~A" (pcli-region-end "routes"))))

(defun render-model-entry (m)
  "Render a single model-entry as a plist line."
  (format nil "  (:name ~A :file ~S~@[ :fields (~{~A~^ ~})~]~@[ :note ~S~])"
          (pcli-sym (model-entry-name m))
          (model-entry-file m)
          (mapcar #'pcli-sym (model-entry-fields m))
          (model-entry-note m)))

(defun render-defmodels (models)
  "Render the (defmodels ...) form from MODELS (model-entry list)."
  (with-output-to-string (s)
    (format s "~A~%" (pcli-region-begin "models"))
    (if models
        (progn
          (format s "(defmodels~%")
          (loop for (m . more) on models
                do (format s "~A~A~%" (render-model-entry m) (if more "" ")"))))
        (format s "(defmodels)~%"))
    (format s "~A" (pcli-region-end "models"))))

(defun pcli-extract-user-region (existing-content)
  "Return the user region (from the USER REGION marker to EOF), or NIL."
  (when existing-content
    (let ((pos (search *pcli-user-region-marker* existing-content)))
      (when pos (subseq existing-content pos)))))

(defun pcli-default-user-region ()
  (format nil "~A~%;; Add custom project commands below with define-project-command;~%;; the generator preserves anything from this marker onward.~%"
          *pcli-user-region-marker*))

(defun pcli-render-file (project stack routes models &optional existing-content)
  "Render the full text of CWD/.hactar.lisp as a data map."
  (let ((user-region (or (pcli-extract-user-region existing-content)
                         (pcli-default-user-region))))
    (with-output-to-string (s)
      (let ((*print-case* :downcase))
        (format s ";;; -*- mode: lisp; package: hactar -*-~%")
        (format s ";;; .hactar.lisp — generated project data map for ~S~%" project)
        (format s ";;; A high-level map of the project (routes, models, stack).~%")
        (format s ";;; Edit freely. BEGIN/END GENERATED regions are owned by the~%")
        (format s ";;; generator + diffusion model and may be rewritten.~%")
        (format s "(in-package :hactar)~%~%")
        (format s "~A~%~%" (render-defproject project stack "*repo-root*"))
        (format s "~A~%~%" (render-defroutes routes))
        (format s "~A~%~%" (render-defmodels models))
        (format s "~A~%" (string-right-trim '(#\Newline) user-region))))))

;;** Region splicing (for incremental re-serialization)

(defun pcli-region-tag-string (tag)
  (string-downcase (string tag)))

(defun pcli-splice-region (content tag new-body)
  "Replace the text between the BEGIN/END markers for TAG (inclusive) in
CONTENT with NEW-BODY (which must include its own markers). If the region
is absent, return CONTENT unchanged."
  (let* ((tag-s (pcli-region-tag-string tag))
         (begin (pcli-region-begin tag-s))
         (end (pcli-region-end tag-s))
         (bpos (search begin content))
         (epos (and bpos (search end content :start2 bpos))))
    (if (and bpos epos)
        (concatenate 'string
                     (subseq content 0 bpos)
                     new-body
                     (subseq content (+ epos (length end))))
        content)))

(defun reserialize-region (project tag)
  "Regenerate only the data region TAG (:meta | :routes | :models) of
PROJECT's .hactar.lisp from the in-memory model and splice it back, then
reload. Returns the file pathname or NIL."
  (let* ((pcli (get-project-cli project))
         (root (or (and pcli (project-cli-root pcli)) *repo-root*))
         (file (project-cli-pathname root)))
    (when (and pcli file (probe-file file))
      (let* ((content (uiop:read-file-string file))
             (body (ecase tag
                     (:meta (let ((*print-case* :downcase))
                              (render-defproject (project-cli-name pcli)
                                                 (project-cli-stack pcli) "*repo-root*")))
                     (:routes (let ((*print-case* :downcase))
                                (render-defroutes (project-cli-routes pcli))))
                     (:models (let ((*print-case* :downcase))
                                (render-defmodels (project-cli-models pcli))))))
             (updated (pcli-splice-region content (pcli-region-tag-string tag) body)))
        (write-file-content (uiop:native-namestring file) updated)
        (load-project-cli-file root)
        file))))

;;** Generation = extract -> serialize -> write -> load

(defun pcli-refresh-map (&optional (root *repo-root*))
  "Re-extract the project's routes and models from source. Returns
(values project stack routes models)."
  (let ((project (pcli-project-name root))
        (stack (pcli-detect-stack root))
        (routes (extract-routes root))
        (models (extract-models root)))
    (values project stack routes models)))

(defun generate-project-cli-file (&key (root *repo-root*) (load t))
  "Extract the project map and (re)generate CWD/.hactar.lisp as data,
preserving the user region. When LOAD, load the result. Returns pathname."
  (unless root
    (ensure-repo-root)
    (setf root *repo-root*))
  (let* ((name (pcli-project-name root))
         (stack (pcli-detect-stack root))
         (file (project-cli-pathname root))
         (pcli (ensure-project-cli name)))
    (setf (project-cli-name pcli) name
          (project-cli-root pcli) root
          (project-cli-stack pcli) stack)
    (setf *pcli-current-project* name)
    (write-hactar-lisp (list `(defproject ,name :stack ,stack :root *repo-root*))
                       file
                       :merge-p nil)
    (run-active-fusions pcli)
    (when load (load-project-cli-file root))
    file))

;;** Sub-commands: scan / init / project.update

(define-sub-command scan (args)
  "Scan the current project and generate CWD/.hactar.lisp (project data map)."
  (declare (ignore args))
  (generate-project-cli-file :load t))

(define-sub-command init (args)
  "Initialize a project map: generate CWD/.hactar.lisp if absent."
  (declare (ignore args))
  (let ((file (project-cli-pathname *repo-root*)))
    (if (and file (probe-file file))
        (progn
          (format t "~A already exists. Use `project.update` to regenerate.~%"
                  (uiop:native-namestring file))
          (load-project-cli-file *repo-root*))
        (generate-project-cli-file :load t))))

(define-sub-command project.update (args)
  "Rescan the project and regenerate the generated data regions of
CWD/.hactar.lisp, preserving the user region."
  (declare (ignore args))
  (generate-project-cli-file :load t))

;;** Continuous diffusion-driven updates

(defun pcli-diffusion-relevant-p (pathname event-type)
  "True if a file change should trigger project-map regeneration."
  (and (member event-type '(:file-added :file-changed :file-deleted))
       *auto-project-cli*
       *diffusion-model*
       (let ((p (string-downcase (namestring pathname))))
         (and (not (search ".hactar.lisp" p))
              (or (search "route" p)
                  (search "/pages/" p)
                  (search "/app/" p)
                  (search "worker" p)
                  (search "schema.prisma" p)
                  (search "package.json" p))))))

(defun pcli-enqueue-update (pathname)
  (pushnew (namestring pathname) *project-cli-update-queue* :test #'equal))

(defun pcli-diffusion-regenerate ()
  "Re-extract the project map using active fusions and regenerate CWD/.hactar.lisp."
  (let* ((root *repo-root*)
         (name (pcli-project-name root))
         (pcli (ensure-project-cli name)))
    (setf (project-cli-root pcli) root)
    (setf *pcli-current-project* name)
    (run-active-fusions pcli)
    (load-project-cli-file root)
    (setf *project-cli-update-queue* '())))

(def-analyzer project-cli-diffusion ((*file-event-hook*)) nil (pathname event-type)
  "On relevant source changes, enqueue and (debounced) re-extract the project
map via the diffusion model."
  (when (pcli-diffusion-relevant-p pathname event-type)
    (debug-log "Project map diffusion analyzer triggered by:" pathname)
    (pcli-enqueue-update pathname)
    (pcli-diffusion-regenerate)))
