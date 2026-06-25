;;* Project CLI
;;
;; Implements the design in ProjectCLIPlan.org: a single, per-project Lisp
;; file at CWD/.hactar.lisp that defines dev tooling commands scoped to a
;; project and its stack. The file is generated (partly by hard-coded logic,
;; partly by an LLM), human-editable, loaded by Hactar on startup, and kept
;; in sync by a fast diffusion model.
;;
;; User-facing surface:  hactar <project> <command> [args/flags]
;;   e.g.  hactar tal routes.list --md
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(register-project-cli
            define-project-command
            generate-project-cli-file
            load-project-cli-file
            *project-clis*)))

;;** Registry

(defstruct project-cli
  "A generated per-project CLI."
  (name nil :type (or null string))
  (stack '() :type list)
  (root nil)
  (commands (make-hash-table :test 'equal)))

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
  "Register (or update) a project CLI named NAME. Called from CWD/.hactar.lisp.
Also installs a top-level sub-command named NAME so that
`hactar <name> <command>` dispatches into this project's command table."
  (let* ((key (project-cli-key name))
         (pcli (ensure-project-cli key)))
    (setf (project-cli-name pcli) key)
    (when stack (setf (project-cli-stack pcli) (if (listp stack) stack (list stack))))
    (setf (project-cli-root pcli) (or root *repo-root*))
    ;; Register a runtime sub-command for the project name. The sub-command
    ;; table stores (fn docstring cli-options positional-args examples syntax);
    ;; FN is funcalled with the remaining args.
    (setf (gethash key *sub-commands*)
          (list (lambda (args) (dispatch-project-command key args))
                (format nil "Project CLI for ~A. Usage: hactar ~A <command> [args]" key key)
                nil nil nil nil))
    pcli))

(defun register-project-command (project name fn &optional docstring)
  "Register command NAME (a string) with handler FN in PROJECT's command table.
FN is a function of one argument: the list of command arguments."
  (let* ((pcli (ensure-project-cli project))
         (cmd-name (string-downcase (string name))))
    (setf (gethash cmd-name (project-cli-commands pcli))
          (list :fn fn :doc (or docstring "")))
    cmd-name))

(defmacro define-project-command (project name args &body body)
  "Define a command NAME in PROJECT (a string). ARGS is a one-element lambda
list bound to the list of command arguments. The first BODY form may be a
docstring. Intended to be written into CWD/.hactar.lisp but usable anywhere.

Example:
  (define-project-command \"tal\" routes.list (args)
    \"List all routes.\"
    (pcli-routes-list \"tal\" args))"
  (let ((doc (when (stringp (first body)) (first body))))
    `(register-project-command ,project ',(string-downcase (string name))
                               (lambda ,args ,@body)
                               ,doc)))

;;** Dispatch

(defun dispatch-project-command (project args)
  "Dispatch ARGS to a command in PROJECT. With no args, or `commands`, list
the available commands."
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
    (if names
        (dolist (n names)
          (let ((doc (cdr n)))
            (format t "  ~A~@[  — ~A~]~%"
                    (colorize (car n) :bold-cyan)
                    (when (and doc (> (length doc) 0))
                      (first (cl-ppcre:split "\\n" doc))))))
        (format t "  (no commands defined)~%"))))

;;** Loading CWD/.hactar.lisp

(defun project-cli-pathname (&optional (root *repo-root*))
  "Return the pathname of the project CLI file under ROOT."
  (when root (merge-pathnames ".hactar.lisp" root)))

(defun load-project-cli-file (&optional (root *repo-root*))
  "Load CWD/.hactar.lisp if present. Safe to call repeatedly (idempotent
registration). Returns the loaded pathname or NIL."
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

(defun pcli-package-scripts (&optional (root *repo-root*))
  "Return the package.json scripts as an alist of (name . command), or NIL."
  (let ((json (pcli-read-package-json root)))
    (when json
      (mapcar (lambda (pair)
                (cons (string-downcase (string (car pair))) (cdr pair)))
              (cdr (assoc :scripts json))))))

(defun pcli-detect-stack (&optional (root *repo-root*))
  "Detect the project's stack from package.json dependencies, unioned with the
already-known *stack*. Returns a list of lowercased technology names."
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
            ;; strip any npm scope, e.g. @org/tal -> tal
            (let ((slash (position #\/ n)))
              (string-downcase (if slash (subseq n (1+ slash)) n))))))
      (and root (string-downcase (car (last (pathname-directory root)))))
      "project"))

;;** File generation (hard-coded + region markers)

(defparameter *pcli-user-region-marker*
  ";; --- USER REGION (never auto-overwritten) ---"
  "Everything from this marker to end-of-file is preserved across regeneration.")

(defun pcli-region-begin (tag) (format nil ";; <<<BEGIN GENERATED: ~A>>>" tag))
(defun pcli-region-end (tag) (format nil ";; <<<END GENERATED: ~A>>>" tag))

(defun pcli-extract-user-region (existing-content)
  "Return the user region (from the USER REGION marker to EOF) of
EXISTING-CONTENT, or NIL if there is none."
  (when existing-content
    (let ((pos (search *pcli-user-region-marker* existing-content)))
      (when pos (subseq existing-content pos)))))

(defun pcli-default-user-region ()
  (format nil "~A~%;; Add custom project commands below; the generator preserves~%;; anything from this marker onward.~%"
          *pcli-user-region-marker*))

(defun pcli-emit-route-commands (project stack)
  "Emit the generated `routes` region for PROJECT."
  (declare (ignore stack))
  (format nil "~A~%~
(define-project-command ~S routes.list (args)~%~
  \"List routes. Flags: tag <t>, --md, --org.\"~%~
  (pcli-routes-list ~S args))~%~%~
(define-project-command ~S routes.create (args)~%~
  \"Scaffold a new route. Usage: routes.create <name> --path <p>.\"~%~
  (pcli-routes-create ~S args))~%~%~
(define-project-command ~S routes.gen (args)~%~
  \"Generate a route from a natural-language description (LLM).\"~%~
  (pcli-routes-gen ~S args))~%~
~A~%"
          (pcli-region-begin "routes")
          project project
          project project
          project project
          (pcli-region-end "routes")))

(defun pcli-emit-model-commands (project)
  "Emit the generated `models` region for PROJECT."
  (format nil "~A~%~
(define-project-command ~S models.list (args)~%~
  \"List all models from the Prisma schema.\"~%~
  (pcli-models-list ~S args))~%~%~
(define-project-command ~S schema (args)~%~
  \"Print the schema. Usage: schema [<table>].\"~%~
  (pcli-schema ~S args))~%~
~A~%"
          (pcli-region-begin "models")
          project project
          project project
          (pcli-region-end "models")))

(defun pcli-emit-feature-commands (project)
  (format nil "~A~%~
(define-project-command ~S feature (args)~%~
  \"List/summarize code dealing with a concern. Usage: feature <name>.\"~%~
  (pcli-feature ~S args))~%~
~A~%"
          (pcli-region-begin "feature")
          project project
          (pcli-region-end "feature")))

(defun pcli-emit-lifecycle-commands (project)
  (format nil "~A~%~
(define-project-command ~S build (args)~%~
  \"Build the project.\"~%~
  (pcli-build ~S args))~%~%~
(define-project-command ~S release (args)~%~
  \"Release/deploy the project.\"~%~
  (pcli-release ~S args))~%~
~A~%"
          (pcli-region-begin "lifecycle")
          project project
          project project
          (pcli-region-end "lifecycle")))

(defun pcli-emit-stack-wrapped (project stack)
  "Emit thin wrappers re-exporting the active stack mode's commands under the
project scope. Generated each run from the currently-active modes."
  (with-output-to-string (s)
    (format s "~A~%" (pcli-region-begin "stack-wrapped"))
    (format s ";; Wrappers around active stack-mode commands (~{~A~^, ~}).~%" stack)
    (let ((emitted nil))
      (dolist (mode-name *active-modes*)
        (let ((m (and (fboundp 'get-mode) (get-mode mode-name))))
          (when m
            (dolist (entry (mode-command-definers m))
              (let ((cmd (car entry)))
                (setf emitted t)
                (format s "(define-project-command ~S ~A (args)~%  ~S~%  (pcli-run-mode-command ~S ~S args))~%"
                        project cmd
                        (format nil "Wraps the ~A mode command ~A." mode-name cmd)
                        mode-name cmd))))))
      (unless emitted
        (format s ";; (no active stack modes to wrap)~%")))
    (format s "~A~%" (pcli-region-end "stack-wrapped"))))

(defun pcli-render-file (project stack &optional existing-content)
  "Render the full text of CWD/.hactar.lisp for PROJECT with STACK.
Preserves the user region from EXISTING-CONTENT when present."
  (let ((user-region (or (pcli-extract-user-region existing-content)
                         (pcli-default-user-region))))
    (with-output-to-string (s)
      (format s ";;; -*- mode: lisp; package: hactar -*-~%")
      (format s ";;; .hactar.lisp — generated project CLI for ~S~%" project)
      (format s ";;; Edit freely. Regions between BEGIN/END GENERATED markers are~%")
      (format s ";;; owned by the generator + diffusion model and may be rewritten.~%")
      (format s "(in-package :hactar)~%~%")
      (format s "~A~%(register-project-cli ~S~%  :stack '(~{~S~^ ~})~%  :root *repo-root*)~%~A~%~%"
              (pcli-region-begin "meta") project stack (pcli-region-end "meta"))
      (format s "~A~%~%" (pcli-emit-route-commands project stack))
      (format s "~A~%~%" (pcli-emit-model-commands project))
      (format s "~A~%~%" (pcli-emit-feature-commands project))
      (format s "~A~%~%" (pcli-emit-lifecycle-commands project))
      (format s "~A~%~%" (pcli-emit-stack-wrapped project stack))
      (format s "~A~%" user-region))))

(defun generate-project-cli-file (&key (root *repo-root*) (load t))
  "Detect the project type/stack and (re)generate CWD/.hactar.lisp, preserving
the user region. When LOAD, load the resulting file. Returns the pathname."
  (unless root
    (ensure-repo-root)
    (setf root *repo-root*))
  (let* ((project (pcli-project-name root))
         (stack (pcli-detect-stack root))
         (file (project-cli-pathname root))
         (existing (when (probe-file file) (uiop:read-file-string file)))
         (content (pcli-render-file project stack existing)))
    (ensure-directories-exist file)
    (write-file-content (uiop:native-namestring file) content)
    (unless *silent*
      (format t "~&Generated project CLI: ~A (stack: ~{~A~^, ~})~%"
              (uiop:native-namestring file) stack))
    (when load (load-project-cli-file root))
    file))

;;** Backing command helpers (RedwoodSDK-aware, generic fallbacks)

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
                ;; treat --flag value as consuming the next token unless it's --md/--org
                (unless (member a '("--md" "--org") :test #'string=)
                  (setf skip t)))
               ((string= a "tag") (setf skip t)) ; `tag <name>` filter
               (t (push a result))))
    (nreverse result)))

(defun pcli-list-files (&optional (root *repo-root*))
  "List repo-relative file paths under ROOT. Prefers git, then fd, then a
plain recursive directory walk (skipping common noise directories)."
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
  "Extract tags from `// @tag: a, b` or `hactar-tags:` style comments in a file."
  (handler-case
      (let* ((content (uiop:read-file-string full-path))
             (tags '()))
        (cl-ppcre:do-register-groups (grp)
            ("(?i)(?:@tags?|hactar-tags?):\\s*([^\\n]+)" content)
          (dolist (tag (cl-ppcre:split "[,\\s]+" (string-trim " " grp)))
            (when (> (length tag) 0) (push (string-downcase tag) tags))))
        (nreverse tags))
    (error () nil)))

(defun pcli-routes-list (project args)
  "List routes. Supports `tag <t>` filter and `--md` / `--org` output."
  (declare (ignore project))
  (let* ((root *repo-root*)
         (files (pcli-route-files root))
         (tag-filter (pcli-flag-value args "tag"))
         (md (pcli-flag-present-p args "--md"))
         (org (pcli-flag-present-p args "--org")))
    (when tag-filter
      (setf files
            (remove-if-not
             (lambda (f) (member (string-downcase tag-filter)
                                 (pcli-file-tags (merge-pathnames f root))
                                 :test #'string=))
             files)))
    (cond
      (org (pcli-emit-routes-org files root))
      (md  (pcli-emit-routes-md files root))
      (t   (if files
               (dolist (f files) (format t "~A~%" f))
               (format t "No route files found.~%"))))
    files))

(defun pcli-emit-routes-md (files root)
  "Emit a markdown bundle of all route FILES."
  (format t "# Routes~%~%")
  (dolist (f files)
    (let ((full (merge-pathnames f root)))
      (format t "## ~A~%~%```~A~%~A~%```~%~%"
              f (or (pathname-type full) "")
              (handler-case (uiop:read-file-string full) (error () ""))))))

(defun pcli-emit-routes-org (files root)
  "Emit a literate org-mode document of all route FILES."
  (format t "#+TITLE: Routes~%~%")
  (dolist (f files)
    (let ((full (merge-pathnames f root)))
      (format t "* ~A~%#+begin_src ~A~%~A~%#+end_src~%~%"
              f (or (pathname-type full) "text")
              (handler-case (uiop:read-file-string full) (error () ""))))))

(defun pcli-routes-create (project args)
  "Scaffold a new RedwoodSDK route. Usage: routes.create <name> --path <p>."
  (declare (ignore project))
  (let* ((positionals (pcli-positional-args args))
         (name (first positionals))
         (path (or (pcli-flag-value args "--path")
                   (and name (string-downcase name)))))
    (if (null name)
        (format t "Usage: routes.create <name> --path <path>~%")
        (let* ((class-name (pascal-case name))
               (file-name (format nil "src/app/pages/~A/~A.tsx"
                                  (string-downcase (kebab-case name)) class-name))
               (content (format nil "import { RequestInfo } from 'rwsdk/worker';~%~%export function ~A({ request }: RequestInfo) {~%  return (~%    <div>~%      <h1>~A</h1>~%    </div>~%  );~%}~%"
                                class-name class-name)))
          (scaffold-create-file file-name content)
          (format t "Route '~A' created at /~A~%" name path)))))

(defun pcli-routes-gen (project args)
  "Generate a route from a natural-language description using the LLM."
  (declare (ignore project))
  (let ((instructions (str:join " " args)))
    (if (or (null args) (string= (string-trim " " instructions) ""))
        (format t "Usage: routes.gen \"<instructions>\"~%")
        (let ((prompt (format nil "You are generating a RedwoodSDK route for this project (stack: ~{~A~^, ~}).~%Produce a single TSX route component and describe where it should live and how to register it.~%Follow RedwoodSDK conventions (rwsdk/worker, route()/render()).~%~%Instructions: ~A"
                              *stack* instructions)))
          (if *current-model*
              (get-llm-response prompt :stream t :add-to-history nil)
              (format t "No model selected. Use /model to select one before routes.gen.~%"))))))

(defun pcli-schema-file (&optional (root *repo-root*))
  "Locate the Prisma schema file under ROOT, or NIL."
  (let ((candidates (list (merge-pathnames "prisma/schema.prisma" root)
                          (merge-pathnames "db/schema.prisma" root)
                          (merge-pathnames "schema.prisma" root))))
    (find-if #'probe-file candidates)))

(defun pcli-parse-models (schema-content)
  "Return an alist of (model-name . block-text) parsed from a Prisma SCHEMA."
  (let ((models '()))
    (cl-ppcre:do-register-groups (name body)
        ("(?s)model\\s+(\\w+)\\s*\\{(.*?)\\}" schema-content)
      (push (cons name (string-trim '(#\Newline #\Space) body)) models))
    (nreverse models)))

(defun pcli-models-list (project args)
  "List models from the Prisma schema."
  (declare (ignore project args))
  (let ((schema (pcli-schema-file)))
    (if (null schema)
        (format t "No Prisma schema found.~%")
        (let ((models (pcli-parse-models (uiop:read-file-string schema))))
          (if models
              (dolist (m models) (format t "~A~%" (car m)))
              (format t "No models found in ~A~%" schema))))))

(defun pcli-schema (project args)
  "Print the schema, or a single table's block. Usage: schema [<table>]."
  (declare (ignore project))
  (let ((schema (pcli-schema-file))
        (table (first args)))
    (cond
      ((null schema) (format t "No Prisma schema found.~%"))
      ((null table)
       (format t "~A~%" (uiop:read-file-string schema)))
      (t
       (let* ((models (pcli-parse-models (uiop:read-file-string schema)))
              (match (assoc table models :test #'string-equal)))
         (if match
             (format t "model ~A {~%~A~%}~%" (car match) (cdr match))
             (format t "No model named ~A.~%" table)))))))

(defun pcli-feature (project args)
  "List/summarize code dealing with a concern using grep + LLM ranking."
  (declare (ignore project))
  (let ((feature (str:join " " args)))
    (if (or (null args) (string= (string-trim " " feature) ""))
        (format t "Usage: feature <name>~%")
        (let* ((root *repo-root*)
               (matches
                 (handler-case
                     (multiple-value-bind (out err code)
                         (uiop:run-program
                          (list "git" "-C" (namestring root) "grep" "-l" "-i" feature)
                          :output :string :error-output :string :ignore-error-status t)
                       (declare (ignore err))
                       (when (zerop code)
                         (remove-if (lambda (s) (string= s "")) (str:lines out))))
                   (error () nil))))
          (if (null matches)
              (format t "No code found related to '~A'.~%" feature)
              (progn
                (format t "Files related to '~A':~%" feature)
                (dolist (f matches) (format t "  ~A~%" f))
                (when *current-model*
                  (format t "~%Summary:~%")
                  (get-llm-response
                   (format nil "Briefly summarize how the concern '~A' is implemented across these files: ~{~A~^, ~}. Do not propose edits."
                           feature matches)
                   :stream t :add-to-history nil))))))))

(defun pcli-run-script (script-names &key (fallback nil))
  "Run the first matching package.json script from SCRIPT-NAMES, or FALLBACK
command string. Streams output to the terminal."
  (let* ((scripts (pcli-package-scripts))
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
          (uiop:run-program cmd
                            :directory *repo-root*
                            :output t :error-output t
                            :ignore-error-status t)))))

(defun pcli-build (project args)
  "Build the project."
  (declare (ignore project args))
  (pcli-run-script '("build") :fallback "npm run build"))

(defun pcli-release (project args)
  "Release/deploy the project."
  (declare (ignore project args))
  (pcli-run-script '("release" "deploy") :fallback "npm run deploy"))

(defun pcli-run-mode-command (mode-name cmd-name args)
  "Invoke an active stack-mode command CMD-NAME (from MODE-NAME) with ARGS.
Falls back to a message if the command is not currently defined."
  (declare (ignore mode-name))
  (let* ((slash (format nil "/~A" cmd-name))
         (info (or (gethash slash *commands*)
                   (gethash cmd-name *commands*))))
    (if info
        (funcall (first info) args)
        (format t "Mode command ~A is not currently active.~%" cmd-name))))

;;** Sub-commands: scan / init / project.update

(define-sub-command scan (args)
  "Scan the current project and generate CWD/.hactar.lisp (project CLI)."
  (declare (ignore args))
  (generate-project-cli-file :load t))

(define-sub-command init (args)
  "Initialize a project CLI: generate CWD/.hactar.lisp if absent."
  (declare (ignore args))
  (let ((file (project-cli-pathname *repo-root*)))
    (if (and file (probe-file file))
        (progn
          (format t "~A already exists. Use `project.update` to regenerate.~%"
                  (uiop:native-namestring file))
          (load-project-cli-file *repo-root*))
        (generate-project-cli-file :load t))))

(define-sub-command project.update (args)
  "Rescan the project and regenerate the generated regions of CWD/.hactar.lisp,
preserving the user region."
  (declare (ignore args))
  (generate-project-cli-file :load t))

;;** Continuous diffusion-driven updates

(defun pcli-diffusion-relevant-p (pathname event-type)
  "True if a file change should trigger project-CLI regeneration."
  (and (member event-type '(:file-added :file-changed :file-deleted))
       *auto-project-cli*
       *diffusion-model*
       (let ((p (string-downcase (namestring pathname))))
         (and (not (search ".hactar.lisp" p))
              (or (search "route" p)
                  (search "/pages/" p)
                  (search "/app/" p)
                  (search "schema.prisma" p)
                  (search "package.json" p))))))

(defun pcli-enqueue-update (pathname)
  (pushnew (namestring pathname) *project-cli-update-queue* :test #'equal))

(defun pcli-diffusion-regenerate ()
  "Regenerate the generated regions of CWD/.hactar.lisp using the diffusion
model, splicing fresh regions while preserving the user region. Falls back to
the hard-coded generator when the diffusion model is unavailable."
  (let ((file (project-cli-pathname *repo-root*)))
    (when file
      (handler-case
          (let* ((project (pcli-project-name *repo-root*))
                 (stack (pcli-detect-stack *repo-root*))
                 (existing (when (probe-file file) (uiop:read-file-string file)))
                 (content (pcli-render-file project stack existing)))
            ;; The diffusion model, when wired to a backend, would rewrite the
            ;; generated regions here using delta context. We splice the
            ;; freshly generated regions and preserve the user region.
            (write-file-content (uiop:native-namestring file) content)
            (load-project-cli-file *repo-root*)
            (debug-log "Project CLI regenerated via diffusion update."))
        (error (e)
          (debug-log "Project CLI diffusion regenerate failed:" e)))
      (setf *project-cli-update-queue* '()))))

(def-analyzer project-cli-diffusion ((*file-event-hook*)) nil (pathname event-type)
  "On relevant source changes, enqueue and (debounced) regenerate the project
CLI file via the diffusion model."
  (when (pcli-diffusion-relevant-p pathname event-type)
    (debug-log "Project CLI diffusion analyzer triggered by:" pathname)
    (pcli-enqueue-update pathname)
    (pcli-diffusion-regenerate)))
