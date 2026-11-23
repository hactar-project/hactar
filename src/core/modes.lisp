(in-package #:hactar-rebuild)

;;; ---------------------------------------------------------------------------
;;; Stack mode registry: each mode has a detector and a harness-forms builder.
;;; ---------------------------------------------------------------------------

(defun register-stack-mode (&key name detect forms stack-commands stack (priority 50))
  "Register a stack mode. DETECT is (root)->boolean, FORMS is (root)->list of
generated DSL forms, STACK-COMMANDS is a plist of :test/:lint/... command
templates, STACK is the normalized stack descriptor list, and PRIORITY orders
detection (lower is checked first; the generic fallback should be highest)."
  (setf (gethash (key-string name) *stack-modes*)
        (list :name (string-downcase (string name))
              :detect detect :forms forms :stack-commands stack-commands
              :stack stack :priority priority)))

(defun sorted-stack-modes ()
  "Return registered modes sorted by ascending :priority then name."
  (let ((modes '()))
    (maphash (lambda (k v) (declare (ignore k)) (push v modes)) *stack-modes*)
    (stable-sort (sort modes #'string< :key (lambda (m) (getf m :name)))
                 #'< :key (lambda (m) (or (getf m :priority) 50)))))

(defun file-exists-p (root name)
  (probe-file (merge-pathnames name (uiop:ensure-directory-pathname root))))

(defun file-contains-p (root name substring)
  (let ((f (file-exists-p root name)))
    (and f (search substring (uiop:read-file-string f)))))

(defun file-glob-p (root pattern)
  (let ((rootd (uiop:ensure-directory-pathname root)))
    (some (lambda (p) (probe-file p))
          (directory (merge-pathnames pattern rootd)))))

(defun eval-detect-spec (spec root)
  "Evaluate a declarative :DETECT data spec against repository ROOT."
  (cond
    ((null spec) nil)
    ((eq spec t) t)
    ((functionp spec) (funcall spec root))
    ((and (consp spec) (keywordp (first spec)))
     (ecase (first spec)
       (:file-exists (not (null (file-exists-p root (second spec)))))
       (:file-glob (not (null (file-glob-p root (second spec)))))
       (:file-contains (let ((pair (second spec)))
                         (not (null (file-contains-p root (first pair) (second pair))))))
       (:any (some (lambda (sub) (eval-detect-spec sub root)) (rest spec)))
       (:all (every (lambda (sub) (eval-detect-spec sub root)) (rest spec)))))
    (t nil)))

(defun compile-detect-spec (spec)
  "Compile a declarative :DETECT data spec into a (root) -> boolean predicate."
  (if (functionp spec)
      spec
      (lambda (root) (eval-detect-spec spec root))))

(defun detect-stack (&optional (root (or *repo-root* (uiop:getcwd))))
  "Return (values stack-list mode-plist). Modes are tried in priority order;
the first match wins. The generic fallback matches everything and is tried last."
  (let ((generic nil))
    (dolist (mode (sorted-stack-modes))
      (cond
        ((string= (getf mode :name) "generic") (setf generic mode))
        ((funcall (getf mode :detect) root)
         (return-from detect-stack (values (mode-stack-list mode root) mode)))))
    (values (if generic (mode-stack-list generic root) '("unknown"))
            generic)))

(defun mode-stack-list (mode root)
  (declare (ignore root))
  (or (getf mode :stack) (list (getf mode :name))))

(defun install-stack-commands (mode)
  "Populate *stack-commands* from MODE's :stack-commands plist."
  (clrhash *stack-commands*)
  (loop for (k v) on (getf mode :stack-commands) by #'cddr
        do (setf (gethash k *stack-commands*) v)))

;;; ---------------------------------------------------------------------------
;;; Extraction: a compact project map
;;; ---------------------------------------------------------------------------

(defun read-file-string* (root name)
  (let ((f (file-exists-p root name)))
    (and f (uiop:read-file-string f))))

(defun extract-npm-scripts (root)
  "Naively extract npm script names from package.json's \"scripts\" object."
  (let ((json (read-file-string* root "package.json")))
    (when json
      (let ((start (search "\"scripts\"" json)))
        (when start
          (let* ((brace (position #\{ json :start start))
                 (close (and brace (position #\} json :start brace))))
            (when (and brace close)
              (let ((body (subseq json (1+ brace) close))
                    (names '()))
                (cl-ppcre:do-register-groups (name)
                    ("\"([A-Za-z0-9:_-]+)\"\\s*:" body)
                  (push name names))
                (nreverse names)))))))))

(defparameter *dependency-manifests*
  '("package.json" "Cargo.toml" "go.mod" "pyproject.toml" "requirements.txt"
    "Gemfile" "*.asd"))

(defun present-manifests (root)
  (remove nil (mapcar (lambda (n)
                        (cond ((string= n "*.asd")
                               (and (directory (merge-pathnames
                                                "*.asd"
                                                (uiop:ensure-directory-pathname root)))
                                    "*.asd"))
                              ((file-exists-p root n) n)))
                      *dependency-manifests*)))

(defun present-docs (root)
  (remove nil (mapcar (lambda (n) (and (file-exists-p root n) n))
                      '("README.md" "README.org" "README.rst" "docs/"
                        "CONTRIBUTING.md"))))

(defun extract-project-map (&optional (root (or *repo-root* (uiop:getcwd)))
                              (stack *stack*))
  "Build a compact project map plist: stack, manifests, scripts, docs, and the
resolved test command for the detected stack."
  (list :root (namestring (truename root))
        :stack stack
        :dependency-manifests (present-manifests root)
        :scripts (extract-npm-scripts root)
        :docs (present-docs root)
        :test-command (gethash :test *stack-commands*)))

;;; ---------------------------------------------------------------------------
;;; generate-harness: deterministic mode forms + defproject + project map
;;; ---------------------------------------------------------------------------

(defun stack-harness-forms (&optional (root (or *repo-root* (uiop:getcwd))))
  (multiple-value-bind (stack mode) (detect-stack root)
    (declare (ignore stack))
    (when mode
      (install-stack-commands mode)
      (funcall (getf mode :forms) root))))

(defun generate-harness (&optional (root (or *repo-root* (uiop:getcwd))))
  "Compose the generated forms for ROOT: defproject + harness forms."
  (multiple-value-bind (stack mode) (detect-stack root)
    (setf *stack* stack)
    (when mode (install-stack-commands mode))
    (append
     (list `(defproject :name ,(project-name root) :stack ,stack))
     (and mode (funcall (getf mode :forms) root)))))

(defun project-name (root)
  (let ((p (uiop:ensure-directory-pathname (truename root))))
    (car (last (pathname-directory p)))))

;;; ---------------------------------------------------------------------------
;;; Built-in fallback mode (stack modes live in Tier 4 content packs)
;;; ---------------------------------------------------------------------------

(defun generic-forms (root)
  (declare (ignore root))
  '((deftool test (&optional file)
      "Best-effort test entry point."
      (declare (ignore file))
      (run-test-cmd))
    (defsandbox project :allow ("git" "make"))
    (defagent dev
      :model "anthropic/claude-sonnet-4-6"
      :tools (test) :sandbox project
      :instructions "Inspect the repository first. Prefer compound tools; avoid destructive writes. Ask the user to refine .hactar.lisp.")))

(register-stack-mode
 :name "generic"
 :detect (constantly t)
 :forms #'generic-forms
 :stack '("generic")
 :priority 1000
 :stack-commands '(:test "make test"))
