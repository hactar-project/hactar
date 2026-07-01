(in-package #:hactar-rebuild)

(defvar *state-registry* (make-hash-table :test 'equal)
  "Registry of rebuild state variables declared with DEFSTATE.")

(defmacro defstate (name &optional value doc)
  "Declare mutable rebuild state and register it for introspection.
This is the ONLY sanctioned way to introduce global mutable engine state."
  `(progn
     (defvar ,name ,value ,doc)
     (setf (gethash (string-downcase (symbol-name ',name)) *state-registry*)
           (list :name ',name :doc ,doc :initial-value ,value))
     ',name))

;; --- process / repository ---
(defstate *repo-root* nil "Absolute path to the repository root.")
(defstate *stack* '() "Detected stack as a list of strings.")
(defstate *project-map* nil "Compact project map produced by extraction.")

;; --- project data forms ---
(defstate *project* nil "Current project descriptor plist.")
(defstate *models* nil "Current project model descriptors.")
(defstate *routes-data* nil "Current project route descriptors.")

;; --- registries (one hash-table each, key = downcased name) ---
(defstate *hooks* (make-hash-table :test 'equal) "Hook registry.")
(defstate *modes* (make-hash-table :test 'equal) "Mode registry.")
(defstate *agents* (make-hash-table :test 'equal) "Agent registry.")
(defstate *interfaces* (make-hash-table :test 'equal) "Interface registry.")
(defstate *commands* (make-hash-table :test 'equal) "Command registry.")
(defstate *rules* (make-hash-table :test 'equal) "Rule registry.")
(defstate *skills* (make-hash-table :test 'equal) "Skill registry.")
(defstate *prompts* (make-hash-table :test 'equal) "Prompt registry.")
(defstate *actions* (make-hash-table :test 'equal) "Action registry.")
(defstate *tools* (make-hash-table :test 'equal) "Tool registry.")
(defstate *route-parsers* (make-hash-table :test 'equal) "Route parser registry.")
(defstate *hypertexts* (make-hash-table :test 'equal) "Hypertext registry.")
(defstate *docs* (make-hash-table :test 'equal) "Doc registry.")
(defstate *sandboxes* (make-hash-table :test 'equal) "Sandbox registry.")

;; --- context / interfaces ---
(defstate *context-files* '()
  "Ordered list of (path . annotation) entries selected for agent context.")

;; --- stack modes (engine-owned, not reset by reload) ---
(defstate *stack-modes* (make-hash-table :test 'equal)
  "Registered stack modes: name -> (:detect fn :forms fn).")

;; --- self-update / build / validation ---
(defstate *auto-update* t "Whether self-update runs after each turn.")
(defstate *update-ledger* '() "Recent activity feeding self-update.")
(defstate *update-status* nil "Last self-update outcome plist.")
(defstate *build-result* nil "Last build result.")
(defstate *validation-result* nil "Last validation result.")

;; --- engine reinstall thunks (run after every reset-registries) ---
(defstate *post-reset-thunks* '()
  "Ordered engine reinstall functions run after reset-registries so that
engine-owned hooks/handlers/interfaces survive .hactar.lisp reloads.")

(defun register-post-reset (fn)
  "Register FN (a symbol/function) to run after each reset, in load order."
  (setf *post-reset-thunks* (append (remove fn *post-reset-thunks*) (list fn)))
  fn)

(defun reset-registries ()
  "Clear everything a reload of .hactar.lisp re-populates, then reinstall
engine-owned defaults. Engine registries (stack modes) are preserved."
  (setf *project* nil *models* nil *routes-data* nil)
  (dolist (registry (list *hooks* *modes* *agents* *interfaces* *commands*
                          *rules* *skills* *prompts* *actions* *tools*
                          *route-parsers* *hypertexts* *docs* *sandboxes*))
    (clrhash registry))
  (dolist (fn *post-reset-thunks*)
    (ignore-errors (funcall fn)))
  t)

(defun registry-count (registry-name)
  "Return the number of entries in a named registry."
  (hash-table-count
   (ecase registry-name
     (:hooks *hooks*) (:modes *modes*) (:agents *agents*)
     (:interfaces *interfaces*) (:commands *commands*) (:rules *rules*)
     (:skills *skills*) (:prompts *prompts*) (:actions *actions*)
     (:tools *tools*) (:routes *route-parsers*) (:hypertexts *hypertexts*)
     (:docs *docs*) (:sandboxes *sandboxes*) (:stack-modes *stack-modes*))))

(defun state-summary ()
  "Return a plist describing all declared state (for the /state surface)."
  (loop for k being the hash-keys of *state-registry* using (hash-value v)
        collect (list :name k :doc (getf v :doc))))
