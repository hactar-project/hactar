(in-package #:hactar-rebuild)

(defvar *state-registry* (make-hash-table :test 'equal)
  "Registry of rebuild state variables declared with DEFSTATE.")

(defmacro defstate (name &optional value doc)
  "Declare mutable rebuild state and register it for introspection."
  `(progn
     (defvar ,name ,value ,doc)
     (setf (gethash (string-downcase (symbol-name ',name)) *state-registry*)
           (list :name ',name :doc ,doc :initial-value ,value))
     ',name))

(defstate *project* nil "Current project descriptor.")
(defstate *models* nil "Current project model descriptors.")
(defstate *routes-data* nil "Current project route descriptors.")

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

(defun reset-registries ()
  "Clear all registries populated by a .hactar.lisp reload."
  (setf *project* nil
        *models* nil
        *routes-data* nil)
  (dolist (registry (list *hooks* *modes* *agents* *interfaces* *commands*
                          *rules* *skills* *prompts* *actions* *tools*
                          *route-parsers* *hypertexts* *docs* *sandboxes*))
    (clrhash registry))
  t)

(defun registry-count (registry-name)
  "Return the number of entries in a named registry."
  (let ((registry (ecase registry-name
                    (:hooks *hooks*)
                    (:modes *modes*)
                    (:agents *agents*)
                    (:interfaces *interfaces*)
                    (:commands *commands*)
                    (:rules *rules*)
                    (:skills *skills*)
                    (:prompts *prompts*)
                    (:actions *actions*)
                    (:tools *tools*)
                    (:routes *route-parsers*)
                    (:hypertexts *hypertexts*)
                    (:docs *docs*)
                    (:sandboxes *sandboxes*))))
    (hash-table-count registry)))
