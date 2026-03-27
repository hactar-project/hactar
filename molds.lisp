;; molds — Mold system for Hactar
;; A mold defines the architectural shape of a project: entities, interfaces, rules, and metadata.
(in-package :hactar)

;;* Mold Registry & State

(defvar *molds* (make-hash-table :test 'equal)
  "Registry of all loaded mold definitions, keyed by name string.")

(defvar *active-mold* nil
  "The currently active mold definition, or NIL.")

(defvar *molds-path* (uiop:subpathname *hactar-data-path* "molds/")
  "Directory for installed mold files.")

;;* Mold Structs

(defstruct mold-definition
  "A mold defines the architectural shape of a project."
  (name nil :type (or null string))
  (description nil :type (or null string))
  (llms '(:all) :type list)
  (entities '() :type list)
  (interfaces '() :type list)
  (rules '() :type list)
  (source nil :type (or null string))
  (version nil :type (or null string)))

(defstruct mold-entity
  "An entity definition within a mold."
  (name nil :type (or null symbol string))
  (description nil :type (or null string))
  (default-tags '() :type list)
  (required-behaviors '() :type list)
  (example nil :type (or null string))
  (rules '() :type list)
  (schema '() :type list))

(defstruct mold-interface
  "An interface definition within a mold."
  (name nil :type (or null symbol string keyword))
  (entity nil :type (or null symbol string))
  (description nil :type (or null string))
  (rules '() :type list))

(defstruct mold-rule
  "A rule within a mold. Can be generic, entity-scoped, or interface-scoped."
  (text nil :type (or null string))
  (scope nil :type (or null keyword))
  (target nil :type (or null symbol string keyword))
  (source nil :type (or null string)))

;;* Mold Rule Parsing

(defun %parse-mold-rule (rule-spec)
  "Parse a rule specification into a mold-rule struct.
   RULE-SPEC can be:
   - A string (generic rule)
   - A list like (:entity route \"rule text\")
   - A list like (:interface :landing-page \"rule text\")"
  (cond
    ((stringp rule-spec)
     (make-mold-rule :text rule-spec :scope :generic))
    ((and (listp rule-spec) (>= (length rule-spec) 3))
     (let ((scope (first rule-spec))
           (target (second rule-spec))
           (text (third rule-spec)))
       (make-mold-rule :text text
                       :scope (if (keywordp scope) scope
                                  (intern (string-upcase (string scope)) :keyword))
                       :target target)))
    (t (warn "Invalid mold rule spec: ~S" rule-spec) nil)))

(defun %parse-mold-entity (entity-spec)
  "Parse an entity specification from a defmold form.
   ENTITY-SPEC is like (route :default-tags (:api) :desc \"A route\" :rules '(...))"
  (let* ((name (first entity-spec))
         (plist (rest entity-spec))
         (default-tags (getf plist :default-tags))
         (required-behaviors (getf plist :required-behaviors))
         (desc (getf plist :desc))
         (example (getf plist :example))
         (rules (getf plist :rules))
         (schema (getf plist :schema)))
    (make-mold-entity :name name
                      :description desc
                      :default-tags (if (listp default-tags) default-tags nil)
                      :required-behaviors (if (listp required-behaviors) required-behaviors nil)
                      :example example
                      :rules (if (listp rules) rules nil)
                      :schema (if (listp schema) schema nil))))

(defun %parse-mold-interface (iface-spec)
  "Parse an interface specification from a defmold form.
   IFACE-SPEC can be:
   - (entity-name :iface-name :desc \"...\" :rules '(...))
   - (entity-name '(\"rule1\" \"rule2\"))"
  (cond
    ;; Plist form: (route :landing-page :desc "..." :rules '(...))
    ((and (listp iface-spec)
          (>= (length iface-spec) 2)
          (keywordp (second iface-spec)))
     (let* ((entity (first iface-spec))
            (name (second iface-spec))
            (rest-plist (cddr iface-spec))
            (desc (getf rest-plist :desc))
            (rules (getf rest-plist :rules)))
       (make-mold-interface :name name
                            :entity entity
                            :description desc
                            :rules (if (listp rules) rules nil))))
    ;; Simple list form: (routes '("Login route" "Logout route"))
    ((and (listp iface-spec)
          (= (length iface-spec) 2)
          (listp (second iface-spec)))
     (let ((entity (first iface-spec))
           (rules (second iface-spec)))
       (make-mold-interface :name entity
                            :entity entity
                            :description nil
                            :rules (if (listp rules) rules nil))))
    (t
     (warn "Invalid mold interface spec: ~S" iface-spec)
     nil)))

;;* defmold Macro

(defmacro defmold (name description &body body)
  "Define a mold — the architectural shape of a project.

   NAME: Symbol name for the mold.
   DESCRIPTION: String describing the mold.

   Keyword arguments in BODY:
   :llms         — List of supported LLMs (default '(:all))
   :entities     — List of entity specifications
   :interfaces   — List of interface specifications
   :rules        — List of rule specifications (generic, entity-scoped, or interface-scoped)

   Example:
   (defmold saas-api-rwsdk
     \"Standard SaaS API mold\"
     :llms '(:all)
     :entities
     ((route
       :default-tags (:api :router)
       :required-behaviors (:error-handling :middleware)
       :desc \"A route\"
       :example \"src/worker.tsx\"
       :rules '(\"Use defineApp and route\"
                \"Return Response objects\"))
      (migration
       :default-tags (:sqlite :kysely)
       :required-behaviors (:idempotent-down :schema)
       :desc \"A database migration\"
       :rules '(\"implement up and down functions\")))
     :interfaces
     ((route
       :landing-page
       :desc \"A landing page.\"
       :rules '(\"Use tailwind for styles\"))
      (routes '(\"Login route\" \"Logout Route\")))
     :rules '(\"Use modern coding standards\"
              (:entity route \"Always implement a JSON response type\")
              (:interface :landing-page \"Make sure to use links.\")))"
  (let* ((llms (getf body :llms '(:all)))
         (entities-spec (getf body :entities))
         (interfaces-spec (getf body :interfaces))
         (rules-spec (getf body :rules))
         (name-str (string-downcase (if (stringp name) name (symbol-name name)))))
    `(progn
       (let* ((parsed-entities (mapcar #'%parse-mold-entity ',entities-spec))
              (parsed-interfaces (remove nil (mapcar #'%parse-mold-interface ',interfaces-spec)))
              (parsed-rules (remove nil (mapcar #'%parse-mold-rule ',rules-spec)))
              (mold (make-mold-definition
                     :name ,name-str
                     :description ,description
                     :llms ',llms
                     :entities parsed-entities
                     :interfaces parsed-interfaces
                     :rules parsed-rules
                     :source "lisp")))
         ;; Register in mold registry
         (setf (gethash ,name-str *molds*) mold)
         (format t "~&Mold '~A' defined with ~A entities, ~A interfaces, ~A rules.~%"
                 ,name-str
                 (length parsed-entities)
                 (length parsed-interfaces)
                 (length parsed-rules))
         ',name))))

;;* Mold Activation

(defun mold-activate-rules (mold)
  "Activate all rules from a mold into the *active-rules* system."
  ;; Entity-level rules
  (dolist (entity (mold-definition-entities mold))
    (let ((entity-name (mold-entity-name entity)))
      (dolist (rule (mold-entity-rules entity))
        (let ((rule-key (format nil "mold/~A/entity/~A/~A"
                                (mold-definition-name mold)
                                entity-name
                                (sxhash rule))))
          (setf (gethash rule-key *active-rules*) rule)))))
  ;; Interface-level rules
  (dolist (iface (mold-definition-interfaces mold))
    (let ((iface-name (mold-interface-name iface)))
      (dolist (rule (mold-interface-rules iface))
        (let ((rule-key (format nil "mold/~A/interface/~A/~A"
                                (mold-definition-name mold)
                                iface-name
                                (sxhash rule))))
          (setf (gethash rule-key *active-rules*) rule)))))
  ;; Generic and scoped rules from the :rules list
  (dolist (rule (mold-definition-rules mold))
    (let ((rule-key (format nil "mold/~A/rule/~A"
                            (mold-definition-name mold)
                            (sxhash (mold-rule-text rule)))))
      (setf (gethash rule-key *active-rules*) (mold-rule-text rule)))))

(defun mold-deactivate-rules (mold)
  "Remove all rules from a mold from the *active-rules* system."
  (let ((prefix (format nil "mold/~A/" (mold-definition-name mold))))
    (let ((keys-to-remove '()))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (when (str:starts-with-p prefix k)
                   (push k keys-to-remove)))
               *active-rules*)
      (dolist (k keys-to-remove)
        (remhash k *active-rules*)))))

(defun mold-activate-entities (mold)
  "Register mold entities as entity type definitions if not already registered."
  (dolist (me (mold-definition-entities mold))
    (let* ((name (mold-entity-name me))
           (name-str (string-downcase (if (stringp name) name (symbol-name name)))))
      (unless (gethash name-str *entity-registry*)
        (setf (gethash name-str *entity-registry*)
              (make-entity-definition
               :name (if (symbolp name) name (intern (string-upcase name-str) :hactar))
               :description (or (mold-entity-description me) "")
               :schema (mold-entity-schema me)
               :default-behaviors (mold-entity-required-behaviors me)
               :commands-p nil
               :storage :memory
               :searchable '()))))))

(defun mold-use (mold-name)
  "Activate a mold by name. Deactivates any previously active mold."
  (let ((mold (gethash (string-downcase mold-name) *molds*)))
    (unless mold
      (format t "~&Mold '~A' not found. Use /mold.list to see available molds.~%" mold-name)
      (return-from mold-use nil))
    ;; Deactivate current mold
    (when *active-mold*
      (mold-deactivate-rules *active-mold*)
      (format t "~&Deactivated mold '~A'.~%" (mold-definition-name *active-mold*)))
    ;; Activate new mold
    (setf *active-mold* mold)
    (mold-activate-rules mold)
    (mold-activate-entities mold)
    (format t "~&Activated mold '~A' (~A entities, ~A interfaces, ~A rules).~%"
            (mold-definition-name mold)
            (length (mold-definition-entities mold))
            (length (mold-definition-interfaces mold))
            (length (mold-definition-rules mold)))
    mold))

;;* Mold Display

(defun mold-show (&optional mold)
  "Display a mold's details."
  (let ((m (or mold *active-mold*)))
    (unless m
      (format t "~&No active mold. Use /mold.use <name> to activate one.~%")
      (return-from mold-show nil))
    (format t "~&Mold: ~A~%" (mold-definition-name m))
    (format t "  Description: ~A~%" (or (mold-definition-description m) "(none)"))
    (format t "  LLMs: ~{~A~^, ~}~%" (mold-definition-llms m))
    (format t "  Source: ~A~%" (or (mold-definition-source m) "unknown"))
    ;; Entities
    (format t "~%  Entities (~A):~%" (length (mold-definition-entities m)))
    (dolist (e (mold-definition-entities m))
      (format t "    ~A~@[ — ~A~]~%" (mold-entity-name e) (mold-entity-description e))
      (when (mold-entity-default-tags e)
        (format t "      Tags: ~{~A~^, ~}~%" (mold-entity-default-tags e)))
      (when (mold-entity-required-behaviors e)
        (format t "      Behaviors: ~{~A~^, ~}~%" (mold-entity-required-behaviors e)))
      (when (mold-entity-example e)
        (format t "      Example: ~A~%" (mold-entity-example e)))
      (when (mold-entity-rules e)
        (format t "      Rules:~%")
        (dolist (r (mold-entity-rules e))
          (format t "        - ~A~%" r))))
    ;; Interfaces
    (format t "~%  Interfaces (~A):~%" (length (mold-definition-interfaces m)))
    (dolist (iface (mold-definition-interfaces m))
      (format t "    ~A~@[ (entity: ~A)~]~@[ — ~A~]~%"
              (mold-interface-name iface)
              (mold-interface-entity iface)
              (mold-interface-description iface))
      (when (mold-interface-rules iface)
        (format t "      Rules:~%")
        (dolist (r (mold-interface-rules iface))
          (format t "        - ~A~%" r))))
    ;; Rules
    (format t "~%  Rules (~A):~%" (length (mold-definition-rules m)))
    (dolist (r (mold-definition-rules m))
      (case (mold-rule-scope r)
        (:generic (format t "    [generic] ~A~%" (mold-rule-text r)))
        (:entity (format t "    [entity:~A] ~A~%" (mold-rule-target r) (mold-rule-text r)))
        (:interface (format t "    [interface:~A] ~A~%" (mold-rule-target r) (mold-rule-text r)))
        (t (format t "    ~A~%" (mold-rule-text r)))))))

;;* Mold Listing

(defun mold-list ()
  "List all available molds."
  (if (zerop (hash-table-count *molds*))
      (format t "~&No molds loaded. Use /mold.install to install molds.~%")
      (progn
        (format t "~&Available molds:~%")
        (maphash (lambda (name mold)
                   (format t "  ~A~@[ — ~A~]~@[ [ACTIVE]~]~%"
                           name
                           (mold-definition-description mold)
                           (and *active-mold*
                                (string= name (mold-definition-name *active-mold*)))))
                 *molds*))))

;;* Mold Installation

(defun %mold-install-from-path (path)
  "Install a mold from a local file path."
  (let* ((resolved (expand-path path))
         (full-path (if (uiop:absolute-pathname-p resolved)
                        resolved
                        (merge-pathnames resolved *repo-root*))))
    (cond
      ((not (probe-file full-path))
       (format t "~&Mold file not found: ~A~%" full-path)
       nil)
      ((str:ends-with-p ".lisp" (namestring full-path))
       (handler-case
           (progn (load full-path)
                  (format t "~&Loaded mold from ~A~%" full-path)
                  t)
         (error (e)
           (format t "~&Error loading mold from ~A: ~A~%" full-path e)
           nil)))
      ((or (str:ends-with-p ".org" (namestring full-path))
           (str:ends-with-p ".md" (namestring full-path)))
       (handler-case
           (let ((mold (mold-parse-org-file full-path)))
             (when mold
               (setf (gethash (mold-definition-name mold) *molds*) mold)
               (format t "~&Installed mold '~A' from ~A~%" (mold-definition-name mold) full-path)
               ;; Copy to molds directory
               (ensure-directories-exist *molds-path*)
               (let ((dest (merge-pathnames (format nil "~A.org" (mold-definition-name mold)) *molds-path*)))
                 (uiop:copy-file full-path dest))
               t))
         (error (e)
           (format t "~&Error parsing org mold from ~A: ~A~%" full-path e)
           nil)))
      (t
       (format t "~&Unsupported mold file format: ~A~%" full-path)
       nil))))

(defun %mold-install-from-github (spec)
  "Install a mold from a GitHub repository. SPEC is like 'org/repo' or 'github:org/repo'."
  (let* ((clean-spec (remove-prefix "github:" spec))
         (parts (str:split "/" clean-spec))
         (user (first parts))
         (repo (second parts)))
    (unless (and user repo)
      (format t "~&Invalid GitHub mold spec: ~A. Expected format: github:org/repo~%" spec)
      (return-from %mold-install-from-github nil))
    (let ((repo-dir (fetch-github-repo user repo)))
      (if repo-dir
          (let* ((lisp-files (directory (merge-pathnames "*.lisp" repo-dir)))
                 (org-files (directory (merge-pathnames "*.org" repo-dir)))
                 (mold-files (append lisp-files org-files))
                 (installed 0))
            (if mold-files
                (progn
                  (dolist (f mold-files)
                    (when (%mold-install-from-path (namestring f))
                      (incf installed)))
                  (format t "~&Installed ~A mold(s) from ~A/~A.~%" installed user repo)
                  (> installed 0))
                (progn
                  (format t "~&No mold files (.lisp or .org) found in ~A/~A.~%" user repo)
                  nil)))
          (progn
            (format t "~&Failed to fetch GitHub repo: ~A/~A~%" user repo)
            nil)))))

(defun %mold-install-from-registry (name)
  "Install a mold from the hactar registry (data path)."
  (let* ((name-str (string-downcase name))
         (lisp-path (merge-pathnames (format nil "~A.lisp" name-str) *molds-path*))
         (org-path (merge-pathnames (format nil "~A.org" name-str) *molds-path*)))
    (cond
      ((probe-file lisp-path) (%mold-install-from-path (namestring lisp-path)))
      ((probe-file org-path) (%mold-install-from-path (namestring org-path)))
      (t
       ;; Try fetching from hactar project data
       (let ((hactar-lisp (merge-pathnames (format nil "molds/~A.lisp" name-str) *hactar-data-path*))
             (hactar-org (merge-pathnames (format nil "molds/~A.org" name-str) *hactar-data-path*)))
         (cond
           ((probe-file hactar-lisp) (%mold-install-from-path (namestring hactar-lisp)))
           ((probe-file hactar-org) (%mold-install-from-path (namestring hactar-org)))
           (t
            (format t "~&Mold '~A' not found in registry or molds directory.~%" name-str)
            nil)))))))

(defun mold-install (source)
  "Install a mold from various sources.
   SOURCE can be:
   - A local path (./my-mold or /path/to/mold.lisp)
   - A GitHub spec (github:org/repo)
   - A registry name (saas-api)"
  (cond
    ((str:starts-with-p "github:" source)
     (%mold-install-from-github source))
    ((or (str:starts-with-p "./" source)
         (str:starts-with-p "/" source)
         (str:starts-with-p "~" source)
         (search "." source :test #'char=))
     ;; Looks like a path if it starts with ./ / ~ or has a file extension
     (if (or (str:ends-with-p ".lisp" source)
             (str:ends-with-p ".org" source)
             (str:ends-with-p ".md" source)
             (uiop:directory-pathname-p source)
             (str:starts-with-p "./" source)
             (str:starts-with-p "/" source)
             (str:starts-with-p "~" source))
         (%mold-install-from-path source)
         (%mold-install-from-registry source)))
    (t
     (%mold-install-from-registry source))))

;;* Mold Init / Scaffold

(defun mold-init (&optional mold-name)
  "Scaffold a mold for the current project. If MOLD-NAME is given, base it on that mold."
  (let* ((name (or mold-name
                   (and *name* (string-downcase *name*))
                   "my-project"))
         (mold-file (merge-pathnames (format nil "~A.mold.lisp" name) *repo-root*)))
    (if (probe-file mold-file)
        (format t "~&Mold file already exists: ~A~%" mold-file)
        (progn
          (write-file-content
           (namestring mold-file)
           (format nil "(in-package :hactar)

(defmold ~A
  \"Mold for ~A\"
  :llms '(:all)
  :entities
  (;; Define your entities here
   ;; (route
   ;;   :default-tags (:api)
   ;;   :desc \"An API route\"
   ;;   :rules '(\"Return JSON responses\"))
   )
  :interfaces
  (;; Define your interfaces here
   ;; (route :landing-page
   ;;   :desc \"Landing page\"
   ;;   :rules '(\"Use responsive design\"))
   )
  :rules
  '(;; Generic rules
    ;; \"Use modern coding standards\"
    ;; Entity-scoped: (:entity route \"Always validate input\")
    ;; Interface-scoped: (:interface :landing-page \"Include CTA\")
    ))
"
                  name (or *name* name)))
          (format t "~&Created mold scaffold: ~A~%" mold-file)
          (format t "Edit the file and then use /mold.use ~A to activate it.~%" name)))))

;;* Mold Validation

(defun mold-validate (&optional mold)
  "Validate that the current project conforms to the active mold.
   Returns (values ok-p issues-list)."
  (let* ((m (or mold *active-mold*))
         (issues '()))
    (unless m
      (format t "~&No active mold to validate against.~%")
      (return-from mold-validate (values nil '("No active mold"))))
    (format t "~&Validating against mold '~A'...~%" (mold-definition-name m))
    ;; Check that each entity type has at least one instance or implementation
    (dolist (me (mold-definition-entities m))
      (let* ((name (mold-entity-name me))
             (name-str (string-downcase (if (stringp name) name (symbol-name name))))
             (impl (resolve-entity-implementation name-str))
             (instances (find-entity-instances name-str)))
        (unless (or impl instances)
          (push (format nil "Entity '~A' has no implementation or instances." name-str) issues))))
    ;; Check that required behaviors exist
    (dolist (me (mold-definition-entities m))
      (dolist (beh (mold-entity-required-behaviors me))
        (let ((beh-name (string-downcase (if (stringp beh) beh (symbol-name beh)))))
          (unless (get-behavior beh-name)
            (push (format nil "Required behavior '~A' for entity '~A' is not defined."
                          beh-name (mold-entity-name me))
                  issues)))))
    ;; Report
    (if issues
        (progn
          (format t "~&Validation found ~A issue(s):~%" (length issues))
          (dolist (issue (nreverse issues))
            (format t "  ⚠ ~A~%" issue))
          (values nil (nreverse issues)))
        (progn
          (format t "~&Mold validation passed. ✓~%")
          (values t nil)))))

;;* Mold Pour (AI Implementation)

(defun %mold-pour-entity-prompt (mold entity)
  "Generate a prompt for the LLM to implement a mold entity."
  (let* ((entity-name (mold-entity-name entity))
         (entity-rules (mold-entity-rules entity))
         (generic-rules (remove-if-not (lambda (r) (eq (mold-rule-scope r) :generic))
                                       (mold-definition-rules mold)))
         (scoped-rules (remove-if-not (lambda (r)
                                        (and (eq (mold-rule-scope r) :entity)
                                             (let ((target (mold-rule-target r)))
                                               (string-equal
                                                (if (stringp target) target (symbol-name target))
                                                (if (stringp entity-name) entity-name (symbol-name entity-name))))))
                                      (mold-definition-rules mold)))
         (iface-rules (remove-if-not (lambda (iface)
                                       (let ((ent (mold-interface-entity iface)))
                                         (and ent (string-equal
                                                   (if (stringp ent) ent (symbol-name ent))
                                                   (if (stringp entity-name) entity-name (symbol-name entity-name))))))
                                     (mold-definition-interfaces mold))))
    (with-output-to-string (s)
      (format s "Implement the following entity for the project.~%~%")
      (format s "Entity: ~A~%" entity-name)
      (when (mold-entity-description entity)
        (format s "Description: ~A~%" (mold-entity-description entity)))
      (when (mold-entity-default-tags entity)
        (format s "Tags: ~{~A~^, ~}~%" (mold-entity-default-tags entity)))
      (when (mold-entity-example entity)
        (format s "Example file: ~A~%" (mold-entity-example entity))
        (let ((example-path (merge-pathnames (mold-entity-example entity) *repo-root*)))
          (when (probe-file example-path)
            (format s "~%Example content:~%```~%~A~%```~%"
                    (get-file-content example-path)))))
      ;; Entity-level rules
      (when entity-rules
        (format s "~%Entity Rules:~%")
        (dolist (r entity-rules)
          (format s "- ~A~%" r)))
      ;; Scoped rules from mold :rules
      (when scoped-rules
        (format s "~%Scoped Rules:~%")
        (dolist (r scoped-rules)
          (format s "- ~A~%" (mold-rule-text r))))
      ;; Interface rules
      (when iface-rules
        (format s "~%Interface Specifications:~%")
        (dolist (iface iface-rules)
          (format s "~%Interface: ~A~%" (mold-interface-name iface))
          (when (mold-interface-description iface)
            (format s "  Description: ~A~%" (mold-interface-description iface)))
          (dolist (r (mold-interface-rules iface))
            (format s "  - ~A~%" r))))
      ;; Generic rules
      (when generic-rules
        (format s "~%General Rules:~%")
        (dolist (r generic-rules)
          (format s "- ~A~%" (mold-rule-text r)))))))

(defun mold-pour (entity-name &optional mold)
  "Ask AI to implement a specific entity from the mold."
  (let* ((m (or mold *active-mold*))
         (name-str (string-downcase (if (stringp entity-name) entity-name (symbol-name entity-name)))))
    (unless m
      (format t "~&No active mold. Use /mold.use <name> first.~%")
      (return-from mold-pour nil))
    (let ((entity (find name-str (mold-definition-entities m)
                        :key (lambda (e)
                               (string-downcase
                                (if (stringp (mold-entity-name e))
                                    (mold-entity-name e)
                                    (symbol-name (mold-entity-name e)))))
                        :test #'string=)))
      (unless entity
        (format t "~&Entity '~A' not found in mold '~A'.~%Available entities: ~{~A~^, ~}~%"
                name-str (mold-definition-name m)
                (mapcar (lambda (e) (mold-entity-name e)) (mold-definition-entities m)))
        (return-from mold-pour nil))
      (let ((prompt (%mold-pour-entity-prompt m entity)))
        (format t "~&Pouring entity '~A'...~%" name-str)
        ;; Use the existing LLM interaction - just send the prompt
        (when (fboundp 'get-llm-response)
          (funcall 'get-llm-response prompt))))))

(defun mold-pour-all (&optional mold)
  "Ask AI to implement all entities from the mold, one by one."
  (let ((m (or mold *active-mold*)))
    (unless m
      (format t "~&No active mold. Use /mold.use <name> first.~%")
      (return-from mold-pour-all nil))
    (format t "~&Pouring all ~A entities from mold '~A'...~%"
            (length (mold-definition-entities m))
            (mold-definition-name m))
    (dolist (entity (mold-definition-entities m))
      (mold-pour (mold-entity-name entity) m))))

;;* Mold Export

(defun mold-export-as-json (mold)
  "Export a mold as JSON."
  (let ((alist `(("name" . ,(mold-definition-name mold))
                 ("description" . ,(or (mold-definition-description mold) ""))
                 ("llms" . ,(coerce (mapcar #'string (mold-definition-llms mold)) 'vector))
                 ("entities" . ,(coerce
                                 (mapcar (lambda (e)
                                           `(("name" . ,(format nil "~A" (mold-entity-name e)))
                                             ("description" . ,(or (mold-entity-description e) ""))
                                             ("defaultTags" . ,(coerce (mapcar #'string (mold-entity-default-tags e)) 'vector))
                                             ("requiredBehaviors" . ,(coerce (mapcar #'string (mold-entity-required-behaviors e)) 'vector))
                                             ("example" . ,(or (mold-entity-example e) ""))
                                             ("rules" . ,(coerce (mold-entity-rules e) 'vector))))
                                         (mold-definition-entities mold))
                                 'vector))
                 ("interfaces" . ,(coerce
                                   (mapcar (lambda (iface)
                                             `(("name" . ,(format nil "~A" (mold-interface-name iface)))
                                               ("entity" . ,(format nil "~A" (or (mold-interface-entity iface) "")))
                                               ("description" . ,(or (mold-interface-description iface) ""))
                                               ("rules" . ,(coerce (mold-interface-rules iface) 'vector))))
                                           (mold-definition-interfaces mold))
                                   'vector))
                 ("rules" . ,(coerce
                              (mapcar (lambda (r)
                                        `(("text" . ,(mold-rule-text r))
                                          ("scope" . ,(format nil "~A" (or (mold-rule-scope r) "generic")))
                                          ("target" . ,(format nil "~A" (or (mold-rule-target r) "")))))
                                      (mold-definition-rules mold))
                              'vector)))))
    (to-json alist)))

(defun mold-export-as-org (mold)
  "Export a mold as org-mode text."
  (with-output-to-string (s)
    (format s "#+TITLE: Mold: ~A~%" (mold-definition-name mold))
    (format s "#+DESCRIPTION: ~A~%" (or (mold-definition-description mold) ""))
    (format s "~%* Metadata~%")
    (format s ":PROPERTIES:~%")
    (format s ":NAME: ~A~%" (mold-definition-name mold))
    (format s ":LLMS: ~{~A~^, ~}~%" (mold-definition-llms mold))
    (format s ":END:~%")
    ;; Entities
    (format s "~%* Entities~%")
    (dolist (e (mold-definition-entities mold))
      (format s "** ~A~%" (mold-entity-name e))
      (format s ":PROPERTIES:~%")
      (when (mold-entity-default-tags e)
        (format s ":DEFAULT_TAGS: ~{~A~^, ~}~%" (mold-entity-default-tags e)))
      (when (mold-entity-required-behaviors e)
        (format s ":REQUIRED_BEHAVIORS: ~{~A~^, ~}~%" (mold-entity-required-behaviors e)))
      (when (mold-entity-example e)
        (format s ":EXAMPLE: ~A~%" (mold-entity-example e)))
      (format s ":END:~%")
      (when (mold-entity-description e)
        (format s "~A~%" (mold-entity-description e)))
      (when (mold-entity-rules e)
        (format s "*** Rules~%")
        (dolist (r (mold-entity-rules e))
          (format s "- ~A~%" r))))
    ;; Interfaces
    (format s "~%* Interfaces~%")
    (dolist (iface (mold-definition-interfaces mold))
      (format s "** ~A~%" (mold-interface-name iface))
      (format s ":PROPERTIES:~%")
      (when (mold-interface-entity iface)
        (format s ":ENTITY: ~A~%" (mold-interface-entity iface)))
      (format s ":END:~%")
      (when (mold-interface-description iface)
        (format s "~A~%" (mold-interface-description iface)))
      (when (mold-interface-rules iface)
        (format s "*** Rules~%")
        (dolist (r (mold-interface-rules iface))
          (format s "- ~A~%" r))))
    ;; Rules
    (format s "~%* Rules~%")
    (dolist (r (mold-definition-rules mold))
      (case (mold-rule-scope r)
        (:generic (format s "- ~A~%" (mold-rule-text r)))
        (:entity (format s "- [entity:~A] ~A~%" (mold-rule-target r) (mold-rule-text r)))
        (:interface (format s "- [interface:~A] ~A~%" (mold-rule-target r) (mold-rule-text r)))
        (t (format s "- ~A~%" (mold-rule-text r)))))))

(defun mold-export (format-opt &optional mold)
  "Export the active mold in the given format (json, org, xml)."
  (let ((m (or mold *active-mold*)))
    (unless m
      (format t "~&No active mold to export.~%")
      (return-from mold-export nil))
    (let ((fmt (string-downcase format-opt)))
      (cond
        ((string= fmt "json")
         (mold-export-as-json m))
        ((or (string= fmt "org") (string= fmt "org-mode"))
         (mold-export-as-org m))
        ((string= fmt "xml")
         ;; Simple XML export
         (with-output-to-string (s)
           (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
           (format s "<mold name=\"~A\">~%" (mold-definition-name m))
           (format s "  <description>~A</description>~%" (or (mold-definition-description m) ""))
           (dolist (e (mold-definition-entities m))
             (format s "  <entity name=\"~A\">~%" (mold-entity-name e))
             (when (mold-entity-description e)
               (format s "    <description>~A</description>~%" (mold-entity-description e)))
             (dolist (r (mold-entity-rules e))
               (format s "    <rule>~A</rule>~%" r))
             (format s "  </entity>~%"))
           (dolist (iface (mold-definition-interfaces m))
             (format s "  <interface name=\"~A\"~@[ entity=\"~A\"~]>~%"
                     (mold-interface-name iface) (mold-interface-entity iface))
             (when (mold-interface-description iface)
               (format s "    <description>~A</description>~%" (mold-interface-description iface)))
             (dolist (r (mold-interface-rules iface))
               (format s "    <rule>~A</rule>~%" r))
             (format s "  </interface>~%"))
           (dolist (r (mold-definition-rules m))
             (format s "  <rule scope=\"~A\"~@[ target=\"~A\"~]>~A</rule>~%"
                     (or (mold-rule-scope r) "generic")
                     (mold-rule-target r)
                     (mold-rule-text r)))
           (format s "</mold>~%")))
        (t
         (format t "~&Unsupported export format: ~A. Use json, org, or xml.~%" fmt)
         nil)))))

;;* Org-mode Mold Parsing

(defun %org-extract-list-items (content)
  "Extract unordered list items (lines starting with '- ') from org content."
  (let ((items '()))
    (dolist (line (str:lines content))
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (when (str:starts-with-p "- " trimmed)
          (push (subseq trimmed 2) items))))
    (nreverse items)))

(defun %org-parse-scoped-rule (text)
  "Parse a rule string that might have a scope prefix like [entity:route] or [interface:foo]."
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "^\\[(entity|interface):([^\\]]+)\\]\\s*(.+)$" text)
    (if match
        (let ((scope (intern (string-upcase (aref regs 0)) :keyword))
              (target (aref regs 1))
              (rule-text (aref regs 2)))
          (make-mold-rule :text rule-text :scope scope :target target))
        (make-mold-rule :text text :scope :generic))))

(defun mold-parse-org-file (file-path)
  "Parse an org-mode file into a mold-definition.
   Expected structure:
   * Metadata (with :NAME: and :LLMS: properties)
   * Entities (** headings with properties and rules sub-headings)
   * Interfaces (** headings with properties and rules sub-headings)
   * Rules (list items, optionally scoped with [entity:x] or [interface:x])"
  (let* ((parsed (org-mode:parse-org-file-full file-path))
         (headings (getf parsed :headings))
         (file-props (getf parsed :properties))
         (mold-name (or (cdr (assoc :NAME file-props)) 
                        (pathname-name file-path)))
         (mold-desc nil)
         (mold-llms '(:all))
         (mold-entities '())
         (mold-interfaces '())
         (mold-rules '()))
    ;; Process top-level headings
    (dolist (h headings)
      (let ((title (string-trim '(#\Space #\Tab) (getf h :title)))
            (content (getf h :content)))
        (cond
          ;; Metadata section
          ((string-equal title "Metadata")
           (let ((sub-props (org-mode:parse-org-properties-from-content content)))
             (let ((name-prop (cdr (assoc :NAME sub-props))))
               (when name-prop (setf mold-name name-prop)))
             (let ((llms-prop (cdr (assoc :LLMS sub-props))))
               (when llms-prop
                 (setf mold-llms
                       (mapcar (lambda (s)
                                 (intern (string-upcase (string-trim '(#\Space) s)) :keyword))
                               (str:split "," llms-prop)))))
             (let ((desc-prop (cdr (assoc :DESCRIPTION sub-props))))
               (when desc-prop (setf mold-desc desc-prop)))
             ;; Also check for non-property description text
             (unless mold-desc
               (let ((lines (str:lines content)))
                 (let ((desc-lines (remove-if (lambda (l)
                                                (let ((tr (string-trim '(#\Space #\Tab) l)))
                                                  (or (string= tr "")
                                                      (str:starts-with-p ":" tr))))
                                              lines)))
                   (when desc-lines
                     (setf mold-desc (string-trim '(#\Space #\Tab #\Newline)
                                                  (str:join " " desc-lines)))))))))
          ;; Entities section - parse sub-headings from content
          ((string-equal title "Entities")
           (let ((sub-headings (org-mode:parse-org-code-blocks-from-content content)))
             (declare (ignore sub-headings)))
           ;; Parse ** headings within the entities content
           (dolist (line (str:lines content))
             (let ((heading (org-mode:parse-org-heading-line line)))
               (when heading
                 ;; We need to parse the full entity sub-sections
                 ;; This is handled below by re-parsing content blocks
                 nil)))
           ;; Actually parse entity sub-sections from the raw content
           (setf mold-entities (%org-parse-entity-subsections content)))

          ;; Interfaces section
          ((string-equal title "Interfaces")
           (setf mold-interfaces (%org-parse-interface-subsections content)))

          ;; Rules section
          ((string-equal title "Rules")
           (let ((items (%org-extract-list-items content)))
             (setf mold-rules (mapcar #'%org-parse-scoped-rule items)))))))

    (make-mold-definition
     :name (string-downcase mold-name)
     :description mold-desc
     :llms mold-llms
     :entities mold-entities
     :interfaces mold-interfaces
     :rules mold-rules
     :source (namestring file-path))))

(defun %org-parse-entity-subsections (content)
  "Parse entity sub-sections from org content under the Entities heading."
  (let ((entities '())
        (current-entity nil)
        (current-rules '())
        (in-rules nil))
    (dolist (line (str:lines content))
      (let* ((trimmed (string-trim '(#\Space #\Tab) line))
             (heading (org-mode:parse-org-heading-line line)))
        (cond
          ;; New entity heading (level 2 = ** under * Entities)
          ((and heading (= (car heading) 2))
           ;; Save previous entity
           (when current-entity
             (setf (mold-entity-rules current-entity) (nreverse current-rules))
             (push current-entity entities))
           ;; Start new entity
           (setf current-entity (make-mold-entity :name (intern (string-upcase (cdr heading)) :hactar))
                 current-rules '()
                 in-rules nil))
          ;; Rules sub-heading
          ((and heading (>= (car heading) 3)
                (string-equal (string-trim '(#\Space #\Tab) (cdr heading)) "Rules"))
           (setf in-rules t))
          ;; Other sub-heading ends rules section
          ((and heading (>= (car heading) 3))
           (setf in-rules nil))
          ;; Property lines within entity
          ((and current-entity (not in-rules))
           (let ((props (org-mode:parse-org-properties-from-content
                         (format nil ":PROPERTIES:~%~A~%:END:" trimmed))))
             (dolist (prop props)
               (cond
                 ((eq (car prop) :DEFAULT_TAGS)
                  (setf (mold-entity-default-tags current-entity)
                        (mapcar (lambda (s)
                                  (intern (string-upcase (string-trim '(#\Space) s)) :keyword))
                                (str:split "," (cdr prop)))))
                 ((eq (car prop) :REQUIRED_BEHAVIORS)
                  (setf (mold-entity-required-behaviors current-entity)
                        (mapcar (lambda (s)
                                  (intern (string-upcase (string-trim '(#\Space) s)) :keyword))
                                (str:split "," (cdr prop)))))
                 ((eq (car prop) :EXAMPLE)
                  (setf (mold-entity-example current-entity) (cdr prop)))
                 ((eq (car prop) :DESCRIPTION)
                  (setf (mold-entity-description current-entity) (cdr prop))))))
           ;; Also check for plain description text (non-property, non-list)
           (when (and current-entity
                      (not (string= trimmed ""))
                      (not (str:starts-with-p ":" trimmed))
                      (not (str:starts-with-p "-" trimmed))
                      (not (str:starts-with-p "*" trimmed))
                      (null (mold-entity-description current-entity)))
             (setf (mold-entity-description current-entity) trimmed)))
          ;; Rule list items
          ((and in-rules (str:starts-with-p "- " trimmed))
           (push (subseq trimmed 2) current-rules)))))
    ;; Save last entity
    (when current-entity
      (setf (mold-entity-rules current-entity) (nreverse current-rules))
      (push current-entity entities))
    (nreverse entities)))

(defun %org-parse-interface-subsections (content)
  "Parse interface sub-sections from org content under the Interfaces heading."
  (let ((interfaces '())
        (current-iface nil)
        (current-rules '())
        (in-rules nil))
    (dolist (line (str:lines content))
      (let* ((trimmed (string-trim '(#\Space #\Tab) line))
             (heading (org-mode:parse-org-heading-line line)))
        (cond
          ;; New interface heading
          ((and heading (= (car heading) 2))
           (when current-iface
             (setf (mold-interface-rules current-iface) (nreverse current-rules))
             (push current-iface interfaces))
           (setf current-iface (make-mold-interface :name (intern (string-upcase (cdr heading)) :keyword))
                 current-rules '()
                 in-rules nil))
          ;; Rules sub-heading
          ((and heading (>= (car heading) 3)
                (string-equal (string-trim '(#\Space #\Tab) (cdr heading)) "Rules"))
           (setf in-rules t))
          ((and heading (>= (car heading) 3))
           (setf in-rules nil))
          ;; Properties
          ((and current-iface (not in-rules))
           (let ((props (org-mode:parse-org-properties-from-content
                         (format nil ":PROPERTIES:~%~A~%:END:" trimmed))))
             (dolist (prop props)
               (cond
                 ((eq (car prop) :ENTITY)
                  (setf (mold-interface-entity current-iface) (cdr prop)))
                 ((eq (car prop) :DESCRIPTION)
                  (setf (mold-interface-description current-iface) (cdr prop))))))
           (when (and current-iface
                      (not (string= trimmed ""))
                      (not (str:starts-with-p ":" trimmed))
                      (not (str:starts-with-p "-" trimmed))
                      (not (str:starts-with-p "*" trimmed))
                      (null (mold-interface-description current-iface)))
             (setf (mold-interface-description current-iface) trimmed)))
          ;; Rule list items
          ((and in-rules (str:starts-with-p "- " trimmed))
           (push (subseq trimmed 2) current-rules)))))
    (when current-iface
      (setf (mold-interface-rules current-iface) (nreverse current-rules))
      (push current-iface interfaces))
    (nreverse interfaces)))

;;* Load molds from disk on startup

(defun mold-load-installed ()
  "Load all mold files from the *molds-path* directory."
  (ensure-directories-exist *molds-path*)
  (let ((lisp-files (directory (merge-pathnames "*.lisp" *molds-path*)))
        (org-files (directory (merge-pathnames "*.org" *molds-path*)))
        (count 0))
    (dolist (f lisp-files)
      (handler-case
          (progn (load f) (incf count))
        (error (e)
          (debug-log "Error loading mold" (namestring f) ":" e))))
    (dolist (f org-files)
      (handler-case
          (let ((mold (mold-parse-org-file f)))
            (when mold
              (setf (gethash (mold-definition-name mold) *molds*) mold)
              (incf count)))
        (error (e)
          (debug-log "Error loading org mold" (namestring f) ":" e))))
    (when (> count 0)
      (debug-log "Loaded" count "mold(s) from" (namestring *molds-path*)))))

;;* Commands

;;** mold.list
(define-command "mold.list" (args)
  "List all available molds."
  (declare (ignore args))
  (mold-list)
  :sub t
  :acp (lambda (cmd-args)
         (declare (ignore cmd-args))
         (let ((molds-list '()))
           (maphash (lambda (name mold)
                      (push `(("name" . ,name)
                              ("description" . ,(or (mold-definition-description mold) ""))
                              ("active" . ,(if (and *active-mold*
                                                    (string= name (mold-definition-name *active-mold*)))
                                               t nil))
                              ("entities" . ,(length (mold-definition-entities mold)))
                              ("interfaces" . ,(length (mold-definition-interfaces mold)))
                              ("rules" . ,(length (mold-definition-rules mold))))
                            molds-list))
                    *molds*)
           `(("text" . ,(format nil "~A mold(s) available." (hash-table-count *molds*)))
             ("data" . ,(coerce (nreverse molds-list) 'vector))))))

;;** mold.use
(define-command "mold.use" (args)
  "Activate a mold for the current project.
   Usage: /mold.use <mold-name>"
  (if args
      (mold-use (first args))
      (format t "Usage: /mold.use <mold-name>~%"))
  :sub t
  :acp (lambda (cmd-args)
         (if cmd-args
             (let ((result (mold-use (first cmd-args))))
               (if result
                   `(("text" . ,(format nil "Activated mold '~A'." (first cmd-args))))
                   `(("text" . ,(format nil "Failed to activate mold '~A'." (first cmd-args))))))
             `(("text" . "Usage: /mold.use <mold-name>")))))

;;** mold.show
(define-command "mold.show" (args)
  "Display the current project's mold (all entities, interfaces, rules).
   Usage: /mold.show [mold-name]"
  (if args
      (let ((mold (gethash (string-downcase (first args)) *molds*)))
        (if mold
            (mold-show mold)
            (format t "Mold '~A' not found.~%" (first args))))
      (mold-show))
  :sub t
  :acp (lambda (cmd-args)
         (let* ((m (if cmd-args
                       (gethash (string-downcase (first cmd-args)) *molds*)
                       *active-mold*)))
           (if m
               `(("text" . ,(with-output-to-string (*standard-output*) (mold-show m)))
                 ("data" . (("name" . ,(mold-definition-name m))
                             ("description" . ,(or (mold-definition-description m) "")))))
               `(("text" . "No active mold. Use /mold.use <name> first."))))))

;;** mold.install
(define-command "mold.install" (args)
  "Install a mold from various sources.
   Usage: /mold.install <source>
   
   Sources:
     /mold.install saas-api           # from hactar registry
     /mold.install ./my-company-mold  # from local path
     /mold.install github:org/mold    # from GitHub"
  (if args
      (mold-install (first args))
      (format t "Usage: /mold.install <source>~%"))
  :sub t
  :acp (lambda (cmd-args)
         (if cmd-args
             (let ((result (mold-install (first cmd-args))))
               `(("text" . ,(if result
                                (format nil "Installed mold from '~A'." (first cmd-args))
                                (format nil "Failed to install mold from '~A'." (first cmd-args))))))
             `(("text" . "Usage: /mold.install <source>")))))

;;** mold.init
(define-command "mold.init" (args)
  "Scaffold a mold for the current project.
   Usage: /mold.init [mold-name]"
  (mold-init (first args))
  :sub t
  :acp (lambda (cmd-args)
         (let ((output (with-output-to-string (*standard-output*)
                         (mold-init (first cmd-args)))))
           `(("text" . ,output)))))

;;** mold.validate
(define-command "mold.validate" (args)
  "Check that all code conforms to the active mold."
  (declare (ignore args))
  (multiple-value-bind (ok-p issues)
      (mold-validate)
    (declare (ignore ok-p issues)))
  :sub t
  :acp (lambda (cmd-args)
         (declare (ignore cmd-args))
         (multiple-value-bind (ok-p issues)
             (mold-validate)
           `(("text" . ,(if ok-p "Validation passed." (format nil "~A issue(s) found." (length issues))))
             ("data" . (("valid" . ,ok-p)
                        ("issues" . ,(coerce (or issues #()) 'vector))))))))

;;** mold.pour
(define-command "mold.pour" (args)
  "Ask AI to implement an entity from the mold.
   Usage: /mold.pour <entity-name>
          /mold.pour --all"
  (cond
    ((and args (or (string= (first args) "--all")
                   (string= (first args) "-a")))
     (mold-pour-all))
    (args
     (mold-pour (first args)))
    (t
     (format t "Usage: /mold.pour <entity-name> or /mold.pour --all~%")))
  :sub t
  :acp (lambda (cmd-args)
         (cond
           ((and cmd-args (or (string= (first cmd-args) "--all")
                              (string= (first cmd-args) "-a")))
            (let ((output (with-output-to-string (*standard-output*)
                            (mold-pour-all))))
              `(("text" . ,output))))
           (cmd-args
            (let ((output (with-output-to-string (*standard-output*)
                            (mold-pour (first cmd-args)))))
              `(("text" . ,output))))
           (t
            `(("text" . "Usage: /mold.pour <entity-name> or /mold.pour --all"))))))

;;** mold.export
(define-command "mold.export" (args)
  "Export the active mold as a portable schema.
   Usage: /mold.export --format json|org|xml"
  (let ((format-opt (or (first args) "json")))
    (let ((result (mold-export format-opt)))
      (when result
        (format t "~A~%" result))))
  :sub t
  :acp (lambda (cmd-args)
         (let* ((format-opt (or (first cmd-args) "json"))
                (result (mold-export format-opt)))
           (if result
               `(("text" . ,result)
                 ("data" . (("format" . ,format-opt))))
               `(("text" . "Export failed. No active mold or unsupported format.")))))
  :cli-options ((:long "format" :short "f" :description "Export format: json, org, xml (default: json)")))

;;** molds (plural alias for mold.list)
(define-command molds (args)
  "Mold management commands. Without arguments, lists available molds.
   Usage: /molds [list|install|use]"
  (cond
    ((null args) (mold-list))
    ((string-equal (first args) "list") (mold-list))
    ((string-equal (first args) "install")
     (if (rest args)
         (mold-install (second args))
         (format t "Usage: /molds install <source>~%")))
    ((string-equal (first args) "use")
     (if (rest args)
         (mold-use (second args))
         (format t "Usage: /molds use <mold-name>~%")))
    (t (format t "Unknown molds subcommand: ~A~%" (first args))))
  :sub t
  :acp (lambda (cmd-args)
         (cond
           ((or (null cmd-args) (string-equal (first cmd-args) "list"))
            (let ((molds-list '()))
              (maphash (lambda (name mold)
                         (push `(("name" . ,name)
                                 ("description" . ,(or (mold-definition-description mold) "")))
                               molds-list))
                       *molds*)
              `(("text" . ,(format nil "~A mold(s)." (hash-table-count *molds*)))
                ("data" . ,(coerce (nreverse molds-list) 'vector)))))
           (t `(("text" . ,(format nil "Use /mold.list, /mold.install, /mold.use etc.")))))))
