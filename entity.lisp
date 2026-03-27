;;; entity.lisp — BOT (Behavior-Object-Tags) Entity-Component-System
(in-package :hactar)

;;* exports

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defentity
            defskill
            defbehavior
            deftag
            defentity-recipe
            defmold
            entity/create
            entity/list
            entity/update
            entity/delete
            entity-get
            entity-set
            entity-instance-p
            entity-instance-id
            entity-instance-type
            entity-instance-tags
            find-entity-instances
            create-entity-instance
            delete-entity-instance
            get-skill
            get-behavior
            get-tag
            get-active-entity-rules
            *entity-registry*
            *entity-instances*
            *entity-implementations*
            *behavior-registry*
            *tag-registry*
            *skill-registry*
            *molds*
            *active-mold*
            mold-definition
            mold-definition-name
            mold-definition-description
            mold-definition-entities
            mold-definition-interfaces
            mold-definition-rules
            mold-entity
            mold-entity-name
            mold-interface
            mold-interface-name
            mold-use
            mold-show
            mold-list
            mold-install
            mold-validate
            mold-pour
            mold-pour-all
            mold-export
            mold-init
            mold-load-installed)))

;;* registries

(defvar *entity-registry* (make-hash-table :test 'equal)
  "Registry of all entity type definitions, keyed by type name string.")

(defvar *entity-instances* (make-hash-table :test 'equal)
  "Storage for entity instances, keyed by (type . id) cons.")

(defvar *entity-implementations* (make-hash-table :test 'equal)
  "Registry of entity implementations (def<entity> forms), keyed by type name string.
   Each entry is a list of implementation structs; highest priority wins for current stack.")

(defvar *behavior-registry* (make-hash-table :test 'equal)
  "Registry of behaviors, keyed by name string.")

(defvar *tag-registry* (make-hash-table :test 'equal)
  "Registry of tags and their associated behaviors, keyed by keyword.")

(defvar *skill-registry* (make-hash-table :test 'equal)
  "Registry of skills, keyed by keyword.")

(defvar *active-rules* (make-hash-table :test 'equal)
  "Currently active rules from behavior activation, keyed by behavior name.")

(defvar *entity-counter* 0
  "Counter for generating unique entity instance IDs.")

;;* structs

(defstruct entity-definition
  "Defines an entity type (route, model, component, lib, recipe)."
  (name nil :type (or null symbol))
  (description nil :type (or null string))
  (schema '() :type list)
  (default-behaviors '() :type list)
  (commands-p nil :type boolean)
  (storage :memory :type keyword)
  (searchable '() :type list))

(defstruct entity-instance
  "A concrete instance of an entity."
  (id nil :type (or null string integer))
  (type nil :type (or null symbol string))
  (properties (make-hash-table :test 'equal) :type hash-table)
  (tags '() :type list)
  (created-at nil :type (or null integer))
  (updated-at nil :type (or null integer)))

(defstruct entity-implementation
  "A framework-specific implementation of an entity type."
  (name nil :type (or null symbol string))
  (entity-type nil :type (or null symbol string))
  (description nil :type (or null string))
  (priority 0 :type integer)
  (when-fn nil :type (or null function))
  (options '() :type list)
  (skills '() :type list)
  (schema-additions '() :type list)
  (create-fn nil :type (or null function))
  (list-fn nil :type (or null function))
  (update-fn nil :type (or null function))
  (delete-fn nil :type (or null function)))

(defstruct bot-behavior
  "A composable unit of functionality that attaches to entities via tags."
  (name nil :type (or null symbol string))
  (description nil :type (or null string))
  (tags '() :type list)
  (skill nil :type (or null keyword string))
  (rule nil)
  (generator nil)
  (validator nil :type (or null function))
  (before-create nil :type (or null function))
  (after-create nil :type (or null function))
  (before-update nil :type (or null function))
  (after-update nil :type (or null function))
  (before-delete nil :type (or null function))
  (after-delete nil :type (or null function)))

(defstruct bot-tag
  "A tag that groups behaviors."
  (name nil :type (or null keyword))
  (description nil :type (or null string))
  (behaviors '() :type list)
  (inherits '() :type list)
  (requires '() :type list)
  (skills '() :type list))

(defstruct bot-skill
  "An instruction set loaded into context during entity operations."
  (name nil :type (or null keyword))
  (description nil :type (or null string))
  (instructions nil :type (or null string))
  (rules nil :type (or null string))
  (examples '() :type list)
  (prefixed-p nil :type boolean)
  (prefix nil :type (or null keyword))
  (base-name nil :type (or null keyword)))

;;* skill helpers

(defun skill-name-prefixed-p (name)
  "Check if a skill keyword has an operation prefix like :create/skill-name."
  (and (keywordp name)
       (if (find #\/ (symbol-name name)) t nil)))

(defun skill-name-prefix (name)
  "Extract the operation prefix from a prefixed skill keyword."
  (let* ((str (symbol-name name))
         (slash-pos (position #\/ str)))
    (when slash-pos
      (intern (subseq str 0 slash-pos) :keyword))))

(defun skill-name-base (name)
  "Extract the base skill name from a prefixed skill keyword."
  (let* ((str (symbol-name name))
         (slash-pos (position #\/ str)))
    (if slash-pos
        (intern (subseq str (1+ slash-pos)) :keyword)
        name)))

;;* entity instance operations

(defun entity-get (entity property)
  "Get a property value from an entity instance."
  (gethash (if (keywordp property)
               (string-downcase (symbol-name property))
               property)
           (entity-instance-properties entity)))

(defun entity-set (entity property value)
  "Set a property value on an entity instance."
  (setf (gethash (if (keywordp property)
                     (string-downcase (symbol-name property))
                     property)
                 (entity-instance-properties entity))
        value)
  (setf (entity-instance-updated-at entity) (get-universal-time)))

(defun make-entity-id ()
  "Generate a unique entity instance ID."
  (incf *entity-counter*)
  (format nil "ent-~A-~A" *entity-counter* (get-universal-time)))

(defun create-entity-instance (type &rest properties)
  "Create a new entity instance of TYPE with PROPERTIES (a plist)."
  (let* ((instance (make-entity-instance
                    :id (make-entity-id)
                    :type (if (symbolp type) (string-downcase (symbol-name type)) type)
                    :created-at (get-universal-time)
                    :updated-at (get-universal-time)))
         (props (entity-instance-properties instance)))
    (loop for (k v) on properties by #'cddr
          do (setf (gethash (if (keywordp k) (string-downcase (symbol-name k)) k) props) v))
    (let ((tags (gethash "tags" props)))
      (when tags
        (setf (entity-instance-tags instance) (if (listp tags) tags (list tags)))))
    (let ((key (cons (entity-instance-type instance) (entity-instance-id instance))))
      (setf (gethash key *entity-instances*) instance))
    instance))

(defun find-entity-instances (type &key tags)
  "Find entity instances by type and optional tag filter."
  (let ((type-str (if (symbolp type) (string-downcase (symbol-name type)) type))
        (results '()))
    (maphash (lambda (key instance)
               (when (string= (car key) type-str)
                 (if tags
                     (when (every (lambda (tag)
                                    (member tag (entity-instance-tags instance) :test #'string-equal))
                                  tags)
                       (push instance results))
                     (push instance results))))
             *entity-instances*)
    (nreverse results)))

(defun delete-entity-instance (type id)
  "Delete an entity instance."
  (let ((type-str (if (symbolp type) (string-downcase (symbol-name type)) type)))
    (remhash (cons type-str id) *entity-instances*)))

;;* skill operations

(defun register-skill (name &key description instructions rules examples)
  "Register a skill in the skill registry."
  (let* ((prefixed (skill-name-prefixed-p name))
         (skill (make-bot-skill
                 :name name
                 :description description
                 :instructions instructions
                 :rules rules
                 :examples examples
                 :prefixed-p prefixed
                 :prefix (when prefixed (skill-name-prefix name))
                 :base-name (skill-name-base name))))
    (setf (gethash name *skill-registry*) skill)
    skill))

(defun get-skill (name)
  "Get a skill by name."
  (gethash name *skill-registry*))

(defun load-skill-into-context (skill-name)
  "Load a skill's instructions into the current context (adds to active rules)."
  (let ((skill (if (bot-skill-p skill-name) skill-name (get-skill skill-name))))
    (when skill
      (when (bot-skill-instructions skill)
        (setf (gethash (bot-skill-name skill) *active-rules*)
              (bot-skill-instructions skill)))
      skill)))

;;* behavior operations

(defun register-behavior (name &key description tags skill rule generator validator
                                 before-create after-create before-update after-update
                                 before-delete after-delete)
  "Register a behavior."
  (let ((behavior (make-bot-behavior
                   :name name
                   :description description
                   :tags tags
                   :skill skill
                   :rule rule
                   :generator generator
                   :validator validator
                   :before-create before-create
                   :after-create after-create
                   :before-update before-update
                   :after-update after-update
                   :before-delete before-delete
                   :after-delete after-delete)))
    (setf (gethash (if (stringp name) name (string-downcase (symbol-name name)))
                   *behavior-registry*)
          behavior)
    behavior))

(defun get-behavior (name)
  "Get a behavior by name."
  (gethash (if (stringp name) name (string-downcase (symbol-name name)))
           *behavior-registry*))

;;* tag operations

(defun register-tag (name &key description behaviors inherits requires skills)
  "Register a tag."
  (let ((tag (make-bot-tag
              :name name
              :description description
              :behaviors behaviors
              :inherits inherits
              :requires requires
              :skills skills)))
    (setf (gethash name *tag-registry*) tag)
    tag))

(defun get-tag (name)
  "Get a tag by name."
  (gethash name *tag-registry*))

(defun get-tag-behaviors (tag-name)
  "Get all behaviors for a tag, including inherited ones."
  (let ((tag (get-tag tag-name)))
    (when tag
      (let ((behaviors (mapcar #'get-behavior (bot-tag-behaviors tag))))
        (dolist (parent (bot-tag-inherits tag))
          (setf behaviors (append behaviors (get-tag-behaviors parent))))
        (remove nil behaviors)))))

;;* implementation resolution

(defun register-entity-implementation (entity-type impl)
  "Register an entity implementation."
  (let ((type-str (if (symbolp entity-type)
                      (string-downcase (symbol-name entity-type))
                      entity-type)))
    (push impl (gethash type-str *entity-implementations*))
    impl))

(defun resolve-entity-implementation (entity-type)
  "Find the best (highest-priority, active) implementation for an entity type."
  (let* ((type-str (if (symbolp entity-type)
                       (string-downcase (symbol-name entity-type))
                       entity-type))
         (impls (gethash type-str *entity-implementations*)))
    (when impls
      (let ((active (remove-if (lambda (impl)
                                 (and (entity-implementation-when-fn impl)
                                      (not (funcall (entity-implementation-when-fn impl)))))
                               impls)))
        (when active
          (first (sort (copy-list active) #'> :key #'entity-implementation-priority)))))))

;;* entity lifecycle

(defun entity/load-skills-for-operation (impl operation)
  "Load skills for a specific entity operation."
  (let ((skills (entity-implementation-skills impl)))
    (dolist (skill-name skills)
      (cond
        ((skill-name-prefixed-p skill-name)
         (when (eq (skill-name-prefix skill-name) operation)
           (let ((skill (get-skill skill-name)))
             (when skill
               (when (bot-skill-instructions skill)
                 (setf (gethash (skill-name-base skill-name) *active-rules*)
                       (bot-skill-instructions skill)))))))
        (t
         (load-skill-into-context skill-name))))))

(defun entity/activate-behaviors (entity operation)
  "Activate all behaviors associated with an entity's tags for an operation."
  (let ((tags (entity-instance-tags entity)))
    (dolist (tag-name tags)
      (let ((tag-key (if (keywordp tag-name) tag-name
                         (intern (string-upcase tag-name) :keyword))))
        (let ((behaviors (get-tag-behaviors tag-key)))
          (dolist (behavior behaviors)
            (when behavior
              (when (bot-behavior-skill behavior)
                (load-skill-into-context (bot-behavior-skill behavior)))
              (when (bot-behavior-rule behavior)
                (setf (gethash (bot-behavior-name behavior) *active-rules*)
                      (if (functionp (bot-behavior-rule behavior))
                          (funcall (bot-behavior-rule behavior) entity)
                          (bot-behavior-rule behavior))))
              (let ((hook-fn (case operation
                               (:create (bot-behavior-before-create behavior))
                               (:update (bot-behavior-before-update behavior))
                               (:delete (bot-behavior-before-delete behavior)))))
                (when hook-fn
                  (funcall hook-fn entity))))))))))

(defun entity/run-after-hooks (entity operation)
  "Run after-operation hooks for an entity's behaviors."
  (let ((tags (entity-instance-tags entity)))
    (dolist (tag-name tags)
      (let ((tag-key (if (keywordp tag-name) tag-name
                         (intern (string-upcase tag-name) :keyword))))
        (let ((behaviors (get-tag-behaviors tag-key)))
          (dolist (behavior behaviors)
            (when behavior
              (let ((hook-fn (case operation
                               (:create (bot-behavior-after-create behavior))
                               (:update (bot-behavior-after-update behavior))
                               (:delete (bot-behavior-after-delete behavior)))))
                (when hook-fn
                  (funcall hook-fn entity))))))))))

(defun entity/validate (entity)
  "Run all validators from an entity's behaviors. Returns (values ok-p errors)."
  (let ((errors '()))
    (dolist (tag-name (entity-instance-tags entity))
      (let ((tag-key (if (keywordp tag-name) tag-name
                         (intern (string-upcase tag-name) :keyword))))
        (dolist (behavior (get-tag-behaviors tag-key))
          (when (and behavior (bot-behavior-validator behavior))
            (handler-case
                (funcall (bot-behavior-validator behavior) entity)
              (error (e)
                (push (format nil "Validation error (~A): ~A"
                              (bot-behavior-name behavior) e)
                      errors)))))))
    (values (null errors) (nreverse errors))))

;;* entity CRUD operations

(defun entity/create (entity-type name &rest args)
  "Create an entity instance using the best available implementation."
  (let ((impl (resolve-entity-implementation entity-type)))
    (unless impl
      (format t "~&No implementation found for entity type: ~A~%" entity-type)
      (return-from entity/create nil))

    (unless *silent*
      (format t "~&Using implementation: ~A (priority: ~A)~%"
              (entity-implementation-name impl)
              (entity-implementation-priority impl)))

    (entity/load-skills-for-operation impl :create)

    (let ((instance (apply #'create-entity-instance entity-type
                           :name name args)))
      (entity/activate-behaviors instance :create)

      (multiple-value-bind (ok-p errors) (entity/validate instance)
        (unless ok-p
          (format t "~&Validation errors:~%")
          (dolist (err errors) (format t "  - ~A~%" err))))

      (when (entity-implementation-create-fn impl)
        (apply (entity-implementation-create-fn impl) name args))

      (entity/run-after-hooks instance :create)

      instance)))

(defun entity/list (entity-type &rest args)
  "List entities using the best available implementation."
  (let ((impl (resolve-entity-implementation entity-type)))
    (if (and impl (entity-implementation-list-fn impl))
        (apply (entity-implementation-list-fn impl) args)
        (find-entity-instances entity-type))))

(defun entity/update (entity-type name &rest args)
  "Update an entity using the best available implementation."
  (let ((impl (resolve-entity-implementation entity-type)))
    (when (and impl (entity-implementation-update-fn impl))
      (apply (entity-implementation-update-fn impl) name args))))

(defun entity/delete (entity-type name &rest args)
  "Delete an entity using the best available implementation."
  (let ((impl (resolve-entity-implementation entity-type)))
    (when (and impl (entity-implementation-delete-fn impl))
      (apply (entity-implementation-delete-fn impl) name args))))

;;* active entity rules for system prompt

(defun get-active-entity-rules ()
  "Collect all active entity rules into a string for the system prompt."
  (let ((rules '()))
    (maphash (lambda (name rule-text)
               (declare (ignore name))
               (when (and rule-text (stringp rule-text) (> (length rule-text) 0))
                 (push rule-text rules)))
             *active-rules*)
    (if rules
        (format nil "~%## Active Entity Rules~%~{- ~A~%~}" (nreverse rules))
        "")))
