;;* BOT Entity Storage Backends
(in-package :hactar)

;;** Memory Storage (Default)

(defun entity-storage/memory-store (instance)
  "Store an entity instance in memory."
  (let ((key (cons (entity-instance-type instance) (entity-instance-id instance))))
    (setf (gethash key *entity-instances*) instance)
    instance))

(defun entity-storage/memory-find (type &key id name tags)
  "Find entity instances in memory."
  (let ((type-str (if (symbolp type) (string-downcase (symbol-name type)) type))
        (results '()))
    (maphash (lambda (key instance)
               (when (string= (car key) type-str)
                 (let ((match t))
                   (when (and id (not (string= (entity-instance-id instance) id)))
                     (setf match nil))
                   (when (and name (not (string-equal (entity-get instance :name) name)))
                     (setf match nil))
                   (when (and tags (not (every (lambda (tag)
                                                 (member tag (entity-instance-tags instance)
                                                         :test #'string-equal))
                                               tags)))
                     (setf match nil))
                   (when match (push instance results)))))
             *entity-instances*)
    (nreverse results)))

(defun entity-storage/memory-delete (type id)
  "Delete an entity from memory storage."
  (let ((type-str (if (symbolp type) (string-downcase (symbol-name type)) type)))
    (remhash (cons type-str id) *entity-instances*)))

(defun entity-storage/memory-clear (type)
  "Clear all entities of a type from memory."
  (let ((type-str (if (symbolp type) (string-downcase (symbol-name type)) type))
        (keys-to-remove '()))
    (maphash (lambda (key instance)
               (declare (ignore instance))
               (when (string= (car key) type-str)
                 (push key keys-to-remove)))
             *entity-instances*)
    (dolist (key keys-to-remove)
      (remhash key *entity-instances*))))

;;** Entity Serialization

(defun entity-instance-to-alist (instance)
  "Serialize an entity instance to an alist."
  (let ((props '()))
    (maphash (lambda (k v) (push (cons k v) props))
             (entity-instance-properties instance))
    `((:id . ,(entity-instance-id instance))
      (:type . ,(entity-instance-type instance))
      (:tags . ,(entity-instance-tags instance))
      (:properties . ,(nreverse props))
      (:created-at . ,(entity-instance-created-at instance))
      (:updated-at . ,(entity-instance-updated-at instance)))))

(defun alist-to-entity-instance (alist)
  "Deserialize an entity instance from an alist."
  (let ((instance (make-entity-instance
                   :id (cdr (assoc :id alist))
                   :type (cdr (assoc :type alist))
                   :tags (cdr (assoc :tags alist))
                   :created-at (cdr (assoc :created-at alist))
                   :updated-at (cdr (assoc :updated-at alist)))))
    (dolist (prop (cdr (assoc :properties alist)))
      (setf (gethash (car prop) (entity-instance-properties instance))
            (cdr prop)))
    instance))
