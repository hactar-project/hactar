(in-package :hactar-tests)

;;* BOT (Behavior-Object-Tags) Tests

(def-suite bot-tests
  :description "Tests for the BOT Entity-Component-System architecture.")

(in-suite bot-tests)

;;* Skill Tests

(test defskill-registers-skill
  "Test that defskill correctly registers a skill."
  (clrhash hactar::*skill-registry*)
  (eval '(hactar::defskill :test-skill
           "A test skill"
           :instructions "Do the thing"
           :rules "Follow the rules"))
  (let ((skill (hactar::get-skill :test-skill)))
    (is-true skill)
    (is (string= (hactar::bot-skill-description skill) "A test skill"))
    (is (string= (hactar::bot-skill-instructions skill) "Do the thing"))
    (is (string= (hactar::bot-skill-rules skill) "Follow the rules"))
    (is (not (hactar::bot-skill-prefixed-p skill)))))

(test defskill-prefixed-skill
  "Test that prefixed skills are correctly parsed."
  (clrhash hactar::*skill-registry*)
  (eval '(hactar::defskill :create/react-route-skill
           "Create-specific skill"
           :instructions "When creating a route..."))
  (let ((skill (hactar::get-skill :create/react-route-skill)))
    (is-true skill)
    (is-true (hactar::bot-skill-prefixed-p skill))
    (is (eq (hactar::bot-skill-prefix skill) :create))
    (is (eq (hactar::bot-skill-base-name skill) :react-route-skill))))

(test skill-name-helpers
  "Test skill name parsing helpers."
  (is-true (hactar::skill-name-prefixed-p :create/my-skill))
  (is (not (hactar::skill-name-prefixed-p :my-skill)))
  (is (eq (hactar::skill-name-prefix :create/my-skill) :create))
  (is (eq (hactar::skill-name-base :create/my-skill) :my-skill))
  (is (eq (hactar::skill-name-base :my-skill) :my-skill)))

;;* Behavior Tests

(test defbehavior-registers-behavior
  "Test that defbehavior correctly registers a behavior."
  (clrhash hactar::*behavior-registry*)
  (eval '(hactar::defbehavior test-behavior
           "A test behavior"
           :tags (:test-tag :other-tag)
           :skill :test-patterns))
  (let ((behavior (hactar::get-behavior 'test-behavior)))
    (is-true behavior)
    (is (string= (hactar::bot-behavior-description behavior) "A test behavior"))
    (is (equal (hactar::bot-behavior-tags behavior) '(:test-tag :other-tag)))
    (is (eq (hactar::bot-behavior-skill behavior) :test-patterns))))

;;* Tag Tests

(test deftag-registers-tag
  "Test that deftag correctly registers a tag."
  (clrhash hactar::*tag-registry*)
  (eval '(hactar::deftag :api
           "Tag for API endpoints"
           :behaviors (json-response error-handling)
           :inherits (:restful)
           :skills (:api-patterns)))
  (let ((tag (hactar::get-tag :api)))
    (is-true tag)
    (is (string= (hactar::bot-tag-description tag) "Tag for API endpoints"))
    (is (equal (hactar::bot-tag-behaviors tag) '(json-response error-handling)))
    (is (equal (hactar::bot-tag-inherits tag) '(:restful)))
    (is (equal (hactar::bot-tag-skills tag) '(:api-patterns)))))

(test get-tag-behaviors-with-inheritance
  "Test that get-tag-behaviors resolves inherited behaviors."
  (clrhash hactar::*tag-registry*)
  (clrhash hactar::*behavior-registry*)
  ;; Register behaviors
  (hactar::register-behavior 'base-behavior :description "Base" :tags '(:base))
  (hactar::register-behavior 'child-behavior :description "Child" :tags '(:child))
  ;; Register tags with inheritance
  (hactar::register-tag :base :description "Base tag" :behaviors '(base-behavior))
  (hactar::register-tag :child :description "Child tag"
                        :behaviors '(child-behavior) :inherits '(:base))
  (let ((behaviors (hactar::get-tag-behaviors :child)))
    (is (= 2 (length behaviors)))
    (is (find 'child-behavior behaviors :key #'hactar::bot-behavior-name))
    (is (find 'base-behavior behaviors :key #'hactar::bot-behavior-name))))

;;* Entity Instance Tests

(test create-entity-instance-test
  "Test creating entity instances."
  (clrhash hactar::*entity-instances*)
  (let ((instance (hactar::create-entity-instance 'route
                                                  :name "login"
                                                  :path "/login"
                                                  :method :post
                                                  :tags '("api" "public"))))
    (is-true instance)
    (is (string= (hactar::entity-instance-type instance) "route"))
    (is (string= (hactar::entity-get instance :name) "login"))
    (is (string= (hactar::entity-get instance :path) "/login"))
    (is (equal (hactar::entity-instance-tags instance) '("api" "public")))))

(test find-entity-instances-test
  "Test finding entity instances."
  (clrhash hactar::*entity-instances*)
  (hactar::create-entity-instance 'route :name "login" :tags '("api" "public"))
  (hactar::create-entity-instance 'route :name "dashboard" :tags '("api" "protected"))
  (hactar::create-entity-instance 'model :name "user" :tags '("crud"))
  (is (= 2 (length (hactar::find-entity-instances 'route))))
  (is (= 1 (length (hactar::find-entity-instances 'route :tags '("public")))))
  (is (= 2 (length (hactar::find-entity-instances 'route :tags '("api")))))
  (is (= 1 (length (hactar::find-entity-instances 'model)))))

(test delete-entity-instance-test
  "Test deleting entity instances."
  (clrhash hactar::*entity-instances*)
  (let ((instance (hactar::create-entity-instance 'route :name "temp")))
    (is (= 1 (length (hactar::find-entity-instances 'route))))
    (hactar::delete-entity-instance 'route (hactar::entity-instance-id instance))
    (is (= 0 (length (hactar::find-entity-instances 'route))))))

;;* Entity Implementation Tests

(test register-entity-implementation-test
  "Test registering entity implementations."
  (clrhash hactar::*entity-implementations*)
  (let ((impl (hactar::make-entity-implementation
               :name 'test-impl
               :entity-type "route"
               :description "Test implementation"
               :priority 10
               :create-fn (lambda (name &rest args)
                            (declare (ignore args))
                            (format nil "Created ~A" name)))))
    (hactar::register-entity-implementation "route" impl)
    (let ((resolved (hactar::resolve-entity-implementation "route")))
      (is-true resolved)
      (is (eq (hactar::entity-implementation-name resolved) 'test-impl))
      (is (= 10 (hactar::entity-implementation-priority resolved))))))

(test resolve-implementation-priority
  "Test that highest-priority active implementation wins."
  (clrhash hactar::*entity-implementations*)
  (let ((low-impl (hactar::make-entity-implementation
                   :name 'low-priority
                   :entity-type "route"
                   :priority 0))
        (high-impl (hactar::make-entity-implementation
                    :name 'high-priority
                    :entity-type "route"
                    :priority 10)))
    (hactar::register-entity-implementation "route" low-impl)
    (hactar::register-entity-implementation "route" high-impl)
    (let ((resolved (hactar::resolve-entity-implementation "route")))
      (is (eq (hactar::entity-implementation-name resolved) 'high-priority)))))

(test resolve-implementation-when-predicate
  "Test that :when predicate filters implementations."
  (clrhash hactar::*entity-implementations*)
  (let ((always-impl (hactar::make-entity-implementation
                      :name 'always-active
                      :entity-type "route"
                      :priority 0))
        (never-impl (hactar::make-entity-implementation
                     :name 'never-active
                     :entity-type "route"
                     :priority 100
                     :when-fn (lambda () nil))))
    (hactar::register-entity-implementation "route" always-impl)
    (hactar::register-entity-implementation "route" never-impl)
    (let ((resolved (hactar::resolve-entity-implementation "route")))
      (is (eq (hactar::entity-implementation-name resolved) 'always-active)))))

;;* Skill Loading for Operations

(test entity-load-skills-for-operation
  "Test that skills are loaded correctly based on operation type."
  (clrhash hactar::*skill-registry*)
  (clrhash hactar::*active-rules*)
  ;; Register skills
  (hactar::register-skill :global-skill :instructions "Global instructions")
  (hactar::register-skill :create/specific-skill :instructions "Create-only instructions")
  (hactar::register-skill :update/specific-skill :instructions "Update-only instructions")
  ;; Create implementation with these skills
  (let ((impl (hactar::make-entity-implementation
               :name 'test
               :entity-type "route"
               :skills '(:global-skill :create/specific-skill :update/specific-skill))))
    ;; Load for :create
    (clrhash hactar::*active-rules*)
    (hactar::entity/load-skills-for-operation impl :create)
    ;; Global skill should be loaded
    (is-true (gethash :global-skill hactar::*active-rules*))
    ;; Create-specific should be loaded (prefix matches)
    (is-true (gethash :specific-skill hactar::*active-rules*))))

;;* Entity Storage Tests

(test entity-memory-storage-test
  "Test memory storage operations."
  (clrhash hactar::*entity-instances*)
  (let ((instance (hactar::create-entity-instance 'test :name "item1" :tags '("tag1"))))
    (hactar::entity-storage/memory-store instance)
    (let ((found (hactar::entity-storage/memory-find 'test :name "item1")))
      (is (= 1 (length found)))
      (is (string= (hactar::entity-get (first found) :name) "item1")))
    (let ((found (hactar::entity-storage/memory-find 'test :tags '("tag1"))))
      (is (= 1 (length found))))
    (hactar::entity-storage/memory-delete 'test (hactar::entity-instance-id instance))
    (is (= 0 (length (hactar::entity-storage/memory-find 'test))))))

(test entity-serialization-test
  "Test entity serialization to/from alist."
  (clrhash hactar::*entity-instances*)
  (let* ((instance (hactar::create-entity-instance 'route
                                                   :name "test-route"
                                                   :path "/test"
                                                   :tags '("api")))
         (alist (hactar::entity-instance-to-alist instance))
         (restored (hactar::alist-to-entity-instance alist)))
    (is (string= (hactar::entity-instance-id instance)
                 (hactar::entity-instance-id restored)))
    (is (string= (hactar::entity-instance-type instance)
                 (hactar::entity-instance-type restored)))
    (is (equal (hactar::entity-instance-tags instance)
               (hactar::entity-instance-tags restored)))
    (is (string= (hactar::entity-get instance :name)
                 (hactar::entity-get restored :name)))))

;;* Integration Test: Full Entity Lifecycle

(test entity-full-lifecycle
  "Test a complete entity lifecycle: implement, create, list, delete."
  (clrhash hactar::*entity-implementations*)
  (clrhash hactar::*entity-instances*)
  (clrhash hactar::*skill-registry*)
  (clrhash hactar::*active-rules*)

  (hactar::register-skill :test-patterns :instructions "Test patterns")

  (let* ((created-names '())
         (impl (hactar::make-entity-implementation
                :name 'test-route-impl
                :entity-type "route"
                :description "Test route implementation"
                :priority 10
                :skills '(:test-patterns)
                :create-fn (lambda (name &rest args)
                             (declare (ignore args))
                             (push name created-names))
                :list-fn (lambda (&rest args)
                           (declare (ignore args))
                           (hactar::find-entity-instances 'route))
                :delete-fn (lambda (name &rest args)
                             (declare (ignore args))
                             (let ((instances (hactar::find-entity-instances 'route)))
                               (dolist (i instances)
                                 (when (string-equal (hactar::entity-get i :name) name)
                                   (hactar::delete-entity-instance
                                    'route (hactar::entity-instance-id i)))))))))
    (hactar::register-entity-implementation "route" impl)

    (let ((instance (hactar::entity/create 'route "login" :path "/login" :tags '("api"))))
      (is-true instance)
      (is (member "login" created-names :test #'string=)))

    (let ((results (hactar::entity/list 'route)))
      (is (= 1 (length results))))

    (hactar::entity/delete 'route "login")
    (is (= 0 (length (hactar::find-entity-instances 'route))))))

;;* Parse Entity Command Args

(test parse-entity-command-args-test
  "Test parsing entity command arguments."
  (let ((result (hactar::parse-entity-command-args
                 '("--path" "/api/users" "--method" "GET" "--tags" "api,auth"))))
    (is (string= (getf result :path) "/api/users"))
    (is (string= (getf result :method) "GET"))
    (is (equal (getf result :tags) '("api" "auth")))))

;;* Active Rules Collection

(test get-active-entity-rules-test
  "Test collecting active entity rules."
  (clrhash hactar::*active-rules*)
  (is (string= (hactar::get-active-entity-rules) ""))
  (setf (gethash :test-rule hactar::*active-rules*) "Always test your code")
  (let ((result (hactar::get-active-entity-rules)))
    (is (search "Active Entity Rules" result))
    (is (search "Always test your code" result))))
