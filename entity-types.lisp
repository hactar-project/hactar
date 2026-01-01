;;* BOT Core Entity Type Definitions
(in-package :hactar)

;;** Route Entity

(defentity frameworkroute
  "A route represents an HTTP endpoint or page route."
  :schema
  ((:path :type string :required t)
   (:method :type keyword :default :get)
   (:handler :type string)
   (:middleware :type list :default nil)
   (:tags :type list :default nil)
   (:params :type list)
   (:query :type list)
   (:body :type (or null symbol)))
  :behaviors (:add-skills :generate-handler :register-route)
  :commands t
  :storage :memory
  :searchable (:path :method :tags))

;;** Model Entity

(defentity model
  "A data model or schema definition."
  :schema
  ((:name :type string :required t)
   (:fields :type list :required t)
   (:relations :type list)
   (:tags :type list :default nil)
   (:table-name :type string)
   (:timestamps :type boolean :default t))
  :behaviors (:add-skills :generate-schema :run-migrations)
  :commands t
  :storage :memory
  :searchable (:name :tags))

;;** Component Entity

(defentity component
  "A UI component definition."
  :schema
  ((:name :type string :required t)
   (:props :type list)
   (:style :type keyword :default :css)
   (:tags :type list :default nil)
   (:path :type string))
  :behaviors (:add-skills :generate-component :add-exports)
  :commands t
  :storage :memory
  :searchable (:name :tags))

;;** Lib Entity

(defentity lib
  "A shared library or utility module."
  :schema
  ((:name :type string :required t)
   (:exports :type list)
   (:imports :type list)
   (:tags :type list :default nil)
   (:path :type string))
  :behaviors (:add-skills :generate-module)
  :commands t
  :storage :memory
  :searchable (:name :exports :tags))

;;** Built-in Tags

(deftag :api
  "Tag for API endpoints"
  :behaviors ()
  :skills (:api-patterns))

(deftag :crud
  "Tag for full CRUD entities"
  :behaviors ()
  :requires (:model))

(deftag :protected
  "Tag for routes requiring authentication"
  :behaviors ()
  :skills (:auth-patterns))

(deftag :public
  "Tag for publicly accessible routes"
  :behaviors ())

(deftag :authenticated
  "Tag for routes requiring authentication"
  :behaviors ()
  :inherits (:protected))

;;** Built-in Behaviors

(defbehavior add-skills
  "Loads entity skills into context before operations."
  :before-create (lambda (entity)
                   (declare (ignore entity))
                   nil))

(defbehavior generate-handler
  "Generates handler code for routes."
  :tags (:route-handler))

(defbehavior generate-schema
  "Generates schema code for models."
  :tags (:model-schema))

(defbehavior generate-component
  "Generates component code."
  :tags (:ui-component))

(defbehavior generate-module
  "Generates module code for libs."
  :tags (:lib-module))

(defbehavior register-route
  "Registers a route in the router configuration."
  :tags (:route-registration))

(defbehavior run-migrations
  "Runs database migrations for models."
  :tags (:database-migration))

(defbehavior add-exports
  "Adds exports to module files."
  :tags (:module-exports))
