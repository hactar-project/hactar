;;* Rails mode
(in-package :hactar)

(defmode rails
  "Ruby on Rails conventions: model and route scaffolding."
  :when (member "rails" *stack* :test #'string-equal)
  :rules "Follow Rails conventions: models live in app/models as PascalCase
classes subclassing ApplicationRecord; RESTful routes go in config/routes.rb;
file names are snake_case."
  :commands
  ((defmode-command rails "model.create" (args)
     "Create a Rails model. Usage: /rails.model.create <Name>"
     (if (null args)
         (format t "Usage: /rails.model.create <Name>~%")
         (let* ((name (first args))
                (class-name (pascal-case name))
                (file-name (format nil "app/models/~A.rb"
                                   (string-downcase (kebab-case name))))
                (content (format nil "class ~A < ApplicationRecord~%end~%"
                                 class-name)))
           (scaffold-create-file file-name content))))

   (defmode-command rails "route.create" (args)
     "Add a RESTful resource route. Usage: /rails.route.create <resource>"
     (if (null args)
         (format t "Usage: /rails.route.create <resource>~%")
         (let ((resource (string-downcase (first args))))
           (scaffold-modify-file
            "config/routes.rb"
            "Rails.application.routes.draw do"
            (format nil "Rails.application.routes.draw do~%  resources :~A"
                    resource)))))))
