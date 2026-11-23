(in-package :hactar-user)

(defmode rails
  :priority 20
  :stack ("ruby" "rails")
  :detect (:any (:file-contains ("Gemfile" "rails"))
                (:file-exists "config/application.rb"))
  :stack-commands (:test "bundle exec rspec" :lint "bundle exec rubocop")
  :forms ((deftool test (&optional file)
            "Run RSpec tests."
            (run-test-cmd file))
          (deftool lint ()
            "Run Rubocop."
            (run-lint-cmd))
          (deftool rails-routes ()
            "List Rails routes."
            (shell-cmd "bin/rails routes" :sandbox "project"))
          (deftool rails-db-migrate ()
            "Run pending database migrations."
            (shell-cmd "bin/rails db:migrate" :sandbox "project"))
          (defskill rails :doc "Rails project conventions.")
          (defsandbox project :allow ("bin/rails" "bundle" "rspec" "git"))
          (defaction migration.create
            :doc "Create a database migration."
            :prompt "Create a Rails migration named {{name}}." :apply :create)
          (defaction model.create
            :doc "Create a model." :prompt "Create a Rails model {{name}}."
            :apply :create)
          (defaction controller.create
            :doc "Create a controller." :prompt "Create a Rails controller {{name}}."
            :apply :create)
          (defagent dev
            :model "anthropic/claude-sonnet-4-6"
            :tools (test lint rails-routes rails-db-migrate)
            :skills (rails) :sandbox project
            :instructions "Build Rails features end-to-end. Use (test) to verify.")))

(defmode sinatra
  :priority 25
  :stack ("ruby" "sinatra")
  :detect (:file-contains ("Gemfile" "sinatra"))
  :stack-commands (:test "bundle exec rspec")
  :forms ((deftool test (&optional file)
            "Run Sinatra RSpec tests."
            (run-test-cmd file))
          (defskill sinatra :doc "Sinatra lightweight web app conventions.")
          (defsandbox project :allow ("bundle" "ruby" "rspec" "git"))
          (defagent dev
            :model "anthropic/claude-sonnet-4-6"
            :tools (test) :skills (sinatra) :sandbox project
            :instructions "Build Sinatra endpoints cleanly and verify with (test).")))
