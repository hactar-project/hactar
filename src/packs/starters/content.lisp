(in-package :hactar-user)

(defstarter react-router-v7
  :title "React Router v7 Full-Stack Starter"
  :stack ("typescript" "react" "react-router" "vite")
  :language "typescript"
  :description "Typed React Router v7 application with loaders, actions, and Vitest.")

(defstarter rails-api
  :title "Rails JSON API Starter"
  :stack ("ruby" "rails")
  :language "ruby"
  :description "Ruby on Rails API starter with RSpec and Rubocop.")

(defstarter common-lisp-lib
  :title "Common Lisp Split Library Starter"
  :stack ("common-lisp" "asdf")
  :language "lisp"
  :description "Minimal ASDF library with FiveAM test suite and clean package exports.")

(defprompt starter-create (name stack)
  "Scaffold a new {{stack}} project named {{name}} using minimal, idiomatic files.")
