(asdf:defsystem #:hactar-migrations
  :description "Database migration runner for hactar."
  :author "K-2052 <k@2052.me>"
  :license "MIT"
  :depends-on (#:uiop #:asdf #:sqlite #:cl-ppcre #:cffi)
  :components ((:module "migrations"
                :components ((:file "run-migrations")))))
