;;; Run tests with sb-cover instrumentation and generate an HTML coverage report.
(require :sb-cover)

;; Enable coverage instrumentation for all code compiled after this point
(declaim (optimize (sb-cover:store-coverage-data 3)))

;; Clear ASDF output cache so all systems are recompiled with instrumentation
(asdf:clear-output-translations)

;; Load systems with forced recompilation so they get instrumented
(asdf:load-system :hactar-migrations :force :all)
(asdf:load-system :llm :force :all)
(asdf:load-system :hactar :force :all)
(asdf:load-system :hactar-tests :force :all)
(asdf:load-system :llm-tests :force :all)

;; Run migrations for the test DB
(hactar-migrations:run-migrations :test t)

;; Run tests
(format t "~&--- Running tests with coverage instrumentation ---~%")
(hactar-tests:run-tests)

;; Generate HTML coverage report
(let ((output-dir #p"coverage/"))
  (ensure-directories-exist output-dir)
  (format t "~&--- Generating coverage report in ~A ---~%" (namestring output-dir))
  (sb-cover:report output-dir)
  (format t "~&--- Coverage report written to ~A ---~%" (namestring output-dir)))

;; Reset coverage data and disable instrumentation
(sb-cover:reset-coverage)
(declaim (optimize (sb-cover:store-coverage-data 0)))

(format t "~&--- Coverage complete ---~%")
