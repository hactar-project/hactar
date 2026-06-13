;;; Run tests with sb-cover instrumentation and generate an HTML coverage report.
(require :sb-cover)
(declaim (optimize (sb-cover:store-coverage-data 3)))
(asdf:clear-output-translations)

(asdf:load-system :llm :force :all)
(asdf:load-system :hactar :force :all)
(asdf:load-system :hactar-tests :force :all)
(asdf:load-system :llm-tests :force :all)

(format t "~&--- Running tests with coverage instrumentation ---~%")
(hactar-tests:run-tests)

(let ((output-dir #p"coverage/"))
  (ensure-directories-exist output-dir)
  (format t "~&--- Generating coverage report in ~A ---~%" (namestring output-dir))
  (sb-cover:report output-dir)
  (format t "~&--- Coverage report written to ~A ---~%" (namestring output-dir)))

(sb-cover:reset-coverage)
(declaim (optimize (sb-cover:store-coverage-data 0)))

(format t "~&--- Coverage complete ---~%")
