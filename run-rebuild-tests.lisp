;; Standalone runner for the greenfield rebuild + split-library test suites
;; (network-free).
(require :asdf)
(let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file q) (load q)))
(dolist (sys '("hactar-llm" "hactar-org" "hactar-md" "hactar-fzf" "hactar-fern"
               "hactar-rebuild" "hactar-rebuild-tests"))
  (asdf:load-asd (truename (format nil "~A.asd" sys))))
(ql:quickload :hactar-rebuild-tests :silent t)
(format t "~&--- Running Hactar Rebuild + Library Tests ---~%")
(let ((ok (uiop:symbol-call :hactar-rebuild-tests :run-tests)))
  (format t "~&--- Rebuild Tests ~:[FAILED~;PASSED~] ---~%" ok)
  (uiop:quit (if ok 0 1)))
