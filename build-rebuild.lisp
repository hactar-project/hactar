;; Build the greenfield rebuild engine binary to ./hactar/build/hactar
(require :asdf)
(let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file q) (load q)))
(dolist (sys '("hactar-llm" "hactar-org" "hactar-md" "hactar-fzf" "hactar-fern"
               "hactar-rebuild"))
  (asdf:load-asd (truename (format nil "~A.asd" sys))))
(ql:quickload :hactar-rebuild :silent t)
(asdf:make :hactar-rebuild)
(format t "~&Built ./hactar/build/hactar~%")
