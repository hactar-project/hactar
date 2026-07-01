;; Build the greenfield rebuild engine binary to ./hactar/build/hactar
(require :asdf)
(let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file q) (load q)))
(asdf:load-asd (truename "hactar-rebuild.asd"))
(ql:quickload :hactar-rebuild :silent t)
(asdf:make :hactar-rebuild)
(format t "~&Built ./hactar/build/hactar~%")
