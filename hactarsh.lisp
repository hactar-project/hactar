;;  experimental shell implementation for hactar
;; placeholder for now
(defpackage #:hactarsh
  (:use #:cl)
  (:export #:start-shell))

(in-package #:hactarsh)

(defun start-shell ()
  "Starts a simple readline-based shell."
  (rl:initialize)
  (unwind-protect
       (loop
         (let ((input (rl:readline :prompt "hactarsh> " :add-history t)))
           (unless input
             (format t "~&Exiting.~%")
             (return))
           (let ((trimmed-input (string-trim '(#\Space #\Tab) input)))
             (when (string-equal trimmed-input "exit")
               (format t "~&Exiting.~%")
               (return))
             (format t "You entered: ~A~%" input))))
    ;; (rl:deactivate-readline)
    ))
