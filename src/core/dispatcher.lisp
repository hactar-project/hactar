(in-package #:hactar-rebuild)

;;; Turn CLI argv into an action/agent/tool/command invocation.

(defun args->plist (args)
  "Turn --flag=value / --flag value / positionals into a plist.
Positionals accumulate under :args as a space-joined string."
  (let ((plist '()) (positionals '()))
    (loop with rest = args
          while rest
          for arg = (pop rest)
          do (cond
               ((and (>= (length arg) 2) (string= (subseq arg 0 2) "--"))
                (let* ((body (subseq arg 2))
                       (eq-pos (position #\= body)))
                  (if eq-pos
                      (setf (getf plist (intern (string-upcase (subseq body 0 eq-pos))
                                                :keyword))
                            (subseq body (1+ eq-pos)))
                      (setf (getf plist (intern (string-upcase body) :keyword))
                            (if (and rest (not (str:starts-with-p "--" (first rest))))
                                (pop rest) t)))))
               (t (push arg positionals))))
    (when positionals
      (setf (getf plist :args)
            (format nil "~{~A~^ ~}" (nreverse positionals))))
    plist))

(defun dispatch (argv)
  "Resolve ARGV to an action, agent, tool, or command and run it."
  (let* ((verb (first argv))
         (rest (rest argv))
         (key (and verb (string-downcase verb))))
    (cond
      ((null verb) (ok "No command given. Try `hactar dev \"...\"`."))
      ((gethash key *actions*) (run-action key (args->plist rest)))
      ((gethash key *agents*) (run-agent key (format nil "~{~A~^ ~}" rest)))
      ((gethash key *tools*) (apply #'run-tool key rest))
      ((gethash key *commands*) (run-command key rest))
      ((parse-route (format nil "~{~A~^ ~}" argv))
       (let ((route (parse-route (format nil "~{~A~^ ~}" argv))))
         (ok (format nil "route: ~S" route) :data route)))
      (t (err (format nil "Unknown verb: ~A" verb))))))
