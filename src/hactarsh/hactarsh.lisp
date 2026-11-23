(in-package #:hactarsh)

(defun eval-shell-line (line)
  "Dispatch a single shell LINE through HACTAR-CORE:DISPATCH."
  (let* ((parts (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
                        :test #'string=))
         (result (hactar-core:dispatch parts)))
    (hactar-core:render-result result :text)))

(defun start-shell (&key (input *standard-input*) (output *standard-output*)
                         (use-readline nil))
  "Start the Hactar interactive shell over HACTAR-TUI and HACTAR-CORE."
  (let ((hactar-tui:*use-readline* use-readline))
    (hactar-tui:with-tui (:input input :output output)
      (hactar-tui:repl-loop #'eval-shell-line :prompt "hactarsh> "))))
