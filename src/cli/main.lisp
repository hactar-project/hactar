(in-package #:hactar)

(defun select-interface (argv)
  "Select interface mode and return (values mode remaining-argv).
MODE is one of :PLAIN, :ACP, :REPL, or :HEADLESS."
  (let ((mode nil)
        (rest nil))
    (dolist (arg argv)
      (cond
        ((or (string= arg "--plain") (string= arg "--in-editor"))
         (setf mode :plain))
        ((string= arg "--acp")
         (unless (eq mode :plain)
           (setf mode :acp)))
        ((string= arg "--repl")
         (unless (member mode '(:plain :acp))
           (setf mode :repl)))
        (t (push arg rest))))
    (values (or mode :headless)
            (nreverse rest))))

(defun %ensure-repo-loaded ()
  (let* ((root (hactar-core:find-repo-root))
         (path (hactar-core:hactar-file-path root)))
    (setf hactar-core:*repo-root* root)
    (if (probe-file path)
        (hactar-core:load-hactar-file path)
        (hactar-core:bootstrap root))
    root))

(defun run-cli (argv &key (input *standard-input*) (output *standard-output*))
  "Compose hactar-core + hactar-tui according to ARGV flags without calling UIOP:QUIT."
  (multiple-value-bind (mode args) (select-interface argv)
    (ecase mode
      (:acp
       (unless (find-package :hactar-acp)
         (asdf:load-system :hactar-acp))
       (uiop:symbol-call :hactar-acp :serve-acp :input input :output output)
       0)
      (:repl
       (hactar-tui:with-tui (:input input :output output)
         (hactar-tui:repl-loop
          (lambda (line)
            (let* ((parts (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
                                  :test #'string=))
                   (result (hactar-core:dispatch parts)))
              (hactar-core:render-result result :text)))
          :prompt "hactar> "))
       0)
      ((:plain :headless)
       (let ((result (hactar-core:dispatch args)))
         (write-string (hactar-core:render-result result :text) output)
         (when hactar-core:*auto-update*
           (hactar-core:run-hook 'hactar-core::turn.after args result))
         (if (hactar-core:result-ok-p result) 0 1))))))

(defun main ()
  "Thin Tier 5 executable entry point: bootstrap repo and run CLI."
  (%ensure-repo-loaded)
  (uiop:quit (run-cli (uiop:command-line-arguments))))
