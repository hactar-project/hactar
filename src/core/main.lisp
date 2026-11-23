(in-package #:hactar-rebuild)

(defun main ()
  "Executable entry point: ensure repo context, bootstrap if needed, dispatch."
  (let* ((argv (uiop:command-line-arguments))
         (root (find-repo-root))
         (path (hactar-file-path root)))
    (setf *repo-root* root)
    (if (probe-file path)
        (load-hactar-file path)   ; already present: load it
        (bootstrap root))          ; first run: bootstrap already loads it
    (let ((result (dispatch argv)))
      (format t "~A" (render-result result :text))
      (when *auto-update*
        (run-hook 'turn.after argv result))
      (uiop:quit (if (result-ok-p result) 0 1)))))
