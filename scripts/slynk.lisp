(in-package :hactar)
(defun start-slynk ()
  "Start the Slynk server if not already running."
  (if *slynk-started*
      (progn
        (unless *silent*
          (if *lisp-rpc-mode*
              (rpc-log :info "Slynk already running" :port *slynk-port*)
              (format t "Slynk server already running on port ~A.~%" *slynk-port*)))
        *slynk-port*)
      (let ((actual-slynk-port (find-free-port *slynk-port*)))
        (when (and (/= actual-slynk-port *slynk-port*) (not *silent*))
          (if *lisp-rpc-mode*
              (rpc-log :warning "Slynk port unavailable, falling back"
                       :requested *slynk-port* :using actual-slynk-port)
              (format t "~&Warning: Slynk port ~A unavailable, falling back to port ~A.~%"
                      *slynk-port* actual-slynk-port)))
        (handler-case
            (with-suppressed-output-if-rpc
		(unless *silent*
                  (format t "~&Starting Slynk server on port ~A...~%" actual-slynk-port))
              (slynk:create-server :port actual-slynk-port :dont-close t))
          (error (e)
            (if *lisp-rpc-mode*
                (rpc-error (format nil "Failed to start Slynk: ~A" e))
                (progn
                  (format *error-output* "~&Error starting Slynk server: ~A~%" e)
                  (format *error-output* "~&Ensure Slynk is installed and port ~A is free.~%" actual-slynk-port)))
            (return-from start-slynk nil)))
        (write-slynk-port-file actual-slynk-port)
        (setf *slynk-port* actual-slynk-port)
        (setf *slynk-started* t)
        (when *lisp-rpc-mode*
          (rpc-log :info "Slynk started" :port *slynk-port*))
        actual-slynk-port)))
;;* commands
(define-command slynk (args)
  "Start the Slynk server manually (if not already running)."
  (declare (ignore args))
  (let ((port (start-slynk)))
    (when port
      (format t "Slynk server running on port ~A.~%" port)))
  :json (lambda (args)
          (declare (ignore args))
          (let ((port (start-slynk)))
            (to-json `(("started" . ,(if port t :false))
                       ("port" . ,(or port *slynk-port* :null))))))
  :yaml (lambda (args)
          (declare (ignore args))
          (let ((port (start-slynk)))
            (with-output-to-string (s)
              (format s "started: ~A~%port: ~A~%"
                      (if port "true" "false")
                      (or port *slynk-port* "null")))))
  :acp (lambda (cmd-args)
         (declare (ignore cmd-args))
         (let ((port (start-slynk)))
           (if port
               `(("text" . ,(format nil "Slynk server running on port ~A." port))
                 ("data" . (("port" . ,port))))
               `(("text" . "Failed to start Slynk server."))))))

(define-command slynk.stop (args)
  "Stop the Slynk server if it is running."
  (declare (ignore args))
  (if *slynk-started*
      (progn
        (handler-case (slynk:stop-server *slynk-port*)
          (error (e) (format t "Error stopping Slynk server: ~A~%" e)))
        (setf *slynk-started* nil)
        (delete-slynk-port-file)
        (format t "Slynk server stopped.~%"))
      (format t "Slynk server is not running.~%"))
  :acp (lambda (cmd-args)
         (declare (ignore cmd-args))
         (if *slynk-started*
             (progn
               (ignore-errors (slynk:stop-server *slynk-port*))
               (setf *slynk-started* nil)
               (delete-slynk-port-file)
               `(("text" . "Slynk server stopped.")))
             `(("text" . "Slynk server is not running.")))))
