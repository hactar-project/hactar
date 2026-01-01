;; watchers attach to filesystem events and do things in response
;; usually they are just detecting a type of file e.g a react componenent and then passing it off via hooks to modes
(in-package :hactar)
;;; FS Watcher implementation adapted from cl-fs-watcher

(defclass fs-watcher ()
  ((dir :reader dir
        :initarg :dir
        :initform (error "specify a directory!")
        :type pathname
        :documentation "Main or Root Directory which will be watched,
        all its subdirectories will be watched too.")
   (skip-duplicated :reader skip-duplicated
                    :initarg :skip-duplicated
                    :initform t
                    :type boolean
                    :documentation "Flag to skip duplicated events.")
   (queue :type lparallel.queue:queue
          :initform (lparallel.queue:make-queue)
          :documentation "Queue which is used to call functions from
          any thread inside the event-loop thread.")
   (queue-notifier :type (or null cl-async:notifier)
                   :initform nil
                   :documentation "Notifier which is fired when
                   functions are pushed to the QUEUE.")
   (thread :reader thread
           :type (or null bt:thread)
           :initform nil
           :documentation "BT:THREAD which will run the event-loop.")
   (event-queue :reader event-queue
                :type lparallel.queue:queue
                :initform (lparallel.queue:make-queue)
                :documentation "Queue which holds all events in order.")
   (event-loop-busy-p :type boolean
                      :initform nil
                      :documentation "Boolean (t or nil) which is set
                      to t when the event-loop is currently processing
                      a event.")
   (hook-busy-p :type boolean
                :initform nil
                :documentation "Boolean (t or nil) which is set to t
                when hook-thread is currently processing a hook.")
   (hook-thread :reader hook-thread
                :initform nil
                :type (or null bt:thread)
                :documentation "BT:THREAD which consumes events from
                the event-queue.")
   (hook :accessor hook
         :initarg :hook
         :initform nil
         :type (or null function)
         :documentation "The function which gets called if a event
         occures.")
   (directory-handles :reader directory-handles
                      :initform (make-hash-table :test 'equal)
                      :type hash-table
                      :documentation "Hash-table of all watched
                      directories.")
   (alive-p :reader alive-p
            :initform nil
            :type boolean
            :documentation "To check if watcher is alive and
            running.")
   (error-cb :reader error-cb
             :initarg :error-cb
             :initform nil
             :type (or null function)
             :documentation "Callback which gets called when a error
             is thrown.")))

;; macro which can be used to run code inside the event-loop
(defmacro in-event-loop ((watcher) &body body)
  `(progn
     (lparallel.queue:push-queue (lambda () ,@body)
                                 (slot-value ,watcher 'queue))
     (as:trigger-notifier (slot-value ,watcher 'queue-notifier))))

(defun escape-wildcards (thing &optional escape-char)
  "Got the inspiration for that code from
  sb-impl::unparse-physical-piece, credits go to Xach for finding it.
  Thanks again for the helping me out"
  (if (not (wild-pathname-p thing))
      thing
      (progn
        (unless escape-char
          (setf escape-char
                #-os-windows #\\
                #+os-windows #\^))
        (let* ((srclen (length thing))
               (dstlen srclen))
          (dotimes (i srclen)
            (let ((char (char thing i)))
              (case char
                ((#\* #\? #\[ #+os-windows #\~)
                 (incf dstlen))
                (t (when (char= char escape-char)
                     (incf dstlen))))))
          (let ((result (make-string dstlen))
                (dst 0))
            (dotimes (src srclen)
              (let ((char (char thing src)))
                (case char
                  ((#\* #\? #\[ #+os-windows #\~)
                   (setf (char result dst) escape-char)
                   (incf dst))
                  (t (when (char= char escape-char)
                       (setf (char result dst) escape-char)
                       (incf dst))))
                (setf (char result dst) char)
                (incf dst)))
            result)))))

(defun escaped-directory-exists-p (directory)
  (uiop:directory-exists-p
   (etypecase directory
     (pathname directory)
     (string (escape-wildcards directory)))))

(defun escaped-file-exists-p (file)
  (uiop:file-exists-p
   (etypecase file
     (pathname file)
     (string (escape-wildcards file)))))

(defun escaped-directory-files (directory &rest args)
  (apply #'uiop:directory-files
         (etypecase directory
           (pathname directory)
           (string (escape-wildcards directory)))
         args))

(defun escaped-subdirectories (directory)
  (uiop:subdirectories
   (etypecase directory
     (pathname directory)
     (string
      (escape-wildcards directory)))))

(defun get-event-type (pathname renamed-p changed-p)
  "Will determine the Event-Type by using ESCAPED-DIRECTORY-EXISTS-P
   and ESCAPED-FILE-EXISTS-P."
  (let ((file-exists-p (escaped-file-exists-p pathname))
        (directory-exists-p (escaped-directory-exists-p pathname)))
    (cond ((and renamed-p
                (not changed-p)
                file-exists-p
                (not directory-exists-p))
           :file-added)
          ((and renamed-p
                (not changed-p)
                (not file-exists-p)
                (not directory-exists-p))
           :file-removed)
          ((and (not renamed-p)
                changed-p
                file-exists-p
                (not directory-exists-p))
           :file-changed)
          ((and renamed-p
                (not changed-p)
                (not file-exists-p)
                directory-exists-p)
           :directory-added)
          ((and renamed-p
                changed-p
                (not file-exists-p)
                directory-exists-p)
           nil)
          ((and (not renamed-p)
                changed-p
                (not file-exists-p)
                (not directory-exists-p))
           nil)
          #+os-windows
          ((and (not renamed-p)
                changed-p
                (not file-exists-p)
                directory-exists-p)
           nil)
          (t
           (error (format nil
                          "Could not determine event type in GET-EVENT-TYPE, file: ~a~%~
                          (file-exists-p: ~a, directory-exists-p: ~a, renamed-p: ~a, changed-p: ~a)"
                          pathname
                          file-exists-p directory-exists-p renamed-p changed-p))))))

(defun handle-sub-directories (watcher handle pathname)
  (dolist (sub-file (ignore-errors
                      (mapcar #'uiop:native-namestring
                              (escaped-directory-files pathname))))
    (handler-case
        (watcher-callback watcher handle (subseq sub-file
                                         (length
                                          (uiop:native-namestring
                                           (get-handle-path handle))))
                  t
                  nil)
      (error (e)
        (debug-log "Skipping problematic sub-file:" sub-file e))))
  (dolist (sub-dir (ignore-errors
                     (mapcar #'uiop:native-namestring
                             (escaped-subdirectories pathname))))
    (handler-case
        (add-dir watcher (pathname (escape-wildcards sub-dir)))
      (error (e)
        (debug-log "Skipping problematic sub-directory:" sub-dir e)))))

(defun add-dir (watcher pathname)
  "adds the specified pathname to watcher, this function has to be called
   inside the event-loop!"
  ;; Skip directories with problematic wildcard characters in their names
  (let ((path-string (ignore-errors (namestring pathname))))
    (when (and path-string
               (or (find #\[ path-string)
                   (find #\] path-string)))
      (return-from add-dir nil)))
  (let ((table (directory-handles watcher)))
    (multiple-value-bind (value present-p) (gethash pathname table)
      (declare (ignore value))
      (unless present-p
        (let ((handle #-os-windows (as:fs-watch (uiop:native-namestring pathname)
                                                (lambda (&rest args)
                                                  (apply #'watcher-callback watcher args)))
                      #+os-windows (when (equal (dir watcher) pathname)
                                     (as:fs-watch (uiop:native-namestring pathname)
                                                  (lambda (&rest args)
                                                    (apply #'watcher-callback watcher args))))))
          (setf (gethash pathname table) handle)
          (handle-sub-directories watcher
                                  #-os-windows handle
                                  #+os-windows (gethash (dir watcher) table)
                                  pathname))))))

(defun add-directory-to-watch (watcher pathname)
  "adds dir to watcher, can be safetly called by any thread."
  (if (eql (bt:current-thread) (thread watcher))
      (add-dir watcher pathname)
      (in-event-loop (watcher)
        (add-dir watcher pathname))))

(defun remove-directory-from-watch (watcher pathname &optional (remove-subdirectories t))
  "removes dir from watcher, can be safetly called by any thread."
  (let* ((table (directory-handles watcher)))
    (flet ((remove-entry (entry)
             (let ((handle (gethash pathname table)))
               (remhash entry table)
               (when handle
                 (if (eql (bt:current-thread)
                          (thread watcher))
                     (as:fs-unwatch handle)
                     (in-event-loop (watcher)
                       (as:fs-unwatch handle)))))))
      (remove-entry pathname)
      (when remove-subdirectories
        (map nil #'remove-entry
             (loop :for file-pathname :being :the :hash-keys :of table
                   :when (uiop:subpathp file-pathname pathname)
                   :collect file-pathname))))))

(defun get-handle-path (handle)
  "gets the path (string) of the given cl-async fs-handle, returns a
  pathname object."
  (let ((buffer (cffi:foreign-alloc :char
                                    :initial-element 0
                                    :count 2048))
        (size (cffi:foreign-alloc :uint
                                  :initial-element 2048))
        (result nil))
    ;;(setf (cffi:mem-ref size :uint) 2048)
    (uv:uv-fs-event-getpath (as::fs-monitor-c handle)
                            buffer
                            size)
    (setf result (cffi:foreign-string-to-lisp buffer))
    (cffi:foreign-free buffer)
    (cffi:foreign-free size)
    (uiop:ensure-absolute-pathname
     (uiop:ensure-directory-pathname
      (escape-wildcards result)))))

(defgeneric watcher-callback (watcher handle namestring renamed-p changed-p)
  (:documentation "the main callback which gets called if a Event
  occures."))

(defmethod watcher-callback :around ((watcher fs-watcher) handle namestring renamed-p changed-p)
  (with-slots (event-loop-busy-p) watcher
    (setf event-loop-busy-p t)
    (unwind-protect
         (call-next-method)
      (setf event-loop-busy-p nil))))

(defmethod watcher-callback ((watcher fs-watcher) handle namestring renamed-p changed-p)
  ;; Skip paths with problematic characters that can't be parsed as pathnames
  (when (or (find #\[ namestring)
            (find #\] namestring)
            (find #\* namestring)
            (find #\? namestring))
    (return-from watcher-callback nil))
  (let ((event-type nil)
        (full-pathname (handler-case
                           (merge-pathnames (pathname (escape-wildcards namestring))
                                            (get-handle-path handle))
                         (error (e)
                           (debug-log "Skipping unparseable path:" namestring e)
                           (return-from watcher-callback nil)))))
    (setf event-type
          (if (not (eql 0 (length namestring)))
              (get-event-type full-pathname renamed-p changed-p)
              (when (equal (dir watcher) full-pathname)
                #-os-windows :on-deleted
                #+os-windows nil)))
    (unless event-type
      (return-from watcher-callback))
    (when (eql event-type :file-removed)
      (multiple-value-bind (value present-p)
          (gethash (uiop:ensure-directory-pathname full-pathname)
                   (directory-handles watcher))
        (declare (ignore value))
        (when present-p
          (setf event-type :directory-removed))))
    (when (or (eql event-type :directory-added)
              (eql event-type :directory-removed))
      (setf full-pathname (uiop:ensure-directory-pathname full-pathname)))
    (case event-type
      (:directory-added
       (add-directory-to-watch watcher full-pathname))
      (:directory-removed
       (remove-directory-from-watch watcher full-pathname)))
    (let ((fn (hook watcher)))
      (when fn
        (lparallel.queue:push-queue
         (list fn watcher full-pathname event-type)
         (slot-value watcher 'event-queue))))
    (when (eql event-type :on-deleted)
      (stop-fs-watcher watcher))))

(defun watcher-event-loop (watcher)
  "Watcher event loop, will be called by the watcher thread."
  (as:with-event-loop (:catch-app-errors (error-cb watcher))
    (with-slots (dir alive-p queue-notifier queue) watcher
      (setf queue-notifier
            (cl-async:make-notifier
             (lambda ()
               (as:delay
                 (lambda ()
                   (loop :while (not (lparallel.queue:queue-empty-p queue))
                         :do (funcall (lparallel.queue:pop-queue queue))))))
             :single-shot nil))
      (add-dir watcher dir)
      (setf alive-p t)
      #+os-windows
      (labels ((check-if-dir-deleted ()
                 (unless (escaped-directory-exists-p dir)
                   (let ((handle (gethash (dir watcher) (directory-handles watcher))))
                     (when handle
                       (watcher-callback watcher
                                 handle
                                 ""
                                 nil
                                 nil))))
                 (when alive-p
                   (as:delay #'check-if-dir-deleted
                     :time 1))))
        (check-if-dir-deleted)))))

(defmethod initialize-instance :after ((watcher fs-watcher) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (dir) watcher
    (setf dir (uiop:ensure-absolute-pathname
               (uiop:ensure-directory-pathname
                dir)))
    (unless (escaped-directory-exists-p dir)
      (error "ERROR: The directory '~a' does not exist."
             dir))))

(defun hook-thread-main-loop (watcher)
  "Main Function of hook-thread."
  (with-slots (event-queue error-cb hook-busy-p skip-duplicated)
      watcher
    (loop :for event = (lparallel.queue:pop-queue event-queue)
          :while (not (eql event :stop))
          :do (destructuring-bind (hook watcher filename event-type)
                  event
                (let ((skip nil))
                  (when skip-duplicated
                    (let ((next (lparallel.queue:peek-queue event-queue)))
                      (when (and next
                                 (not (eql next :stop)))
                        (destructuring-bind (ignored1 ignored2 n-filename n-event-type)
                            next
                          (declare (ignorable ignored1 ignored2))
                          (when (and (equal filename n-filename)
                                     (eql event-type n-event-type))
                            (setf skip t))))))
                  (unless skip
                    (setf hook-busy-p t)
                    (unwind-protect
                         (handler-case
                             (funcall hook watcher filename event-type)
                           (error (ev)
                             (if error-cb
                                 (funcall error-cb ev)
                                 (error ev))))
                      (setf hook-busy-p nil))))))))

(defun start-fs-watcher (watcher &optional thread-local-bindings)
  "starts the given watcher."
  (with-slots (thread hook-thread dir) watcher
    (let ((symbols (map 'list #'car thread-local-bindings))
          (values (map 'list #'cdr thread-local-bindings)))
      (setf thread
            (bt:make-thread
             (lambda ()
               (progv symbols values
                 (watcher-event-loop watcher)))
             :name (format nil "fs-watcher:event-loop ~a" dir)))
      (setf hook-thread
            (bt:make-thread
             (lambda ()
               (progv symbols values
                 (hook-thread-main-loop watcher)))
             :name (format nil "fs-watcher:hook-thread ~a" dir))))))

(defun stop-fs-watcher (watcher)
  "Will stop the Watcher."
  (with-slots (thread hook-thread event-queue alive-p directory-handles queue-notifier)
      watcher
    (when alive-p
      (loop :for path :being :the :hash-key :of directory-handles
            :do (remove-directory-from-watch watcher path nil))
      (in-event-loop (watcher)
        (when queue-notifier
          (as:free-notifier queue-notifier)
          (setf queue-notifier nil)))
      (lparallel.queue:push-queue :stop event-queue)
      (setf alive-p nil))))

(defstruct watcher-definition
  name
  command
  help
  daemon-p
  hook-var)

(defstruct active-watcher
  definition
  process-info
  output-buffer
  reader-thread)
(nhooks:define-hook-type watcher-output (function (active-watcher string) t)
                         "Hook for watcher process output events. Handler takes the active-watcher struct and a line of output.")

(nhooks:define-hook-type file-event (function (pathname symbol) t)
                         "Hook for file system events. Handler takes pathname and event type symbol.")
(defvar *file-event-hook* (make-instance 'hook-file-event))

(defmacro defwatcher (name command help &key (daemon nil))
  "Define a watcher process.
   NAME: Symbol, the name of the watcher.
   COMMAND: String or list of strings for the command.
   HELP: String, help text for the watcher.
   DAEMON: Boolean, whether the process runs continuously."
  (let* ((watcher-name-string (string-downcase (symbol-name name)))
         (hook-var-name (intern (format nil "*WATCHER-~A-OUTPUT-HOOK*" watcher-name-string))))
    `(progn
       (defvar ,hook-var-name (make-instance 'hook-watcher-output)
         ,(format nil "Hook for output from the ~A watcher." watcher-name-string))
       (setf (gethash ',name *watcher-definitions*)
             (make-watcher-definition :name ',name
                                      :command (if (stringp ,command) (uiop:split-string ,command :separator " ") ,command)
                                      :help ,help
                                      :daemon-p ,daemon
                                      :hook-var ',hook-var-name))
       ',name)))

(defun read-stream-loop (stream hook active-watcher)
  "Reads lines from a stream and runs the hook for each line."
  (declare (ignore hook))
  (handler-case
      (loop for line = (read-line stream nil nil)
            while line do
              (debug-log (format nil "Watcher [~A]: ~A" (watcher-definition-name (active-watcher-definition active-watcher)) line))
	      ;; Append to buffer
              (write-string line (active-watcher-output-buffer active-watcher))
              (write-char #\Newline (active-watcher-output-buffer active-watcher))
	      ;; Run the specific watcher's hook
              (nhooks:run-hook (symbol-value (watcher-definition-hook-var (active-watcher-definition active-watcher)))
                               active-watcher line))
    (end-of-file () (debug-log "Watcher stream EOF:" (watcher-definition-name (active-watcher-definition active-watcher))))
    (error (e) (format t "~&Error reading watcher stream [~A]: ~A~%"
                       (watcher-definition-name (active-watcher-definition active-watcher)) e)))
  ;; Cleanup after loop finishes or errors out
  (debug-log "Watcher reader thread finished:" (watcher-definition-name (active-watcher-definition active-watcher))))

(defun start-watcher (watcher-def)
  "Starts a watcher process based on its definition."
  (when (and (watcher-definition-daemon-p watcher-def)
             (find-if (lambda (aw) (eq (watcher-definition-name (active-watcher-definition aw))
                                       (watcher-definition-name watcher-def)))
                      (alexandria:hash-table-values *active-watchers*)))
    (format t "Watcher [~A] is a daemon and already running.~%" (watcher-definition-name watcher-def))
    (return-from start-watcher nil))

  (format t "Starting watcher [~A]: ~{~A~^ ~}~%"
          (watcher-definition-name watcher-def)
          (watcher-definition-command watcher-def))
  (handler-case
      (let* ((process-info (uiop:launch-program (watcher-definition-command watcher-def)
                                                :input :stream
                                                :output :stream
                                                :error-output :stream ; Merge stderr for simplicity for now
                                                :directory *repo-root*))
             (active-watcher (make-active-watcher
                               :definition watcher-def
                               :process-info process-info
                               :output-buffer (make-string-output-stream))))
        ;; Store the active watcher immediately
        (setf (gethash process-info *active-watchers*) active-watcher)

        ;; Start reader thread
        (setf (active-watcher-reader-thread active-watcher)
              (bt:make-thread (lambda () (read-stream-loop (uiop:process-info-output process-info)
                                                           (watcher-definition-hook-var watcher-def)
                                                           active-watcher))
                              :name (format nil "Watcher Reader [~A]" (watcher-definition-name watcher-def))))
        (format t "Watcher [~A] started (PID: ~A).~%"
                (watcher-definition-name watcher-def)
                (uiop:process-info-pid process-info))
        active-watcher)
    (error (e)
      (format t "~&Error starting watcher [~A]: ~A~%" (watcher-definition-name watcher-def) e)
     nil)))

(defun stop-watcher (active-watcher)
  "Stops an active watcher process."
  (let* ((watcher-def (active-watcher-definition active-watcher))
         (process-info (active-watcher-process-info active-watcher))
         (thread (active-watcher-reader-thread active-watcher)))
    (format t "Stopping watcher [~A] (PID: ~A)...~%"
            (watcher-definition-name watcher-def)
            (uiop:process-info-pid process-info))
    (ignore-errors (uiop:terminate-process process-info :urgent t)) ; Force termination
    (when (and thread (bt:thread-alive-p thread))
      (ignore-errors (bt:destroy-thread thread)))
    (remhash process-info *active-watchers*)
    (format t "Watcher [~A] stopped.~%" (watcher-definition-name watcher-def))))

(defun handle-file-event (watcher pathname event-type)
  "Callback function for the file watcher. Only processes git-tracked files."
  (declare (ignore watcher))
  ;; Skip if pathname contains problematic characters
  (let ((path-string (ignore-errors (namestring pathname))))
    (unless path-string
      (return-from handle-file-event nil))
    ;; git-check-ignore returns T if file is NOT tracked (ignored/untracked)
    ;; We only want to process tracked files
    (unless (ignore-errors (git-check-ignore pathname *repo-root*))
      (when *debug*
        (format t "~&File event: ~A on ~A~%" event-type pathname))
      (nhooks:run-hook *file-event-hook* pathname event-type))))

(defun start-file-watcher (directory)
  "Starts the file watcher for the given directory."
  (when *file-watcher*
    (stop-fs-watcher *file-watcher*))
  (let ((watch-target (uiop:ensure-directory-pathname directory)))
    (unless (equal watch-target *repo-root*)
      (warn "start-file-watcher called with directory ~A different from *repo-root* ~A. Watching *repo-root*."
            watch-target *repo-root*)
      (setf watch-target *repo-root*))

    (setf *file-watcher*
          (make-instance 'fs-watcher
                         :dir watch-target
                         :hook #'handle-file-event
                         :error-cb (lambda (ev)
                                     (debug-log
                                      (format t "~&Watcher Error: ~a~%" ev))
				     ;; TODO add a backoff retrying mechanism here
                                     ;; Optionally try to restart or just stop
                                     ;; (when *file-watcher*
                                     ;;   (ignore-errors (stop-fs-watcher *file-watcher*))
                                     ;;   (setf *file-watcher* nil))
                                     )))
    (start-fs-watcher *file-watcher*)
    (debug-log "File watcher started for directory:" watch-target)))

(defun stop-file-watcher ()
  "Stops the file watcher if it's running."
  (when *file-watcher*
    (format t "~&Stopping file watcher...~%")
    (ignore-errors (stop-fs-watcher *file-watcher*))
    (setf *file-watcher* nil)))

(defun get-watcher-output (active-watcher)
  "Returns the accumulated output of the watcher."
  (get-output-stream-string (active-watcher-output-buffer active-watcher)))
(define-command watch (args)
                "List available watchers and start the selected one."
                (declare (ignore args))
                (let* ((definitions (alexandria:hash-table-values *watcher-definitions*))
                       (items (mapcar (lambda (d) (format nil "~A~C~A"
                                                          (watcher-definition-name d)
                                                          #\Tab
                                                          (watcher-definition-help d)))
                                      definitions)))
                  (when items
                    (let* ((selected-line (select-with-fzf items))
                           (selected-name-str (when selected-line (first (str:split #\Tab selected-line)))))
                      (when selected-name-str
                        (let* ((selected-name (ignore-errors (read-from-string selected-name-str)))
                               (watcher-def (when selected-name (gethash selected-name *watcher-definitions*))))
                          (if watcher-def
                            (start-watcher watcher-def)
                            (format t "Invalid watcher selected.~%"))))
                      (unless selected-line (format t "No watcher selected.~%")))))
                (unless (alexandria:hash-table-values *watcher-definitions*)
                  (format t "No watchers defined.~%")))

(define-command unwatch (args)
                "List active daemon watchers and stop the selected one."
                (declare (ignore args))
                (let* ((active-daemon-watchers (remove-if-not
                                                 (lambda (aw) (watcher-definition-daemon-p (active-watcher-definition aw)))
                                                 (alexandria:hash-table-values *active-watchers*)))
                       (items (mapcar (lambda (aw) (format nil "~A (PID: ~A)"
                                                           (watcher-definition-name (active-watcher-definition aw))
                                                           (uiop:process-info-pid (active-watcher-process-info aw))))
                                      active-daemon-watchers)))
                  (if items
                    (let* ((selected-item (select-with-fzf items))
                           (selected-pid-str (when selected-item
                                               (cadr (ppcre:all-matches-as-strings "\\(PID: (\\d+)\\)" selected-item)))))
                      (when selected-pid-str
                        (let* ((selected-pid (parse-integer selected-pid-str))
                               (watcher-to-stop (find selected-pid active-daemon-watchers
                                                      :key (lambda (aw) (uiop:process-info-pid (active-watcher-process-info aw))))))
                          (if watcher-to-stop
                            (stop-watcher watcher-to-stop)
                            (format t "Could not find active watcher with PID ~A.~%" selected-pid))))
                      (unless selected-item (format t "No watcher selected to stop.~%")))
                    (format t "No active daemon watchers to stop.~%"))))

(define-command rerun (args)
                "Re-run a non-daemon watcher command."
                (declare (ignore args))
                (let* ((non-daemon-defs (remove-if #'watcher-definition-daemon-p
                                                   (alexandria:hash-table-values *watcher-definitions*)))
                       (items (mapcar (lambda (d) (format nil "~A~C~A"
                                                          (watcher-definition-name d)
                                                          #\Tab
                                                          (watcher-definition-help d)))
                                      non-daemon-defs)))
                  (if items
                    (let* ((selected-line (select-with-fzf items))
                           (selected-name-str (when selected-line (first (str:split #\Tab selected-line)))))
                      (when selected-name-str
                        (let* ((selected-name (ignore-errors (read-from-string selected-name-str)))
                               (watcher-def (when selected-name (gethash selected-name *watcher-definitions*))))
                          (if watcher-def
                            (start-watcher watcher-def)
                            (format t "Invalid watcher selected.~%"))))
                      (unless selected-line (format t "No watcher selected to rerun.~%")))
                    (format t "No non-daemon watchers defined to rerun.~%"))))
