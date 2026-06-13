(in-package :hactar)

;;* History Interface for Hactar
;;  Manages chat history and synchronizes it with the instance history.lisp file

(nhooks:define-hook-type history-changed (function () t)
  "Hook run when chat history changes.")

(defvar *history-changed-hook* (make-instance 'hook-history-changed))

(definterface :history "history.lisp"
  :format :lisp
  :hooks ((*history-changed-hook* :out))
  :render (lambda ()
            (format nil ";;; Chat history (edit and save to apply)~%(in-package :hactar)~%~%~
                         (setf *chat-history* '~S)~%"
                    *chat-history*))
  :parse (lambda (text)
           (declare (ignore text))
           (let ((iface (gethash :history *interfaces*)))
             (when iface
               (ignore-errors (load (interface-abs-path iface)))))))

(defun serialize-history-to-yaml (history)
  "Serialize chat history to a YAML string."
  (with-output-to-string (s)
    (dolist (message history)
      (format s "- role: ~A~%" (or (cdr (assoc :role message)) ""))
      (let ((content (cdr (assoc :content message))))
        (if (and content (find #\Newline content))
            (progn
              (format s "  content: |~%")
              (dolist (line (str:lines content))
                (format s "    ~A~%" line)))
            (format s "  content: ~S~%" (or content ""))))
      (let ((tool-calls (cdr (assoc :tool_calls message))))
        (when tool-calls
          (format s "  tool_calls:~%")
          (dolist (tc tool-calls)
            (format s "    - id: ~A~%" (or (cdr (assoc :id tc)) ""))
            (format s "      type: ~A~%" (or (cdr (assoc :type tc)) ""))
            (let ((fn (cdr (assoc :function tc))))
              (when fn
                (format s "      function:~%")
                (format s "        name: ~A~%" (or (cdr (assoc :name fn)) ""))
                (format s "        arguments: ~S~%" (or (cdr (assoc :arguments fn)) "")))))))
      (let ((tool-call-id (cdr (assoc :tool_call_id message))))
        (when tool-call-id
          (format s "  tool_call_id: ~A~%" tool-call-id)))
      (let ((name (cdr (assoc :name message))))
        (when name
          (format s "  name: ~A~%" name))))))
