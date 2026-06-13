(in-package :hactar-tests)

(def-suite history-tests
  :description "Tests for the history command and format flags")

(in-suite history-tests)

(test history-format-flags-cli
  "Test the history command format flags in CLI/REPL mode."
  (let* ((hactar::*chat-history* '(((:role . "user") (:content . "test message"))))
         (hactar::*interfaces* (make-hash-table :test 'equal))
         (hactar::*instance-id* "history-cli-test")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*history-changed-hook* (make-instance 'hactar::hook-history-changed)))
    (hactar::definterface :history "history.lisp"
      :format :lisp
      :hooks ((hactar::*history-changed-hook* :out))
      :render (lambda () "rendered")
      :parse (lambda (text) (declare (ignore text)) nil))
    (let ((cli-fn (gethash "/history" hactar::*commands*)))
      (is-true cli-fn)
      (when cli-fn
        ;; Test --json
        (let ((output (with-output-to-string (*standard-output*)
                        (funcall (first cli-fn) '("--json")))))
          (is (search "test message" output))
          (is (search "role" output)))
        ;; Test --lisp
        (let ((output (with-output-to-string (*standard-output*)
                        (funcall (first cli-fn) '("--lisp")))))
          (is (search "test message" output))
          (is (search "*chat-history*" output)))
        ;; Test --yaml
        (let ((output (with-output-to-string (*standard-output*)
                        (funcall (first cli-fn) '("--yaml")))))
          (is (search "test message" output))
          (is (search "role: user" output)))
        ;; Test --toml
        (let ((output (with-output-to-string (*standard-output*)
                        (funcall (first cli-fn) '("--toml")))))
          (is (search "test message" output))
          (is (search "role = " output)))))))

(test history-format-flags-acp
  "Test the history command format flags in ACP mode."
  (let* ((hactar::*chat-history* '(((:role . "user") (:content . "acp test"))))
         (hactar::*interfaces* (make-hash-table :test 'equal))
         (hactar::*instance-id* "history-acp-test")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*history-changed-hook* (make-instance 'hactar::hook-history-changed)))
    (hactar::definterface :history "history.lisp"
      :format :lisp
      :hooks ((hactar::*history-changed-hook* :out))
      :render (lambda () "rendered")
      :parse (lambda (text) (declare (ignore text)) nil))
    (let ((acp-fn (gethash "/history" hactar::*acp-commands*)))
      (is-true acp-fn)
      (when acp-fn
        ;; Test --json
        (let ((res (funcall acp-fn '("--json"))))
          (is (search "acp test" (cdr (assoc "text" res :test #'string=))))
          (is (search "role" (cdr (assoc "text" res :test #'string=)))))
        ;; Test --lisp
        (let ((res (funcall acp-fn '("--lisp"))))
          (is (search "acp test" (cdr (assoc "text" res :test #'string=))))
          (is (search "*chat-history*" (cdr (assoc "text" res :test #'string=)))))
        ;; Test --yaml
        (let ((res (funcall acp-fn '("--yaml"))))
          (is (search "acp test" (cdr (assoc "text" res :test #'string=))))
          (is (search "role: user" (cdr (assoc "text" res :test #'string=)))))
        ;; Test --toml
        (let ((res (funcall acp-fn '("--toml"))))
          (is (search "acp test" (cdr (assoc "text" res :test #'string=))))
          (is (search "role = " (cdr (assoc "text" res :test #'string=)))))))))

(test history-save-formats
  "Test saving history in json, lisp, yaml, and toml formats."
  (let* ((hactar::*chat-history* '(((:role . "user") (:content . "save format test"))))
         (hactar::*interfaces* (make-hash-table :test 'equal))
         (hactar::*instance-id* "history-save-test")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*history-changed-hook* (make-instance 'hactar::hook-history-changed)))
    (hactar::definterface :history "history.lisp"
      :format :lisp
      :hooks ((hactar::*history-changed-hook* :out))
      :render (lambda () "rendered")
      :parse (lambda (text) (declare (ignore text)) nil))
    (let ((cli-fn (gethash "/history" hactar::*commands*)))
      (is-true cli-fn)
      (when cli-fn
        (let ((json-file (merge-pathnames "history.json" hactar::*repo-root*))
              (lisp-file (merge-pathnames "history.lisp" hactar::*repo-root*))
              (yaml-file (merge-pathnames "history.yaml" hactar::*repo-root*))
              (toml-file (merge-pathnames "history.toml" hactar::*repo-root*)))
          (unwind-protect
               (progn
                 ;; Save as JSON
                 (funcall (first cli-fn) (list "save" (namestring json-file) "--json"))
                 (is-true (probe-file json-file))
                 (let ((content (uiop:read-file-string json-file)))
                   (is (search "save format test" content))
                   (is (search "role" content)))

                 ;; Save as Lisp
                 (funcall (first cli-fn) (list "save" (namestring lisp-file) "--lisp"))
                 (is-true (probe-file lisp-file))
                 (let ((content (uiop:read-file-string lisp-file)))
                   (is (search "save format test" content))
                   (is (search "*chat-history*" content)))

                 ;; Save as YAML
                 (funcall (first cli-fn) (list "save" (namestring yaml-file) "--yaml"))
                 (is-true (probe-file yaml-file))
                 (let ((content (uiop:read-file-string yaml-file)))
                   (is (search "save format test" content))
                   (is (search "role: user" content)))

                 ;; Save as TOML
                 (funcall (first cli-fn) (list "save" (namestring toml-file) "--toml"))
                 (is-true (probe-file toml-file))
                 (let ((content (uiop:read-file-string toml-file)))
                   (is (search "save format test" content))
                   (is (search "role = " content))))
            (ignore-errors (delete-file json-file))
            (ignore-errors (delete-file lisp-file))
            (ignore-errors (delete-file yaml-file))
            (ignore-errors (delete-file toml-file))))))))

(test history-execute-format-command
  "Test execute-format-command with /history for json, yaml, and toml formats."
  (let* ((hactar::*chat-history* '(((:role . "user") (:content . "format command test")))))
    (is (search "format command test" (hactar::execute-format-command "/history" :json nil)))
    (is (search "format command test" (hactar::execute-format-command "/history" :yaml nil)))
    (is (search "format command test" (hactar::execute-format-command "/history" :toml nil)))))
