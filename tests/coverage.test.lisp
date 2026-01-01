(in-package :hactar-tests)

(test router-unregister-route-removes
  "unregister-route should remove a previously registered route."
  (clrhash hactar::*routes*)
  (let ((name (hactar::register-route 'test-route "^x:(\\d+)$" '(:n) 10 (lambda (n) n))))
    (declare (ignore name))
    (multiple-value-bind (route params)
        (hactar::match-route "x:123")
      (is-true route)
      (is (equal (cdr (assoc :n params)) "123")))
    (hactar::unregister-route 'test-route)
    (multiple-value-bind (route2 params2)
        (hactar::match-route "x:123")
      (is (null route2))
      (is (null params2)))))

(test normalize-http-extra-headers-hash-table
  "normalize-http-extra-headers should accept hash tables and coerce keys/values to strings."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "X-Test" ht) 123)
    (setf (gethash :foo ht) :bar)
    (let ((alist (hactar::normalize-http-extra-headers ht)))
      (is (equal (assoc "X-Test" alist :test #'string=) '("X-Test" . "123")))
      (is (equal (assoc "FOO" alist :test #'string=) '("FOO" . "BAR"))))))

(test normalize-http-extra-headers-invalid-signals
  "normalize-http-extra-headers should signal an error for invalid inputs."
  (signals error (hactar::normalize-http-extra-headers 42)))

(test tokenize-cli-string-respects-quotes-and-escapes
  "tokenize-cli-string should preserve quoted segments and allow escaped quotes."
  (let ((tokens (hactar::tokenize-cli-string "cmd sub --name \"A B\" --x \\\"q\\\"")))
    (is (equal tokens '("cmd" "sub" "--name" "A B" "--x" "\"q\"")))))

(test parse-cli-args-s-short-map-and-multi-values
  "parse-cli-args-s should map short flags and split comma-separated lists."
  (let ((plist (hactar::parse-cli-args-s "hactar sub -t=1,2 --covers a,b"
                                         '(("t" . "tags")))))
    (is (string= (getf plist :command) "hactar"))
    (is (string= (getf plist :subcommand) "sub"))
    (is (equal (getf plist :tags) '("1" "2")))
    (is (equal (getf plist :covers) '("a" "b")))))

(test add-file-to-context-warns-on-token-limit
  "add-file-to-context should warn when projected tokens exceed model max input."
  (let ((hactar::*files* '())
        (hactar::*exposed-context-file* nil)
        (hactar::*repo-root* (uiop:getcwd))
        (hactar::*current-model* (hactar::make-model-config :name "t" :provider "x" :model-name "y" :max-input-tokens 10))
        ;; make history non-empty and large
        (hactar::*chat-history* (list `((:role . "user") (:content . ,(make-string 200 :initial-element #\a))))))
    (with-dynamic-stubs ((hactar::get-file-content (lambda (&rest _) (declare (ignore _)) (make-string 200 :initial-element #\b)))
                         (hactar::generate-context-without-files (lambda () "base"))
                         (hactar::estimate-tokens (lambda (txt) (length txt))) ; exaggerate to force overflow
                         (hactar::context-expose-upsert-files-section (lambda () nil)))
      (let* ((buf (make-string-output-stream))
             (*standard-output* buf))
        (hactar::add-file-to-context "x.txt")
        (let ((out (get-output-stream-string buf)))
          (is-true (search "might exceed the model's input limit" out)))))))

