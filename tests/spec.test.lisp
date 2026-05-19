(in-package :hactar-tests)

(def-suite spec-tests
  :description "Tests for the CLI specification emitter")

(in-suite spec-tests)

(test normalize-cli-option-defaults
  "Options with no :type or :default get sensible defaults."
  (let ((entry (hactar::normalize-cli-option
                '(:short "t" :long "tags" :description "Filter by tags"))))
    (is (string= "string" (cdr (assoc "type" entry :test #'string=))))
    (is (string= "Filter by tags" (cdr (assoc "description" entry :test #'string=))))
    (let ((names (cdr (assoc "name" entry :test #'string=))))
      (is (find "-t" names :test #'string=))
      (is (find "--tags" names :test #'string=)))
    (is (null (assoc "defaultValue" entry :test #'string=)))
    (is (null (assoc "isRequired" entry :test #'string=)))))

(test normalize-cli-option-with-type
  "Custom :type, :default, and :required pass through."
  (let ((entry (hactar::normalize-cli-option
                '(:long "limit" :type :integer :default 10
                  :required t :description "Max results"))))
    (is (string= "integer" (cdr (assoc "type" entry :test #'string=))))
    (is (equal 10 (cdr (assoc "defaultValue" entry :test #'string=))))
    (is (eq t (cdr (assoc "isRequired" entry :test #'string=))))))

(test normalize-positional-arg-required-variadic
  "Positional args carry isRequired and isVariadic."
  (let ((entry (hactar::normalize-positional-arg
                '(:name "files" :type :string
                  :description "Files to build"
                  :required t :variadic t))))
    (is (string= "files" (cdr (assoc "name" entry :test #'string=))))
    (is (string= "string" (cdr (assoc "type" entry :test #'string=))))
    (is (eq t (cdr (assoc "isRequired" entry :test #'string=))))
    (is (eq t (cdr (assoc "isVariadic" entry :test #'string=))))))

(test build-command-spec-entry-strip-slash
  "STRIP-SLASH removes the leading slash for slash commands."
  (let* ((info (list 'some-fn "Test description" nil nil))
         (entry (hactar::build-command-spec-entry "/test" info :strip-slash t)))
    (is (string= "test" (cdr (assoc "name" entry :test #'string=))))
    (is (string= "Test description" (cdr (assoc "description" entry :test #'string=))))
    (is (null (assoc "args" entry :test #'string=)))
    (is (null (assoc "options" entry :test #'string=)))))

(test build-command-spec-entry-with-args
  "Command entries include args when positional-args are present."
  (let* ((pos-args '((:name "file" :type :string :required t)))
         (info (list 'fn "desc" nil pos-args))
         (entry (hactar::build-command-spec-entry "build" info)))
    (is (not (null (assoc "args" entry :test #'string=))))
    (let ((args-vec (cdr (assoc "args" entry :test #'string=))))
      (is (vectorp args-vec))
      (is (= 1 (length args-vec))))))

(test build-cli-spec-shape
  "The top-level spec contains commands, subcommands, and flags keys."
  (let ((spec (hactar::build-cli-spec)))
    (is (not (null (assoc "commands" spec :test #'string=))))
    (is (not (null (assoc "subcommands" spec :test #'string=))))
    (is (not (null (assoc "flags" spec :test #'string=))))
    (is (vectorp (cdr (assoc "commands" spec :test #'string=))))
    (is (vectorp (cdr (assoc "subcommands" spec :test #'string=))))
    (is (vectorp (cdr (assoc "flags" spec :test #'string=))))))

(test emit-cli-spec-json
  "JSON output is wrapped in <json>...</json> tags."
  (let ((output (with-output-to-string (s)
                  (hactar::emit-cli-spec :stream s))))
    (is (search "<json>" output))
    (is (search "</json>" output))
    (is (search "commands" output))
    (is (search "subcommands" output))
    (is (search "flags" output))))

(test emit-cli-spec-lisp
  "Lisp output is a printed s-expression, no <json> tags."
  (let ((output (with-output-to-string (s)
                  (hactar::emit-cli-spec :lisp-p t :stream s))))
    (is (not (search "<json>" output)))
    (is (search "commands" output))
    (is (search "subcommands" output))
    (is (search "flags" output))))
