(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

;;; ---- hactar-fzf ----
(test fzf-matches-subsequence
  (is-true (hactar-fzf:fuzzy-match-p "gac" "git-add-commit"))
  (is-false (hactar-fzf:fuzzy-match-p "xyz" "git-add-commit")))

(test fzf-ranks-boundary-and-contiguous-higher
  (let ((ranked (hactar-fzf:fuzzy-filter "test"
                 '("latest" "test-runner" "contest"))))
    (is (string= "test-runner" (first ranked)))))

(test fzf-select-best-and-limit
  (is (string= "route.create"
               (hactar-fzf:select-best "rc" '("route.create" "model.new" "lint"))))
  (is (<= (length (hactar-fzf:select "e" '("a" "be" "ce" "de") :limit 2)) 2)))

;;; ---- hactar-md ----
(test md-parses-code-fence-and-heading
  (let ((blocks (hactar-md:parse-markdown
                 (format nil "# Title~%~%para~%~%```lisp~%(foo)~%```~%"))))
    (is (eq :heading (hactar-md:md-block-type (first blocks))))
    (is (find :code blocks :key #'hactar-md:md-block-type))))

(test md-code-blocks-and-links
  (is (equal '("(foo)")
             (hactar-md:code-blocks (format nil "```lisp~%(foo)~%```") "lisp")))
  (is (equal '(("docs" . "http://x"))
             (hactar-md:extract-links "see [docs](http://x) now"))))

(test md-parses-compositional-blocks
  (let ((blocks (hactar-md:parse-markdown
                 (format nil "> quote~%- item~%---~%"))))
    (is (equal '(:blockquote :list-item :thematic-break)
               (mapcar #'hactar-md:md-block-type blocks)))
    (is (search "| quote" (hactar-md:render-terminal "> quote")))))

;;; ---- hactar-org ----
(test org-parses-headline-and-src
  (let ((nodes (hactar-org:parse-org
                (format nil "* H~%#+begin_src lisp~%(a)~%#+end_src~%"))))
    (is (find :headline nodes :key #'hactar-org:org-node-type))
    (is (find :src nodes :key #'hactar-org:org-node-type))))

(test org-extract-src-blocks
  (is (search "(a)"
              (hactar-org:extract-src-blocks
               (format nil "#+begin_src lisp~%(a)~%#+end_src~%") "lisp"))))

(test org-document-round-trips-structure
  (let* ((source (format nil
                         "#+title: Demo~%~%* TODO [#A] Parent :one:two:~%:PROPERTIES:~%:ID: p1~%:END:~%** Child~%#+begin_src lisp :tangle child.lisp~%(a)~%#+end_src~%"))
         (doc (hactar-org:parse-org-document source))
         (parent (hactar-org:find-org-node
                  doc (lambda (node)
                        (and (eq :headline (hactar-org:org-node-type node))
                             (= 1 (hactar-org:org-node-level node)))))))
    (is (string= "Demo" (cdr (assoc :title
                                    (hactar-org:org-node-settings doc)))))
    (is (string= "TODO" (hactar-org:org-node-todo parent)))
    (is (equal '("one" "two") (hactar-org:org-node-tags parent)))
    (is (= 1 (length (hactar-org:org-src-blocks doc "lisp"))))
    (let ((rendered (hactar-org:render-org doc)))
      (is (search ":ID: p1" rendered))
      (is (search "** Child" rendered))
      (is (search "lisp :tangle child.lisp" rendered))
      (let ((reparsed (hactar-org:parse-org-document rendered)))
        (let ((reparsed-parent
                (hactar-org:find-org-node
                 reparsed
                 (lambda (node)
                   (and (eq :headline (hactar-org:org-node-type node))
                        (= 1 (hactar-org:org-node-level node)))))))
          (is (string= "Parent" (hactar-org:org-node-title reparsed-parent)))
          (is (equal '("one" "two")
                     (hactar-org:org-node-tags reparsed-parent))))))))

(test org-lisp-forms-round-trip
  (let* ((forms '((defproject demo :stack ("lisp"))
                  (deftool test () :ok)))
         (org (hactar-org:to-org forms)))
    (let ((round-tripped (hactar-org:from-org org)))
      (is (= 3 (length round-tripped)))
      (is (equal '(in-package :hactar-user) (first round-tripped)))
      (is (eq (find-package :hactar-rebuild)
              (symbol-package (first (second round-tripped)))))
      (is (eq (find-package :hactar-user)
              (symbol-package (second (second round-tripped)))))
      (is (string= "DEMO" (symbol-name (second (second round-tripped))))))))

;;; ---- hactar-llm ----
(test llm-model-registry
  (is (hactar-llm:find-model "anthropic/claude-sonnet-4-6"))
  (is (null (hactar-llm:find-model "no/such"))))

(test llm-mock-complete-and-diffuse
  (let ((hactar-llm:*mock-llm-output* "MOCKED"))
    (is (string= "MOCKED" (hactar-llm:complete "hi"))))
  (let ((hactar-llm:*mock-diffusion-output* "DIFF"))
    (is (string= "DIFF" (hactar-llm:diffuse "region")))))

(test llm-token-estimate
  (is (plusp (hactar-llm:estimate-tokens "some text here"))))

 (test llm-composable-effect-seams
  (let ((chunks nil)
        (hactar-llm:*mock-llm-output* "streamed"))
    (is (string= "streamed"
                 (hactar-llm:llm-stream
                  "prompt" (lambda (chunk) (push chunk chunks)))))
    (is (equal '("streamed") chunks))
    (is (= 1 (length (hactar-llm:embed "input")))))
  (let ((provider (hactar-llm:make-provider
                   :name :test
                   :oauth (lambda (&key token) token))))
    (hactar-llm:register-provider provider)
    (is (string= "secret" (hactar-llm:oauth-login :test :token "secret")))))

(test llm-loads-models-yaml-catalog
  (let ((path (merge-pathnames "data/models.yaml"
                               (asdf:system-source-directory "hactar-core"))))
    (when (probe-file path)
      (let ((names (hactar-llm:load-models-yaml path)))
        (is (> (length names) 20))
        (let ((claude (hactar-llm:find-model "anthropic/claude-3-7-sonnet-20250219")))
          (is-true claude)
          (is (eq :anthropic (hactar-llm:model-provider claude)))
          (is (string= "claude-3-7" (hactar-llm:model-id claude))))))))

(test llm-oauth-pkce-and-credentials-roundtrip
  (multiple-value-bind (verifier challenge method) (hactar-llm:generate-pkce)
    (is (plusp (length verifier)))
    (is (plusp (length challenge)))
    (is (string= "S256" method)))
  (is (string= "abc" (hactar-llm:oauth-parse-code-input "http://localhost/cb?code=abc&state=xyz")))
  (is (string= "abc" (hactar-llm:oauth-parse-code-input "abc#xyz")))
  (let ((creds (hactar-llm:oauth-creds-from-token-response
                "{\"access_token\":\"tok\",\"refresh_token\":\"ref\",\"expires_in\":3600}")))
    (is (string= "tok" (getf creds :access)))
    (is (string= "ref" (getf creds :refresh)))
    (is-false (hactar-llm:oauth-expired-p creds)))
  (with-temp-project (root)
    (let ((cred-path (merge-pathnames "creds.lisp" root)))
      (hactar-llm:set-oauth-credentials "test-prov" (list :access "a1" :refresh "r1" :expires 9999999999))
      (hactar-llm:save-oauth-credentials cred-path)
      (clrhash hactar-llm:*oauth-credentials*)
      (hactar-llm:load-oauth-credentials cred-path)
      (is (string= "a1" (getf (hactar-llm:get-oauth-credentials "test-prov") :access))))))

;;; ---- hactar-llm-live ----
(test llm-live-builds-and-extracts-providers-offline
  (multiple-value-bind (url headers payload family)
      (hactar-llm-live:build-provider-request :anthropic "hi" :model "claude-sonnet-4-6"
                                              :api-key "sk-test")
    (is (search "anthropic.com" url))
    (is (string= "sk-test" (cdr (assoc "x-api-key" headers :test #'string=))))
    (is (eq :anthropic family))
    (is (search "claude-sonnet-4-6" payload)))
  (is (string= "hello"
               (hactar-llm-live:extract-provider-response
                :openai (hactar-llm-live:json->alist
                         "{\"choices\":[{\"message\":{\"content\":\"hello\"}}]}"))))
  (is (string= "hi-anth"
               (hactar-llm-live:extract-provider-response
                :anthropic (hactar-llm-live:json->alist
                            "{\"content\":[{\"type\":\"text\",\"text\":\"hi-anth\"}]}"))))
  (is (string= "hi-gem"
               (hactar-llm-live:extract-provider-response
                :gemini (hactar-llm-live:json->alist
                         "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"hi-gem\"}]}}]}")))))

(test llm-live-streams-sse-and-swaps-client
  (let* ((sse (format nil "data: {\"choices\":[{\"delta\":{\"content\":\"hel\"}}]}~%data: {\"choices\":[{\"delta\":{\"content\":\"lo\"}}]}~%data: [DONE]~%"))
         (chunks nil)
         (hactar-llm-live:*http-request-fn*
           (lambda (url &key method headers content want-stream timeout)
             (declare (ignore url method headers content timeout))
             (if want-stream
                 (values (make-string-input-stream sse) 200)
                 (values "{\"choices\":[{\"message\":{\"content\":\"ok\"}}]}" 200))))
         (hactar-llm:*llm-client* (hactar-llm-live:make-live-client)))
    (is (string= "ok" (hactar-llm:complete "ping" :model "openai/gpt-4o-mini")))
    (is (string= "hello"
                 (hactar-llm:llm-stream "ping" (lambda (c) (push c chunks))
                                        :model "openai/gpt-4o-mini")))
    (is (equal '("lo" "hel") chunks))))

;;; ---- hactar-fern ----
(test fern-parses-definitions
  (let ((nodes (hactar-fern:parse-lisp "(defun foo (x) x) (defvar *y* 1)")))
    (is (hactar-fern:find-definition nodes "foo" :function))
    (is (hactar-fern:find-definition nodes "*y*" :variable))))

(test fern-rename-preserves-rest
  (multiple-value-bind (out n)
      (hactar-fern:edit-source
       "(defun foo (x) x)
(defun bar (y) y)"
       '((:action :rename :target :function :name "foo" :value "baz")))
    (is (= 1 n))
    (is (search "baz" out))
    (is (search "bar" out))
    (is (not (search "(defun foo" out)))))

(test fern-set-body-and-add-form
  (multiple-value-bind (out n)
      (hactar-fern:edit-source
       "(defun foo (x) x)"
       '((:action :set-body :target :function :name "foo" :new-code "(* x 2)")
         (:action :add-form :new-code "(defvar *z* 9)")))
    (is (= 2 n))
    (is (search "(* x 2)" out))
    (is (search "*z*" out))
    (is-true (hactar-fern:valid-lisp-p out))))

(test fern-rejects-unknown-definition
  (signals error
    (hactar-fern:edit-source "(defun a () 1)"
      '((:action :rename :name "missing" :value "x")))))

(test fern-generic-structural-api
  (let* ((source "(defun one () 1)~%(defvar *two* 2)")
         (source (format nil source))
         (nodes (hactar-fern:parse-source source :language "common-lisp"))
         (one (hactar-fern:find-node nodes "one" :kind :function)))
    (is (eq :function (hactar-fern:node-kind one)))
    (is (search "(defun changed" (hactar-fern:replace-node
                                  source one "(defun changed () 1)")))
    (is (= 2 (length (hactar-fern:symbol-index source))))))

(test fern-code-values-retain-provenance
  (let ((original (hactar-fern:code-from-string "(defun old () 1)")))
    (multiple-value-bind (changed count)
        (hactar-fern:transform-code-value
         original
         '((:action :rename :target :function :name "old" :value "new"))
         :description "rename")
      (is (= 1 count))
      (is (eq original (hactar-fern:code-value-parent changed)))
      (is (string= "rename" (hactar-fern:code-value-transformation changed)))
      (is (search "(defun new" (hactar-fern:code-value-source changed))))))

(test fern-parses-ctags-data
  (let ((tags (hactar-fern:parse-tags
               (format nil "!_TAG_FILE_FORMAT~%foo~Csrc/a.lisp~C/^foo$/;\"~Cfunction~Cline:12~%"
                       #\Tab #\Tab #\Tab #\Tab))))
    (is (= 1 (length tags)))
    (is (string= "foo" (hactar-fern:tag-name (first tags))))
    (is (= 12 (hactar-fern:tag-line (first tags))))))

;;; ---- hactar-jsonrpc ----
(test jsonrpc-encodes-and-dispatches
  (let ((server (hactar-jsonrpc:make-server)))
    (hactar-jsonrpc:register-method
     server "sum"
     (lambda (params) (+ (cdr (assoc "a" params :test #'string=))
                         (cdr (assoc "b" params :test #'string=)))))
    (let* ((request (hactar-jsonrpc:make-request
                     7 "sum" '(("a" . 2) ("b" . 3))))
           (decoded (hactar-jsonrpc:decode-message
                     (hactar-jsonrpc:encode-message request)))
           (response (hactar-jsonrpc:dispatch-message server decoded)))
      (is (= 5 (cdr (assoc "result" response :test #'string=)))))))

(test jsonrpc-content-length-round-trip
  (let ((output (make-string-output-stream)))
    (hactar-jsonrpc:write-message
     (hactar-jsonrpc:make-notification "changed" '(("path" . "a.lisp")))
     output :framing :content-length)
    (let ((message
            (hactar-jsonrpc:read-message
             (make-string-input-stream (get-output-stream-string output))
             :framing :content-length)))
      (is (string= "changed"
                   (cdr (assoc "method" message :test #'string=)))))))

(test jsonrpc-newline-and-unicode-framing
  (dolist (framing '(:newline :content-length))
    (let ((output (make-string-output-stream))
          (text (format nil "lambda-~C" (code-char #x3bb))))
      (hactar-jsonrpc:write-message
       (hactar-jsonrpc:make-notification "changed"
                                         `(("text" . ,text)))
       output :framing framing)
      (let ((message
              (hactar-jsonrpc:read-message
               (make-string-input-stream (get-output-stream-string output))
               :framing framing)))
        (is (string= "changed"
                     (cdr (assoc "method" message :test #'string=))))
        (is (string= text
                     (cdr (assoc "text"
                                 (cdr (assoc "params" message
                                             :test #'string=))
                                 :test #'string=))))))))

(test jsonrpc-invalid-request-is-data
  (let* ((server (hactar-jsonrpc:make-server))
         (response (hactar-jsonrpc:dispatch-message server #(1 2 3))))
    (is (= hactar-jsonrpc:+invalid-request+
           (cdr (assoc "code"
                       (cdr (assoc "error" response :test #'string=))
                       :test #'string=))))))

(test jsonrpc-distinguishes-null-id-from-notification
  (let ((server (hactar-jsonrpc:make-server)))
    (is (hactar-jsonrpc:dispatch-message
         server '(("jsonrpc" . "2.0") ("id") ("method" . "missing"))))
    (is (null (hactar-jsonrpc:dispatch-message
               server '(("jsonrpc" . "2.0") ("method" . "missing")))))
    (let ((response
            (hactar-jsonrpc:dispatch-message
             server '(("jsonrpc" . 2) ("id" . 1) ("method" . "bad")))))
      (is (= hactar-jsonrpc:+invalid-request+
             (cdr (assoc "code"
                         (cdr (assoc "error" response :test #'string=))
                         :test #'string=)))))))

(test jsonrpc-rejects-malformed-content-length
  (signals hactar-jsonrpc:rpc-error
    (hactar-jsonrpc:read-message
     (make-string-input-stream
      (format nil "Content-Length: nope~C~C~C~C{}"
              #\Return #\Newline #\Return #\Newline))
     :framing :content-length)))

(test jsonrpc-server-does-not-treat-empty-object-as-eof
  (let ((output (make-string-output-stream)))
    (hactar-jsonrpc:serve
     (hactar-jsonrpc:make-server
      :input (make-string-input-stream (format nil "{}~%"))
      :output output))
    (let ((response
            (hactar-jsonrpc:decode-message
             (get-output-stream-string output))))
      (is (= hactar-jsonrpc:+invalid-request+
             (cdr (assoc "code"
                         (cdr (assoc "error" response :test #'string=))
                         :test #'string=)))))))

;;; ---- hactar-vfs ----
(test vfs-stages-diffs-and-commits
  (with-temp-project (root :files '(("existing.txt" . "old")))
    (let ((vfs (hactar-vfs:make-vfs :root root)))
      (hactar-vfs:vfs-write vfs "existing.txt" "new")
      (hactar-vfs:vfs-write vfs "nested/new.txt" "created")
      (is (string= "new" (hactar-vfs:vfs-read vfs "existing.txt")))
      (is (= 2 (length (hactar-vfs:vfs-diff vfs))))
      (is (string= "old" (uiop:read-file-string
                          (merge-pathnames "existing.txt" root))))
      (is (= 2 (length (hactar-vfs:vfs-commit! vfs))))
      (is (string= "new" (uiop:read-file-string
                          (merge-pathnames "existing.txt" root))))
      (is (null (hactar-vfs:vfs-plan vfs))))))

(test vfs-rejects-root-escape
  (with-temp-project (root)
    (signals error
      (hactar-vfs:vfs-write (hactar-vfs:make-vfs :root root)
                            "../outside" "no"))))

(test vfs-rejects-symlink-escape
  (with-temp-project (root)
    (let ((outside (uiop:ensure-directory-pathname
                    (format nil "/tmp/hactar-vfs-outside-~A/" (random 100000)))))
      (unwind-protect
           (progn
             (ensure-directories-exist outside)
             (uiop:run-program
              (list "ln" "-s" (namestring outside)
                    (namestring (merge-pathnames "link" root))))
             (signals error
               (hactar-vfs:vfs-read (hactar-vfs:make-vfs :root root)
                                    "link/escaped.txt")))
        (ignore-errors
          (uiop:delete-directory-tree outside :validate t
                                              :if-does-not-exist :ignore))))))

;;; ---- hactar-watch ----
(test watch-emits-data-to-callbacks
  (with-temp-project (root)
    (let ((seen nil)
          (watcher (hactar-watch:watch root)))
      (hactar-watch:on-change watcher (lambda (event) (push event seen)))
      (with-open-file (stream (merge-pathnames "new.txt" root)
                              :direction :output :if-does-not-exist :create)
        (write-string "new" stream))
      (let ((events (hactar-watch:poll-watch watcher)))
        (is (= 1 (length events)))
        (is (eq :created (getf (first events) :kind)))
        (is (= 1 (length seen)))
        (is (= 1 (length (hactar-watch:drain-events watcher))))
        (is (null (hactar-watch:pending-events watcher))))
      (hactar-watch:unwatch watcher)
      (is-false (hactar-watch:watching-p watcher)))))

(test watch-detects-same-timestamp-content-change
  (with-temp-project (root :files '(("watched.txt" . "one")
                                    ("timestamp.ref" . "reference")))
    (let* ((path (merge-pathnames "watched.txt" root))
           (reference (merge-pathnames "timestamp.ref" root)))
      (uiop:run-program (list "touch" "-r" (namestring path)
                              (namestring reference)))
      (let ((watcher (hactar-watch:watch root))
            (timestamp (file-write-date path)))
      (with-open-file (stream path :direction :output :if-exists :supersede)
        (write-string "two" stream))
      (uiop:run-program (list "touch" "-r" (namestring reference)
                              (namestring path)))
      (is (= timestamp (file-write-date path)))
      (let ((events (hactar-watch:poll-watch watcher)))
        (is (= 1 (length events)))
        (is (eq :modified (getf (first events) :kind))))))))

(test watch-does-not-follow-directory-symlinks
  (with-temp-project (root)
    (let ((outside (uiop:ensure-directory-pathname
                    (format nil "/tmp/hactar-watch-outside-~A/"
                            (random 100000)))))
      (unwind-protect
           (progn
             (ensure-directories-exist outside)
             (with-open-file (stream (merge-pathnames "secret.txt" outside)
                                     :direction :output
                                     :if-does-not-exist :create)
               (write-string "one" stream))
             (uiop:run-program
              (list "ln" "-s" (namestring outside)
                    (namestring (merge-pathnames "link" root))))
             (let ((watcher (hactar-watch:watch root)))
               (with-open-file (stream (merge-pathnames "secret.txt" outside)
                                       :direction :output
                                       :if-exists :supersede)
                 (write-string "two" stream))
               (is (null (hactar-watch:poll-watch watcher)))))
        (ignore-errors
          (uiop:delete-directory-tree outside :validate t
                                              :if-does-not-exist :ignore))))))

;;; ---- hactar-sandbox ----
(test sandbox-policy-is-data-and-enforced
  (let ((policy (hactar-sandbox:make-policy
                 :name "test" :allow '("echo") :deny '("--danger"))))
    (is-true (hactar-sandbox:policy-allows-p policy '("echo" "ok")))
    (is-false (hactar-sandbox:policy-allows-p policy '("rm" "file")))
    (is (search "does not allow"
                (hactar-sandbox:explain-denial policy '("rm" "file"))))
    (signals hactar-sandbox:sandbox-violation
      (hactar-sandbox:sandbox-exec '("rm" "file") :policy policy))
    (is-false (hactar-sandbox:policy-allows-p
               policy "echo ok; touch /tmp/bypassed"))
    (is (search "ok"
                (hactar-sandbox:sandbox-exec
                 '("echo" "ok") :policy policy :output :string)))))

(test engine-unknown-sandbox-fails-closed
  (signals error
    (hactar-rebuild:sandbox-check "misspelled" "echo no")))

;;; ---- hactar-tui ----
(test tui-renders-and-selects-without-dispatch
  (let ((output (make-string-output-stream)))
    (hactar-tui:with-tui (:output output)
      (hactar-tui:render "# Heading" :format :markdown)
      (is (string= "route.create"
                   (first (hactar-tui:pick
                           "rc" '("model.new" "route.create"))))))
    (is (search "HEADING" (get-output-stream-string output)))))

(test tui-hydras-compose-functions
  (hactar-tui:define-hydra
   :test "Test" (list (cons "x" (lambda () :ran))))
  (is (eq :ran (hactar-tui:run-hydra :test :key "x"))))

(test tui-loads-themes-renders-ansi-and-runs-repl
  (let ((themes-dir (merge-pathnames "data/themes/"
                                     (asdf:system-source-directory "hactar-core"))))
    (when (probe-file themes-dir)
      (let ((themes (hactar-tui:load-themes-dir themes-dir)))
        (is (>= (length themes) 6))
        (let ((dracula (hactar-tui:find-theme "dracula")))
          (is-true dracula)
          (is (string= "#ff79c6" (hactar-tui:theme-color dracula :header)))
          (let ((ansi (hactar-tui:render-markdown-ansi "# Hello" :theme dracula)))
            (is (search "Hello" ansi))
            (is (search "[38;2;255;121;198m" ansi)))))))
  (let ((out (make-string-output-stream))
        (in (make-string-input-stream (format nil "route~%2~%"))))
    (hactar-tui:with-tui (:input in :output out)
      (is (string= "route.create"
                   (hactar-tui:pick-interactive '("model.new" "route.create" "route.list"))))))
  (let ((out (make-string-output-stream))
        (in (make-string-input-stream (format nil "ping~%quit~%")))
        (seen nil))
    (hactar-tui:with-tui (:input in :output out)
      (hactar-tui:repl-loop (lambda (line) (push line seen) (concatenate 'string "pong:" line))))
    (is (equal '("ping") seen))
    (is (search "pong:ping" (get-output-stream-string out)))))

(test cli-composes-plain-and-repl-modes
  (is (eq :plain (hactar:select-interface '("--plain" "help"))))
  (is (eq :plain (hactar:select-interface '("--in-editor" "--repl"))))
  (is (eq :repl (hactar:select-interface '("--repl"))))
  (let ((out (make-string-output-stream))
        (in (make-string-input-stream (format nil "quit~%"))))
    (is (= 0 (hactar:run-cli '("--repl") :input in :output out)))))

;;; ---- engine -> fern integration ----
(test engine-ast-edit-uses-fern-for-lisp
  (with-temp-project (root)
    (let ((path (merge-pathnames "code.lisp" root)))
      (with-open-file (s path :direction :output)
        (write-string "(defun hello (x) x)" s))
      (let ((r (hactar-rebuild:run-ast-edit
                (namestring path)
                :action :rename :target :function :name "hello" :value "hi")))
        (is (eq :ok (hactar-rebuild:result-status r)))
        (is (search "hi" (uiop:read-file-string path)))
        (is (not (search "hello" (uiop:read-file-string path))))))))

;;; ---- hactar-codegen ----
(test codegen-emits-staged-vfs-plan-and-checks
  (with-temp-project (root)
    (let* ((source "(target :javascript) (in-file \"app.js\") (defun greet (name) (return name))")
           (vfs (hactar-codegen:emit source :root root :commit nil))
           (plan (hactar-vfs:vfs-plan vfs)))
      (is (= 1 (length plan)))
      (is (string= "app.js" (second (first plan))))
      (is (search "function greet(name)" (hactar-vfs:vfs-read vfs "app.js")))
      (is (null (probe-file (merge-pathnames "app.js" root))))
      (hactar-vfs:vfs-commit! vfs)
      (is-true (probe-file (merge-pathnames "app.js" root))))
    (let ((diagnostics (hactar-codegen:check-file-source
                        "(target :redwood) (a (:href \"javascript:void(0)\") \"bad\")")))
      (is (plusp (length diagnostics))))))

(test engine-emits-via-codegen-seam
  (with-temp-project (root)
    (let ((hactar-rebuild:*codegen-emitter* #'hactar-codegen:emit))
      (let ((res (hactar-rebuild:run-emit
                  "(target :json) (in-file \"pkg.json\") (:object :name \"demo\")"
                  :root root)))
        (is (eq :ok (hactar-rebuild:result-status res)))
        (is (equal '("pkg.json") (getf (hactar-rebuild:result-data res) :paths)))))))
