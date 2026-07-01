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
