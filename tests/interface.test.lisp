(in-package :hactar-tests)

(def-suite interface-tests
  :description "Tests for the interface layer")

(in-suite interface-tests)

;;* Tests for the refactored interface layer (interface.lisp)
(test refactor-interface-system
  "Test defining, materializing, and syncing interfaces."
  (let ((hactar::*interfaces* (make-hash-table :test 'equal))
        (hactar::*active-interfaces* '())
        (hactar::*instance-id* "testinstance")
        (hactar::*instance-dir* nil)
        (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
        (parsed-value nil))
    (hactar::definterface :test-iface "test-file.txt"
      :format :text
      :render (lambda () "rendered-content")
      :parse (lambda (text) (setf parsed-value text)))

    (let ((iface (gethash :test-iface hactar::*interfaces*)))
      (is-true iface)
      (is (eq :test-iface (hactar::iface-name iface)))
      (is (string= "test-file.txt" (hactar::iface-path iface)))

      ;; Materialize
      (hactar::materialize-interface iface)
      (let ((path (hactar::interface-abs-path iface)))
        (is-true (probe-file path))
        (is (string= "rendered-content" (uiop:read-file-string path)))

        ;; Test sync-interface-in
        (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-string "user-content" s))
        (hactar::sync-interface-in :test-iface)
        (is (string= "user-content" parsed-value))

        ;; Test with-interface-suppressed
        (hactar::with-interface-suppressed (:test-iface)
          (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
            (write-string "suppressed-content" s)))
        (is (eql (sxhash "suppressed-content") (hactar::iface-last-hash iface)))

        ;; Clean up
        (ignore-errors (delete-file path))))))

(test refactor-interface-declares-hooks
  "An interface's :hooks drive state->file syncing without external wiring."
  (let* ((hactar::*interfaces* (make-hash-table :test 'eq))
         (hactar::*active-interfaces* '())
         (hactar::*instance-id* "hooktest")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*files* '())
         (hactar::*current-model* nil)
         (hactar::*exposed-context-file* nil)
         (hactar::*context-file-added-hook*
           (make-instance 'hactar::hook-context-file-added))
         (hactar::*context-file-dropped-hook*
           (make-instance 'hactar::hook-context-file-dropped)))
    (hactar::definterface :files "context/files.dir"
      :format :dir
      :hooks ((hactar::*context-file-added-hook* :out)
              (hactar::*context-file-dropped-hook* :out))
      :render (lambda () (hactar::render-dir-list hactar::*files*))
      :parse (lambda (text) (declare (ignore text)) nil))
    (let ((iface (gethash :files hactar::*interfaces*)))
      (hactar::materialize-interface iface)
      ;; The hook handler should be registered on the bound hook.
      (is (= 2 (length (hactar::iface-installed-handlers iface))))
      (let ((path (hactar::interface-abs-path iface)))
        (uiop:with-temporary-file (:pathname p :stream s :direction :output :keep t)
          (write-string "x" s)
          (finish-output s)
          (let ((native (uiop:native-namestring p)))
            ;; Adding to context fires the :out hook -> re-renders files.dir.
            (hactar::add-file-to-context native)
            (let ((rel (uiop:native-namestring
                        (uiop:enough-pathname native hactar::*repo-root*))))
              (is (search rel (uiop:read-file-string path))))
            ;; Dropping fires the :out hook -> file no longer lists it.
            (hactar::drop-file-from-context native)
            (let ((rel (uiop:native-namestring
                        (uiop:enough-pathname native hactar::*repo-root*))))
              (is (not (search rel (uiop:read-file-string path))))))
          (ignore-errors (delete-file p)))
        (ignore-errors (delete-file path))))))

(test refactor-interface-no-duplicate-hooks
  "Re-materializing an interface does not accumulate duplicate hook handlers."
  (let* ((hactar::*interfaces* (make-hash-table :test 'eq))
         (hactar::*active-interfaces* '())
         (hactar::*instance-id* "duptest")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*files* '())
         (hactar::*context-file-added-hook*
           (make-instance 'hactar::hook-context-file-added)))
    (hactar::definterface :files "context/files.dir"
      :format :dir
      :hooks ((hactar::*context-file-added-hook* :out))
      :render (lambda () (hactar::render-dir-list hactar::*files*))
      :parse (lambda (text) (declare (ignore text)) nil))
    (let ((iface (gethash :files hactar::*interfaces*)))
      (hactar::materialize-interface iface)
      (hactar::materialize-interface iface)
      (is (= 1 (length (hactar::iface-installed-handlers iface))))
      (is (= 1 (length (nhooks:handlers hactar::*context-file-added-hook*))))
      (ignore-errors (delete-file (hactar::interface-abs-path iface))))))

(test refactor-interface-parse-in-sync
  "File->state parsing via sync-interface-in updates *files*."
  (let* ((hactar::*interfaces* (make-hash-table :test 'eq))
         (hactar::*active-interfaces* '())
         (hactar::*instance-id* "intest")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*files* '())
         (hactar::*current-model* nil)
         (hactar::*exposed-context-file* nil)
         (hactar::*context-file-added-hook*
           (make-instance 'hactar::hook-context-file-added))
         (hactar::*context-file-dropped-hook*
           (make-instance 'hactar::hook-context-file-dropped)))
    (uiop:with-temporary-file (:pathname p :stream s :direction :output :keep t)
      (write-string "y" s)
      (finish-output s)
      (let ((native (uiop:native-namestring p)))
        (hactar::definterface :files "context/files.dir"
          :format :dir
          :render (lambda () (hactar::render-dir-list hactar::*files*))
          :parse (lambda (text)
                   (let ((wanted (hactar::parse-dir-list text)))
                     (dolist (rel wanted)
                       (let ((abs (uiop:native-namestring
                                   (merge-pathnames rel hactar::*repo-root*))))
                         (when (probe-file abs)
                           (pushnew abs hactar::*files* :test #'string=)))))))
        (let ((iface (gethash :files hactar::*interfaces*)))
          (hactar::materialize-interface iface)
          (let ((path (hactar::interface-abs-path iface))
                (rel (uiop:native-namestring
                      (uiop:enough-pathname native hactar::*repo-root*))))
            ;; Simulate an external edit listing the file.
            (with-open-file (out path :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
              (format out "# files~%~A~%" rel))
            (hactar::sync-interface-in :files)
            (is (member native hactar::*files* :test #'string=)))
          (ignore-errors (delete-file (hactar::interface-abs-path iface)))))
      (ignore-errors (delete-file p)))))

(test refactor-org-aggregate-interface
  "An :org aggregate interface renders headlines from state and parses them back."
  (let* ((hactar::*interfaces* (make-hash-table :test 'eq))
         (hactar::*active-interfaces* '())
         (hactar::*instance-id* "orgtest")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (state (list :a "1")))
    (hactar::definterface :agg "agg.org"
      :format :org
      :title "Aggregate"
      :watch nil
      :sections
      ((:id "sec-a" :title "Section A"
        :render (lambda () (format nil "- value: ~A" (getf state :a)))
        :parse (lambda (h)
                 (let ((body (hactar::%heading-body-string h)))
                   (when (search "value: 9" body)
                     (setf (getf state :a) "9")))))))
    (let ((iface (gethash :agg hactar::*interfaces*)))
      (hactar::materialize-interface iface)
      (let ((path (hactar::interface-abs-path iface)))
        (is (search ":ID: sec-a" (uiop:read-file-string path)))
        (is (search "value: 1" (uiop:read-file-string path)))
        ;; Simulate an external edit of the headline body.
        (with-open-file (out path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
          (format out "* Section A~%:PROPERTIES:~%:ID: sec-a~%:END:~%- value: 9~%"))
        (hactar::sync-interface-in :agg)
        (is (string= "9" (getf state :a)))
        (ignore-errors (delete-file path))))))

;;* Helpers

(defun %iface-test-heading (text id)
  "Parse TEXT and return the heading with :ID: equal to ID."
  (org-mode-parser:find-heading-by-custom-id
   (org-mode-parser:parse-org-string text) id))

;;* Instance dir

(test refactor-instance-id-and-dir
  "instance-id defaults to a string and instance-dir is created."
  (let ((hactar::*instance-id* nil)
        (hactar::*instance-dir* nil)
        (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory))))
    (is (stringp (hactar::instance-id)))
    ;; memoized
    (is (string= (hactar::instance-id) (hactar::instance-id)))
    (let ((d (hactar::instance-dir)))
      (is (uiop:directory-exists-p d))
      (is (search ".hactar" (namestring d))))))

(test refactor-instance-id-explicit
  "An explicitly set instance-id is used verbatim."
  (let ((hactar::*instance-id* "myinstance"))
    (is (string= "myinstance" (hactar::instance-id)))))

;;* Format helpers

(test refactor-render-dir-list
  "render-dir-list renders repo-relative paths with a header."
  (let ((hactar::*repo-root* #P"/repo/"))
    (let ((out (hactar::render-dir-list '("/repo/src/a.lisp" "/repo/b.lisp"))))
      (is (search "# hactar files" out))
      (is (search "src/a.lisp" out))
      (is (search "b.lisp" out)))))

(test refactor-render-dir-list-custom
  "render-dir-list honours :base and :header."
  (let ((out (hactar::render-dir-list '("/x/y/z.txt")
                                      :base #P"/x/" :header "# custom")))
    (is (search "# custom" out))
    (is (search "y/z.txt" out))))

(test refactor-parse-dir-list
  "parse-dir-list strips blank and comment lines and trims entries."
  (let ((parsed (hactar::parse-dir-list
                 (format nil "# comment~%~%src/a.lisp~%  src/b.lisp  ~%"))))
    (is (equal '("src/a.lisp" "src/b.lisp") parsed))))

;;* render/parse dispatch

(test refactor-iface-render-apply-dispatch
  "%iface-render-content and %iface-apply-in dispatch on render/parse/on-change."
  (let ((render-iface (hactar::make-iface :name :r :path "r.txt"
                                          :render (lambda () "RC")))
        (parsed nil))
    (is (string= "RC" (hactar::%iface-render-content render-iface)))
    (let ((parse-iface (hactar::make-iface :name :p :path "p.txt"
                                           :parse (lambda (txt) (setf parsed txt)))))
      (hactar::%iface-apply-in parse-iface "hello")
      (is (string= "hello" parsed)))
    (let ((onchange-iface (hactar::make-iface
                           :name :o :path "o.txt"
                           :on-change (lambda (txt)
                                        (setf parsed (concatenate 'string "oc:" txt))))))
      (hactar::%iface-apply-in onchange-iface "z")
      (is (string= "oc:z" parsed)))
    (let ((empty-iface (hactar::make-iface :name :e :path "e.txt")))
      (is (string= "" (hactar::%iface-render-content empty-iface))))))

;;* org aggregate helpers

(test refactor-render-parse-org-aggregate-direct
  "render-org-aggregate emits headlines; parse-org-aggregate dispatches to sections."
  (let* ((captured nil)
         (iface (hactar::make-iface
                 :name :direct :path "d.org" :format :org :title "T"
                 :sections (list
                            (hactar::make-org-section
                             :id "s1" :title "S1"
                             :render (lambda () "- one")
                             :parse (lambda (h)
                                      (setf captured (hactar::%heading-body-string h))))))))
    (let ((out (hactar::render-org-aggregate iface)))
      (is (search "#+TITLE: T" out))
      (is (search ":ID: s1" out))
      (is (search "- one" out)))
    (hactar::parse-org-aggregate
     iface (format nil "* S1~%:PROPERTIES:~%:ID: s1~%:END:~%- changed~%"))
    (is (search "changed" captured))))

(test refactor-heading-body-string
  "%heading-body-string serializes body without the properties drawer."
  (let ((h (%iface-test-heading
            (format nil "* Title~%:PROPERTIES:~%:ID: x~%:END:~%- a~%- b~%") "x")))
    (let ((body (hactar::%heading-body-string h)))
      (is (search "- a" body))
      (is (search "- b" body))
      (is (not (search ":ID:" body))))))

(test refactor-org-list-items
  "%org-list-items collects '- ' list entries."
  (is (equal '("Name: X" "Author: Y")
             (hactar::%org-list-items
              (format nil "text~%- Name: X~%- Author: Y~%more")))))

;;* context-org section render/parse

(test refactor-render-project-details-body
  "render-project-details-body includes all project fields."
  (let ((hactar::*name* "P") (hactar::*author* "A") (hactar::*language* "lisp")
        (hactar::*shell* "bash") (hactar::*stack* '("x" "y")))
    (let ((b (hactar::render-project-details-body)))
      (is (search "Name: P" b))
      (is (search "Author: A" b))
      (is (search "Language: lisp" b))
      (is (search "Stack: x, y" b)))))

(test refactor-parse-project-details-heading
  "parse-project-details-heading sets project variables from list items."
  (let ((hactar::*name* nil) (hactar::*author* nil) (hactar::*language* nil)
        (hactar::*shell* nil) (hactar::*stack* nil)
        (hactar::*exposed-context-file* nil)
        (hactar::*context-variable-changed-hook*
          (make-instance 'hactar::hook-context-variable-changed)))
    (hactar::parse-project-details-heading
     (%iface-test-heading
      (format nil "* Project Details~%:PROPERTIES:~%:ID: project-details~%:END:~%~
                   - Name: NewName~%- Author: NewAuthor~%- Language: lisp~%~
                   - Shell: zsh~%- Stack: a, b, c~%")
      "project-details"))
    (is (string= "NewName" hactar::*name*))
    (is (string= "NewAuthor" hactar::*author*))
    (is (string= "lisp" hactar::*language*))
    (is (string= "zsh" hactar::*shell*))
    (is (equal '("a" "b" "c") hactar::*stack*))))

(test refactor-render-ctx-files-body
  "render-ctx-files-body renders relative file paths, empty when none."
  (let ((hactar::*repo-root* #P"/repo/")
        (hactar::*files* '("/repo/a.lisp")))
    (is (search "- a.lisp" (hactar::render-ctx-files-body))))
  (let ((hactar::*files* nil))
    (is (string= "" (hactar::render-ctx-files-body)))))

(test refactor-parse-ctx-files-heading
  "parse-ctx-files-heading adds listed files to *files*."
  (uiop:with-temporary-file (:pathname p :stream s :direction :output :keep t)
    (write-string "x" s) (finish-output s)
    (let* ((hactar::*repo-root* (uiop:pathname-directory-pathname p))
           (rel (uiop:native-namestring (uiop:enough-pathname p hactar::*repo-root*)))
           (hactar::*files* nil)
           (hactar::*current-model* nil)
           (hactar::*exposed-context-file* nil)
           (hactar::*litmode-active* nil)
           (hactar::*context-file-added-hook*
             (make-instance 'hactar::hook-context-file-added))
           (hactar::*context-file-dropped-hook*
             (make-instance 'hactar::hook-context-file-dropped)))
      (hactar::parse-ctx-files-heading
       (%iface-test-heading
        (format nil "* Files~%:PROPERTIES:~%:ID: ctx-files~%:END:~%- ~A~%" rel)
        "ctx-files"))
      (is (member (uiop:native-namestring p) hactar::*files* :test #'string=)))
    (ignore-errors (delete-file p))))

(test refactor-render-ctx-docs-body
  "render-ctx-docs-body lists doc titles."
  (let ((hactar::*docs-context* '(((:title . "Doc1")) ((:title . "Doc2")))))
    (let ((b (hactar::render-ctx-docs-body)))
      (is (search "- Doc1" b))
      (is (search "- Doc2" b))))
  (let ((hactar::*docs-context* nil))
    (is (string= "" (hactar::render-ctx-docs-body)))))

(test refactor-parse-ctx-docs-heading
  "parse-ctx-docs-heading drops docs not present in the headline."
  (let ((hactar::*docs-context* '(((:id . 1) (:title . "Keep"))
                                  ((:id . 2) (:title . "Remove"))))
        (hactar::*exposed-context-file* nil))
    (hactar::parse-ctx-docs-heading
     (%iface-test-heading
      (format nil "* Documentation~%:PROPERTIES:~%:ID: ctx-docs~%:END:~%- Keep~%")
      "ctx-docs"))
    (is (= 1 (length hactar::*docs-context*)))
    (is (string= "Keep" (cdr (assoc :title (first hactar::*docs-context*)))))))

(test refactor-render-ctx-errors-body
  "render-ctx-errors-body lists 'code: title' lines."
  (let ((hactar::*errors-context* '(((:code . "E1") (:title . "Err1")))))
    (is (search "E1: Err1" (hactar::render-ctx-errors-body))))
  (let ((hactar::*errors-context* nil))
    (is (string= "" (hactar::render-ctx-errors-body)))))

(test refactor-parse-ctx-errors-heading
  "parse-ctx-errors-heading drops errors not present in the headline."
  (let ((hactar::*errors-context* '(((:code . "E1") (:title . "Keep"))
                                    ((:code . "E2") (:title . "Drop"))))
        (hactar::*exposed-context-file* nil))
    (hactar::parse-ctx-errors-heading
     (%iface-test-heading
      (format nil "* Errors~%:PROPERTIES:~%:ID: ctx-errors~%:END:~%- E1: Keep~%")
      "ctx-errors"))
    (is (= 1 (length hactar::*errors-context*)))
    (is (string= "E1" (cdr (assoc :code (first hactar::*errors-context*)))))))

;;* hooks: :in direction, uninstall

(test refactor-interface-hook-in
  "A :in hook re-parses the file into state when fired."
  (let* ((hactar::*interfaces* (make-hash-table :test 'eq))
         (hactar::*active-interfaces* '())
         (hactar::*instance-id* "inhook")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (parsed nil)
         (hactar::*context-file-added-hook*
           (make-instance 'hactar::hook-context-file-added)))
    (hactar::definterface :inh "inh.txt"
      :watch nil
      :hooks ((hactar::*context-file-added-hook* :in))
      :render (lambda () "r")
      :parse (lambda (text) (setf parsed text)))
    (let ((iface (gethash :inh hactar::*interfaces*)))
      (hactar::materialize-interface iface)
      (let ((path (hactar::interface-abs-path iface)))
        (with-open-file (s path :direction :output
                                :if-exists :supersede :if-does-not-exist :create)
          (write-string "external" s))
        (nhooks:run-hook hactar::*context-file-added-hook* "x")
        (is (string= "external" parsed))
        (ignore-errors (delete-file path))))))

(test refactor-interface-uninstall-hooks
  "uninstall-interface-hooks removes installed handlers from the hook."
  (let* ((hactar::*interfaces* (make-hash-table :test 'eq))
         (hactar::*active-interfaces* '())
         (hactar::*instance-id* "unhook")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*files* '())
         (hactar::*context-file-added-hook*
           (make-instance 'hactar::hook-context-file-added)))
    (hactar::definterface :unh "unh.dir"
      :format :dir :watch nil
      :hooks ((hactar::*context-file-added-hook* :out))
      :render (lambda () "x"))
    (let ((iface (gethash :unh hactar::*interfaces*)))
      (hactar::materialize-interface iface)
      (is (= 1 (length (hactar::iface-installed-handlers iface))))
      (is (= 1 (length (nhooks:handlers hactar::*context-file-added-hook*))))
      (hactar::uninstall-interface-hooks iface)
      (is (= 0 (length (hactar::iface-installed-handlers iface))))
      (is (= 0 (length (nhooks:handlers hactar::*context-file-added-hook*))))
      (ignore-errors (delete-file (hactar::interface-abs-path iface))))))

(test tui-path-completions-basic
  "TUI path completion returns matching repo-relative paths."
  (let* ((root (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*repo-root* root))
    (ensure-directories-exist (merge-pathnames "src/" root))
    (with-open-file (s (merge-pathnames "src/main.lisp" root)
                       :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string "x" s))
    (let ((matches (hactar::tui-path-completions "src/ma")))
      (is (member "src/main.lisp" matches :test #'string=)))
    (ignore-errors (delete-file (merge-pathnames "src/main.lisp" root)))))

(test refactor-history-interface
  "Test defining, materializing, rendering, and parsing the history interface."
  (let* ((hactar::*interfaces* (make-hash-table :test 'eq))
         (hactar::*active-interfaces* '())
         (hactar::*instance-id* "history-test")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*chat-history* '(((:role . "user") (:content "hello"))))
         (hactar::*history-changed-hook* (make-instance 'hactar::hook-history-changed)))
    (hactar::definterface :history "history.lisp"
      :format :lisp
      :hooks ((hactar::*history-changed-hook* :out))
      :render (lambda ()
                (format nil ";;; Chat history~%(in-package :hactar)~%~%(setf *chat-history* '~S)~%"
                        hactar::*chat-history*))
      :parse (lambda (text)
               (declare (ignore text))
               (let ((iface (gethash :history hactar::*interfaces*)))
                 (when iface
                   (ignore-errors (load (hactar::interface-abs-path iface)))))))
    (let ((iface (gethash :history hactar::*interfaces*)))
      (hactar::materialize-interface iface)
      (let ((path (hactar::interface-abs-path iface)))
        (is-true (probe-file path))
        (is (search "hello" (uiop:read-file-string path)))

        ;; Test hook triggers out sync
        (setf hactar::*chat-history* '(((:role . "user") (:content "new msg"))))
        (nhooks:run-hook hactar::*history-changed-hook*)
        (is (search "new msg" (uiop:read-file-string path)))

        ;; Test parse syncs in
        (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
          (format out ";;; Edited~%(in-package :hactar)~%~%(setf *chat-history* '(((:role . \"user\") (:content . \"external edit\"))))~%"))
        (hactar::sync-interface-in :history)
        (is (equal '(((:role . "user") (:content . "external edit"))) hactar::*chat-history*))

        ;; Clean up
        (ignore-errors (delete-file path))))))

(test docs-interface-render-and-parse
  "Test rendering and parsing the documentation context interface."
  (let* ((hactar::*docs-context* '())
         (hactar::*docs* '())
         (hactar::*interfaces* (make-hash-table :test 'eq))
         (hactar::*active-interfaces* '())
         (hactar::*instance-id* "docs-test")
         (hactar::*instance-dir* nil)
         (hactar::*repo-root* (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (hactar::*context-doc-added-hook* (make-instance 'hactar::hook-context-doc-added))
         (hactar::*context-doc-dropped-hook* (make-instance 'hactar::hook-context-doc-dropped))
         (doc-a '((:id . "react") (:uri . "npm:react@19.2") (:title . "React")))
         (doc-b '((:id . "lisp") (:uri . "cl:lisp") (:title . "Common Lisp"))))

    ;; 1. Test render-docs-list
    (setf hactar::*docs-context* (list doc-a doc-b))
    (let ((rendered (hactar::render-docs-list hactar::*docs-context*)))
      (is-true (search "npm:react@19.2" rendered))
      (is-true (search "cl:lisp" rendered)))

    ;; 2. Test parse-docs-list (adding already known docs)
    (setf hactar::*docs-context* '())
    (setf hactar::*docs* (list doc-a doc-b))
    (hactar::parse-docs-list (format nil "npm:react@19.2~%cl:lisp~%"))
    (is (= 2 (length hactar::*docs-context*)))
    (is-true (member "react" hactar::*docs-context* :key (lambda (d) (cdr (assoc :id d))) :test #'string=))
    (is-true (member "lisp" hactar::*docs-context* :key (lambda (d) (cdr (assoc :id d))) :test #'string=))

    ;; 3. Test parse-docs-list (removing docs)
    (hactar::parse-docs-list (format nil "npm:react@19.2~%"))
    (is (= 1 (length hactar::*docs-context*)))
    (is-true (member "react" hactar::*docs-context* :key (lambda (d) (cdr (assoc :id d))) :test #'string=))
    (is-false (member "lisp" hactar::*docs-context* :key (lambda (d) (cdr (assoc :id d))) :test #'string=))

    ;; 4. Test parse-docs-list (triggering import for unknown doc)
    (let ((imported-uri nil)
          (orig-execute-import (fdefinition 'hactar::execute-import)))
      (unwind-protect
           (progn
             (setf (fdefinition 'hactar::execute-import)
                   (lambda (uri &key tags covers meta)
                     (declare (ignore tags covers meta))
                     (setf imported-uri uri)
                     (push '((:id . "vue") (:uri . "npm:vue@3.0") (:title . "Vue")) hactar::*docs*)))
             (hactar::parse-docs-list (format nil "npm:react@19.2~%npm:vue@3.0~%"))
             (is (string= "npm:vue@3.0" imported-uri))
             (is (= 2 (length hactar::*docs-context*)))
             (is-true (member "vue" hactar::*docs-context* :key (lambda (d) (cdr (assoc :id d))) :test #'string=)))
        (setf (fdefinition 'hactar::execute-import) orig-execute-import)))

    ;; 5. Test materializing and syncing via the definterface :docs
    (hactar::definterface :docs "context/docs.org"
      :format :text
      :hooks ((hactar::*context-doc-added-hook* :out)
              (hactar::*context-doc-dropped-hook* :out))
      :render (lambda () (hactar::render-docs-list hactar::*docs-context*))
      :parse #'hactar::parse-docs-list)
    (let ((iface (gethash :docs hactar::*interfaces*)))
      (is-true iface)
      (hactar::materialize-interface iface)
      (let ((path (hactar::interface-abs-path iface)))
        (is-true (probe-file path))
        (is-true (search "npm:react@19.2" (uiop:read-file-string path)))

        ;; Test out hook syncs
        (setf hactar::*docs-context* (list doc-a doc-b))
        (nhooks:run-hook hactar::*context-doc-added-hook* doc-b)
        (is-true (search "cl:lisp" (uiop:read-file-string path)))

        ;; Clean up
        (ignore-errors (delete-file path))))))
