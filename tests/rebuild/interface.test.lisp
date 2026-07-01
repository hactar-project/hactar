(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test files-dir-add-drop-and-roundtrip
  (setf hactar-rebuild:*context-files* '())
  (hactar-rebuild:context-add-file "src/a.ts" "entry")
  (hactar-rebuild:context-add-file "src/b.ts")
  (hactar-rebuild:context-add-file "src/a.ts")  ; idempotent
  (is (= 2 (length hactar-rebuild:*context-files*)))
  (let ((manifest (hactar-rebuild:render-context-files)))
    (setf hactar-rebuild:*context-files* '())
    (hactar-rebuild:parse-context-files manifest)
    (is (= 2 (length hactar-rebuild:*context-files*)))
    (is (string= "src/a.ts" (car (first hactar-rebuild:*context-files*))))
    (is (string= "entry" (cdr (first hactar-rebuild:*context-files*))))
    (is (null (cdr (second hactar-rebuild:*context-files*)))))
  (hactar-rebuild:context-drop-file "src/a.ts")
  (is (= 1 (length hactar-rebuild:*context-files*))))

(test files-dir-writes-and-reads-from-disk
  (with-temp-project (root)
    (setf hactar-rebuild:*context-files* '())
    (hactar-rebuild:context-add-file "README.md")
    (hactar-rebuild:write-interface 'files.dir root)
    (is (probe-file (merge-pathnames ".hactar/context.files" root)))
    (setf hactar-rebuild:*context-files* '())
    (hactar-rebuild:read-interface 'files.dir root)
    (is (member "README.md" hactar-rebuild:*context-files*
                :key #'car :test #'string=))))

(test org-interface-render-has-sections
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"react-router\":\"7\"}}")))
    (hactar-rebuild:bootstrap root)
    (hactar-rebuild:load-hactar-file (hactar-rebuild:hactar-file-path root))
    (let ((org (hactar-rebuild:render-interface 'org-interface)))
      (is (search "#+title: Hactar Context" org))
      (is (search "* Generated Harness" org))
      (is (search "#+begin_src lisp" org))
      (is (search "deftool" org)))))

(test org-interface-parse-validates-src-block
  (let ((org (format nil "* H~%~%#+begin_src lisp~%~
                          (defproject :stack (\"ts\"))~%~
                          (deftool t1 () \"d\" 1)~%#+end_src~%")))
    (multiple-value-bind (ok forms) (hactar-rebuild:parse-context-org org)
      (is-true ok)
      (is (= 2 (length forms))))))

(test org-interface-parse-rejects-out-of-surface
  (let ((org (format nil "#+begin_src lisp~%(defun sneaky () 1)~%#+end_src~%")))
    (multiple-value-bind (ok errs) (hactar-rebuild:parse-context-org org)
      (is-false ok)
      (is (find-if (lambda (e) (search "surface" e)) errs)))))

(test shell-exec-feeds-update-ledger
  (with-temp-project (root)
    (let ((hactar-rebuild::*dry-run-shell* t)
          (hactar-rebuild:*update-ledger* '()))
      (eval-in-user "(defsandbox project :allow (\"echo\"))")
      (let ((r (hactar-rebuild:shell-exec "echo hi" :sandbox "project")))
        (is (eq :ok (hactar-rebuild:result-status r)))
        (is (= 1 (length hactar-rebuild:*update-ledger*)))
        (is (eq :shell (getf (first hactar-rebuild:*update-ledger*) :kind)))))))

(test sync-interfaces-writes-files
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"react-router\":\"7\"}}")))
    (hactar-rebuild:bootstrap root)
    (setf hactar-rebuild:*context-files* '())
    (let ((written (hactar-rebuild:sync-interfaces root)))
      (is (member "files.dir" written :test #'string=))
      (is (member "org-interface" written :test #'string=))
      (is (probe-file (merge-pathnames ".hactar/context.org" root)))
      (is (probe-file (merge-pathnames ".hactar/context.files" root))))))

(test turn-after-hook-syncs-interfaces-to-disk
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"react-router\":\"7\"}}")))
    (hactar-rebuild:bootstrap root)
    (hactar-rebuild:load-hactar-file (hactar-rebuild:hactar-file-path root))
    ;; isolate interface sync from self-update
    (let ((hactar-rebuild:*auto-update* nil))
      (let ((org (merge-pathnames ".hactar/context.org" root)))
        (when (probe-file org) (delete-file org))
        (hactar-rebuild:run-hook 'turn.after '("dev") (hactar-rebuild:ok "x"))
        (is (probe-file org))))))

(test turn-after-hook-survives-reload-and-runs-self-update
  ;; After a reload (reset-registries), turn.after must still exist with a
  ;; handler (proving engine-owned hooks are reinstalled post-reset).
  (with-temp-project (root)
    (hactar-rebuild:write-hactar-file
     (hactar-rebuild:hactar-file-path root)
     (engine-forms "(defproject :name \"x\" :stack (\"ts\"))"))
    (hactar-rebuild:load-hactar-file (hactar-rebuild:hactar-file-path root))
    (is (gethash "turn.after" hactar-rebuild::*hooks*))
    (is (plusp (length (hactar-rebuild::hook-def-handlers
                        (gethash "turn.after" hactar-rebuild::*hooks*)))))))
