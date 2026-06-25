(in-package :hactar-tests)

(def-suite project-cli-tests
  :description "Tests for the generated per-project CLI (project-cli.lisp).")

(in-suite project-cli-tests)

(defun %make-tal-fixture ()
  "Create a temporary Tal/RedwoodSDK-like project and return its directory
pathname."
  (let* ((dir (uiop:ensure-directory-pathname (make-temp-dir))))
    (ensure-directories-exist (merge-pathnames "src/app/pages/" dir))
    (ensure-directories-exist (merge-pathnames "prisma/" dir))
    (hactar::write-file-content
     (uiop:native-namestring (merge-pathnames "package.json" dir))
     "{ \"name\": \"tal\",
        \"scripts\": { \"build\": \"vite build\", \"deploy\": \"wrangler deploy\" },
        \"dependencies\": { \"rwsdk\": \"^0.1.0\", \"react\": \"^19.0.0\" },
        \"devDependencies\": { \"typescript\": \"^5.0.0\", \"prisma\": \"^6.0.0\" } }")
    (hactar::write-file-content
     (uiop:native-namestring (merge-pathnames "prisma/schema.prisma" dir))
     "model User {
  id    Int    @id @default(autoincrement())
  email String @unique
}
model Game {
  id  Int    @id @default(autoincrement())
  pgn String
}")
    (hactar::write-file-content
     (uiop:native-namestring (merge-pathnames "src/app/pages/Home.tsx" dir))
     "// @tags: home, public
export function Home() { return null; }")
    (hactar::write-file-content
     (uiop:native-namestring (merge-pathnames "src/worker.tsx" dir))
     "// @tags: auth
import { route } from 'rwsdk/router';")
    dir))

(defmacro with-tal-fixture ((dir-var) &body body)
  "Bind DIR-VAR to a fresh Tal fixture with *repo-root* set, run BODY silently,
then reset project CLI state."
  `(let* ((,dir-var (%make-tal-fixture))
          (hactar::*repo-root* ,dir-var)
          (hactar::*silent* t)
          (hactar::*project-clis* (make-hash-table :test 'equal))
          (hactar::*stack* '()))
     ,@body))

(test detect-stack-test
  "Stack detection reads package.json deps and the Prisma schema."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (let ((stack (hactar::pcli-detect-stack)))
      (is-true (member "redwood" stack :test #'string-equal))
      (is-true (member "react" stack :test #'string-equal))
      (is-true (member "typescript" stack :test #'string-equal))
      (is-true (member "prisma" stack :test #'string-equal)))))

(test project-name-test
  "Project name is derived from package.json name."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (is (string= "tal" (hactar::pcli-project-name)))))

(test generate-and-register-test
  "Generating the file registers a project CLI and a top-level sub-command."
  (with-tal-fixture (dir)
    (let ((file (hactar::generate-project-cli-file :load t)))
      (is-true (probe-file file))
      (is-true (hactar::get-project-cli "tal"))
      (is-true (gethash "tal" hactar::*sub-commands*))
      ;; Generated file contains region markers.
      (let ((content (uiop:read-file-string file)))
        (is-true (search "BEGIN GENERATED: meta" content))
        (is-true (search "BEGIN GENERATED: routes" content))
        (is-true (search hactar::*pcli-user-region-marker* content))))))

(test command-catalogue-test
  "All expected commands are registered after generation."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (hactar::generate-project-cli-file :load t)
    (let ((cmds (hactar::project-cli-commands (hactar::get-project-cli "tal"))))
      (dolist (name '("routes.list" "routes.create" "routes.gen"
                      "models.list" "schema" "feature" "build" "release"))
        (is-true (gethash name cmds) "command ~A should be registered" name)))))

(test models-and-schema-test
  "models.list and schema parse the Prisma schema."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (hactar::generate-project-cli-file :load t)
    (let ((models-out
            (with-output-to-string (*standard-output*)
              (hactar::dispatch-project-command "tal" (list "models.list")))))
      (is-true (search "User" models-out))
      (is-true (search "Game" models-out)))
    (let ((schema-out
            (with-output-to-string (*standard-output*)
              (hactar::dispatch-project-command "tal" (list "schema" "User")))))
      (is-true (search "model User" schema-out))
      (is-true (search "email" schema-out)))))

(test routes-list-and-tag-filter-test
  "routes.list lists route files and filters by tag."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (hactar::generate-project-cli-file :load t)
    (let ((all (hactar::pcli-routes-list "tal" '())))
      (is-true (some (lambda (f) (search "Home.tsx" f)) all))
      (is-true (some (lambda (f) (search "worker.tsx" f)) all)))
    (let ((auth (hactar::pcli-routes-list "tal" (list "tag" "auth"))))
      (is (= 1 (length auth)))
      (is-true (search "worker.tsx" (first auth))))))

(test routes-list-md-and-org-test
  "routes.list emits markdown and org output."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (hactar::generate-project-cli-file :load t)
    (let ((md (with-output-to-string (*standard-output*)
                (hactar::dispatch-project-command "tal" (list "routes.list" "--md")))))
      (is-true (search "# Routes" md))
      (is-true (search "```" md)))
    (let ((org (with-output-to-string (*standard-output*)
                 (hactar::dispatch-project-command "tal" (list "routes.list" "--org")))))
      (is-true (search "#+TITLE: Routes" org))
      (is-true (search "#+begin_src" org)))))

(test user-region-preserved-test
  "Regeneration preserves hand-written content in the user region."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (let ((file (hactar::generate-project-cli-file :load nil)))
      (with-open-file (s file :direction :output :if-exists :append
                              :if-does-not-exist :create)
        (format s "~%(define-project-command \"tal\" hello (args)~%  (declare (ignore args))~%  (format t \"hi\"))~%"))
      ;; Regenerate; the custom command must survive.
      (hactar::generate-project-cli-file :load nil)
      (let ((content (uiop:read-file-string file)))
        (is-true (search "\"tal\" hello" content))))))

(test flag-helpers-test
  "Flag parsing helpers extract values and strip flags."
  (is (string= "kings"
               (hactar::pcli-flag-value '("king" "--path" "kings") "--path")))
  (is-true (hactar::pcli-flag-present-p '("a" "--md") "--md"))
  (is (equal '("king")
             (hactar::pcli-positional-args '("king" "--path" "kings")))))

(test diffusion-relevance-test
  "Diffusion analyzer only triggers when enabled and for relevant files."
  (let ((hactar::*auto-project-cli* t)
        (hactar::*diffusion-model* "fast-diffusion"))
    (is-true (hactar::pcli-diffusion-relevant-p "src/app/pages/Foo.tsx" :file-changed))
    (is-true (hactar::pcli-diffusion-relevant-p "prisma/schema.prisma" :file-changed))
    (is-false (hactar::pcli-diffusion-relevant-p "README.md" :file-changed))
    (is-false (hactar::pcli-diffusion-relevant-p ".hactar.lisp" :file-changed)))
  (let ((hactar::*auto-project-cli* nil)
        (hactar::*diffusion-model* "fast-diffusion"))
    (is-false (hactar::pcli-diffusion-relevant-p "src/app/pages/Foo.tsx" :file-changed))))
