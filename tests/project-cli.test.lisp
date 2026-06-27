(in-package :hactar-tests)

(def-suite project-cli-tests
  :description "Tests for the generated per-project data map (project-cli.lisp).")

(in-suite project-cli-tests)

(defun %make-tal-fixture ()
  "Create a temporary Tal/RedwoodSDK-like project and return its directory
pathname. worker.tsx contains multiple route() calls to exercise line-range
extraction; Home.tsx is a file-based route with tags."
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
import { route } from 'rwsdk/router';
export default defineApp([
  route('/login', LoginHandler),
  route('/logout', LogoutHandler),
]);")
    dir))

(defmacro with-tal-fixture ((dir-var) &body body)
  "Bind DIR-VAR to a fresh Tal fixture with *repo-root* set, run BODY silently,
then reset project CLI state."
  `(let* ((,dir-var (%make-tal-fixture))
          (hactar::*repo-root* ,dir-var)
          (hactar::*name* nil)
          (hactar::*silent* t)
          (hactar::*project-clis* (make-hash-table :test 'equal))
          (hactar::*pcli-current-project* nil)
          (hactar::*stack* '()))
     ,@body))

(test parse-loc-test
  "parse-loc splits path and a 1-based inclusive line range."
  (multiple-value-bind (path start end) (hactar::parse-loc "src/worker.tsx:222-555")
    (is (string= "src/worker.tsx" path))
    (is (= 222 start))
    (is (= 555 end)))
  (multiple-value-bind (path start end) (hactar::parse-loc "src/routes/login.tsx")
    (is (string= "src/routes/login.tsx" path))
    (is (null start))
    (is (null end))))

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

(test extract-routes-line-range-test
  "extract-routes emits per-call entries with line ranges for files with
multiple route() calls, and a whole-file entry for file-based routes."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (let* ((routes (hactar::extract-routes))
           (login (find "/login" routes :key #'hactar::route-entry-path :test #'equal))
           (home (find-if (lambda (r) (search "Home.tsx" (hactar::route-entry-file r)))
                          routes)))
      (is-true login "a /login route should be extracted")
      (is-true (search "src/worker.tsx:" (hactar::route-entry-file login))
               "embedded route should carry a line range")
      (is-true (hactar::route-entry-line-start login))
      (is-true (member (hactar::pcli-as-symbol "auth")
                       (hactar::route-entry-tags login)))
      (is-true home "Home.tsx file-based route should be extracted")
      (is (null (hactar::route-entry-line-start home))
          "file-based route uses whole-file LOC"))))

(test generate-and-register-test
  "Generating the file writes data regions and registers a project CLI."
  (with-tal-fixture (dir)
    (let ((file (hactar::generate-project-cli-file :load t)))
      (is-true (probe-file file))
      (is-true (hactar::get-project-cli "tal"))
      (is-true (gethash "tal" hactar::*sub-commands*))
      (let ((content (uiop:read-file-string file)))
        (is-true (search "BEGIN GENERATED: meta" content))
        (is-true (search "BEGIN GENERATED: routes" content))
        (is-true (search "BEGIN GENERATED: models" content))
        (is-true (search "(defproject" content))
        (is-true (search "(defroutes" content))
        (is-true (search "(defmodels" content))
        (is-true (search hactar::*pcli-user-region-marker* content)))
      ;; Loading the file populated the in-memory model.
      (is-true (plusp (length (hactar::project-routes "tal"))))
      (is-true (plusp (length (hactar::project-models "tal")))))))

(test command-catalogue-test
  "The default engine command set is registered after generation."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (hactar::generate-project-cli-file :load t)
    (let ((cmds (hactar::project-cli-commands (hactar::get-project-cli "tal"))))
      (dolist (name '("routes.list" "routes.search" "routes.add" "routes.gen"
                      "models.list" "schema" "feature" "build" "release"))
        (is-true (gethash name cmds) "command ~A should be registered" name)))))

(test models-and-schema-test
  "models.list and schema read the model map / source spans."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (hactar::generate-project-cli-file :load t)
    (let ((models-out
            (with-output-to-string (*standard-output*)
              (hactar::dispatch-project-command "tal" (list "models.list")))))
      (is-true (search "user" (string-downcase models-out)))
      (is-true (search "game" (string-downcase models-out))))
    (let ((schema-out
            (with-output-to-string (*standard-output*)
              (hactar::dispatch-project-command "tal" (list "schema" "User")))))
      (is-true (search "model User" schema-out))
      (is-true (search "email" schema-out)))))

(test routes-list-and-tag-filter-test
  "routes.list returns route-entries from the map and filters by tag."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (hactar::generate-project-cli-file :load t)
    (let ((all (hactar::pcli-cmd-routes-list "tal" '())))
      (is-true (every #'hactar::route-entry-p all))
      (is-true (some (lambda (r) (search "worker.tsx" (hactar::route-entry-file r))) all)))
    (let ((auth (hactar::pcli-cmd-routes-list "tal" (list "tag" "auth"))))
      ;; worker.tsx has two route() calls, both tagged auth.
      (is (= 2 (length auth)))
      (is-true (every (lambda (r) (search "worker.tsx" (hactar::route-entry-file r))) auth)))))

(test routes-search-test
  "routes.search filters by path/name/tag substring over the map."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (hactar::generate-project-cli-file :load t)
    (let ((hits (hactar::pcli-cmd-routes-search "tal" (list "logout"))))
      (is (= 1 (length hits)))
      (is (string= "/logout" (hactar::route-entry-path (first hits)))))))

(test routes-list-md-and-org-test
  "routes.list emits markdown and org output reading source spans."
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

(test routes-add-mutates-map-and-file-test
  "routes.add scaffolds a component, adds an entry to the map, and
re-serializes the defroutes region so the new route survives a reload."
  (with-tal-fixture (dir)
    (let ((file (hactar::generate-project-cli-file :load t))
          (before (length (hactar::project-routes "tal"))))
      (hactar::dispatch-project-command
       "tal" (list "routes.add" "settings" "--path" "/settings" "--tags" "auth"))
      ;; In-memory map gained the route.
      (is (= (1+ before) (length (hactar::project-routes "tal"))))
      (is-true (hactar::find-route "tal" :path "/settings"))
      ;; The file's defroutes region was rewritten with the new entry.
      (let ((content (uiop:read-file-string file)))
        (is-true (search "/settings" content)))
      ;; A fresh reload (clearing the map first) rebuilds the route from file.
      (setf (hactar::project-cli-routes (hactar::get-project-cli "tal")) '())
      (hactar::load-project-cli-file dir)
      (is-true (hactar::find-route "tal" :path "/settings")))))

(test round-trip-fixpoint-test
  "Generation is a fixpoint: generate, then regenerate, yields byte-identical
generated regions (no file-watcher churn)."
  (with-tal-fixture (dir)
    (let* ((file (hactar::generate-project-cli-file :load t))
           (first-pass (uiop:read-file-string file)))
      (hactar::generate-project-cli-file :load t)
      (let ((second-pass (uiop:read-file-string file)))
        (is (string= first-pass second-pass))))))

(test user-region-preserved-test
  "Regeneration preserves hand-written content in the user region."
  (with-tal-fixture (dir)
    (declare (ignore dir))
    (let ((file (hactar::generate-project-cli-file :load nil)))
      (with-open-file (s file :direction :output :if-exists :append
                              :if-does-not-exist :create)
        (format s "~%(define-project-command \"tal\" hello (args)~%  (declare (ignore args))~%  (format t \"hi\"))~%"))
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

(test harness-primitives-test
  "Validate and verify defaction, deftool, defskill, defsandbox, defagent."
  (let ((hactar::*defined-actions* (make-hash-table :test 'equal))
        (hactar::*defined-skills* (make-hash-table :test 'equal))
        (hactar::*defined-sandboxes* (make-hash-table :test 'equal))
        (hactar::*agent-harnesses* (make-hash-table :test 'equal))
        (hactar::*pcli-current-project* "tal"))
    ;; 1. Evaluate
    (hactar::defaction test-action :doc "A test action" :prompt "Prompt name {{name}}" :apply :create)
    (hactar::deftool test-tool :run "echo {{args}}" :doc "Run echo")
    (hactar::defskill test-skill :doc "A skill" :rules :test)
    (hactar::defsandbox test-sandbox :kind :local :allow ("echo"))
    (hactar::defagent test-harness-agent :model "anthropic/claude-sonnet" :tools (test-tool) :skills (test-skill) :sandbox test-sandbox :instructions "Use echo")

    (is-true (gethash "test-action" hactar::*defined-actions*))
    (is-true (gethash "test_tool" hactar::*defined-tools*))
    (is-true (gethash "test-skill" hactar::*defined-skills*))
    (is-true (gethash "test-sandbox" hactar::*defined-sandboxes*))
    (is-true (gethash 'hactar::test-harness-agent hactar::*agent-harnesses*))

    ;; 2. Validate
    (multiple-value-bind (valid-p errors corrected)
        (hactar::validate-hactar-form '(defaction test-action :doc "Doc" :prompt "Prompt"))
      (is-true valid-p)
      (is (null errors))
      (is-true corrected))

    (multiple-value-bind (valid-p errors corrected)
        (hactar::validate-hactar-form '(defagent test-agent :model "m" :tools (t1)))
      (is-true valid-p)
      (is (null errors))
      (is-true corrected))

    ;; 3. Merge
    (let* ((existing '((defaction test-action :doc "Old doc" :prompt "P1")
                       (defskill test-skill :doc "Old skill" :rules :r1)))
           (new '((defaction test-action :doc "New doc" :prompt "P2")
                  (defskill another-skill :doc "Another" :rules :r2)))
           (merged (hactar::merge-hactar-forms existing new)))
      (is (= 3 (length merged)))
      (let ((act (find 'defaction merged :key #'car)))
        (is (string= "New doc" (getf (cddr act) :doc)))))))
