(in-package :hactar-tests)

(def-suite redwood-mode-tests
  :description "Tests for the Redwood SDK mode and fusion.")

(in-suite redwood-mode-tests)

(test redwood-mode-registration-test
  "Verify redwood mode is defined, registered, and contains the correct rules/description."
  (let ((mode (hactar::get-mode :redwood)))
    (is-true mode "redwood mode should be registered in *modes*")
    (when mode
      (is (string= "redwood" (hactar::mode-name-string mode)))
      (is (search "Redwood SDK" (hactar::mode-description mode)))
      (is (search "When working with Redwood SDK (rwsdk):" (hactar::mode-rules mode))))))

(test redwood-sdk-fusion-registration-test
  "Verify redwood-sdk fusion is registered and has correct metadata."
  (let ((fusion (gethash 'hactar::redwood-sdk hactar::*fusion-registry*)))
    (is-true fusion "redwood-sdk fusion should be registered")
    (when fusion
      (is (eq 'hactar::redwood-sdk (hactar::fusion-name fusion)))
      (is (eq :redwood (getf (hactar::fusion-options fusion) :mode)))
      (is (equal '("src/worker.tsx" "wrangler.toml" "package.json" "src/app/**/*.tsx" "src/app/**/*.ts")
                 (getf (hactar::fusion-options fusion) :watch))))))

(test defenvs-schema-and-validation-test
  "Verify defenvs form validation and parsing works correctly."
  (let ((form '(hactar:defenvs
                (:name "DATABASE_URL" :value "sqlite://test.db" :type :secret :file "wrangler.toml")
                (:name "API_KEY" :value "123456" :file "wrangler.toml"))))
    (multiple-value-bind (valid-p errors corrected) (hactar:validate-hactar-form form)
      (is-true valid-p "Form should be valid against defenvs schema")
      (is (null errors) "Errors list should be empty")
      (is (eq 'hactar:defenvs (car corrected)) "Corrected form operator should be defenvs")
      (is (= 2 (length (cdr corrected))) "Corrected form should have two elements")
      (is (string= "DATABASE_URL" (getf (first (cdr corrected)) :name))))))

(test defenvs-serialization-test
  "Verify defenvs merges and serializes correctly to .hactar.lisp."
  (let* ((temp-dir (uiop:ensure-directory-pathname (make-temp-dir)))
         (hactar-file (merge-pathnames ".hactar.lisp" temp-dir))
         (forms '((hactar:defproject "test-env-proj" :stack (redwood) :root "*repo-root*")
                  (hactar:defenvs
                   (:name "PORT" :value "8080" :file "wrangler.toml")))))
    (unwind-protect
         (progn
           ;; Write initial
           (hactar:write-hactar-lisp forms hactar-file :merge-p nil)
           (is-true (probe-file hactar-file) ".hactar.lisp should be created")
           
           ;; Read back and verify content contains defenvs
           (let ((content (uiop:read-file-string hactar-file)))
             (is-true (search "BEGIN GENERATED: envs" content) "Should contain envs region begin")
             (is-true (search "(defenvs" content) "Should contain serialized defenvs form")
             (is-true (search "8080" content) "Should contain PORT value"))

           ;; Merge/update with new env var
           (let ((new-forms '((hactar:defenvs
                               (:name "PORT" :value "9000" :file "wrangler.toml")
                               (:name "HOST" :value "localhost" :file "wrangler.toml")))))
             (hactar:write-hactar-lisp new-forms hactar-file :merge-p t)
             (let ((updated-content (uiop:read-file-string hactar-file)))
               ;; Check PORT was updated/replaced and HOST was added
               (is-true (search "9000" updated-content) "PORT value should be updated to 9000")
               (is-false (search "8080" updated-content) "Old PORT value 8080 should be removed")
               (is-true (search "localhost" updated-content) "HOST value localhost should be added"))))
      (uiop:delete-directory-tree temp-dir :validate t))))

(test redwood-info-command-test
  "Verify redwood.info command executes without errors."
  (let* ((hactar::*pcli-current-project* "redwood-test-proj")
         (pcli (hactar::ensure-project-cli "redwood-test-proj")))
    (setf (hactar::project-cli-routes pcli)
          (list (hactar::make-route-entry :file "src/worker.tsx" :path "/")))
    (setf (hactar::project-cli-models pcli)
          (list (hactar::make-model-entry :name 'user :file "schema.prisma")))
    (unwind-protect
         (progn
           ;; Activate redwood mode to register its commands
           (hactar::activate-mode :redwood)
           (let* ((cmd-info (gethash "/redwood.info" hactar::*commands*))
                  (out (when cmd-info
                         (with-output-to-string (*standard-output*)
                           (funcall (first cmd-info) '())))))
             (is-true cmd-info "/redwood.info should be registered when redwood mode is active")
             (when cmd-info
               (is-true (search "Redwood SDK Project" out))
               (is-true (search "Routes count: 1" out))
               (is-true (search "Models count: 1" out)))))
      ;; Cleanup: deactivate mode
      (hactar::deactivate-mode :redwood))))
