(in-package :hactar-tests)

(def-suite session-tests
  :description "Tests for session management (save, load, list, delete, auto-save/restore)")

(in-suite session-tests)

;;* Helpers

(defmacro with-temp-session-env ((&key (files nil) (chat-history nil) (model-name nil)
                                       (stack nil) (auto-save nil))
                                 &body body)
  "Set up a temporary environment for session tests with isolated directories."
  (let ((temp-dir (gensym "TEMP-DIR"))
        (sessions-dir (gensym "SESSIONS-DIR"))
        (global-dir (gensym "GLOBAL-DIR")))
    `(let* ((,temp-dir (merge-pathnames (format nil "hactar-session-test-~A/" (uuid:make-v4-uuid))
                                         (uiop:temporary-directory)))
            (,sessions-dir (merge-pathnames ".hactar/sessions/" ,temp-dir))
            (,global-dir (merge-pathnames "global-sessions/" ,temp-dir))
            (hactar::*repo-root* ,temp-dir)
            (hactar::*sessions-dir* ,sessions-dir)
            (hactar::*global-sessions-dir* ,global-dir)
            (hactar::*current-session-name* nil)
            (hactar::*auto-save-session* ,auto-save)
            (hactar::*session-auto-save-name* ".autosave")
            (hactar::*files* ,files)
            (hactar::*images* nil)
            (hactar::*chat-history* ,chat-history)
            (hactar::*docs-context* nil)
            (hactar::*errors-context* nil)
            (hactar::*active-rules* (make-hash-table :test 'equal))
            (hactar::*active-presets* nil)
            (hactar::*stack* ,stack)
            (hactar::*git-autocommit* t)
            (hactar::*tool-use-enabled* t)
            (hactar::*current-model* nil)
            (hactar::*available-models* nil)
            (hactar::*cheap-model* ,model-name)
            (hactar::*docs-meta-model* nil)
            (hactar::*name* "test-project")
            (hactar::*silent* t)
            (hactar::*transcript-file* (merge-pathnames ".hactar.test.transcript.json" ,temp-dir))
            (hactar::*exposed-context-file* nil))
       (ensure-directories-exist ,sessions-dir)
       (ensure-directories-exist ,global-dir)
       (ensure-directories-exist ,temp-dir)
       (unwind-protect
            (progn ,@body)
         (ignore-errors (uiop:delete-directory-tree ,temp-dir :validate t))))))

;;* Data Structures

(test session-struct-creation
  "Test that hactar-session struct can be created with all fields."
  (let ((session (hactar::make-hactar-session
                  :name "test"
                  :description "A test session"
                  :timestamp 3900000000
                  :repo-root "/tmp/test/"
                  :project-name "test-project"
                  :model-name "ollama/qwen3:14b"
                  :cheap-model "ollama/cheap"
                  :docs-meta-model "ollama/docs"
                  :files '("/tmp/a.lisp" "/tmp/b.lisp")
                  :images '((:path "/tmp/img.png" :text "screenshot"))
                  :docs-context '(("doc1"))
                  :errors-context '(("err1"))
                  :chat-history '(((:role . "user") (:content . "hello")))
                  :stack '("lisp" "react")
                  :active-presets '("core")
                  :active-rules '(("rule1" . "text1"))
                  :git-autocommit t
                  :tool-use-enabled t
                  :metadata '(:key "value"))))
    (is (string= "test" (hactar::hactar-session-name session)))
    (is (string= "A test session" (hactar::hactar-session-description session)))
    (is (= 3900000000 (hactar::hactar-session-timestamp session)))
    (is (string= "/tmp/test/" (hactar::hactar-session-repo-root session)))
    (is (string= "test-project" (hactar::hactar-session-project-name session)))
    (is (string= "ollama/qwen3:14b" (hactar::hactar-session-model-name session)))
    (is (string= "ollama/cheap" (hactar::hactar-session-cheap-model session)))
    (is (string= "ollama/docs" (hactar::hactar-session-docs-meta-model session)))
    (is (= 2 (length (hactar::hactar-session-files session))))
    (is (= 1 (length (hactar::hactar-session-images session))))
    (is (= 1 (length (hactar::hactar-session-chat-history session))))
    (is (= 2 (length (hactar::hactar-session-stack session))))
    (is (equal '("core") (hactar::hactar-session-active-presets session)))
    (is (= 1 (length (hactar::hactar-session-active-rules session))))
    (is-true (hactar::hactar-session-git-autocommit session))
    (is-true (hactar::hactar-session-tool-use-enabled session))
    (is (equal '(:key "value") (hactar::hactar-session-metadata session)))))

(test session-struct-defaults
  "Test that hactar-session struct has correct defaults."
  (let ((session (hactar::make-hactar-session)))
    (is (null (hactar::hactar-session-name session)))
    (is (null (hactar::hactar-session-description session)))
    (is (null (hactar::hactar-session-timestamp session)))
    (is (null (hactar::hactar-session-files session)))
    (is (null (hactar::hactar-session-chat-history session)))
    (is (null (hactar::hactar-session-stack session)))
    (is (null (hactar::hactar-session-active-presets session)))
    (is (null (hactar::hactar-session-active-rules session)))
    (is (null (hactar::hactar-session-git-autocommit session)))
    (is (null (hactar::hactar-session-tool-use-enabled session)))
    (is (null (hactar::hactar-session-metadata session)))))

;;* Capture Current State

(test capture-current-session-basic
  "Test capturing the current hactar state into a session."
  (with-temp-session-env (:files '("/tmp/test.lisp")
                          :chat-history '(((:role . "user") (:content . "hi")))
                          :stack '("common-lisp"))
    (setf (gethash "rule1" hactar::*active-rules*) "Always use defun.")
    (let ((session (hactar::capture-current-session "my-session"
                                                     :description "Test capture")))
      (is (string= "my-session" (hactar::hactar-session-name session)))
      (is (string= "Test capture" (hactar::hactar-session-description session)))
      (is (integerp (hactar::hactar-session-timestamp session)))
      (is (string= "test-project" (hactar::hactar-session-project-name session)))
      (is (equal '("/tmp/test.lisp") (hactar::hactar-session-files session)))
      (is (= 1 (length (hactar::hactar-session-chat-history session))))
      (is (equal '("common-lisp") (hactar::hactar-session-stack session)))
      (is-true (hactar::hactar-session-git-autocommit session))
      (is-true (hactar::hactar-session-tool-use-enabled session))
      ;; Rules should be captured as alist
      (is (= 1 (length (hactar::hactar-session-active-rules session))))
      (is (string= "rule1" (car (first (hactar::hactar-session-active-rules session))))))))

(test capture-current-session-empty-state
  "Test capturing state when everything is empty."
  (with-temp-session-env ()
    (setf hactar::*git-autocommit* nil)
    (setf hactar::*tool-use-enabled* nil)
    (let ((session (hactar::capture-current-session "empty")))
      (is (string= "empty" (hactar::hactar-session-name session)))
      (is (null (hactar::hactar-session-files session)))
      (is (null (hactar::hactar-session-chat-history session)))
      (is (null (hactar::hactar-session-stack session)))
      (is (null (hactar::hactar-session-active-rules session)))
      (is (null (hactar::hactar-session-git-autocommit session)))
      (is (null (hactar::hactar-session-tool-use-enabled session))))))

;;* Session Directory

(test session-project-dir-returns-path
  "Test that session-project-dir returns a valid path when repo-root is set."
  (with-temp-session-env ()
    (let ((dir (hactar::session-project-dir)))
      (is (not (null dir)))
      (is (uiop:directory-exists-p dir)))))

(test session-project-dir-nil-without-repo
  "Test that session-project-dir returns nil without repo-root."
  (let ((hactar::*repo-root* nil)
        (hactar::*sessions-dir* nil))
    (is (null (hactar::session-project-dir)))))

(test session-global-dir-returns-path
  "Test that session-global-dir returns a valid path."
  (with-temp-session-env ()
    (let ((dir (hactar::session-global-dir)))
      (is (not (null dir)))
      (is (uiop:directory-exists-p dir)))))

(test session-file-path-project
  "Test session file path generation for project-local sessions."
  (with-temp-session-env ()
    (let ((path (hactar::session-file-path "my-session")))
      (is (not (null path)))
      (is (search "my-session.session.lisp" (namestring path)))
      (is (search ".hactar/sessions/" (namestring path))))))

(test session-file-path-global
  "Test session file path generation for global sessions."
  (with-temp-session-env ()
    (let ((path (hactar::session-file-path "global-sess" :global t)))
      (is (not (null path)))
      (is (search "global-sess.session.lisp" (namestring path)))
      (is (search "global-sessions/" (namestring path))))))

;;* Save / Load Round-Trip

(test session-save-creates-file
  "Test that session/save creates a session file on disk."
  (with-temp-session-env (:files '("/tmp/test.lisp"))
    (let ((session (hactar::session/save "test-save")))
      (is (not (null session)))
      (is (string= "test-save" hactar::*current-session-name*))
      (let ((path (hactar::session-file-path "test-save")))
        (is (probe-file path))))))

(test session-save-with-description
  "Test that session/save stores a description."
  (with-temp-session-env ()
    (hactar::session/save "described" :description "A described session")
    (let ((loaded (hactar::session/find-and-load "described")))
      (is (not (null loaded)))
      (is (string= "A described session" (hactar::hactar-session-description loaded))))))

(test session-save-global
  "Test saving a session to the global directory."
  (with-temp-session-env ()
    (hactar::session/save "global-test" :global t)
    (let ((path (hactar::session-file-path "global-test" :global t)))
      (is (probe-file path)))))

(test session-save-load-roundtrip
  "Test that saving and loading preserves session state."
  (with-temp-session-env (:files '("/tmp/a.lisp" "/tmp/b.lisp")
                          :chat-history '(((:role . "user") (:content . "hello"))
                                          ((:role . "assistant") (:content . "hi there")))
                          :stack '("lisp" "react")
                          :model-name "ollama/cheap-model")
    (setf hactar::*git-autocommit* nil)
    (setf hactar::*tool-use-enabled* nil)
    (setf (gethash "security" hactar::*active-rules*) "Never expose secrets.")
    (setf hactar::*active-presets* '("core" "auth"))

    (hactar::session/save "roundtrip" :description "Roundtrip test")

    ;; Load into a fresh struct (don't restore state, just deserialize)
    (let ((loaded (hactar::session/find-and-load "roundtrip")))
      (is (not (null loaded)))
      (is (string= "roundtrip" (hactar::hactar-session-name loaded)))
      (is (string= "Roundtrip test" (hactar::hactar-session-description loaded)))
      (is (integerp (hactar::hactar-session-timestamp loaded)))
      (is (string= "test-project" (hactar::hactar-session-project-name loaded)))
      ;; Files
      (is (= 2 (length (hactar::hactar-session-files loaded))))
      (is (member "/tmp/a.lisp" (hactar::hactar-session-files loaded) :test #'string=))
      (is (member "/tmp/b.lisp" (hactar::hactar-session-files loaded) :test #'string=))
      ;; Chat history
      (is (= 2 (length (hactar::hactar-session-chat-history loaded))))
      ;; Stack
      (is (= 2 (length (hactar::hactar-session-stack loaded))))
      (is (member "lisp" (hactar::hactar-session-stack loaded) :test #'string=))
      ;; Model
      (is (string= "ollama/cheap-model" (hactar::hactar-session-cheap-model loaded)))
      ;; Flags
      (is (null (hactar::hactar-session-git-autocommit loaded)))
      (is (null (hactar::hactar-session-tool-use-enabled loaded)))
      ;; Rules
      (is (= 1 (length (hactar::hactar-session-active-rules loaded))))
      (let ((rule (first (hactar::hactar-session-active-rules loaded))))
        (is (string= "security" (car rule)))
        (is (string= "Never expose secrets." (cdr rule))))
      ;; Presets
      (is (equal '("core" "auth") (hactar::hactar-session-active-presets loaded))))))

(test session-save-overwrites-existing
  "Test that saving a session with the same name overwrites the previous one."
  (with-temp-session-env (:files '("/tmp/first.lisp"))
    (hactar::session/save "overwrite-test" :description "First save")
    (setf hactar::*files* '("/tmp/second.lisp"))
    (hactar::session/save "overwrite-test" :description "Second save")
    (let ((loaded (hactar::session/find-and-load "overwrite-test")))
      (is (string= "Second save" (hactar::hactar-session-description loaded)))
      (is (= 1 (length (hactar::hactar-session-files loaded))))
      (is (string= "/tmp/second.lisp" (first (hactar::hactar-session-files loaded)))))))

;;* Session Load / Restore

(test session-load-restores-state
  "Test that session/load restores state into global variables."
  (with-temp-session-env (:files '("/tmp/original.lisp")
                          :chat-history '(((:role . "user") (:content . "saved msg")))
                          :stack '("typescript"))
    (setf hactar::*git-autocommit* nil)
    (setf (gethash "myrule" hactar::*active-rules*) "Do X.")

    (hactar::session/save "restore-test")

    (setf hactar::*files* '("/tmp/changed.lisp"))
    (setf hactar::*chat-history* '(((:role . "user") (:content . "different"))))
    (setf hactar::*stack* '("python"))
    (setf hactar::*git-autocommit* t)
    (clrhash hactar::*active-rules*)

    (hactar::session/load "restore-test" :quiet t)

    (is (string= "restore-test" hactar::*current-session-name*))
    (is (= 1 (length hactar::*chat-history*)))
    (is (string= "saved msg" (cdr (assoc :content (first hactar::*chat-history*)))))
    (is (null hactar::*git-autocommit*))
    (is (string= "Do X." (gethash "myrule" hactar::*active-rules*)))))

(test session-load-not-found
  "Test that session/load returns nil for nonexistent session."
  (with-temp-session-env ()
    (is (null (hactar::session/load "nonexistent" :quiet t)))))

(test session-load-searches-global
  "Test that session/load searches global directory when not found locally."
  (with-temp-session-env ()
    ;; Save to global
    (hactar::session/save "global-only" :global t)

    ;; Clear current session name
    (setf hactar::*current-session-name* nil)

    ;; Load should find it in global dir
    (let ((session (hactar::session/load "global-only" :quiet t)))
      (is (not (null session)))
      (is (string= "global-only" hactar::*current-session-name*)))))

(test session-load-prefers-project-local
  "Test that session/load prefers project-local over global sessions."
  (with-temp-session-env (:files '("/tmp/local.lisp"))
    ;; Save to global first
    (hactar::session/save "both" :global t :description "global version")
    ;; Save to project-local
    (hactar::session/save "both" :description "local version")

    (let ((loaded (hactar::session/find-and-load "both")))
      (is (string= "local version" (hactar::hactar-session-description loaded))))))

;;* Session Delete

(test session-delete-removes-file
  "Test that session/delete removes the session file."
  (with-temp-session-env ()
    (hactar::session/save "delete-me")
    (let ((path (hactar::session-file-path "delete-me")))
      (is (probe-file path))
      (hactar::session/delete "delete-me")
      (is (null (probe-file path))))))

(test session-delete-clears-current-name
  "Test that deleting the current session clears *current-session-name*."
  (with-temp-session-env ()
    (hactar::session/save "current-sess")
    (is (string= "current-sess" hactar::*current-session-name*))
    (hactar::session/delete "current-sess")
    (is (null hactar::*current-session-name*))))

(test session-delete-nonexistent-returns-nil
  "Test that deleting a nonexistent session returns nil."
  (with-temp-session-env ()
    (is (null (hactar::session/delete "does-not-exist")))))

(test session-delete-global
  "Test deleting a global session."
  (with-temp-session-env ()
    (hactar::session/save "global-del" :global t)
    (let ((path (hactar::session-file-path "global-del" :global t)))
      (is (probe-file path))
      (hactar::session/delete "global-del" :global t)
      (is (null (probe-file path))))))

;;* Session Discovery

(test discover-session-files-empty
  "Test discovery with no session files."
  (with-temp-session-env ()
    (is (null (hactar::discover-session-files)))))

(test discover-session-files-project
  "Test discovery finds project-local sessions."
  (with-temp-session-env ()
    (hactar::session/save "sess-a")
    (hactar::session/save "sess-b")
    (let ((entries (hactar::discover-session-files)))
      (is (= 2 (length entries)))
      (is (find "sess-a" entries :key #'first :test #'string=))
      (is (find "sess-b" entries :key #'first :test #'string=))
      (dolist (e entries)
        (is (eq :project (third e)))))))

(test discover-session-files-global
  "Test discovery finds global sessions."
  (with-temp-session-env ()
    (hactar::session/save "global-sess" :global t)
    (let ((entries (hactar::discover-session-files)))
      (is (= 1 (length entries)))
      (is (string= "global-sess" (first (first entries))))
      (is (eq :global (third (first entries)))))))

(test discover-session-files-deduplicates
  "Test that discovery doesn't duplicate sessions found in both project and global."
  (with-temp-session-env ()
    (hactar::session/save "duped" :global t)
    (hactar::session/save "duped") ; project-local
    (let ((entries (hactar::discover-session-files)))
      ;; Should only appear once (project-local takes precedence)
      (is (= 1 (length entries)))
      (is (eq :project (third (first entries)))))))

(test discover-session-files-sorted
  "Test that discovered sessions are sorted alphabetically."
  (with-temp-session-env ()
    (hactar::session/save "zebra")
    (hactar::session/save "alpha")
    (hactar::session/save "middle")
    (let ((entries (hactar::discover-session-files)))
      (is (string= "alpha" (first (first entries))))
      (is (string= "middle" (first (second entries))))
      (is (string= "zebra" (first (third entries)))))))

;;* Session File Detection

(test session-file-p-valid
  "Test session-file-p identifies valid session files."
  (is-true (hactar::session-file-p #P"/tmp/my-session.session.lisp"))
  (is-true (hactar::session-file-p #P"test.session.lisp")))

(test session-file-p-invalid
  "Test session-file-p rejects non-session files."
  (is (null (hactar::session-file-p #P"/tmp/regular.lisp")))
  (is (null (hactar::session-file-p #P"/tmp/session.txt")))
  (is (null (hactar::session-file-p #P"/tmp/my-session.session.json"))))

(test session-name-from-file-extraction
  "Test extracting session name from file path."
  (is (string= "my-session" (hactar::session-name-from-file #P"my-session.session.lisp")))
  (is (string= "test" (hactar::session-name-from-file #P"/some/path/test.session.lisp")))
  (is (string= ".autosave" (hactar::session-name-from-file #P".autosave.session.lisp"))))

;;* Session List / Show

(test session-list-outputs-sessions
  "Test that session/list produces output without errors."
  (with-temp-session-env ()
    (hactar::session/save "list-test" :description "For listing")
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((hactar::*silent* nil))
                      (hactar::session/list)))))
      (is (search "list-test" output))
      (is (search "For listing" output)))))

(test session-list-empty
  "Test session/list with no sessions."
  (with-temp-session-env ()
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((hactar::*silent* nil))
                      (hactar::session/list)))))
      (is (search "No sessions found" output)))))

(test session-show-displays-details
  "Test that session/show displays session details."
  (with-temp-session-env (:files '("/tmp/show-file.lisp")
                          :stack '("react"))
    (setf hactar::*git-autocommit* nil)
    (hactar::session/save "show-test" :description "Show me")
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((hactar::*silent* nil))
                      (hactar::session/show "show-test")))))
      (is (search "show-test" output))
      (is (search "Show me" output))
      (is (search "test-project" output)))))

(test session-show-not-found
  "Test session/show with nonexistent session."
  (with-temp-session-env ()
    (let ((result (hactar::session/show "nonexistent")))
      (is (null result)))))

;;* Auto-save / Auto-restore

(test session-auto-save-when-enabled
  "Test that auto-save creates a session file when enabled."
  (with-temp-session-env (:auto-save t
                          :files '("/tmp/auto.lisp"))
    (hactar::session/auto-save)
    (let ((path (hactar::session-file-path ".autosave")))
      (is (probe-file path)))))

(test session-auto-save-when-disabled
  "Test that auto-save does nothing when disabled."
  (with-temp-session-env (:auto-save nil)
    (hactar::session/auto-save)
    (let ((path (hactar::session-file-path ".autosave")))
      (is (null (probe-file path))))))

(test session-auto-restore-when-autosave-exists
  "Test that auto-restore loads the autosave session."
  (with-temp-session-env (:auto-save t
                          :chat-history '(((:role . "user") (:content . "auto-saved msg"))))
    (hactar::session/auto-save)
    (setf hactar::*chat-history* nil)
    (hactar::session/auto-restore)
    (is (= 1 (length hactar::*chat-history*)))
    (is (string= "auto-saved msg"
                  (cdr (assoc :content (first hactar::*chat-history*)))))))

(test session-auto-restore-no-autosave
  "Test that auto-restore does nothing when no autosave exists."
  (with-temp-session-env (:auto-save t)
    (hactar::session/auto-restore)
    (is (null hactar::*chat-history*))))

(test session-auto-restore-when-disabled
  "Test that auto-restore does nothing when auto-save is disabled."
  (with-temp-session-env (:auto-save nil
                          :chat-history '(((:role . "user") (:content . "msg"))))
    (let ((hactar::*auto-save-session* t))
      (hactar::session/auto-save))
    (setf hactar::*auto-save-session* nil)
    (setf hactar::*chat-history* nil)

    (hactar::session/auto-restore)
    (is (null hactar::*chat-history*))))

;;* Timestamp Formatting

(test session-format-timestamp-valid
  "Test timestamp formatting with a valid universal time."
  (let ((ts (hactar::session-format-timestamp 3900000000)))
    (is (stringp ts))
    (is (> (length ts) 0))
    (is (cl-ppcre:scan "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}" ts))))

(test session-format-timestamp-nil
  "Test timestamp formatting with nil."
  (is (string= "unknown" (hactar::session-format-timestamp nil))))

;;* Plist-to-Session Conversion

(test plist-to-session-complete
  "Test converting a complete plist to a session struct."
  (let ((session (hactar::plist-to-session
                  '(:name "from-plist"
                    :description "Plist test"
                    :timestamp 3900000000
                    :repo-root "/tmp/repo/"
                    :project-name "plist-project"
                    :model-name "test-model"
                    :cheap-model "cheap"
                    :docs-meta-model "docs"
                    :files ("/tmp/f1.lisp")
                    :images nil
                    :docs-context nil
                    :errors-context nil
                    :chat-history (((:role . "user") (:content . "test")))
                    :stack ("lisp")
                    :active-presets ("core")
                    :active-rules (("r1" . "text1"))
                    :git-autocommit t
                    :tool-use-enabled nil
                    :metadata (:custom "data")))))
    (is (string= "from-plist" (hactar::hactar-session-name session)))
    (is (string= "Plist test" (hactar::hactar-session-description session)))
    (is (= 3900000000 (hactar::hactar-session-timestamp session)))
    (is (string= "test-model" (hactar::hactar-session-model-name session)))
    (is (equal '("/tmp/f1.lisp") (hactar::hactar-session-files session)))
    (is (equal '("lisp") (hactar::hactar-session-stack session)))
    (is-true (hactar::hactar-session-git-autocommit session))
    (is (null (hactar::hactar-session-tool-use-enabled session)))))

(test plist-to-session-minimal
  "Test converting a minimal plist to a session struct."
  (let ((session (hactar::plist-to-session '(:name "minimal"))))
    (is (string= "minimal" (hactar::hactar-session-name session)))
    (is (null (hactar::hactar-session-description session)))
    (is (null (hactar::hactar-session-files session)))))

;;* Serialization / Deserialization

(test serialize-deserialize-roundtrip
  "Test that serializing and deserializing a session preserves data."
  (with-temp-session-env ()
    (let* ((original (hactar::make-hactar-session
                      :name "ser-test"
                      :description "Serialization test"
                      :timestamp 3900000000
                      :repo-root "/tmp/test/"
                      :project-name "ser-project"
                      :model-name "test-model"
                      :files '("/tmp/a.lisp" "/tmp/b.lisp")
                      :chat-history '(((:role . "user") (:content . "hello")))
                      :stack '("lisp")
                      :active-presets '("core")
                      :active-rules '(("r1" . "Rule text"))
                      :git-autocommit t
                      :tool-use-enabled nil))
           (path (merge-pathnames "ser-test.session.lisp"
                                   (hactar::session-project-dir))))
      (with-open-file (stream path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
        (hactar::serialize-session-to-stream original stream))

      (let ((loaded (hactar::deserialize-session-from-file path)))
        (is (not (null loaded)))
        (is (string= "ser-test" (hactar::hactar-session-name loaded)))
        (is (string= "Serialization test" (hactar::hactar-session-description loaded)))
        (is (= 3900000000 (hactar::hactar-session-timestamp loaded)))
        (is (= 2 (length (hactar::hactar-session-files loaded))))
        (is (= 1 (length (hactar::hactar-session-chat-history loaded))))
        (is (equal '("lisp") (hactar::hactar-session-stack loaded)))
        (is (equal '("core") (hactar::hactar-session-active-presets loaded)))
        (is (= 1 (length (hactar::hactar-session-active-rules loaded))))
        (is-true (hactar::hactar-session-git-autocommit loaded))
        (is (null (hactar::hactar-session-tool-use-enabled loaded)))))))

(test deserialize-invalid-file
  "Test that deserializing an invalid file returns nil."
  (with-temp-session-env ()
    (let ((bad-path (merge-pathnames "bad.session.lisp"
                                      (hactar::session-project-dir))))
      (with-open-file (stream bad-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string "(this is not valid session data" stream))
      (is (null (hactar::deserialize-session-from-file bad-path))))))

(test deserialize-nonexistent-file
  "Test that deserializing a nonexistent file returns nil."
  (with-temp-session-env ()
    (is (null (hactar::deserialize-session-from-file #P"/tmp/nonexistent-session-12345.session.lisp")))))

;;* Multiple Session Management

(test multiple-sessions-independent
  "Test that multiple sessions are saved independently."
  (with-temp-session-env (:files '("/tmp/first.lisp"))
    (hactar::session/save "session-1" :description "First")
    (setf hactar::*files* '("/tmp/second.lisp"))
    (hactar::session/save "session-2" :description "Second")

    (let ((s1 (hactar::session/find-and-load "session-1"))
          (s2 (hactar::session/find-and-load "session-2")))
      (is (string= "First" (hactar::hactar-session-description s1)))
      (is (string= "Second" (hactar::hactar-session-description s2)))
      (is (equal '("/tmp/first.lisp") (hactar::hactar-session-files s1)))
      (is (equal '("/tmp/second.lisp") (hactar::hactar-session-files s2))))))

;;* Edge Cases

(test session-save-special-characters-in-name
  "Test saving a session with special characters in the name."
  (with-temp-session-env ()
    (hactar::session/save "my-session_v2")
    (let ((loaded (hactar::session/find-and-load "my-session_v2")))
      (is (not (null loaded)))
      (is (string= "my-session_v2" (hactar::hactar-session-name loaded))))))

(test session-save-empty-chat-history
  "Test saving and loading with empty chat history."
  (with-temp-session-env (:chat-history nil)
    (hactar::session/save "empty-chat")
    (let ((loaded (hactar::session/find-and-load "empty-chat")))
      (is (null (hactar::hactar-session-chat-history loaded))))))

(test session-save-without-repo-root
  "Test that session/save returns nil when no repo root is set."
  (let ((hactar::*repo-root* nil)
        (hactar::*sessions-dir* nil)
        (hactar::*silent* t))
    (is (null (hactar::session/save "no-repo")))))

(test session-save-nil-no-error
  "Test that session/save handles nil paths gracefully."
  (let ((hactar::*repo-root* nil)
        (hactar::*sessions-dir* nil)
        (hactar::*silent* t))
    (handler-case
        (progn
          (hactar::session/save "test")
          (pass))
      (error (e)
        (fail (format nil "session/save errored with nil repo-root: ~A" e))))))

;;* Current Session Name Tracking

(test session-name-tracking-on-save
  "Test that saving sets *current-session-name*."
  (with-temp-session-env ()
    (is (null hactar::*current-session-name*))
    (hactar::session/save "tracked")
    (is (string= "tracked" hactar::*current-session-name*))))

(test session-name-tracking-on-load
  "Test that loading sets *current-session-name*."
  (with-temp-session-env ()
    (hactar::session/save "to-load")
    (setf hactar::*current-session-name* nil)
    (hactar::session/load "to-load" :quiet t)
    (is (string= "to-load" hactar::*current-session-name*))))

(test session-name-tracking-on-delete
  "Test that deleting the current session clears *current-session-name*."
  (with-temp-session-env ()
    (hactar::session/save "to-delete")
    (is (string= "to-delete" hactar::*current-session-name*))
    (hactar::session/delete "to-delete")
    (is (null hactar::*current-session-name*))))

(test session-name-unchanged-when-deleting-other
  "Test that deleting a different session doesn't clear *current-session-name*."
  (with-temp-session-env ()
    (hactar::session/save "keep-me")
    (setf hactar::*current-session-name* "other-session")
    ;; Save another to delete
    (let ((hactar::*current-session-name* nil))
      (hactar::session/save "delete-me"))
    (setf hactar::*current-session-name* "keep-me")
    (hactar::session/delete "delete-me")
    (is (string= "keep-me" hactar::*current-session-name*))))

;;* Session Load Clears State

(test session-load-clears-previous-state
  "Test that loading a session clears previous state before restoring."
  (with-temp-session-env ()
    (hactar::session/save "clean-session")

    (setf hactar::*files* '("/tmp/leftover.lisp"))
    (setf hactar::*chat-history* '(((:role . "user") (:content . "leftover"))))
    (setf hactar::*docs-context* '(("leftover-doc")))
    (setf hactar::*errors-context* '(("leftover-error")))
    (setf (gethash "leftover-rule" hactar::*active-rules*) "Old rule")

    (hactar::session/load "clean-session" :quiet t)

    (is (null hactar::*chat-history*))
    (is (null hactar::*docs-context*))
    (is (null hactar::*errors-context*))
    (is (= 0 (hash-table-count hactar::*active-rules*)))))
