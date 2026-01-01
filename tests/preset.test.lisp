(in-package :hactar-tests)

(def-suite preset-tests
  :description "Tests for preset.lisp - context preset and snapshot system.")

(in-suite preset-tests)

(test make-preset-test
  "Test creating a preset struct."
  (let ((preset (hactar::make-preset
                 :name 'test-preset
                 :description "A test preset"
                 :files '("src/*.ts" "lib/utils.ts")
                 :docs '("api-docs" "readme")
                 :rules '(test-rule)
                 :skills '("auth-patterns")
                 :requires-stack '("react" "typescript")
                 :extends 'base-preset)))
    (is (eq (hactar::preset-name preset) 'test-preset))
    (is (string= (hactar::preset-description preset) "A test preset"))
    (is (equal (hactar::preset-files preset) '("src/*.ts" "lib/utils.ts")))
    (is (equal (hactar::preset-docs preset) '("api-docs" "readme")))
    (is (equal (hactar::preset-rules preset) '(test-rule)))
    (is (equal (hactar::preset-skills preset) '("auth-patterns")))
    (is (equal (hactar::preset-requires-stack preset) '("react" "typescript")))
    (is (eq (hactar::preset-extends preset) 'base-preset))
    (is (null (hactar::preset-active-p preset)))))

(test preset-name-string-test
  "Test converting preset name to string."
  (let ((symbol-preset (hactar::make-preset :name 'my-preset))
        (string-preset (hactar::make-preset :name "string-preset")))
    (is (string= (hactar::preset-name-string symbol-preset) "my-preset"))
    (is (string= (hactar::preset-name-string string-preset) "string-preset"))))

(test register-preset-test
  "Test registering and retrieving presets."
  (hactar::clear-presets)

  (let ((preset (hactar::make-preset
                 :name 'register-test
                 :description "Test registration")))
    (hactar::register-preset preset)

    (is-true (hactar::get-preset 'register-test))
    (is-true (hactar::get-preset "register-test"))
    (is (eq preset (hactar::get-preset 'register-test)))

    ;; Check it appears in list
    (is (member "register-test" (hactar::list-presets) :test #'string=))))

(test defpreset-macro-test
  "Test the defpreset macro."
  (hactar::clear-presets)

  (eval '(hactar::defpreset macro-test-preset
           "A preset defined via macro"
           :files ("src/app.ts" "src/utils.ts")
           :docs ("api")
           :requires-stack ("typescript")))

  (let ((preset (hactar::get-preset 'macro-test-preset)))
    (is-true preset)
    (is (string= (hactar::preset-description preset) "A preset defined via macro"))
    (is (equal (hactar::preset-files preset) '("src/app.ts" "src/utils.ts")))
    (is (equal (hactar::preset-docs preset) '("api")))
    (is (equal (hactar::preset-requires-stack preset) '("typescript")))))

(test defpreset-extends-test
  "Test defpreset with inheritance."
  (hactar::clear-presets)

  ;; Define base preset
  (eval '(hactar::defpreset base-preset
           "Base preset"
           :files ("src/core.ts")
           :requires-stack ("typescript")))

  ;; Define child preset
  (eval '(hactar::defpreset child-preset
           "Child preset"
           :extends base-preset
           :files ("src/features.ts")))

  (let ((child (hactar::get-preset 'child-preset)))
    (is-true child)
    (is (eq (hactar::preset-extends child) 'base-preset))))

(test resolve-preset-chain-test
  "Test resolving inheritance chain."
  (hactar::clear-presets)

  ;; Create chain: grandparent -> parent -> child
  (eval '(hactar::defpreset grandparent
           "Grandparent"
           :files ("gp.ts")))

  (eval '(hactar::defpreset parent-preset
           "Parent"
           :extends grandparent
           :files ("parent.ts")))

  (eval '(hactar::defpreset child
           "Child"
           :extends parent-preset
           :files ("child.ts")))

  (let* ((child-preset (hactar::get-preset 'child))
         (chain (hactar::resolve-preset-chain child-preset)))
    (is (= 3 (length chain)))
    ;; Chain should be from base to derived
    (is (eq (hactar::preset-name (first chain)) 'grandparent))
    (is (eq (hactar::preset-name (second chain)) 'parent-preset))
    (is (eq (hactar::preset-name (third chain)) 'child))))

(test merge-preset-files-test
  "Test merging files from inheritance chain."
  (hactar::clear-presets)

  (eval '(hactar::defpreset merge-base
           "Base"
           :files ("base1.ts" "base2.ts")))

  (eval '(hactar::defpreset merge-child
           "Child"
           :extends merge-base
           :files ("child1.ts" "base1.ts")))  ; Duplicate should be removed

  (let* ((child (hactar::get-preset 'merge-child))
         (chain (hactar::resolve-preset-chain child))
         (files (hactar::merge-preset-files chain)))
    ;; Should have 3 unique files
    (is (= 3 (length files)))
    (is (member "base1.ts" files :test #'string=))
    (is (member "base2.ts" files :test #'string=))
    (is (member "child1.ts" files :test #'string=))))

(test merge-preset-docs-test
  "Test merging docs from inheritance chain."
  (hactar::clear-presets)

  (eval '(hactar::defpreset docs-base
           "Base"
           :docs ("doc1" "doc2")))

  (eval '(hactar::defpreset docs-child
           "Child"
           :extends docs-base
           :docs ("doc3" "doc1")))

  (let* ((child (hactar::get-preset 'docs-child))
         (chain (hactar::resolve-preset-chain child))
         (docs (hactar::merge-preset-docs chain)))
    (is (= 3 (length docs)))))

(test merge-preset-skills-test
  "Test merging skills from inheritance chain."
  (hactar::clear-presets)

  (eval '(hactar::defpreset skills-base
           "Base"
           :skills ("skill1")))

  (eval '(hactar::defpreset skills-child
           "Child"
           :extends skills-base
           :skills ("skill2")))

  (let* ((child (hactar::get-preset 'skills-child))
         (chain (hactar::resolve-preset-chain child))
         (skills (hactar::merge-preset-skills chain)))
    (is (= 2 (length skills)))
    (is (member "skill1" skills :test #'string=))
    (is (member "skill2" skills :test #'string=))))

(test merge-preset-requires-stack-test
  "Test merging stack requirements from inheritance chain."
  (hactar::clear-presets)

  (eval '(hactar::defpreset stack-base
           "Base"
           :requires-stack ("typescript")))

  (eval '(hactar::defpreset stack-child
           "Child"
           :extends stack-base
           :requires-stack ("react" "typescript")))

  (let* ((child (hactar::get-preset 'stack-child))
         (chain (hactar::resolve-preset-chain child))
         (reqs (hactar::merge-preset-requires-stack chain)))
    ;; Should have 2 unique requirements (case-insensitive dedup)
    (is (= 2 (length reqs)))))

(test expand-single-glob-literal-test
  "Test expanding a literal (non-glob) file path."
  (uiop:with-temporary-file (:pathname p :keep t)
    (let* ((base-dir (uiop:pathname-directory-pathname p))
           (filename (file-namestring p)))
      (let ((result (hactar::expand-single-glob filename base-dir)))
        (is (= 1 (length result)))
        (is (string= (first result) (uiop:native-namestring p)))))))

(test preset-load-with-files-test
  "Test loading a preset with actual files."
  (let* ((temp-dir (make-temp-dir))
         (test-file1 (merge-pathnames "test1.ts" temp-dir))
         (test-file2 (merge-pathnames "test2.ts" temp-dir)))
    (unwind-protect
        (let ((hactar::*repo-root* (uiop:parse-native-namestring temp-dir))
              (hactar::*files* '())
              (hactar::*active-presets* '())
              (hactar::*exposed-context-file* nil))
          (with-open-file (s test-file1 :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format s "const x = 1;"))
          (with-open-file (s test-file2 :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format s "const y = 2;"))

          (hactar::clear-presets)

          (eval `(hactar::defpreset file-test-preset
                   "Test with files"
                   :files (,(file-namestring test-file1) ,(file-namestring test-file2))))

          (let ((result (hactar::preset/load 'file-test-preset :quiet t)))
            (is-true result)
            (is (hactar::preset-active-p result))
            (is (member "file-test-preset" hactar::*active-presets* :test #'string=))
            (is (= 2 (length hactar::*files*)))))
      ;; Cleanup
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t)))))

(test preset-unload-test
  "Test unloading a preset."
  (let* ((temp-dir (make-temp-dir))
         (test-file (merge-pathnames "unload-test.ts" temp-dir)))
    (unwind-protect
        (let ((hactar::*repo-root* (uiop:parse-native-namestring temp-dir))
              (hactar::*files* '())
              (hactar::*active-presets* '())
              (hactar::*exposed-context-file* nil))
          (with-open-file (s test-file :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format s "test"))

          (hactar::clear-presets)

          (eval `(hactar::defpreset unload-test-preset
                   "Unload test"
                   :files (,(file-namestring test-file))))

          (hactar::preset/load 'unload-test-preset :quiet t)
          (is-true (hactar::preset-active-p (hactar::get-preset 'unload-test-preset)))

          (hactar::preset/unload 'unload-test-preset :quiet t)
          (is (null (hactar::preset-active-p (hactar::get-preset 'unload-test-preset))))
          (is (null (member "unload-test-preset" hactar::*active-presets* :test #'string=))))
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t)))))

(test preset-load-already-active-test
  "Test that loading an already active preset returns early."
  (hactar::clear-presets)
  (let ((hactar::*active-presets* '())
        (hactar::*files* '())
        (hactar::*exposed-context-file* nil))

    (eval '(hactar::defpreset already-active-test
             "Test"
             :files ()))

    (let ((preset (hactar::get-preset 'already-active-test)))
      (hactar::preset/load 'already-active-test :quiet t)
      (is-true (hactar::preset-active-p preset))

      (let ((result (hactar::preset/load 'already-active-test :quiet t)))
        (is (eq result preset))))))

(test preset-unload-inactive-test
  "Test unloading a preset that isn't active."
  (hactar::clear-presets)
  (let ((hactar::*active-presets* '()))

    (eval '(hactar::defpreset inactive-test
             "Test"
             :files ()))

    (let ((result (hactar::preset/unload 'inactive-test :quiet t)))
      (is (null result)))))

(test preset-not-found-test
  "Test loading/unloading non-existent preset."
  (hactar::clear-presets)

  (is (null (hactar::preset/load 'nonexistent :quiet t)))
  (is (null (hactar::preset/unload 'nonexistent :quiet t))))

(test preset-setup-teardown-test
  "Test that setup and teardown functions are called."
  (hactar::clear-presets)
  (let ((hactar::*active-presets* '())
        (hactar::*files* '())
        (hactar::*exposed-context-file* nil)
        (setup-called nil)
        (teardown-called nil))

    (let ((preset (hactar::make-preset
                   :name 'hook-test-preset
                   :description "Hook test"
                   :files '()
                   :setup-fn (lambda () (setf setup-called t))
                   :teardown-fn (lambda () (setf teardown-called t)))))
      (hactar::register-preset preset)

      (hactar::preset/load 'hook-test-preset :quiet t)
      (is-true setup-called)
      (is (null teardown-called))

      (hactar::preset/unload 'hook-test-preset :quiet t)
      (is-true teardown-called))))

(test preset-unload-all-test
  "Test unloading all presets."
  (hactar::clear-presets)
  (let ((hactar::*active-presets* '())
        (hactar::*files* '())
        (hactar::*exposed-context-file* nil))

    (eval '(hactar::defpreset unload-all-1
             "Test 1"
             :files ()))
    (eval '(hactar::defpreset unload-all-2
             "Test 2"
             :files ())))

    (hactar::preset/load 'unload-all-1 :quiet t)
    (hactar::preset/load 'unload-all-2 :quiet t)
    (is (= 2 (length hactar::*active-presets*)))

    (hactar::preset/unload-all)
    (is (= 0 (length hactar::*active-presets*))))

(test preset-reload-test
  "Test reloading a preset."
  (hactar::clear-presets)
  (let ((hactar::*active-presets* '())
        (hactar::*files* '())
        (hactar::*exposed-context-file* nil)
        (load-count 0))

    (let ((preset (hactar::make-preset
                   :name 'reload-test
                   :description "Reload test"
                   :files '()
                   :setup-fn (lambda () (incf load-count)))))
      (hactar::register-preset preset)

      (hactar::preset/load 'reload-test :quiet t)
      (is (= 1 load-count))

      (hactar::preset/reload 'reload-test)
      (is (= 2 load-count)))))

(test preset-stack-warning-test
  "Test that missing stack requirements produce warnings."
  (hactar::clear-presets)
  (let ((hactar::*active-presets* '())
        (hactar::*files* '())
        (hactar::*stack* '("typescript"))  ; Missing "react"
        (hactar::*exposed-context-file* nil))

    (eval '(hactar::defpreset stack-warn-test
             "Stack warning test"
             :files ()
             :requires-stack ("react" "typescript")))

    (let* ((output (make-string-output-stream))
           (*standard-output* output))
      (hactar::preset/load 'stack-warn-test)
      (let ((out (get-output-stream-string output)))
        (is (search "requires 'react'" out))))))
