(in-package :hactar-tests)

(def-suite vfs-tests :description "Virtual File System tests.")
(in-suite vfs-tests)

(test vfs-create-and-write
  "Writing to a selected file accumulates content."
  (let ((vfs (hactar::make-vfs)))
    (hactar::vfs-select-file vfs "src/index.ts")
    (hactar::vfs-write vfs "const x = 1;")
    (hactar::vfs-writeln vfs "")
    (hactar::vfs-write vfs "const y = 2;")
    (is (string= (hactar::vfs-get-content vfs "src/index.ts")
                 (format nil "const x = 1;~%const y = 2;")))))

(test vfs-multiple-files
  "Multiple files can be written independently."
  (let ((vfs (hactar::make-vfs)))
    (hactar::vfs-select-file vfs "a.ts")
    (hactar::vfs-write vfs "file-a")
    (hactar::vfs-select-file vfs "b.ts")
    (hactar::vfs-write vfs "file-b")
    (hactar::vfs-select-file vfs "a.ts")
    (hactar::vfs-write vfs "-more")
    (is (string= (hactar::vfs-get-content vfs "a.ts") "file-a-more"))
    (is (string= (hactar::vfs-get-content vfs "b.ts") "file-b"))))

(test vfs-all-paths-sorted
  "vfs-all-paths returns sorted list."
  (let ((vfs (hactar::make-vfs)))
    (hactar::vfs-select-file vfs "z.ts")
    (hactar::vfs-select-file vfs "a.ts")
    (hactar::vfs-select-file vfs "m.ts")
    (is (equal (hactar::vfs-all-paths vfs) '("a.ts" "m.ts" "z.ts")))))

(test vfs-write-without-file-signals-error
  "Writing without selecting a file signals an error."
  (let ((vfs (hactar::make-vfs)))
    (signals error (hactar::vfs-write vfs "oops"))))

(test vfs-flush-to-org-test
  "Flush to org-mode format with tangle properties."
  (let ((vfs (hactar::make-vfs)))
    (hactar::vfs-select-file vfs "src/index.ts")
    (hactar::vfs-write vfs "const x = 1;")
    (hactar::vfs-select-file vfs "src/style.css")
    (hactar::vfs-write vfs "body { }")
    (let ((output (with-output-to-string (s)
                    (hactar::vfs-flush-to-org vfs s "dist"))))
      (is (search "#+PROPERTY: TANGLE_DIR dist" output))
      (is (search ":tangle src/index.ts" output))
      (is (search ":tangle src/style.css" output))
      (is (search "#+begin_src typescript" output))
      (is (search "#+begin_src css" output))
      (is (search "const x = 1;" output)))))

(test vfs-flush-to-lisp-test
  "Flush to lisp VFS operations."
  (let ((vfs (hactar::make-vfs)))
    (hactar::vfs-select-file vfs "src/index.ts")
    (hactar::vfs-write vfs "const x = 1;")
    (let ((output (with-output-to-string (s)
                    (hactar::vfs-flush-to-lisp vfs s))))
      (is (search "make-vfs" output))
      (is (search "vfs-select-file" output))
      (is (search "vfs-write" output))
      (is (search "src/index.ts" output))
      (is (search "const x = 1;" output)))))

(test vfs-flush-to-tags-test
  "Flush to <file> tag format."
  (let ((vfs (hactar::make-vfs)))
    (hactar::vfs-select-file vfs "src/index.ts")
    (hactar::vfs-write vfs "const x = 1;")
    (let ((output (with-output-to-string (s)
                    (hactar::vfs-flush-to-tags vfs s))))
      (is (search "<file path=" output))
      (is (search "src/index.ts" output))
      (is (search "const x = 1;" output))
      (is (search "</file>" output)))))

(test vfs-metadata
  "Metadata can be set and retrieved per file."
  (let ((vfs (hactar::make-vfs)))
    (hactar::vfs-select-file vfs "worker.ts")
    (hactar::vfs-set-metadata vfs "worker.ts" :pragma "use server")
    (is (string= (hactar::vfs-get-metadata vfs "worker.ts" :pragma) "use server"))
    (is (null (hactar::vfs-get-metadata vfs "worker.ts" :nonexistent)))))

(test vfs-flush-to-disk-test
  "Flush writes files to disk."
  (let* ((vfs (hactar::make-vfs))
         (tmp (make-temp-dir)))
    (hactar::vfs-select-file vfs "src/index.ts")
    (hactar::vfs-write vfs "hello")
    (let ((paths (hactar::vfs-flush-to-disk vfs tmp)))
      (is (= 1 (length paths)))
      (is (string= "hello"
                    (uiop:read-file-string (merge-pathnames "src/index.ts" tmp)))))))
