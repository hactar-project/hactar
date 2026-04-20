(in-package :hactar-tests)

(def-suite toml-target-tests :description "TOML target tests.")
(in-suite toml-target-tests)

;; toml-pairs tests

(test toml-pairs-basic
  "toml-pairs emits a key = value line."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"name\" \"app\")"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "name = \"app\"" content))))

(test toml-pairs-multiple
  "toml-pairs emits multiple key-value pairs."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"name\" \"app\" \"version\" \"1.0\")"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "name = \"app\"" content))
    (is (search "version = \"1.0\"" content))))

(test toml-pairs-numeric
  "toml-pairs handles numeric values."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"port\" 8080)"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "port = 8080" content))))

(test toml-pairs-boolean-true
  "toml-pairs handles :true."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"enabled\" :true)"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "enabled = true" content))))

(test toml-pairs-boolean-false
  "toml-pairs handles :false."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"debug\" :false)"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "debug = false" content))))

;; toml-table tests

(test toml-table-basic
  "toml-table emits [section] header with key-value pairs."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-table \"database\" \"host\" \"localhost\" \"port\" 5432)"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "[database]" content))
    (is (search "host = \"localhost\"" content))
    (is (search "port = 5432" content))))

(test toml-table-single-pair
  "toml-table with one key-value pair."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-table \"ai\" \"binding\" \"AI\")"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "[ai]" content))
    (is (search "binding = \"AI\"" content))))

(test toml-table-boolean-values
  "toml-table handles boolean values."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-table \"settings\" \"verbose\" :true \"quiet\" :false)"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "[settings]" content))
    (is (search "verbose = true" content))
    (is (search "quiet = false" content))))

;; toml-array tests

(test toml-array-strings
  "toml-array emits an inline array of strings."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"flags\" (toml-array \"a\" \"b\"))"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "flags = [\"a\", \"b\"]" content))))

(test toml-array-single-element
  "toml-array with single element."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"flags\" (toml-array \"nodejs_compat\"))"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "flags = [\"nodejs_compat\"]" content))))

(test toml-array-empty
  "toml-array with no elements emits []."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"items\" (toml-array))"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "items = []" content))))

(test toml-array-numbers
  "toml-array with numeric elements."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"ports\" (toml-array 80 443 8080))"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "ports = [80, 443, 8080]" content))))

;; No stray semicolons

(test toml-no-stray-semicolons
  "TOML files emitted via progn have no stray semicolons."
  (let* ((vfs (rw-compile "(defwrangler :name \"app\" :compatibility-date \"2025-08-21\")"))
         (wrangler (hactar::vfs-get-content vfs "wrangler.toml")))
    (is (not (search ";" wrangler))
        "wrangler.toml must not contain semicolons")))

(test toml-with-table-no-semicolons
  "TOML with table sections has no stray semicolons."
  (let* ((vfs (rw-compile "(defwrangler :name \"app\" :ai (:binding \"AI\"))"))
         (wrangler (hactar::vfs-get-content vfs "wrangler.toml")))
    (is (not (search ";" wrangler))
        "wrangler.toml with tables must not contain semicolons")))

;; Integration with redwood macros

(test toml-defwrangler-integration
  "defwrangler macro produces valid TOML through toml emitters."
  (let* ((vfs (rw-compile "(defwrangler :name \"my-app\" :compatibility-date \"2025-08-21\")"))
         (content (hactar::vfs-get-content vfs "wrangler.toml")))
    (is (not (search ";" content)) "No semicolons")
    (is (search "name = \"my-app\"" content))
    (is (search "compatibility-date = \"2025-08-21\"" content))))

(test toml-defwrangler-with-flags
  "defwrangler with compatibility flags produces array."
  (let* ((vfs (rw-compile "(defwrangler :name \"app\" :compatibility-flags (\"nodejs_compat\"))"))
         (content (hactar::vfs-get-content vfs "wrangler.toml")))
    (is (search "compatibility-flags = [\"nodejs_compat\"]" content))))

(test toml-defwrangler-with-table-section
  "defwrangler with nested keyword list produces TOML table."
  (let* ((vfs (rw-compile "(defwrangler :name \"app\" :ai (:binding \"AI\"))"))
         (content (hactar::vfs-get-content vfs "wrangler.toml")))
    (is (search "name = \"app\"" content))
    (is (search "[ai]" content))
    (is (search "binding = \"AI\"" content))))

(test toml-defwrangler-complex
  "defwrangler with multiple sections and arrays."
  (let* ((vfs (rw-compile "(defwrangler :name \"app\" :compatibility-date \"2025-08-21\" :compatibility-flags (\"nodejs_compat\") :ai (:binding \"AI\"))"))
         (content (hactar::vfs-get-content vfs "wrangler.toml")))
    (is (not (search ";" content)) "No semicolons")
    (is (search "name = \"app\"" content))
    (is (search "compatibility-date = \"2025-08-21\"" content))
    (is (search "compatibility-flags = [\"nodejs_compat\"]" content))
    (is (search "[ai]" content))
    (is (search "binding = \"AI\"" content))))

(test toml-mixed-pairs-and-tables
  "Multiple toml-pairs and toml-table forms in sequence."
  (let* ((vfs (rw-compile "(in-file \"test.toml\") (toml-pairs \"name\" \"svc\") (toml-table \"db\" \"url\" \"postgres://localhost\")"))
         (content (hactar::vfs-get-content vfs "test.toml")))
    (is (search "name = \"svc\"" content))
    (is (search "[db]" content))
    (is (search "url = \"postgres://localhost\"" content))))
