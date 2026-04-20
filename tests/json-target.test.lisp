(in-package :hactar-tests)

(def-suite json-target-tests :description "JSON target tests.")
(in-suite json-target-tests)

;; json-object tests

(test json-object-basic
  "json-object emits a JSON object with string values."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-object \"name\" \"app\" \"version\" \"1.0\")"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "\"name\": \"app\"" content))
    (is (search "\"version\": \"1.0\"" content))
    (is (search "{" content))
    (is (search "}" content))))

(test json-object-boolean-values
  "json-object handles :true and :false correctly."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-object \"private\" :true \"debug\" :false)"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "\"private\": true" content))
    (is (search "\"debug\": false" content))))

(test json-object-null-value
  "json-object handles :null correctly."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-object \"data\" :null)"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "\"data\": null" content))))

(test json-object-numeric-values
  "json-object handles numbers correctly."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-object \"count\" 42 \"price\" 9.99)"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "\"count\": 42" content))
    (is (search "\"price\": 9.99" content))))

(test json-object-nested
  "json-object supports nested objects."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-object \"outer\" (json-object \"inner\" \"value\"))"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "\"outer\":" content))
    (is (search "\"inner\": \"value\"" content))))

(test json-object-indentation
  "json-object produces indented multi-line output."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-object \"a\" \"1\" \"b\" \"2\")"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (> (count #\Newline content) 0)
        "json-object output should contain newlines")))

(test json-object-single-key
  "json-object with a single key-value pair."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-object \"only\" \"one\")"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "\"only\": \"one\"" content))))

;; json-array tests

(test json-array-strings
  "json-array emits a JSON array of strings."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-array \"a\" \"b\" \"c\")"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "[\"a\", \"b\", \"c\"]" content))))

(test json-array-numbers
  "json-array emits a JSON array of numbers."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-array 1 2 3)"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "[1, 2, 3]" content))))

(test json-array-mixed-types
  "json-array handles mixed types."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-array \"text\" 42 :true :null)"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "\"text\"" content))
    (is (search "42" content))
    (is (search "true" content))
    (is (search "null" content))))

(test json-array-empty
  "json-array with no elements emits []."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-array)"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "[]" content))))

(test json-array-single-element
  "json-array with one element."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-array \"only\")"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "[\"only\"]" content))))

(test json-array-nested-objects
  "json-array with nested json-objects."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-array (json-object \"id\" 1) (json-object \"id\" 2))"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "\"id\": 1" content))
    (is (search "\"id\": 2" content))))

(test json-array-booleans
  "json-array with boolean values."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-array :true :false :null)"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "true" content))
    (is (search "false" content))
    (is (search "null" content))))

;; No stray semicolons

(test json-no-stray-semicolons
  "JSON files emitted via progn have no stray semicolons."
  (let* ((vfs (rw-compile "(deftsconfig)"))
         (ts (hactar::vfs-get-content vfs "tsconfig.json")))
    (is (not (search ";" ts))
        "tsconfig.json must not contain semicolons")))

(test json-package-json-no-semicolons
  "package.json has no stray semicolons."
  (let* ((vfs (rw-compile "(defconfig :name \"test-app\")"))
         (pkg (hactar::vfs-get-content vfs "package.json")))
    (is (not (search ";" pkg))
        "package.json must not contain semicolons")))

;; Integration with redwood macros

(test json-defconfig-integration
  "defconfig macro produces valid JSON through json-object."
  (let* ((vfs (rw-compile "(defconfig :name \"my-app\")"))
         (pkg (hactar::vfs-get-content vfs "package.json")))
    (is (not (search ";" pkg)) "No semicolons in package.json")
    (is (search "\"name\": \"my-app\"" pkg))
    (is (search "\"private\": true" pkg))
    (is (search "\"scripts\":" pkg))
    (is (search "\"dev\":" pkg))
    (is (search "\"react\": \"19.2.5\"" pkg))
    (is (search "\"react-dom\": \"19.2.5\"" pkg))
    (is (search "\"react-server-dom-webpack\": \"19.2.5\"" pkg))
    (is (search "\"rwsdk\": \"1.2.3\"" pkg))
    (is (search "\"@cloudflare/vite-plugin\": \"1.31.0\"" pkg))
    (is (search "\"@cloudflare/workers-types\": \"4.20260405.1\"" pkg))
    (is (search "\"@types/node\": \"~25.3.5\"" pkg))
    (is (search "\"@types/react\": \"19.2.14\"" pkg))
    (is (search "\"@types/react-dom\": \"19.2.3\"" pkg))
    (is (search "\"typescript\": \"6.0.2\"" pkg))
    (is (search "\"vite\": \"~7.3.2\"" pkg))
    (is (search "\"wrangler\": \"4.80.0\"" pkg))))

(test json-defconfig-with-deps
  "defconfig with dependencies produces correct JSON."
  (let* ((vfs (rw-compile "(defconfig :name \"app\" :dependencies (react \"^18.2.0\" react-dom \"^18.2.0\" lodash \"^4.17.21\") :dev-dependencies (typescript \"^5.0.0\" vite \"^7.0.0\"))"))
         (pkg (hactar::vfs-get-content vfs "package.json")))
    (is (search "\"react\": \"^18.2.0\"" pkg))
    (is (search "\"react-dom\": \"^18.2.0\"" pkg))
    (is (search "\"lodash\": \"^4.17.21\"" pkg))
    (is (search "\"react-server-dom-webpack\": \"19.2.5\"" pkg))
    (is (search "\"rwsdk\": \"1.2.3\"" pkg))
    (is (search "\"typescript\": \"^5.0.0\"" pkg))
    (is (search "\"vite\": \"^7.0.0\"" pkg))
    (is (not (search "\"react\": \"19.2.5\"" pkg)))
    (is (not (search "\"react-dom\": \"19.2.5\"" pkg)))
    (is (not (search "\"typescript\": \"6.0.2\"" pkg)))
    (is (not (search "\"vite\": \"~7.3.2\"" pkg)))))

(test json-deftsconfig-integration
  "deftsconfig macro produces valid JSON through json-object."
  (let* ((vfs (rw-compile "(deftsconfig)"))
         (ts (hactar::vfs-get-content vfs "tsconfig.json")))
    (is (not (search ";" ts)) "No semicolons in tsconfig.json")
    (is (search "\"compilerOptions\"" ts))
    (is (search "\"target\": \"es2021\"" ts))
    (is (search "\"module\": \"es2022\"" ts))
    (is (search "\"jsx\": \"react-jsx\"" ts))
    (is (search "\"strict\": true" ts))
    (is (search "\"noEmit\": true" ts))
    (is (search "node_modules" ts))))

(test json-deftsconfig-custom-options
  "deftsconfig with custom options."
  (let* ((vfs (rw-compile "(deftsconfig :target \"ES2022\" :strict t :lib (\"ESNext\" \"DOM\"))"))
         (ts (hactar::vfs-get-content vfs "tsconfig.json")))
    (is (search "\"target\": \"ES2022\"" ts))
    (is (search "\"strict\": true" ts))
    (is (search "[\"ESNext\", \"DOM\"]" ts))))

(test json-deeply-nested
  "Deeply nested JSON structures emit correctly."
  (let* ((vfs (rw-compile "(in-file \"test.json\") (json-object \"l1\" (json-object \"l2\" (json-object \"l3\" \"deep\")))"))
         (content (hactar::vfs-get-content vfs "test.json")))
    (is (search "\"l3\": \"deep\"" content))
    (is (search "\"l2\":" content))
    (is (search "\"l1\":" content))))
