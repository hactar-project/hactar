;; npm mode
(in-package :hactar)
;;* core
(defun fetch-npm-registry-data (package-name)
  "Fetch package metadata from NPM registry."
  (let ((url (format nil "https://registry.npmjs.org/~A" package-name)))
    (handler-case
        (let ((json-content (fetch-url-content url)))
          (when json-content
            (cl-json:decode-json-from-string json-content)))
      (error (e)
        (format t "Error fetching NPM registry data: ~A~%" e)
        nil))))

(defun search-npm-packages (query &key (limit 20))
  "Search NPM packages and return results."
  (let ((url (format nil "https://registry.npmjs.org/-/v1/search?text=~A&size=~A"
                     (drakma:url-encode query :utf-8) limit)))
    (handler-case
        (let ((json-content (fetch-url-content url)))
          (when json-content
            (let ((parsed (cl-json:decode-json-from-string json-content)))
              (cdr (assoc :objects parsed)))))
      (error (e)
        (format t "Error searching NPM packages: ~A~%" e)
        nil))))

(defun format-npm-search-results (results)
  "Format NPM search results as markdown.
   Accepts either a list or a vector of result objects."
  (with-output-to-string (s)
    (format s "# NPM Package Search Results~%~%")
    (loop for result in (if (vectorp results) (coerce results 'list) results)
          for package = (cdr (assoc :package result))
          for name = (or (and package (cdr (assoc :name package))) "<no-name>")
          for description = (and package (cdr (assoc :description package)))
          for version = (or (and package (cdr (assoc :version package))) "<no-version>")
          for links = (and package (cdr (assoc :links package)))
          for npm-link = (and links (cdr (assoc :npm links)))
          do
            (format s "## ~A@~A~%" name version)
            (when description
              (format s "~A~%~%" description))
            (when npm-link
              (format s "[NPM Package](~A)~%~%" npm-link)))))

(defun get-package-version-info (package-data version-spec)
  "Get version info from package data matching version-spec.
   Returns the version object or nil if not found."
  (let ((versions (cdr (assoc :versions package-data))))
    (if (string= version-spec "latest")
        ;; Get the latest version
        (let ((latest-version (cdr (assoc :latest (cdr (assoc :dist-tags package-data))))))
          (cdr (assoc (intern latest-version :keyword) versions)))
        ;; Try to find exact match or compatible version
        (let ((version-key (intern version-spec :keyword)))
          (or (cdr (assoc version-key versions))
              ;; Try to find a compatible version (simple prefix match)
              (loop for (key . version-obj) in versions
                    when (str:starts-with-p version-spec (symbol-name key))
                    return version-obj))))))

(defun get-package-readme (package-data version-spec)
  "Get README content for a package, preferring the version-specific readme
   and falling back to the package-level readme field."
  (let* ((version-info (get-package-version-info package-data version-spec))
         (version-readme (and version-info (cdr (assoc :readme version-info))))
         (top-readme (cdr (assoc :readme package-data))))
    (or (and version-readme (plusp (length version-readme)) version-readme)
        (and top-readme (plusp (length top-readme)) top-readme))))

(defun normalize-github-repo-url (repo-url)
  "Normalize an NPM repository URL into a plain GitHub URL.
   Strips a leading \"git+\", a \"#fragment\" suffix and a trailing \".git\"."
  (when repo-url
    (let* ((url (string-trim '(#\Space #\Tab #\Newline #\Return) repo-url))
           (url (if (str:starts-with-p "git+" url) (subseq url 4) url))
           (hash (position #\# url))
           (url (if hash (subseq url 0 hash) url))
           (url (if (str:ends-with-p ".git" url)
                    (subseq url 0 (- (length url) 4))
                    url)))
      url)))

(defun fetch-github-readme (repo-url &optional directory)
  "Fetch a README from a GitHub repository URL, probing the main and master
   branches. When DIRECTORY is given (an NPM monorepo subpath), the README is
   looked up inside that subdirectory first."
  (let ((clean-url (normalize-github-repo-url repo-url)))
    (when clean-url
      (cl-ppcre:register-groups-bind (user repo)
          ("github\\.com[/:]([^/]+)/([^/]+)" clean-url)
        (let* ((dir (when (and directory (plusp (length directory)))
                      (string-trim "/" directory)))
               (found
                 (loop named search
                       for branch in '("main" "master")
                       do (loop for fname in '("README.md" "readme.md" "README")
                                for path = (if dir (format nil "~A/~A" dir fname) fname)
                                for url = (format nil "https://raw.githubusercontent.com/~A/~A/refs/heads/~A/~A"
                                                  user repo branch path)
                                when (probe-url url)
                                  do (return-from search url)))))
          (when found
            (fetch-url-content found)))))))

(defun get-npm-docs (package-name &optional version-spec)
  "Get documentation for an NPM package.
   First checks custom doc routes, then falls back to package README."
  (let ((version (or version-spec "latest")))
    ;; Try custom doc route first. Use the no-fallback variant so a missing
    ;; route does not trigger the global wiki/LLM fallback.
    (let* ((route-input (format nil "npm:~A@~A" package-name version))
           (custom-doc (execute-route-no-fallback route-input)))
      (if custom-doc
          custom-doc
          ;; Fall back to default behavior
          (let ((package-data (fetch-npm-registry-data package-name)))
            (if package-data
                (let* ((version-info (get-package-version-info package-data version))
                       (repository (cdr (assoc :repository (or version-info package-data))))
                       (repo-url (if (stringp repository)
                                    repository
                                    (cdr (assoc :url repository))))
                       (directory (and (consp repository)
                                       (cdr (assoc :directory repository)))))
                  ;; Try to get README from GitHub if available, otherwise fall
                  ;; back to the README embedded in the registry metadata.
                  (or (and repo-url (search "github.com" repo-url)
                           (fetch-github-readme repo-url directory))
                      (get-package-readme package-data version)))
                (format nil "Package not found: ~A" package-name)))))))

(defun get-npm-meta (package-name &optional version-spec)
  "Get package.json metadata for an NPM package."
  (let* ((version (or version-spec "latest"))
         (package-data (fetch-npm-registry-data package-name))
         (version-info (get-package-version-info package-data version)))
    (format t "package-info ~A" package-data)
    (when version-info
      (cl-json:encode-json-to-string version-info))))

;;* commands
(defwebcommand npm
  "NPM package management and documentation.

Examples:
  hactar npm search express
  hactar npm docs svelte
  hactar npm meta vue"

  (defwebroute npm-search "Search npm packages"
    ("search" &rest args) (args)
    :priority 10
    (lambda ()
      (let* ((query (format nil "~{~A~^ ~}" args))
             (results (search-npm-packages query)))
        (if results
            (format t "~A" (format-npm-search-results results))
            (format t "No results found for: ~A~%" query)))))

  (defwebroute npm-docs "Get documentation for an npm package"
    ("docs" package-spec) (package-spec)
    :priority 10
    (lambda ()
      (let* ((at-pos (position #\@ package-spec))
             (package-name (if at-pos (subseq package-spec 0 at-pos) package-spec))
             (version (if at-pos (subseq package-spec (1+ at-pos)) "latest"))
             (docs (get-npm-docs package-name version)))
        (if docs
            (format t "~A" docs)
            (format t "Documentation not found for: ~A~%" package-spec)))))

  (defwebroute npm-meta "Get package.json metadata for an npm package"
    ("meta" package-spec) (package-spec)
    :priority 10
    (lambda ()
      (let* ((at-pos (position #\@ package-spec))
             (package-name (if at-pos (subseq package-spec 0 at-pos) package-spec))
             (version (if at-pos (subseq package-spec (1+ at-pos)) "latest"))
             (meta (get-npm-meta package-name version)))
        (if meta
            (format t "~A" meta)
            (format t "Metadata not found for: ~A~%" package-spec)))))

  (def-default-route ()
    (lambda ()
      (format t "NPM Commands:~%  npm search <query> - Search for packages~%  npm docs <package@version> - Get documentation~%  npm meta <package@version> - Get package.json~%~%Examples:~%  hactar npm search express~%  hactar npm docs svelte~%  hactar npm meta vue~%"))))

;; some placeholder doc sourcess
;; TODO: Add top 100 packages here
(defdocsource :name "vue"
              :version "3.^"
              :platform "npm"
              :uri "https://raw.githubusercontent.com/vuejs/core/refs/heads/main/README.md")
(defdocsource :name "svelte"
              :version "5.^"
              :platform "npm"
              :uri "https://raw.githubusercontent.com/sveltejs/svelte/refs/heads/main/README.md")

(defun get-npm-json (args)
  "Get JSON-serializable alist/structure for NPM command with ARGS."
  (let* ((subcmd (first args))
         (sub-args (rest args)))
    (cond
      ((and subcmd (string-equal subcmd "search"))
       (let* ((query (format nil "~{~A~^ ~}" sub-args))
              (results (search-npm-packages query)))
         results))
      ((and subcmd (string-equal subcmd "docs"))
       (let* ((package-spec (first sub-args)))
         (when package-spec
           (let* ((at-pos (position #\@ package-spec))
                  (package-name (if at-pos (subseq package-spec 0 at-pos) package-spec))
                  (version (if at-pos (subseq package-spec (1+ at-pos)) "latest"))
                  (docs (get-npm-docs package-name version)))
             (when docs
               `((:package . ,package-name)
                 (:version . ,version)
                 (:docs . ,docs)))))))
      ((and subcmd (string-equal subcmd "meta"))
       (let* ((package-spec (first sub-args)))
         (when package-spec
           (let* ((at-pos (position #\@ package-spec))
                  (package-name (if at-pos (subseq package-spec 0 at-pos) package-spec))
                  (version (if at-pos (subseq package-spec (1+ at-pos)) "latest"))
                  (package-data (fetch-npm-registry-data package-name)))
             (when package-data
               (get-package-version-info package-data version))))))
      (t nil))))

(register-format-handler "/npm" :json
  (lambda (args)
    (let ((clean-args (remove-if (lambda (arg) (or (str:starts-with-p "-" arg) (string= arg "--format"))) args)))
      (let ((json-struct (get-npm-json clean-args)))
        (when json-struct
          ;; Use cl-json's encoder here: the NPM data was decoded by cl-json
          ;; and contains dotted alists/mixed lists that shasht's to-json
          ;; cannot serialize.
          (cl-json:encode-json-to-string json-struct))))))
