;; npm mode
(in-package :hactar)

;;; NPM Package Operations
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

(defun get-package-readme (package-name version-spec)
  "Get README content for a package version."
  (let* ((package-data (fetch-npm-registry-data package-name))
         (version-info (get-package-version-info package-data version-spec)))
    (when version-info
      (cdr (assoc :readme version-info)))))

(defun get-github-readme-from-repo (repo-url)
  "Extract and fetch README from a GitHub repository URL."
  (let ((github-raw-url (get-github-raw-url repo-url)))
    (when github-raw-url
      (fetch-url-content github-raw-url))))

(defun get-npm-docs (package-name &optional version-spec)
  "Get documentation for an NPM package.
   First checks custom doc routes, then falls back to package README."
  (let ((version (or version-spec "latest")))
    ;; Try custom doc route first
    (let* ((route-input (format nil "npm:~A@~A" package-name version))
           (custom-doc (execute-route route-input)))
      (if custom-doc
          custom-doc
          ;; Fall back to default behavior
          (let ((package-data (fetch-npm-registry-data package-name)))
            (if package-data
                (let* ((version-info (get-package-version-info package-data version))
                       (repository (cdr (assoc :repository (or version-info package-data))))
                       (repo-url (if (stringp repository)
                                    repository
                                    (cdr (assoc :url repository)))))
                  ;; Try to get README from GitHub if available
                  (if (and repo-url (search "github.com" repo-url))
                      (or (get-github-readme-from-repo repo-url)
                          (get-package-readme package-name version))
                      (get-package-readme package-name version)))
                (format nil "Package not found: ~A" package-name)))))))

(defun get-npm-meta (package-name &optional version-spec)
  "Get package.json metadata for an NPM package."
  (let* ((version (or version-spec "latest"))
         (package-data (fetch-npm-registry-data package-name))
         (version-info (get-package-version-info package-data version)))
    (format t "package-info ~A" package-data)
    (when version-info
      (cl-json:encode-json-to-string version-info))))

;;; Web Command Implementation
(defwebcommand npm
  "NPM package management and documentation."

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
      (format t "NPM Commands:~%  npm search <query> - Search for packages~%  npm docs <package@version> - Get documentation~%  npm meta <package@version> - Get package.json~%"))))

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
