;;* Ruhe - A quiet web feed generator built on Hactar and Hyperfractal
;; Takes plaintext org/markdown files describing what to stay updated on,
;; then fetches, compiles, and updates with relevant content from the web.

(defpackage #:ruhe
  (:use #:cl)
  (:export #:ruhe-process
           #:ruhe-compile
           #:*ruhe-default-model*
           #:*ruhe-good-enough-model*
           #:*ruhe-cache-size*
           #:*ruhe-db-path*
           #:*ruhe-schema*
           ;; Hooks
           #:*ruhe-preprocess-input-hook*
           #:*ruhe-process-output-hook*
           #:*ruhe-preprocess-context-hook*
           #:*ruhe-process-context-hook*
           #:*ruhe-schema-changed-hook*
           #:*ruhe-formats-changed-hook*
           #:*ruhe-schemas-changed-hook*
           ;; Formats
           #:*formats*
           #:*formats-by-ext*
           #:defformat
           #:find-format
           #:find-format-with-query
           #:format-object
           #:format-object-name
           #:format-object-extensions
           ;; Schemas
           #:*schemas*
           #:defschema
           #:find-schema
           #:find-schema-with-query
           #:schema-exists-p
           ;; Sources
           #:*sources*
           #:defsource
           #:find-source
           #:fetch-source))

(in-package :ruhe)

;;* Configuration

(defvar *ruhe-default-model* "gemini/gemini-2.5-flash-preview"
  "Default LLM model for Ruhe operations.")

(defvar *ruhe-good-enough-model* "gemini/gemini-2.5-flash-preview"
  "Model for less demanding tasks to save costs.")

(defvar *ruhe-cache-size* 1000
  "Cache size in MB.")

(defvar *ruhe-db-path*
  (uiop:subpathname (hactar::get-xdg-config-dir) "ruhe/ruhe.db")
  "Path to Ruhe's SQLite database.")

(defvar *ruhe-schema* nil
  "Current schema being used. Dynamic and not thread-safe.")

(defvar *agent-file-exts* '("txt" "md" "markdown" "org")
  "File extensions for agent instructions.")

;;* Hooks

(defvar *ruhe-preprocess-input-hook* (make-instance 'nhooks:hook)
  "Hook for preprocessing raw text input. Handlers receive and return text.")

(defvar *ruhe-process-output-hook* (make-instance 'nhooks:hook)
  "Hook for processing final output. Handlers receive and return text.")

(defvar *ruhe-preprocess-context-hook* (make-instance 'nhooks:hook)
  "Hook called before context generation. Handlers receive and return context plist.")

(defvar *ruhe-process-context-hook* (make-instance 'nhooks:hook)
  "Hook for processing assembled context string. Handlers receive and return string.")

(defvar *ruhe-schema-changed-hook* (make-instance 'nhooks:hook)
  "Hook called when *ruhe-schema* changes. Handlers receive (old-schema new-schema).")

(defvar *ruhe-formats-changed-hook* (make-instance 'nhooks:hook)
  "Hook called when formats are added/modified.")

(defvar *ruhe-schemas-changed-hook* (make-instance 'nhooks:hook)
  "Hook called when schemas are added/modified.")

;;* Format System

(defvar *formats* (make-hash-table :test 'equal)
  "Hash table of registered formats, keyed by name symbol.")

(defvar *formats-by-ext* (make-hash-table :test 'equal)
  "Hash table mapping file extensions to format names.")

(defstruct format-object
  "Represents a format with parse/modify/render operations."
  name           ; Symbol name (e.g., 'JSON)
  language       ; Optional Tree-sitter language identifier
  extensions     ; List of file extensions (e.g., ("json" "jsonc"))
  parse          ; (function string -> ast-bundle)
  modify         ; (function ast-bundle &rest fns -> ast-bundle)
  render         ; (function ast-bundle -> string)
  validators)    ; List of validation functions

(defmacro defformat (name &key extends language extensions
                              parser modifier renderer
                              wrap-parse wrap-modify wrap-render
                              validators)
  "Define a format with parse/modify/render operations.
   
   :EXTENDS - Parent format to inherit from
   :LANGUAGE - Tree-sitter language identifier
   :EXTENSIONS - List of file extensions (quoted list)
   :PARSER - (lambda (string) -> bundle)
   :MODIFIER - (lambda (bundle &rest fns) -> bundle)
   :RENDERER - (lambda (bundle) -> string)
   :WRAP-* - Wrapper functions for inherited operations
   :VALIDATORS - List of validation functions"
  (let ((format-sym (if (symbolp name) name (intern (string-upcase name)))))
    `(progn
       (let* ((parent (when ',extends (gethash ',extends *formats*)))
              (ext-list ',extensions)
              (parse-fn (cond
                          (,parser ,parser)
                          ((and parent ,wrap-parse)
                           (lambda (s) (funcall ,wrap-parse (funcall (format-object-parse parent) s))))
                          (parent (format-object-parse parent))
                          (t (lambda (s) (list :source s :tree nil)))))
              (modify-fn (cond
                           (,modifier ,modifier)
                           ((and parent ,wrap-modify)
                            (lambda (b &rest fns) (funcall ,wrap-modify (apply (format-object-modify parent) b fns))))
                           (parent (format-object-modify parent))
                           (t (lambda (b &rest fns) (declare (ignore fns)) b))))
              (render-fn (cond
                           (,renderer ,renderer)
                           ((and parent ,wrap-render)
                            (lambda (b) (funcall ,wrap-render (funcall (format-object-render parent) b))))
                           (parent (format-object-render parent))
                           (t (lambda (b) (getf b :source)))))
              (format-obj (make-format-object
                           :name ',format-sym
                           :language ,language
                           :extensions (or ext-list (when parent (format-object-extensions parent)))
                           :parse parse-fn
                           :modify modify-fn
                           :render render-fn
                           :validators (append ,validators (when parent (format-object-validators parent))))))
         ;; Register format
         (setf (gethash ',format-sym *formats*) format-obj)
         ;; Register extensions
         (dolist (ext (format-object-extensions format-obj))
           (setf (gethash ext *formats-by-ext*) ',format-sym))
         ;; Run hook
         (nhooks:run-hook *ruhe-formats-changed-hook* format-obj)
         ',format-sym))))

(defun find-format (name)
  "Find a format by symbol name."
  (gethash (if (symbolp name) name (intern (string-upcase name))) *formats*))

(defun find-format-by-extension (ext)
  "Find a format by file extension."
  (let ((format-name (gethash (string-downcase ext) *formats-by-ext*)))
    (when format-name
      (gethash format-name *formats*))))

(defun find-format-with-query (query)
  "Use LLM to find a format matching natural language query.
   Results are cached in the database."
  (declare (ignore query))
  ;; TODO: Implement LLM-based format lookup with caching
  nil)

;;* Schema System

(defvar *schemas* (make-hash-table :test 'equal)
  "Hash table of registered schemas, keyed by name symbol.")

(defstruct schema-object
  "Represents a JSON schema for validation."
  name           ; Symbol name
  schema         ; Lisp plist representing JSON Schema
  parent         ; Parent format name
  validators)    ; List of validation functions

(defmacro defschema (name schema &key extends)
  "Define a JSON schema format for validation.
   
   NAME - Symbol name for the schema
   SCHEMA - Lisp plist representing JSON Schema
   :EXTENDS - Parent format (defaults to JSON)"
  (let ((schema-sym (if (symbolp name) name (intern (string-upcase name)))))
    `(progn
       (let ((schema-obj (make-schema-object
                          :name ',schema-sym
                          :schema ',schema
                          :parent (or ',extends 'json)
                          :validators nil)))
         (setf (gethash ',schema-sym *schemas*) schema-obj)
         (nhooks:run-hook *ruhe-schemas-changed-hook* schema-obj)
         ',schema-sym))))

(defun find-schema (name)
  "Find a schema by symbol name."
  (gethash (if (symbolp name) name (intern (string-upcase name))) *schemas*))

(defun find-schema-with-query (query)
  "Use LLM to find a schema matching natural language query.
   Results are memoized."
  (declare (ignore query))
  ;; TODO: Implement LLM-based schema lookup with caching
  nil)

(defun schema-exists-p (name)
  "Return T if schema exists."
  (not (null (find-schema name))))

;;* Source System

(defvar *sources* (make-hash-table :test 'equal)
  "Hash table of registered sources, keyed by name symbol.")

(defstruct source-object
  "Represents a REST-ish source with CRUD operations."
  name           ; Symbol name
  base-url       ; Base URL for the API
  headers        ; Default headers alist
  request-fn     ; Custom request function
  get-fn         ; GET operation
  create-fn      ; CREATE operation
  update-fn      ; UPDATE operation
  delete-fn)     ; DELETE operation

(defmacro defsource (name &key base-url headers request-fn
                              get create update delete)
  "Define a REST-ish source with CRUD operations.
   
   :BASE-URL - Base URL for the API
   :HEADERS - Default headers
   :REQUEST-FN - Custom request function
   :GET/:CREATE/:UPDATE/:DELETE - Operation specs"
  (let ((source-sym (if (symbolp name) name (intern (string-upcase name)))))
    `(progn
       (let ((source-obj (make-source-object
                          :name ',source-sym
                          :base-url ,base-url
                          :headers ',headers
                          :request-fn ,request-fn
                          :get-fn ,get
                          :create-fn ,create
                          :update-fn ,update
                          :delete-fn ,delete)))
         (setf (gethash ',source-sym *sources*) source-obj)
         ',source-sym))))

(defun find-source (name)
  "Find a source by symbol name."
  (gethash (if (symbolp name) name (intern (string-upcase name))) *sources*))

(defun fetch-source (source-name &rest args)
  "Fetch data from a source."
  (let ((source (find-source source-name)))
    (when (and source (source-object-get-fn source))
      (apply (source-object-get-fn source) args))))

;;* Built-in Formats

;;** Org-mode
(defformat org
  :language "org"
  :extensions '("org")
  :parser (lambda (s)
            (list :source s
                  :language :org
                  :tree (org-mode-parser:parse-org-string s)))
  :renderer (lambda (bundle)
              (if (getf bundle :tree)
                  (org-mode-parser:org-to-string (getf bundle :tree))
                  (getf bundle :source))))

;;** Markdown
(defformat markdown
  :language "markdown"
  :extensions '("md" "markdown")
  :parser (lambda (s) (list :source s :language :markdown :tree nil))
  :renderer (lambda (bundle) (getf bundle :source)))

;;** JSON
(defformat json
  :language "json"
  :extensions '("json" "jsonc")
  :parser (lambda (s)
            (list :source s
                  :language :json
                  :tree (cl-json:decode-json-from-string s)))
  :renderer (lambda (bundle)
              (if (getf bundle :tree)
                  (cl-json:encode-json-to-string (getf bundle :tree))
                  (getf bundle :source))))

;;** HTML
(defformat html
  :language "html"
  :extensions '("html" "htm")
  :parser (lambda (s) (list :source s :language :html :tree nil))
  :renderer (lambda (bundle) (getf bundle :source)))

;;* Built-in Sources

;;** RSS/Atom
(defsource rss
  :base-url nil
  :get (lambda (url)
         (let ((content (hactar:fetch-url-content url)))
           (when content
             (hactar::parse-rss-feed content)))))

;;** Hacker News
(defsource hn
  :base-url "https://hnrss.org"
  :get (lambda (feed-type &key (limit 25))
         (let* ((feed-path (case feed-type
                             (:newest "/newest")
                             (:top "/frontpage")
                             (:best "/best")
                             (t "/frontpage")))
                (url (format nil "https://hnrss.org~A" feed-path))
                (content (hactar:fetch-url-content url)))
           (when content
             (let ((items (hactar::parse-rss-feed content)))
               (if limit
                   (subseq items 0 (min limit (length items)))
                   items))))))

;;** Reddit
(defsource reddit
  :base-url "https://old.reddit.com"
  :get (lambda (subreddit &key (limit 25))
         (let* ((url (format nil "https://old.reddit.com/r/~A/.rss" subreddit))
                (content (hactar:fetch-url-content url)))
           (when content
             (let ((items (hactar::parse-rss-feed content)))
               (if limit
                   (subseq items 0 (min limit (length items)))
                   items))))))

;;* Caching Layer

(defvar *ruhe-cache* (make-hash-table :test 'equal)
  "In-memory cache for compiled content.")

(defvar *ruhe-cache-order* '()
  "LRU order for cache eviction.")

(defun ruhe-cache-key (input-path)
  "Generate a cache key for an input file."
  (format nil "~A::~A" input-path (file-write-date input-path)))

(defun ruhe-cache-size-bytes ()
  "Calculate current cache size in bytes."
  (let ((total 0))
    (maphash (lambda (key value)
               (declare (ignore key))
               (incf total (length (princ-to-string value))))
             *ruhe-cache*)
    total))

(defun ruhe-cache-evict ()
  "Evict oldest entries until cache is under size limit."
  (let ((max-bytes (* *ruhe-cache-size* 1024 1024)))
    (loop while (and *ruhe-cache-order*
                     (> (ruhe-cache-size-bytes) max-bytes))
          do (let ((oldest-key (car (last *ruhe-cache-order*))))
               (remhash oldest-key *ruhe-cache*)
               (setf *ruhe-cache-order* (butlast *ruhe-cache-order*))))))

(defun ruhe-cache-get (key)
  "Get content from cache, updating LRU order."
  (let ((content (gethash key *ruhe-cache*)))
    (when content
      (setf *ruhe-cache-order*
            (cons key (remove key *ruhe-cache-order* :test #'string=))))
    content))

(defun ruhe-cache-put (key content)
  "Store content in cache with LRU eviction."
  (setf (gethash key *ruhe-cache*) content)
  (setf *ruhe-cache-order*
        (cons key (remove key *ruhe-cache-order* :test #'string=)))
  (ruhe-cache-evict)
  content)

(defun ruhe-cache-clear ()
  "Clear the entire cache."
  (clrhash *ruhe-cache*)
  (setf *ruhe-cache-order* '()))

;;* Configuration Loading

(defun ruhe-load-env-config ()
  "Load Ruhe configuration from environment variables."
  (let ((model (uiop:getenv "RUHE_MODEL"))
        (good-model (uiop:getenv "RUHE_GOOD_ENOUGH_MODEL"))
        (cache-size (uiop:getenv "RUHE_CACHE_SIZE"))
        (db-path (uiop:getenv "RUHE_DB_PATH")))
    (when model (setf *ruhe-default-model* model))
    (when good-model (setf *ruhe-good-enough-model* good-model))
    (when cache-size
      (let ((size (parse-integer cache-size :junk-allowed t)))
        (when size (setf *ruhe-cache-size* size))))
    (when db-path (setf *ruhe-db-path* (pathname db-path)))))

(defun ruhe-load-toml-config ()
  "Load Ruhe configuration from TOML file."
  (let ((config-file (uiop:subpathname (hactar:get-xdg-config-dir) "ruhe/config.toml")))
    (when (probe-file config-file)
      (handler-case
          (let* ((content (uiop:read-file-string config-file))
                 (config (cl-toml:parse content))
                 (ruhe-table (gethash "ruhe" config)))
            (when ruhe-table
              (let ((model (gethash "model" ruhe-table))
                    (good-model (gethash "good_enough_model" ruhe-table))
                    (cache-size (gethash "cache_size" ruhe-table))
                    (db-path (gethash "db_path" ruhe-table)))
                (when model (setf *ruhe-default-model* model))
                (when good-model (setf *ruhe-good-enough-model* good-model))
                (when cache-size (setf *ruhe-cache-size* cache-size))
                (when db-path (setf *ruhe-db-path* (pathname db-path))))))
        (error (e)
          (format t "~&Warning: Error loading ruhe config.toml: ~A~%" e))))))

(defun ruhe-load-lisp-config ()
  "Load Ruhe Lisp configuration file."
  (let ((config-file (uiop:subpathname (hactar:get-xdg-config-dir) "ruhe/ruhe.lisp")))
    (when (probe-file config-file)
      (handler-case
          (load config-file)
        (error (e)
          (format t "~&Warning: Error loading ruhe.lisp: ~A~%" e))))))

(defun ruhe-init ()
  "Initialize Ruhe configuration."
  (ruhe-load-env-config)
  (ruhe-load-toml-config)
  (ruhe-load-lisp-config))

;;* Intent Parsing

(defun parse-intent (text)
  "Parse user intent from input text using LLM.
   Returns a plist with :sources, :filters, :format, :schedule."
  (let* ((model (or (hactar:find-model-by-name *ruhe-default-model*)
                    hactar::*current-model*))
         (prompt (format nil "Analyze this feed specification and extract:
1. Sources (URLs, feeds, sites to monitor)
2. Filters (topics, keywords, exclusions)
3. Format preference (org, markdown)
4. Update schedule if mentioned

Return as JSON with keys: sources, filters, format, schedule.

Specification:
~A" text)))
    (when model
      (let ((provider-type (intern (string-upcase (hactar:model-config-provider model)) :keyword)))
        (handler-case
            (let ((response (llm:complete provider-type
                                          `(((:role . "user") (:content . ,prompt)))
                                          :model (hactar:model-config-model-name model)
                                          :max-tokens 1024
                                          :system-prompt "You are an intent parser. Return only valid JSON."
                                          :response-format "json_object"
                                          :stream nil)))
              (when response
                (cl-json:decode-json-from-string response)))
          (error (e)
            (format t "~&Error parsing intent: ~A~%" e)
            nil))))))

(defun extract-urls (text)
  "Extract URLs from text."
  (let ((urls '()))
    (cl-ppcre:do-matches-as-strings (url "(https?://[^\\s\\]\\)>]+)" text)
      (push url urls))
    (nreverse urls)))

(defun detect-sources (text)
  "Detect source types from text (rss, reddit, hn, etc.)."
  (let ((sources '()))
    (dolist (url (extract-urls text))
      (cond
        ((search "reddit.com" url) (push (list :type :reddit :url url) sources))
        ((or (search "news.ycombinator.com" url)
             (search "hn.algolia.com" url))
         (push (list :type :hn :url url) sources))
        ((or (search "/rss" url) (search "/feed" url) (search ".xml" url))
         (push (list :type :rss :url url) sources))
        (t (push (list :type :web :url url) sources))))
    (when (search "hacker news" (string-downcase text))
      (pushnew (list :type :hn :feed :top) sources :test #'equal))
    (when (cl-ppcre:scan "r/\\w+" text)
      (cl-ppcre:do-matches-as-strings (sub "r/(\\w+)" text)
        (push (list :type :reddit :subreddit (subseq sub 2)) sources)))
    (nreverse sources)))

;;* Content Generation

(defun fetch-source-content (source-spec)
  "Fetch content from a source specification."
  (let ((source-type (getf source-spec :type)))
    (case source-type
      (:rss (fetch-source 'rss (getf source-spec :url)))
      (:hn (fetch-source 'hn (or (getf source-spec :feed) :top)))
      (:reddit (fetch-source 'reddit (getf source-spec :subreddit)))
      (:web (hactar:hyperfractal-browse (getf source-spec :url)))
      (t nil))))

(defun format-items-as-org (items title)
  "Format a list of items as org-mode content."
  (with-output-to-string (s)
    (format s "* ~A~%" title)
    (format s ":PROPERTIES:~%")
    (format s ":FETCHED: ~A~%" (hactar::format-timestamp (get-universal-time)))
    (format s ":END:~%~%")
    (dolist (item items)
      (let ((item-title (cdr (assoc :title item)))
            (link (cdr (assoc :link item)))
            (description (cdr (assoc :description item))))
        (format s "** ~A~%" (or item-title "Untitled"))
        (when link
          (format s "[[~A][Link]]~%" link))
        (when description
          (format s "~%~A~%~%" description))))))

(defun format-items-as-markdown (items title)
  "Format a list of items as markdown content."
  (with-output-to-string (s)
    (format s "# ~A~%~%" title)
    (format s "_Fetched: ~A_~%~%" (hactar::format-timestamp (get-universal-time)))
    (dolist (item items)
      (let ((item-title (cdr (assoc :title item)))
            (link (cdr (assoc :link item)))
            (description (cdr (assoc :description item))))
        (format s "## ~A~%" (or item-title "Untitled"))
        (when link
          (format s "[Link](~A)~%~%" link))
        (when description
          (format s "~A~%~%" description))))))

(defun generate-output (sources-content format-type)
  "Generate output from fetched sources content."
  (with-output-to-string (s)
    (dolist (source-data sources-content)
      (let ((title (getf source-data :title))
            (items (getf source-data :items)))
        (when items
          (if (eq format-type :org)
              (write-string (format-items-as-org items title) s)
              (write-string (format-items-as-markdown items title) s)))))))

;;* Main Processing Pipeline

(defun ruhe-process (input-file &key (format :org) (use-cache t))
  "Process a text file and generate an updated feed.
   Returns the generated content as a string."
  (ruhe-init)
  
  (when use-cache
    (let ((cache-key (ruhe-cache-key input-file)))
      (let ((cached (ruhe-cache-get cache-key)))
        (when cached
          (return-from ruhe-process cached)))))
  
  (let* ((raw-text (uiop:read-file-string input-file))
         (processed-text (nhooks:run-hook *ruhe-preprocess-input-hook* raw-text)))
    (setf processed-text (or processed-text raw-text))
    
    (let* ((intent (parse-intent processed-text))
           (detected-sources (detect-sources processed-text))
           (all-sources (append (cdr (assoc :sources intent)) detected-sources)))
      
      (let ((sources-content
              (loop for source in all-sources
                    for content = (fetch-source-content source)
                    when content
                    collect (list :title (format nil "~A" (getf source :type))
                                  :items content))))
        
        (let ((output (generate-output sources-content format)))
          (let ((final-output (or (nhooks:run-hook *ruhe-process-output-hook* output)
                                  output)))
            
            (when use-cache
              (ruhe-cache-put (ruhe-cache-key input-file) final-output))
            
            final-output))))))

;;* Compilation System

(defun ruhe-compile (input-file &key output-file)
  "Compile a feed specification to executable Lisp code."
  (ruhe-init)
  
  (let* ((raw-text (uiop:read-file-string input-file))
         (intent (parse-intent raw-text))
         (detected-sources (detect-sources raw-text))
         (all-sources (append (cdr (assoc :sources intent)) detected-sources))
         (feed-name (pathname-name input-file)))
    
    (let ((compiled-code
            (with-output-to-string (s)
              (format s ";;; Generated by Ruhe Compile~%")
              (format s ";;; Source: ~A~%" input-file)
              (format s ";;; Generated: ~A~%~%" (hactar::format-timestamp (get-universal-time)))
              (format s "(defun generate-~A-update ()~%" feed-name)
              (format s "  \"Generates an update for the ~A feed.\"~%" feed-name)
              (format s "  (let ((sources (list~%")
              (dolist (source all-sources)
                (format s "                  '~S~%" source))
              (format s "                  )))~%")
              (format s "    (ruhe::generate-output~%")
              (format s "     (loop for source in sources~%")
              (format s "           for content = (ruhe::fetch-source-content source)~%")
              (format s "           when content~%")
              (format s "           collect (list :title (format nil \"~~A\" (getf source :type))~%")
              (format s "                         :items content))~%")
              (format s "     :org)))~%"))))
      
      (if output-file
          (progn
            (with-open-file (stream output-file
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-string compiled-code stream))
            (format t "Compiled to: ~A~%" output-file)
            output-file)
          compiled-code))))

;;* CLI Commands (Hactar Integration)

(in-package :hactar)

(define-sub-command ruhe (args)
		    "Process a text file and generate an updated feed.
   Usage: hactar ruhe [options] <input-file>"
		    (ruhe::ruhe-init)
  
		    (let* ((model-opt (getf args :model))
			   (format-opt (or (getf args :format) "org"))
			   (compile-opt (getf args :compile))
			   (output-opt (getf args :output))
			   (clear-cache (getf args :clear-cache))
			   (input-file (first (uiop:ensure-list (getf args :args)))))
    
		      (when clear-cache
			(ruhe::ruhe-cache-clear)
			(format t "~&Cache cleared.~%"))
    
		      (when model-opt
			(setf ruhe:*ruhe-default-model* model-opt))
    
		      (cond
		       (input-file
			(if compile-opt
			    ;; Compile mode
			    (ruhe:ruhe-compile input-file :output-file output-opt)
			  ;; Process mode
			  (let ((format-kw (intern (string-upcase format-opt) :keyword)))
			    (let ((output (ruhe:ruhe-process input-file :format format-kw)))
			      (if output-opt
				  (progn
				    (with-open-file (stream output-opt
							    :direction :output
							    :if-exists :supersede
							    :if-does-not-exist :create)
						    (write-string output stream))
				    (format t "~&Output written to: ~A~%" output-opt))
				(format t "~A" output))))))
		       ((not clear-cache)
			(format t "~&Usage: hactar ruhe [options] <input-file>~%"))))
  
		    :cli-options ((:short "m" :long "model" :description "LLM model to use")
				  (:short "g" :long "good-enough-model" :description "Model for simpler tasks")
				  (:short "f" :long "format" :description "Output format (org/markdown)")
				  (:short "c" :long "cache-size" :description "Cache size in MB")
				  (:short "d" :long "date" :description "Date range filter")
				  (:long "compile" :description "Compile to Lisp instead of processing")
				  (:short "o" :long "output" :description "Output file path")
				  (:long "clear-cache" :description "Clear the cache")))

;;* REPL Slash Commands

(define-slash-command ruhe-summary (args)
  "Generate a summary of the current document. Usage: /ruhe-summary [length]"
  (let* ((length (or (first args) "medium"))
         (context (generate-context)))
    (when (and context (> (length context) 0))
      (let* ((prompt (format nil "Summarize the following content in a ~A length summary:~%~%~A" length context))
             (response (get-llm-response prompt :stream t :add-to-history nil)))
        (format t "~&~A~%" response)))))

(define-slash-command ruhe-ask (args)
  "Ask a question about the current document. Usage: /ruhe-ask <query>"
  (let ((query (format nil "~{~A~^ ~}" args))
        (context (generate-context)))
    (when (and query (> (length query) 0) context)
      (let* ((prompt (format nil "Based on the following content, answer this question: ~A~%~%Content:~%~A" query context))
             (response (get-llm-response prompt :stream t :add-to-history nil)))
        (format t "~&~A~%" response)))))

(define-slash-command ruhe-cron (args)
  "Generate a cron command for scheduling feed updates. Usage: /ruhe-cron <schedule> [file]"
  (let* ((schedule (or (first args) "0 * * * *"))
         (file (or (second args) "feed.org"))
         (hactar-path (uiop:argv0)))
    (format t "~&Cron command:~%~A ~A ruhe ~A~%"
            schedule hactar-path file)))

(define-slash-command ruhe-compile (args)
  "Compile the current feed specification to Lisp. Usage: /ruhe-compile [output-file]"
  (let ((output-file (first args))
        (input-file (or *active-guide-file*
                        (first *files*))))
    (if input-file
        (let ((result (ruhe:ruhe-compile input-file :output-file output-file)))
          (unless output-file
            (format t "~&~A~%" result)))
        (format t "~&No input file specified. Add a file to context first.~%"))))

(define-slash-command ruhe-sources (args)
  "Extract and list sources from the current document. Usage: /ruhe-sources"
  (declare (ignore args))
  (let* ((context (generate-context))
         (sources (ruhe::detect-sources context)))
    (if sources
        (progn
          (format t "~&Detected sources:~%")
          (dolist (source sources)
            (format t "  - ~A: ~A~%"
                    (getf source :type)
                    (or (getf source :url)
                        (getf source :subreddit)
                        (getf source :feed)))))
        (format t "~&No sources detected in context.~%"))))

(define-slash-command ruhe-urls (args)
  "Extract all URLs from the current document. Usage: /ruhe-urls"
  (declare (ignore args))
  (let* ((context (generate-context))
         (urls (ruhe::extract-urls context)))
    (if urls
        (progn
          (format t "~&Extracted URLs:~%")
          (dolist (url urls)
            (format t "  ~A~%" url)))
        (format t "~&No URLs found in context.~%"))))

;;** Aliases
(define-slash-command brief (args)
  "Alias for /ruhe-summary. Usage: /brief [length]"
  (funcall (first (gethash "/ruhe-summary" *commands*)) args))

(define-slash-command tldr (args)
  "Alias for /ruhe-summary. Usage: /tldr [length]"
  (funcall (first (gethash "/ruhe-summary" *commands*)) args))
