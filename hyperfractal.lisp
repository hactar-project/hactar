;;; hyperfractal.lisp — Plaintext web browser wrapper for Hactar
(in-package :hactar)

;;* configuration

(defvar *hyperfractal-default-model* nil
  "Default LLM model for HTML conversion. Falls back to *current-model*.")

(defvar *hyperfractal-cache-size* 50
  "Cache size in MB for converted content.")

(defvar *hyperfractal-default-format* "markdown"
  "Output format: 'markdown' or 'org'.")

(defvar *hyperfractal-browser* "curl"
  "Browser backend: 'curl', 'chrome', or 'firefox'.")

(defvar *hyperfractal-auth-file* (merge-pathnames ".netrc" (user-homedir-pathname))
  "Path to netrc-style auth file.")

(defvar *hyperfractal-cache* (make-hash-table :test 'equal)
  "In-memory cache for converted content.")

(defvar *hyperfractal-cache-order* '()
  "LRU order for cache eviction.")

(defvar *hyperfractal-history* '()
  "Navigation history stack.")

(defvar *hyperfractal-history-position* 0
  "Current position in navigation history.")

;;* configuration loading

(defun hyperfractal-load-env-config ()
  "Load Hyperfractal configuration from environment variables."
  (let ((model (uiop:getenv "HYPERFRACTAL_MODEL"))
        (cache-size (uiop:getenv "HYPERFRACTAL_CACHE_SIZE"))
        (format (uiop:getenv "HYPERFRACTAL_FORMAT"))
        (browser (uiop:getenv "HYPERFRACTAL_BROWSER"))
        (auth-file (uiop:getenv "HYPERFRACTAL_AUTH_FILE")))
    (when model (setf *hyperfractal-default-model* model))
    (when cache-size 
      (let ((size (parse-integer cache-size :junk-allowed t)))
        (when size (setf *hyperfractal-cache-size* size))))
    (when format (setf *hyperfractal-default-format* format))
    (when browser (setf *hyperfractal-browser* browser))
    (when auth-file (setf *hyperfractal-auth-file* (pathname auth-file)))))

(defun hyperfractal-load-toml-config ()
  "Load Hyperfractal configuration from TOML file."
  (let ((config-file (merge-pathnames "hyperfractal/hyperfractal.toml" (get-xdg-config-dir))))
    (when (probe-file config-file)
      (handler-case
          (let* ((content (uiop:read-file-string config-file))
                 (config (cl-toml:parse content))
                 (hf-table (gethash "hyperfractal" config)))
            (when hf-table
              (let ((model (gethash "model" hf-table))
                    (cache-size (gethash "cache_size" hf-table))
                    (format (gethash "format" hf-table))
                    (browser (gethash "browser" hf-table))
                    (auth-file (gethash "auth_file" hf-table)))
                (when model (setf *hyperfractal-default-model* model))
                (when cache-size (setf *hyperfractal-cache-size* cache-size))
                (when format (setf *hyperfractal-default-format* format))
                (when browser (setf *hyperfractal-browser* browser))
                (when auth-file (setf *hyperfractal-auth-file* (pathname auth-file))))))
        (error (e)
          (format t "~&Warning: Error loading hyperfractal.toml: ~A~%" e))))))

(defun hyperfractal-init ()
  "Initialize Hyperfractal configuration."
  (hyperfractal-load-env-config)
  (hyperfractal-load-toml-config))

;;* authentication

(defun hyperfractal-parse-netrc (file)
  "Parse a netrc-format authentication file.
   Returns an alist of ((host . ((user . username) (password . pass))) ...)"
  (when (probe-file file)
    (handler-case
        (let ((content (uiop:read-file-string file))
              (entries '())
              (current-machine nil)
              (current-login nil)
              (current-password nil))
          (dolist (token (cl-ppcre:split "\\s+" content))
            (cond
              ((string-equal token "machine")
               (when current-machine
                 (push (cons current-machine 
                             (list (cons :user current-login)
                                   (cons :password current-password)))
                       entries))
               (setf current-machine nil current-login nil current-password nil))
              ((string-equal token "login") nil)
              ((string-equal token "password") nil)
              ((null current-machine) (setf current-machine token))
              ((null current-login) (setf current-login token))
              ((null current-password) (setf current-password token))))
          (when current-machine
            (push (cons current-machine 
                        (list (cons :user current-login)
                              (cons :password current-password)))
                  entries))
          (nreverse entries))
      (error (e)
        (format t "~&Warning: Error parsing netrc file: ~A~%" e)
        nil))))

(defun hyperfractal-get-auth (host)
  "Get authentication credentials for a host from the auth file."
  (let ((entries (hyperfractal-parse-netrc *hyperfractal-auth-file*)))
    (cdr (assoc host entries :test #'string-equal))))

;;* caching

(defun hyperfractal-cache-key (url format)
  "Generate a cache key for URL and format."
  (format nil "~A::~A" url format))

(defun hyperfractal-cache-size-bytes ()
  "Calculate current cache size in bytes."
  (let ((total 0))
    (maphash (lambda (key value)
               (declare (ignore key))
               (incf total (length value)))
             *hyperfractal-cache*)
    total))

(defun hyperfractal-cache-evict ()
  "Evict oldest entries until cache is under size limit."
  (let ((max-bytes (* *hyperfractal-cache-size* 1024 1024)))
    (loop while (and *hyperfractal-cache-order*
                     (> (hyperfractal-cache-size-bytes) max-bytes))
          do (let ((oldest-key (car (last *hyperfractal-cache-order*))))
               (remhash oldest-key *hyperfractal-cache*)
               (setf *hyperfractal-cache-order* 
                     (butlast *hyperfractal-cache-order*))))))

(defun hyperfractal-cache-get (url format)
  "Get content from cache, updating LRU order."
  (let* ((key (hyperfractal-cache-key url format))
         (content (gethash key *hyperfractal-cache*)))
    (when content
      ;; Move to front of LRU list
      (setf *hyperfractal-cache-order* 
            (cons key (remove key *hyperfractal-cache-order* :test #'string=))))
    content))

(defun hyperfractal-cache-put (url format content)
  "Store content in cache with LRU eviction."
  (let ((key (hyperfractal-cache-key url format)))
    (setf (gethash key *hyperfractal-cache*) content)
    (setf *hyperfractal-cache-order*
          (cons key (remove key *hyperfractal-cache-order* :test #'string=)))
    (hyperfractal-cache-evict)
    content))

(defun hyperfractal-cache-clear ()
  "Clear the entire cache."
  (clrhash *hyperfractal-cache*)
  (setf *hyperfractal-cache-order* '()))

;;* URL fetching

(defun hyperfractal-fetch-with-curl (url &key auth)
  "Fetch URL content using curl."
  (let* ((auth-args (when auth
                      (list "-u" (format nil "~A:~A" 
                                         (cdr (assoc :user auth))
                                         (cdr (assoc :password auth))))))
         (args (append (list "curl" "-sL" "-A" "Mozilla/5.0 (compatible; Hyperfractal/1.0)")
                       auth-args
                       (list url))))
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program args :output :string :error-output :string :ignore-error-status t)
          (if (zerop exit-code)
              output
              (progn
                (format t "~&Error fetching URL: ~A~%" error-output)
                nil)))
      (error (e)
        (format t "~&Error running curl: ~A~%" e)
        nil))))

(defun hyperfractal-fetch-with-browser (url browser)
  "Fetch URL content using headless browser."
  (let* ((browser-cmd (cond
                        ((string-equal browser "chrome")
                         (list "chromium" "--headless" "--dump-dom" url))
                        ((string-equal browser "firefox")
                         (list "firefox" "--headless" "-screenshot" "-" url))
                        (t (error "Unknown browser: ~A" browser)))))
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program browser-cmd :output :string :error-output :string :ignore-error-status t)
          (declare (ignore error-output))
          (if (zerop exit-code)
              output
              nil))
      (error (e)
        (format t "~&Error running browser: ~A~%" e)
        nil))))

(defun hyperfractal-extract-host (url)
  "Extract host from URL string."
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "^https?://([^/:]+)" url)
    (when match
      (aref regs 0))))

(defun hyperfractal-fetch-url (url)
  "Fetch URL content using configured backend."
  (let* ((host (hyperfractal-extract-host url))
         (auth (when host (hyperfractal-get-auth host))))
    (cond
      ((string-equal *hyperfractal-browser* "curl")
       (hyperfractal-fetch-with-curl url :auth auth))
      ((member *hyperfractal-browser* '("chrome" "firefox") :test #'string-equal)
       (hyperfractal-fetch-with-browser url *hyperfractal-browser*))
      (t
       (hyperfractal-fetch-with-curl url :auth auth)))))

;;* HTML to plaintext conversion

(defun hyperfractal-html-to-markdown (html url)
  "Convert HTML to Markdown using LLM."
  (let* ((model (or (when (stringp *hyperfractal-default-model*)
                      (find-model-by-name *hyperfractal-default-model*))
                    *current-model*))
         (prompt-path (get-prompt-path "html-to-markdown.mustache"))
         (system-prompt (if (probe-file prompt-path)
                            (uiop:read-file-string prompt-path)
                            "You are an expert at converting HTML to clean, readable Markdown.
Extract the main content from the HTML, ignoring navigation, ads, and boilerplate.
Preserve important formatting: headings, lists, code blocks, links, and emphasis.
Output ONLY the Markdown content, no explanations or preamble."))
         (user-prompt (format nil "Convert this HTML from ~A to clean Markdown:~%~%~A" url html)))
    (unless model
      (error "No LLM model available. Set *current-model* or *hyperfractal-default-model*."))
    (let ((provider-type (intern (string-upcase (model-config-provider model)) :keyword)))
      (llm:complete provider-type
                    `(((:role . "user") (:content . ,user-prompt)))
                    :model (model-config-model-name model)
                    :max-tokens (model-config-max-output-tokens model)
                    :system-prompt system-prompt
                    :stream nil))))

(defun hyperfractal-html-to-org (html url)
  "Convert HTML to Org-mode using LLM."
  (let* ((model (or (when (stringp *hyperfractal-default-model*)
                      (find-model-by-name *hyperfractal-default-model*))
                    *current-model*))
         (prompt-path (get-prompt-path "html-to-org.mustache"))
         (system-prompt (if (probe-file prompt-path)
                            (uiop:read-file-string prompt-path)
                            "You are an expert at converting HTML to clean, readable Org-mode format.
Extract the main content from the HTML, ignoring navigation, ads, and boilerplate.
Use proper Org-mode syntax: * for headings, - for lists, #+begin_src for code, [[url][text]] for links.
Output ONLY the Org-mode content, no explanations or preamble."))
         (user-prompt (format nil "Convert this HTML from ~A to clean Org-mode format:~%~%~A" url html)))
    (unless model
      (error "No LLM model available. Set *current-model* or *hyperfractal-default-model*."))
    (let ((provider-type (intern (string-upcase (model-config-provider model)) :keyword)))
      (llm:complete provider-type
                    `(((:role . "user") (:content . ,user-prompt)))
                    :model (model-config-model-name model)
                    :max-tokens (model-config-max-output-tokens model)
                    :system-prompt system-prompt
                    :stream nil))))

(defun hyperfractal-convert-html (html url &key (format *hyperfractal-default-format*))
  "Convert HTML to specified format."
  (cond
    ((string-equal format "markdown")
     (hyperfractal-html-to-markdown html url))
    ((string-equal format "org")
     (hyperfractal-html-to-org html url))
    (t
     (error "Unknown format: ~A. Use 'markdown' or 'org'." format))))

;;* link extraction

(defun hyperfractal-extract-links-markdown (content)
  "Extract links from Markdown content.
   Returns list of (text . url) pairs."
  (let ((links '()))
    (cl-ppcre:do-register-groups (text url)
        ("\\[([^\\]]+)\\]\\(([^)]+)\\)" content)
      (push (cons text url) links))
    (nreverse links)))

(defun hyperfractal-extract-links-org (content)
  "Extract links from Org-mode content.
   Returns list of (text . url) pairs."
  (let ((links '()))
    (cl-ppcre:do-register-groups (url text)
        ("\\[\\[([^\\]]+)\\]\\[([^\\]]+)\\]\\]" content)
      (push (cons text url) links))
    (nreverse links)))

(defun hyperfractal-extract-links (content &key (format *hyperfractal-default-format*))
  "Extract links from content based on format."
  (cond
    ((string-equal format "markdown")
     (hyperfractal-extract-links-markdown content))
    ((string-equal format "org")
     (hyperfractal-extract-links-org content))
    (t '())))

;;* navigation

(defun hyperfractal-history-push (url)
  "Push URL to navigation history."
  ;; Truncate forward history if we're not at the end
  (when (> *hyperfractal-history-position* 0)
    (setf *hyperfractal-history* 
          (nthcdr *hyperfractal-history-position* *hyperfractal-history*))
    (setf *hyperfractal-history-position* 0))
  (push url *hyperfractal-history*))

(defun hyperfractal-history-back ()
  "Go back in navigation history. Returns URL or nil."
  (when (< *hyperfractal-history-position* (1- (length *hyperfractal-history*)))
    (incf *hyperfractal-history-position*)
    (nth *hyperfractal-history-position* *hyperfractal-history*)))

(defun hyperfractal-history-forward ()
  "Go forward in navigation history. Returns URL or nil."
  (when (> *hyperfractal-history-position* 0)
    (decf *hyperfractal-history-position*)
    (nth *hyperfractal-history-position* *hyperfractal-history*)))

;;* URL resolution

(defun hyperfractal-resolve-url (link-url base-url)
  "Resolve a potentially relative URL against a base URL."
  (if (cl-ppcre:scan "^https?://" link-url)
      link-url
      (multiple-value-bind (match regs)
          (cl-ppcre:scan-to-strings "^(https?://[^/]+)" base-url)
        (if match
            (let ((base (aref regs 0)))
              (if (str:starts-with-p "/" link-url)
                  (concatenate 'string base link-url)
                  (let ((base-path (cl-ppcre:regex-replace "/[^/]*$" base-url "/")))
                    (concatenate 'string base-path link-url))))
            link-url))))

;;* main functions

(defun hyperfractal-browse (url &key (format *hyperfractal-default-format*) (use-cache t))
  "Browse a URL and return converted plaintext content."
  (when use-cache
    (let ((cached (hyperfractal-cache-get url format)))
      (when cached
        (hyperfractal-history-push url)
        (return-from hyperfractal-browse cached))))
  
  (let ((html (hyperfractal-fetch-url url)))
    (unless html
      (return-from hyperfractal-browse nil))
    
    (let ((content (hyperfractal-convert-html html url :format format)))
      (when content
        (hyperfractal-cache-put url format content)
        (hyperfractal-history-push url))
      content)))

(defun hyperfractal-browse-and-print (url &key (format *hyperfractal-default-format*))
  "Browse URL and print the result."
  (let ((content (hyperfractal-browse url :format format)))
    (if content
        (format t "~A" content)
        (format t "~&Failed to fetch or convert: ~A~%" url))))

;;* CLI interface

(define-sub-command hyperfractal (args)
  "Fetch URL and convert to plaintext.
   Usage: hactar hyperfractal [options] <url>
   
   Options:
     -m, --model MODEL      LLM model to use
     -f, --format FORMAT    Output format (markdown/org)
     --clear-cache          Clear the cache"
  (hyperfractal-init)
  
  (let* ((model-opt (getf args :model))
         (format-opt (or (getf args :format) *hyperfractal-default-format*))
         (clear-cache (getf args :clear-cache))
         (url (first (uiop:ensure-list (getf args :args)))))
    
    (when clear-cache
      (hyperfractal-cache-clear)
      (format t "~&Cache cleared.~%"))
    
    (when model-opt
      (let ((model (find-model-by-name model-opt)))
        (if model
            (setf *hyperfractal-default-model* model-opt)
            (format t "~&Warning: Model '~A' not found, using default.~%" model-opt))))
    
    (cond
      ((and clear-cache (not url))
       nil)  ; Already printed cache cleared message, nothing more to do
      (url
       (hyperfractal-browse-and-print url :format format-opt))
      (t
       (format t "~&Usage: hactar hyperfractal [options] <url>~%"))))
  
  :cli-options ((:short "m" :long "model" :description "LLM model to use")
                (:short "f" :long "format" :description "Output format (markdown/org)")
                (:long "clear-cache" :description "Clear the cache")))

;;* slash commands

(define-slash-command hf (args)
  "Browse a URL and convert to plaintext. Usage: /hf <url> [--format markdown|org]"
  (hyperfractal-init)
  (let* ((url (first args))
         (rest-args (rest args))
         (format-opt *hyperfractal-default-format*))
    (loop for (arg next) on rest-args
          when (or (string= arg "--format") (string= arg "-f"))
          do (setf format-opt next))
    (if url
        (hyperfractal-browse-and-print url :format format-opt)
        (format t "~&Usage: /hf <url> [--format markdown|org]~%"))))

(define-slash-command hf-back (args)
  "Go back in Hyperfractal navigation history."
  (declare (ignore args))
  (let ((url (hyperfractal-history-back)))
    (if url
        (hyperfractal-browse-and-print url)
        (format t "~&No previous page in history.~%"))))

(define-slash-command hf-forward (args)
  "Go forward in Hyperfractal navigation history."
  (declare (ignore args))
  (let ((url (hyperfractal-history-forward)))
    (if url
        (hyperfractal-browse-and-print url)
        (format t "~&No next page in history.~%"))))

(define-slash-command hf-links (args)
  "Show links from the last viewed page."
  (declare (ignore args))
  (if *hyperfractal-history*
      (let* ((url (first *hyperfractal-history*))
             (content (hyperfractal-cache-get url *hyperfractal-default-format*)))
        (if content
            (let ((links (hyperfractal-extract-links content)))
              (if links
                  (loop for (text . link-url) in links
                        for i from 1
                        do (format t "~3D. [~A] ~A~%" i text link-url))
                  (format t "~&No links found.~%")))
            (format t "~&No cached content for current page.~%")))
      (format t "~&No pages in history.~%")))

(define-slash-command hf-goto (args)
  "Go to a link by number from /hf-links output."
  (let ((num (parse-integer (or (first args) "") :junk-allowed t)))
    (if (and num *hyperfractal-history*)
        (let* ((url (first *hyperfractal-history*))
               (content (hyperfractal-cache-get url *hyperfractal-default-format*)))
          (if content
              (let ((links (hyperfractal-extract-links content)))
                (if (and links (<= 1 num (length links)))
                    (let* ((link-url (cdr (nth (1- num) links)))
                           (resolved-url (hyperfractal-resolve-url link-url url)))
                      (hyperfractal-browse-and-print resolved-url))
                    (format t "~&Invalid link number. Use /hf-links to see available links.~%")))
              (format t "~&No cached content for current page.~%")))
        (format t "~&Usage: /hf-goto <number>~%"))))

(define-slash-command hf-cache-clear (args)
  "Clear the Hyperfractal cache."
  (declare (ignore args))
  (hyperfractal-cache-clear)
  (format t "~&Hyperfractal cache cleared.~%"))

;;* web command integration

(defwebcommand browse
  "Browse web pages as plaintext."

  (defwebroute browse-url "Fetch and convert a URL to plaintext"
    (url &rest args) (url args)
    :priority 10
    (lambda ()
      (hyperfractal-init)
      (let ((format-opt *hyperfractal-default-format*))
        (loop for (arg next) on args
              when (or (string= arg "--format") (string= arg "-f"))
              do (setf format-opt next))
        (hyperfractal-browse-and-print url :format format-opt))))

  (def-default-route ()
    (lambda ()
      (format t "Browse Commands:~%  browse <url> [--format markdown|org] - Convert webpage to plaintext~%"))))
