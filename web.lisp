;; Web command abstraction for working with URLs and web requests
(in-package :hactar)

(defvar *web-commands* (make-hash-table :test 'equal)
  "Hash table of available web commands.")

(defstruct web-route
  "Represents a route within a web command."
  name           ; Route name (symbol)
  description    ; Route description (string)
  pattern        ; List pattern to match, e.g., ("newest" &rest args)
  priority       ; Integer priority (higher = checked first)
  handler        ; Function that returns code to execute
  bindings)      ; List of variable names to bind from the pattern

(defstruct web-command
  "Represents a web command with routes."
  name           ; Command name (string)
  description    ; Command description
  routes         ; List of web-route structs
  default-route) ; Default route handler (optional)

(defun match-route-pattern (pattern args)
  "Match a route pattern against arguments. Returns bindings alist (non-NIL on success) or NIL."
  (labels ((match-list (pat arg-list bindings)
             (cond
               ;; Pattern exhausted
               ((null pat)
                (if (null arg-list) (or bindings '(())) nil))
               ;; &rest pattern
               ((eq (car pat) '&rest)
                (if (null (cdr pat))
                    nil ; Invalid pattern
                    (acons (cadr pat) arg-list bindings)))
               ;; &optional not implemented yet, but could be added
               ;; Literal string match
               ((stringp (car pat))
                (if (and arg-list (string= (car pat) (car arg-list)))
                    (match-list (cdr pat) (cdr arg-list) bindings)
                    nil))
               ;; Variable binding
               ((symbolp (car pat))
                (if arg-list
                    (match-list (cdr pat) (cdr arg-list)
                                (acons (car pat) (car arg-list) bindings))
                    nil))
               ;; Nested list (not supported in this simple implementation)
               (t nil))))
    (match-list pattern args nil)))

(defun find-matching-route (web-cmd args)
  "Find the first matching route for the given arguments."
  (let ((sorted-routes (sort (copy-list (web-command-routes web-cmd))
                             #'> :key #'web-route-priority)))
    (loop for route in sorted-routes
          for bindings = (match-route-pattern (web-route-pattern route) args)
          when bindings do (return (values route bindings))
          finally (return (values nil nil)))))

(defmacro defwebroute (name description pattern bindings &rest args)
  "Define a route within a defwebcommand. Pattern is matched against args.
   NAME is a symbol identifying this route.
   DESCRIPTION is a string documenting what this route does.
   PATTERN is matched against args.
   BINDINGS specifies which variables from the pattern to bind.
   Body should return code to execute.
   Accepts :priority keyword (default 10) followed by body forms."
  (let ((priority 10)
        (body '()))
    ;; Parse args: look for :priority keyword, rest is body
    (loop for arg in args
          for prev = nil then arg
          do (cond
               ((eq prev :priority) (setf priority arg))
               ((eq arg :priority) nil) ; Skip :priority keyword itself
               (t (push arg body))))
    (setf body (nreverse body))
    `(make-web-route
      :pattern ',pattern
      :priority ,priority
      :bindings ',bindings
      :handler (lambda ,bindings
                 ,@body)
      :name ',name
      :description ,description)))

(defmacro def-default-route (bindings &body body)
  "Define a default route for a web command (when no other routes match)."
  `(lambda ,bindings
     ,@body))

(defmacro defwebcommand (name description &rest route-definitions)
  "Define a web command with routes.

   Example:
   (defwebcommand hn
     \"Fetch news from Hacker News\"
     (defwebroute newest-hn
      \"Get the newest from HN\"
       (\"newest\" &rest args) (args)
       :priority 10
       `(get-hn-newest ',args))
     (def-default-route ()
       `(get-hn-front-page-md)))"
  (let ((routes '())
        (default-route nil))
    ;; Process route definitions
    (dolist (form route-definitions)
      (cond
       ((and (listp form) (eq (car form) 'defwebroute))
        (push form routes))
       ((and (listp form) (eq (car form) 'def-default-route))
        (setf default-route form))))

    (let ((command-name-str (string-downcase (symbol-name name))))
      `(progn
         ;; Create the web command structure
         (setf (gethash ,command-name-str *web-commands*)
               (make-web-command
                :name ,command-name-str
                :description ,description
                :routes (list ,@(nreverse routes))
                :default-route ,(when default-route default-route)))

         ;; Register as a sub-command
         (define-sub-command ,name (args)
			     ,description
			     (execute-web-command ,command-name-str args))))))

(defun %invoke-route-result (value)
  "Interpret a route handler's return VALUE without using EVAL.
If VALUE is a list like (fn arg1 arg2), call fn with args.
If an argument is of the form (quote X), it is unwrapped to X.
If VALUE is a function, call it with no arguments.
Otherwise, return VALUE as-is."
  (cond
    ((functionp value) (funcall value))
    ((and (consp value) (eq (car value) 'quote))
     ;; If it's a quoted form, evaluate the inner form
     (%invoke-route-result (cadr value)))
    ((and (consp value) (symbolp (car value)))
     (let* ((fn (car value))
            (args (mapcar (lambda (arg)
                            (if (and (consp arg) (eq (car arg) 'quote))
                                (cadr arg)
                                arg))
                          (cdr value))))
       (apply (symbol-function fn) args)))
    ((stringp value) value) ; Return strings as-is
    (t value)))

(defun execute-web-command (command-name args)
  "Execute a web command with the given arguments."
  (let ((web-cmd (gethash command-name *web-commands*)))
    (unless web-cmd
      (format t "Unknown web command: ~A~%" command-name)
      (return-from execute-web-command nil))

    (multiple-value-bind (route bindings)
        (find-matching-route web-cmd args)
      (let ((code-to-eval
              (if route
                  ;; Route matched, call its handler with bindings
                  (let ((bound-values
                          (mapcar (lambda (var)
                                    (cdr (assoc var bindings)))
                                  (web-route-bindings route))))
                    (apply (web-route-handler route) bound-values))
                  ;; No route matched, try default
                  (if (web-command-default-route web-cmd)
                      (funcall (web-command-default-route web-cmd))
                      (progn
                        (format t "No matching route for command: ~A ~{~A~^ ~}~%"
                                command-name args)
                        (return-from execute-web-command nil))))))

        ;; Evaluate/execute the returned value without using EVAL
        (handler-case
            (let ((result (%invoke-route-result code-to-eval)))
              (when (stringp result)
                (format t "~A" result))
              result)
          (error (e)
            (format t "Error executing web command ~A: ~A~%" command-name e)
            nil))))))

;;; Example: Hacker News command using RSS feeds

(defun parse-rss-feed (xml-string)
  "Parse RSS feed XML and extract items."
  (handler-case
      (let* ((doc (cxml:parse xml-string (cxml-dom:make-dom-builder)))
             (items (dom:get-elements-by-tag-name doc "item"))
             (results '()))
        (loop for i from 0 below (dom:length items)
              for item = (dom:item items i)
              do (let ((title-nodes (dom:get-elements-by-tag-name item "title"))
                       (link-nodes (dom:get-elements-by-tag-name item "link"))
                       (description-nodes (dom:get-elements-by-tag-name item "description"))
                       (pubdate-nodes (dom:get-elements-by-tag-name item "pubDate")))
                   (push `((:title . ,(when (> (dom:length title-nodes) 0)
                                       (dom:node-value (dom:first-child (dom:item title-nodes 0)))))
                           (:link . ,(when (> (dom:length link-nodes) 0)
                                      (dom:node-value (dom:first-child (dom:item link-nodes 0)))))
                           (:description . ,(when (> (dom:length description-nodes) 0)
                                             (dom:node-value (dom:first-child (dom:item description-nodes 0)))))
                           (:pubdate . ,(when (> (dom:length pubdate-nodes) 0)
                                         (dom:node-value (dom:first-child (dom:item pubdate-nodes 0))))))
                         results)))
        (nreverse results))
    (error (e)
      (format t "Error parsing RSS feed: ~A~%" e)
      nil)))

(defun fetch-hn-feed (feed-path &key (limit nil))
  "Fetch and parse a Hacker News RSS feed."
  (let* ((url (format nil "https://hnrss.org~A" feed-path))
         (xml-content (fetch-url-content url)))
    (when xml-content
      (let ((items (parse-rss-feed xml-content)))
        (if limit
            (subseq items 0 (min limit (length items)))
            items)))))

(defun format-hn-items-markdown (items)
  "Format HN items as markdown."
  (with-output-to-string (s)
    (loop for item in items
          for title = (cdr (assoc :title item))
          for link = (cdr (assoc :link item))
          do (format s "- [~A](~A)~%" title link))))

(defun get-hn-newest (args)
  "Fetch newest posts from Hacker News."
  (let ((limit 25))
    ;; Parse args for --limit
    (loop for arg in args
          for i from 0
          when (string= arg "--limit")
          do (let ((limit-val (parse-integer (nth (1+ i) args) :junk-allowed t)))
               (when limit-val (setf limit limit-val))))

    (let ((items (fetch-hn-feed "/newest" :limit limit)))
      (if items
          (format t "~A" (format-hn-items-markdown items))
          (format t "Failed to fetch HN newest posts.~%")))))

(defun get-hn-top (args)
  "Fetch top posts from Hacker News."
  (let ((limit 25))
    ;; Parse args for --limit
    (loop for arg in args
          for i from 0
          when (string= arg "--limit")
          do (let ((limit-val (parse-integer (nth (1+ i) args) :junk-allowed t)))
               (when limit-val (setf limit limit-val))))

    (let ((items (fetch-hn-feed "/frontpage" :limit limit)))
      (if items
          (format t "~A" (format-hn-items-markdown items))
          (format t "Failed to fetch HN top posts.~%")))))

(defun get-hn-front-page-md ()
  "Fetch front page of Hacker News as markdown."
  (let ((items (fetch-hn-feed "/frontpage" :limit 30)))
    (if items
        (format t "# Hacker News Front Page~%~%~A"
                (format-hn-items-markdown items))
        (format t "Failed to fetch HN front page.~%"))))

;; Define the HN web command
(defwebcommand hn
  "Fetch news from news.ycombinator.com (Hacker News)."

  (defwebroute hn-newest "Fetch newest posts from Hacker News"
    ("newest" &rest args) (args)
    :priority 10
    (lambda () (get-hn-newest args)))

  (defwebroute hn-top "Fetch top posts from Hacker News"
    ("top" &rest args) (args)
    :priority 10
    (lambda () (get-hn-top args)))

  (def-default-route ()
    (lambda () (get-hn-front-page-md))))
