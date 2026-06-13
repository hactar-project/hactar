;; knowledge repos encoded as lisp
(in-package :hactar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defdocset defendpoint defcomponent defvariable defimporter
            *wiki-registry* *wiki-docsets*
            parse-wiki-uri dispatch-wiki-query
            find-wiki-node has-resource-p extract-resource
            wiki-import wiki-list wiki-show)))

(defvar *wiki-registry* (make-hash-table :test 'equal)
  "Hash table mapping fully-qualified wiki keys (\"namespace/path\") to wiki-node structs.")

(defvar *wiki-docsets* (make-hash-table :test 'equal)
  "Hash table mapping docset namespace strings to docset metadata plists.")

(defvar *wiki-importers* (make-hash-table :test 'equal)
  "Hash table of importer name -> function.")

(defstruct wiki-node
  "A single entry in the wiki knowledge graph."
  namespace                       ; string, e.g. "redwood.docs"
  path                            ; string, e.g. "auth/login"
  (type :endpoint)                ; :endpoint :component :variable :other
  (properties (make-hash-table :test 'equal)) ; resource-name (string) -> value
  (related '())                   ; list of (namespace . path) cons pairs
  (stack-tags '()))               ; inherited from docset

(defstruct wiki-docset
  name                            ; e.g. "redwood.docs"
  version
  stack-tags
  description)

(defun %node-key (namespace path)
  "Build the hash key for a wiki node."
  (format nil "~A/~A" namespace path))

(defun %normalize-keyword-string (x)
  "Coerce keyword or symbol or string to a downcased string."
  (cond ((null x) nil)
        ((stringp x) x)
        ((symbolp x) (string-downcase (symbol-name x)))
        (t (princ-to-string x))))

(defun register-wiki-node (namespace path &key (type :endpoint) properties related stack-tags)
  "Register a wiki node. PROPERTIES is a plist of resource-name -> value."
  (let* ((ns (%normalize-keyword-string namespace))
         (p (%normalize-keyword-string path))
         (key (%node-key ns p))
         (node (make-wiki-node :namespace ns
                               :path p
                               :type type
                               :related related
                               :stack-tags stack-tags)))
    (loop for (k v) on properties by #'cddr
          do (setf (gethash (%normalize-keyword-string k)
                            (wiki-node-properties node))
                   v))
    (setf (gethash key *wiki-registry*) node)
    node))

(defun register-wiki-docset (name &key version stack-tags description)
  "Register docset metadata."
  (let* ((ns (%normalize-keyword-string name))
         (ds (make-wiki-docset :name ns
                               :version version
                               :stack-tags stack-tags
                               :description description)))
    (setf (gethash ns *wiki-docsets*) ds)
    ds))
(defun %extract-docset-and-path (qualified-name)
  "Given a symbol like redwood.docs/auth/login, return (values namespace path).
If no slash is present, namespace is NIL."
  (let* ((s (if (symbolp qualified-name) (symbol-name qualified-name) qualified-name))
         (s (string-downcase s))
         (slash (position #\/ s)))
    (if slash
        (values (subseq s 0 slash) (subseq s (1+ slash)))
        (values nil s))))
;;* macros
(defmacro defdocset (name &key version stack-tags description)
  "Declare a docset namespace. Subsequent defendpoint/defcomponent/defvariable
calls in this file inherit its STACK-TAGS unless overridden."
  `(register-wiki-docset ',name
                         :version ,version
                         :stack-tags ',stack-tags
                         :description ,description))

(defmacro defendpoint (qualified-name &rest props)
  "Define an endpoint node. QUALIFIED-NAME is like redwood.docs/auth/login."
  (multiple-value-bind (ns path) (%extract-docset-and-path qualified-name)
    `(register-wiki-node ,ns ,path
                         :type :endpoint
                         :properties (list ,@props)
                         :related ',(getf props :related)
                         :stack-tags ',(getf props :stack-tags))))

(defmacro defcomponent (qualified-name &rest props)
  "Define a component node (e.g., UI component)."
  (multiple-value-bind (ns path) (%extract-docset-and-path qualified-name)
    `(register-wiki-node ,ns ,path
                         :type :component
                         :properties (list ,@props)
                         :related ',(getf props :related)
                         :stack-tags ',(getf props :stack-tags))))

(defmacro defvariable (qualified-name &rest props)
  "Define a variable/configuration node."
  (multiple-value-bind (ns path) (%extract-docset-and-path qualified-name)
    `(register-wiki-node ,ns ,path
                         :type :variable
                         :properties (list ,@props)
                         :related ',(getf props :related)
                         :stack-tags ',(getf props :stack-tags))))

(defun parse-wiki-uri (uri-string)
  "Parse a wiki URI into (values namespace path resource).

Forms accepted:
  redwood.docs/auth/login.types  -> (\"redwood.docs\" \"auth/login\" \"types\")
  redwood.docs/auth.types        -> (\"redwood.docs\" \"auth\" \"types\")
  redwood.docs/auth              -> (\"redwood.docs\" \"auth\" nil)
  auth.types                     -> (nil \"auth\" \"types\")
  wiki:redwood.docs/auth.types   -> same as above (strip wiki: prefix)"
  (let ((s (string-downcase (string uri-string))))
    (when (str:starts-with? "wiki:" s)
      (setf s (subseq s 5)))
    (let* ((slash (position #\/ s))
           (namespace (when slash (subseq s 0 slash)))
           (rest (if slash (subseq s (1+ slash)) s))
           (dot (position #\. rest :from-end t))
           (path (if dot (subseq rest 0 dot) rest))
           (resource (when dot (subseq rest (1+ dot)))))
      (values namespace
              (if (string= path "") nil path)
              resource))))

(defun find-wiki-node (namespace path)
  "Find a wiki node. If NAMESPACE is nil, infer from *STACK*: try each
'<stack-item>.docs' until a node is found."
  (cond
    (namespace
     (gethash (%node-key namespace path) *wiki-registry*))
    (t
     (loop for tech in *stack*
           for tech-str = (string-downcase (if (stringp tech) tech (string tech)))
           for candidate-ns = (format nil "~A.docs" tech-str)
           for node = (gethash (%node-key candidate-ns path) *wiki-registry*)
           when node return node
           finally (return nil)))))

(defun has-resource-p (node resource)
  "T if NODE has the given resource property defined."
  (and node resource
       (multiple-value-bind (val present)
           (gethash resource (wiki-node-properties node))
         (declare (ignore val))
         present)))

(defun extract-resource (node resource)
  "Return the value of NODE's RESOURCE property."
  (gethash resource (wiki-node-properties node)))

(defun find-nodes-under (namespace path-prefix)
  "Return all nodes whose key begins with NAMESPACE/PATH-PREFIX."
  (let ((prefix (%node-key namespace path-prefix))
        (results '()))
    (maphash (lambda (k v)
               (when (or (string= k prefix) (str:starts-with? (format nil "~A/" prefix) k))
                 (push v results)))
             *wiki-registry*)
    results))

(defun %nodes->context-text (nodes)
  "Serialize a list of wiki-nodes into a textual context blob."
  (with-output-to-string (s)
    (dolist (node nodes)
      (format s "~%Node: ~A/~A (~A)~%"
              (wiki-node-namespace node)
              (wiki-node-path node)
              (wiki-node-type node))
      (maphash (lambda (k v)
                 (format s "  ~A: ~A~%" k v))
               (wiki-node-properties node)))))

(defun wiki-synthesize-with-llm (node resource)
  "Ask the LLM to synthesize the requested RESOURCE for an existing NODE.
Looks at sibling nodes under the same path-prefix for richer context."
  (let* ((siblings (find-nodes-under (wiki-node-namespace node)
                                     (or (subseq (wiki-node-path node) 0
                                                 (or (position #\/ (wiki-node-path node) :from-end t)
                                                     (length (wiki-node-path node))))
                                         "")))
         (context (%nodes->context-text (or siblings (list node))))
         (prompt (format nil "You are answering a knowledge graph query.~%~%Node: ~A/~A~%Requested resource: ~A~%~%Related nodes:~%~A~%~%Synthesize the requested resource as concisely and accurately as possible. If it would normally be code (e.g. types, curl, example), return it inside a fenced code block."
                         (wiki-node-namespace node)
                         (wiki-node-path node)
                         resource
                         context)))
    (if (and (fboundp 'get-llm-response) (boundp '*current-model*) *current-model*)
        (get-llm-response prompt :stream nil :add-to-history nil)
        (format nil "[wiki] No LLM available to synthesize ~A for ~A/~A"
                resource (wiki-node-namespace node) (wiki-node-path node)))))

(defun wiki-search-with-llm (namespace path resource)
  "Global fallback: no node matched. Ask the LLM with the user's full intent."
  (let ((prompt (format nil "You are answering a knowledge graph query in Hactar's wiki.~%The user asked for: ~A/~A~@[.~A~]~%~%No documentation node exists for this. Provide the best possible answer using your general knowledge. If the user clearly meant a specific framework, mention it. Keep the response focused."
                        (or namespace "(inferred)")
                        (or path "(root)")
                        resource)))
    (if (and (fboundp 'get-llm-response) (boundp '*current-model*) *current-model*)
        (get-llm-response prompt :stream nil :add-to-history nil)
        (format nil "[wiki] No node found for ~A/~A.~A and no LLM available."
                namespace path resource))))

(defun dispatch-wiki-query (uri-string)
  "Main entry point: parse URI, route to a node or LLM fallback."
  (multiple-value-bind (namespace path resource) (parse-wiki-uri uri-string)
    (let ((node (find-wiki-node namespace path)))
      (cond
        ((and node resource (has-resource-p node resource))
         (extract-resource node resource))
        (node
         (if resource
             (wiki-synthesize-with-llm node resource)
             ;; No resource specified — return the full node summary
             (%nodes->context-text (list node))))
        (t
         (wiki-search-with-llm namespace path resource))))))


(defun %install-wiki-routes ()
  "Register catch-all routes for wiki:* URIs and any '<ns>.docs/...' URIs."
  (unless (loop for r being the hash-values of *routes*
                thereis (and (stringp (route-pattern r))
                             (string= (route-pattern r) "^wiki:(.+)$")))
    (defroute "^wiki:(.+)$" (uri)
              :params (:uri)
              :priority 50
              (dispatch-wiki-query (concatenate 'string "wiki:" uri))))
  ;; Match patterns like <namespace>.docs/<path>(.resource)?
  (unless (loop for r being the hash-values of *routes*
                thereis (and (stringp (route-pattern r))
                             (string= (route-pattern r) "^([a-z0-9_-]+\\.docs)/(.+)$")))
    (defroute "^([a-z0-9_-]+\\.docs)/(.+)$" (ns rest)
              :params (:namespace :rest)
              :priority 25
              (dispatch-wiki-query (format nil "~A/~A" ns rest)))))

(push #'%install-wiki-routes *route-reinit-hooks*)

;; Set the global router fallback so any unknown URI falls into the wiki LLM
;; search. Other modules can override *route-fallback-fn* if desired.
(unless *route-fallback-fn*
  (setf *route-fallback-fn*
        (lambda (input)
          (handler-case
              (dispatch-wiki-query input)
            (error (e)
              (format t "~&Wiki fallback failed: ~A~%" e)
              nil)))))

(%install-wiki-routes)


(defmacro defimporter (name (&rest lambda-list) &body body)
  "Define an importer that produces a list of wiki node forms.

  (defimporter openapi (path &key namespace)
    ...emits defendpoint forms...)"
  `(setf (gethash ',(string-downcase (symbol-name name)) *wiki-importers*)
         (lambda ,lambda-list ,@body)))

(defun wiki-import (importer-name &rest args)
  "Invoke the named importer."
  (let ((fn (gethash (string-downcase (string importer-name)) *wiki-importers*)))
    (if fn
        (apply fn args)
        (progn
          (format t "~&Unknown importer: ~A~%" importer-name)
          nil))))

;;* commands
(define-command wiki (args)
  "Query the wiki knowledge graph.
Usage: /wiki <namespace.docs/path[.resource]>
       /wiki <path>.<resource>      (namespace inferred from *stack*)"
  (if (null args)
      (wiki-list)
      (let ((uri (first args)))
        (let ((result (dispatch-wiki-query uri)))
          (when result (format t "~A~%" result))
          result)))
  :acp (lambda (cmd-args)
         (if cmd-args
             (let ((result (dispatch-wiki-query (first cmd-args))))
               `(("text" . ,(format nil "~A" (or result "")))))
             `(("text" . ,(format nil "~A node(s) in wiki." (hash-table-count *wiki-registry*)))))))

(defun wiki-list (&optional namespace-filter)
  "Print all wiki nodes, optionally filtered by namespace prefix."
  (let ((nodes '()))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (or (null namespace-filter)
                         (str:starts-with? namespace-filter (wiki-node-namespace v)))
                 (push v nodes)))
             *wiki-registry*)
    (setf nodes (sort nodes #'string<
                      :key (lambda (n) (%node-key (wiki-node-namespace n)
                                                  (wiki-node-path n)))))
    (if nodes
        (progn
          (format t "Wiki nodes (~A):~%" (length nodes))
          (dolist (n nodes)
            (format t "  ~A/~A  [~A]~%"
                    (wiki-node-namespace n)
                    (wiki-node-path n)
                    (wiki-node-type n))))
        (format t "Wiki is empty. Use /wiki.import to load knowledge.~%"))))

(defun wiki-show (uri)
  "Display a single node and all its resources."
  (multiple-value-bind (ns path resource) (parse-wiki-uri uri)
    (declare (ignore resource))
    (let ((node (find-wiki-node ns path)))
      (if node
          (progn
            (format t "~A/~A  [~A]~%"
                    (wiki-node-namespace node)
                    (wiki-node-path node)
                    (wiki-node-type node))
            (maphash (lambda (k v) (format t "  ~A: ~A~%" k v))
                     (wiki-node-properties node))
            (when (wiki-node-related node)
              (format t "  related: ~A~%" (wiki-node-related node))))
          (format t "No node: ~A/~A~%" ns path)))))

(define-command wiki.list (args)
  "List nodes in the wiki knowledge graph."
  (wiki-list (first args)))

(define-command wiki.show (args)
  "Show a wiki node and its resources.
Usage: /wiki.show <namespace.docs/path>"
  (if args
      (wiki-show (first args))
      (format t "Usage: /wiki.show <namespace.docs/path>~%")))

(define-command wiki.import (args)
  "Run a wiki importer.
Usage: /wiki.import <importer-name> [args...]"
  (if args
      (let ((name (first args)))
        (apply #'wiki-import name (rest args)))
      (progn
        (format t "Available importers:~%")
        (maphash (lambda (k v) (declare (ignore v))
                   (format t "  ~A~%" k))
                 *wiki-importers*))))

;;* built in importer
(defimporter github-readme (url &key namespace)
  "Fetch a GitHub README and create a single wiki node from it."
  (let* ((content (fetch-url-content url))
         (ns (or namespace "github.docs"))
         (path (or (let ((m (nth-value 1 (cl-ppcre:scan-to-strings
                                          "github\\.com/([^/]+)/([^/]+)" url))))
                     (when (and m (> (length m) 1))
                       (format nil "~A/~A" (aref m 0) (aref m 1))))
                   "imported")))
    (when content
      (let ((out-file (uiop:native-namestring
                       (merge-pathnames
                        (format nil "wiki/~A.lisp"
                                (cl-ppcre:regex-replace-all
                                 "[^A-Za-z0-9_.-]"
                                 (format nil "~A-~A" ns path) "_"))
                        (or *repo-root* (uiop:getcwd))))))
        (ensure-directories-exist out-file)
        (with-open-file (s out-file :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create
                                    :external-format :utf-8)
          (format s ";;; Imported wiki node from ~A~%(in-package :hactar)~%~%" url)
          (let ((*print-case* :downcase))
            (prin1 `(register-wiki-node ,ns ,path
                                        :type :other
                                        :properties (list :readme ,content :source ,url))
                   s))
          (terpri s))
        (load out-file)
        (format t "Imported ~A/~A from ~A -> ~A~%" ns path url out-file)
        (list ns path)))))
