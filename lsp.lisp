;;; LSP Server — Language Server Protocol over stdio
;;; Designed to be extensible: register diagnostic providers for checker, compiler, AI, etc.
(in-package :hactar)

;;* Provider registry
;; Providers generate diagnostics (or other LSP features) for a given source file.
;; Each provider is called with (source file-path) and returns a list of lsp-diagnostic structs.

(defstruct lsp-diagnostic
  "A single LSP diagnostic."
  (range-start-line 0 :type fixnum)
  (range-start-char 0 :type fixnum)
  (range-end-line 0 :type fixnum)
  (range-end-char 0 :type fixnum)
  (severity 1 :type (integer 1 4))  ; 1=error, 2=warning, 3=info, 4=hint
  (code nil)
  (source "hactar" :type string)
  (message "" :type string))

(defstruct lsp-provider
  "A registered LSP diagnostic provider."
  (name "" :type string)
  (enabled t :type boolean)
  (function nil :type (or null function))  ; (lambda (source file-path) -> list of lsp-diagnostic)
  (languages nil :type list))  ; list of language-id strings this provider handles, nil = all

(defvar *lsp-providers* (make-hash-table :test 'equal)
  "Registry of LSP diagnostic providers. Key is provider name string.")

(defmacro deflsp-provider (name &body body)
  "Define an LSP diagnostic provider.
   BODY starts with keyword options:
     :languages (\"lisp\" \"typescript\" ...) — language IDs to handle (nil = all)
     :enabled t/nil
   Then the implementation function form that takes (SOURCE FILE-PATH) and returns diagnostics."
  (let* ((name-str (string-downcase (if (symbolp name) (symbol-name name) name)))
         (fn-name (intern (format nil "LSP-PROVIDER-~A" (string-upcase name-str)) :hactar))
         (languages nil)
         (enabled t)
         (impl-body body))
    (loop while (and impl-body (keywordp (car impl-body)))
          do (case (car impl-body)
               (:languages (setf languages (cadr impl-body)))
               (:enabled (setf enabled (cadr impl-body))))
             (setf impl-body (cddr impl-body)))
    `(progn
       (defun ,fn-name (source file-path)
         "LSP diagnostic provider."
         (declare (ignorable source file-path))
         ,@impl-body)
       (setf (gethash ,name-str *lsp-providers*)
             (make-lsp-provider
              :name ,name-str
              :enabled ,enabled
              :function #',fn-name
              :languages ',languages))
       ',name)))

(defun lsp-collect-diagnostics (source file-path language-id)
  "Run all enabled providers for LANGUAGE-ID and collect diagnostics."
  (let ((all-diags nil))
    (maphash (lambda (name provider)
               (declare (ignore name))
               (when (and (lsp-provider-enabled provider)
                          (or (null (lsp-provider-languages provider))
                              (member language-id (lsp-provider-languages provider) :test #'string=)))
                 (handler-case
                     (let ((diags (funcall (lsp-provider-function provider) source file-path)))
                       (setf all-diags (append all-diags diags)))
                   (error (e)
                     (lsp-log "Provider ~A error: ~A" (lsp-provider-name provider) e)))))
             *lsp-providers*)
    all-diags))

;;* Built-in checker provider
(deflsp-provider checker
  :languages nil  ; works for all — target auto-detected from source
  :enabled t
  (let ((results (check-file-source source :file file-path)))
    (mapcar (lambda (r)
              (let ((hint (check-result-source-hint r)))
                (make-lsp-diagnostic
                 :range-start-line (if hint (source-hint-start-line hint) 0)
                 :range-start-char (if hint (source-hint-start-char hint) 0)
                 :range-end-line (if hint (source-hint-end-line hint) 0)
                 :range-end-char (if hint (source-hint-end-char hint) 0)
                 :severity (ecase (check-result-level r)
                             (:error 1) (:warning 2) (:info 3))
                 :code nil
                 :source "hactar-checker"
                 :message (check-result-message r))))
            results)))

;;* LSP state
(defvar *lsp-initialized* nil)
(defvar *lsp-shutdown-requested* nil)
(defvar *lsp-documents* (make-hash-table :test 'equal)
  "Open documents. URI -> alist with :source, :language-id, :version.")
(defvar *lsp-client-capabilities* nil)

;;* Transport — LSP uses Content-Length framed JSON-RPC

(defun lsp-log (fmt &rest args)
  "Log to stderr for LSP debugging."
  (let ((msg (apply #'format nil fmt args)))
    (format *error-output* "[hactar-lsp] ~A~%" msg)
    (force-output *error-output*)))

(defun lsp-write-message (json-string)
  "Write an LSP message with Content-Length header to stdout."
  (let* ((encoded (babel:string-to-octets json-string :encoding :utf-8))
         (len (length encoded)))
    (format *standard-output* "Content-Length: ~A~C~C~C~C"
            len #\Return #\Linefeed #\Return #\Linefeed)
    (write-string json-string *standard-output*)
    (force-output *standard-output*)))

(defun lsp-read-message ()
  "Read an LSP message from stdin. Returns parsed JSON alist or NIL on EOF."
  (let ((content-length nil))
    ;; Read headers
    (loop for line = (read-line *standard-input* nil nil)
          while line
          do (let ((trimmed (string-trim '(#\Return #\Linefeed #\Space) line)))
               (when (string= trimmed "")
                 (return))
               (when (str:starts-with-p "Content-Length:" trimmed :ignore-case t)
                 (setf content-length
                       (parse-integer (string-trim '(#\Space)
                                                   (subseq trimmed (length "Content-Length:"))))))))
    (unless content-length
      (return-from lsp-read-message nil))
    ;; Read body
    (let ((buf (make-string content-length)))
      (let ((read-count (read-sequence buf *standard-input*)))
        (when (< read-count content-length)
          (return-from lsp-read-message nil)))
      (handler-case
          (let ((shasht:*read-default-object-format* :alist))
            (shasht:read-json buf))
        (error (e)
          (lsp-log "JSON parse error: ~A" e)
          nil)))))

;;* JSON-RPC helpers

(defun lsp-respond (id result)
  "Send a successful JSON-RPC response."
  (let ((shasht:*write-alist-as-object* t)
        (shasht:*write-plist-as-object* nil))
    (lsp-write-message
     (shasht:write-json
      `(("jsonrpc" . "2.0") ("id" . ,id) ("result" . ,result))
      nil))))

(defun lsp-respond-error (id code message)
  "Send a JSON-RPC error response."
  (let ((shasht:*write-alist-as-object* t)
        (shasht:*write-plist-as-object* nil))
    (lsp-write-message
     (shasht:write-json
      `(("jsonrpc" . "2.0")
        ("id" . ,id)
        ("error" . (("code" . ,code) ("message" . ,message))))
      nil))))

(defun lsp-notify (method params)
  "Send a JSON-RPC notification."
  (let ((shasht:*write-alist-as-object* t)
        (shasht:*write-plist-as-object* nil))
    (lsp-write-message
     (shasht:write-json
      `(("jsonrpc" . "2.0") ("method" . ,method) ("params" . ,params))
      nil))))

;;* Diagnostic publishing

(defun lsp-diagnostic-to-alist (d)
  "Convert an lsp-diagnostic to an LSP Diagnostic alist."
  `(("range" . (("start" . (("line" . ,(lsp-diagnostic-range-start-line d))
                             ("character" . ,(lsp-diagnostic-range-start-char d))))
                ("end" . (("line" . ,(lsp-diagnostic-range-end-line d))
                          ("character" . ,(lsp-diagnostic-range-end-char d))))))
    ("severity" . ,(lsp-diagnostic-severity d))
    ("source" . ,(lsp-diagnostic-source d))
    ("message" . ,(lsp-diagnostic-message d))
    ,@(when (lsp-diagnostic-code d)
        `(("code" . ,(lsp-diagnostic-code d))))))

(defun lsp-publish-diagnostics (uri)
  "Run all providers on the document at URI and publish diagnostics."
  (let* ((doc (gethash uri *lsp-documents*))
         (source (cdr (assoc :source doc)))
         (language-id (or (cdr (assoc :language-id doc)) ""))
         (file-path (lsp-uri-to-path uri)))
    (when source
      (let ((diags (lsp-collect-diagnostics source file-path language-id)))
        (lsp-notify "textDocument/publishDiagnostics"
                    `(("uri" . ,uri)
                      ("diagnostics" . ,(coerce
                                         (mapcar #'lsp-diagnostic-to-alist diags)
                                         'vector))))))))

;;* URI helpers

(defun lsp-uri-to-path (uri)
  "Convert a file:// URI to a filesystem path."
  (if (str:starts-with? "file://" uri)
      (let ((path (subseq uri 7)))
        ;; Handle URL encoding if needed
        (cl-ppcre:regex-replace-all "%20" path " "))
      uri))

(defun lsp-path-to-uri (path)
  "Convert a filesystem path to a file:// URI."
  (format nil "file://~A" path))

;;* Language ID detection from URI

(defun lsp-detect-language-id (uri)
  "Detect language ID from file extension in URI."
  (let ((ext (pathname-type (pathname (lsp-uri-to-path uri)))))
    (cond
      ((member ext '("lisp" "cl" "el" "asd") :test #'string-equal) "lisp")
      ((string-equal ext "tsx") "typescriptreact")
      ((string-equal ext "ts") "typescript")
      ((string-equal ext "jsx") "javascriptreact")
      ((string-equal ext "js") "javascript")
      ((string-equal ext "py") "python")
      ((string-equal ext "rs") "rust")
      (t "plaintext"))))

;;* Method handlers

(defun lsp-handle-initialize (id params)
  "Handle initialize request."
  (setf *lsp-client-capabilities*
        (cdr (assoc "capabilities" params :test #'string=)))
  (setf *lsp-initialized* t)
  (lsp-log "Initialize request received")
  (lsp-respond id
               `(("capabilities" .
                  (("textDocumentSync" .
                    (("openClose" . t)
                     ("change" . 1)  ; 1 = full content on change
                     ("save" . (("includeText" . t)))))
                   ("diagnosticProvider" .
                    (("interFileDependencies" . :false)
                     ("workspaceDiagnostics" . :false)))))
                 ("serverInfo" .
                  (("name" . "hactar-lsp")
                   ("version" . ,*hactar-version*))))))

(defun lsp-handle-initialized (params)
  "Handle initialized notification."
  (declare (ignore params))
  (lsp-log "Client initialized"))

(defun lsp-handle-shutdown (id)
  "Handle shutdown request."
  (setf *lsp-shutdown-requested* t)
  (lsp-respond id nil))

(defun lsp-handle-did-open (params)
  "Handle textDocument/didOpen."
  (let* ((td (cdr (assoc "textDocument" params :test #'string=)))
         (uri (cdr (assoc "uri" td :test #'string=)))
         (language-id (or (cdr (assoc "languageId" td :test #'string=))
                          (lsp-detect-language-id uri)))
         (version (cdr (assoc "version" td :test #'string=)))
         (text (cdr (assoc "text" td :test #'string=))))
    (setf (gethash uri *lsp-documents*)
          `((:source . ,text) (:language-id . ,language-id) (:version . ,version)))
    (lsp-log "Opened: ~A (~A)" uri language-id)
    (lsp-publish-diagnostics uri)))

(defun lsp-handle-did-change (params)
  "Handle textDocument/didChange (full sync)."
  (let* ((td (cdr (assoc "textDocument" params :test #'string=)))
         (uri (cdr (assoc "uri" td :test #'string=)))
         (version (cdr (assoc "version" td :test #'string=)))
         (changes (cdr (assoc "contentChanges" params :test #'string=)))
         (text (when (and changes (> (length changes) 0))
                 (cdr (assoc "text" (aref changes 0) :test #'string=)))))
    (when text
      (let ((doc (gethash uri *lsp-documents*)))
        (when doc
          (setf (cdr (assoc :source doc)) text)
          (setf (cdr (assoc :version doc)) version)))
      (lsp-publish-diagnostics uri))))

(defun lsp-handle-did-save (params)
  "Handle textDocument/didSave."
  (let* ((td (cdr (assoc "textDocument" params :test #'string=)))
         (uri (cdr (assoc "uri" td :test #'string=)))
         (text (cdr (assoc "text" params :test #'string=))))
    (when text
      (let ((doc (gethash uri *lsp-documents*)))
        (when doc
          (setf (cdr (assoc :source doc)) text))))
    (lsp-publish-diagnostics uri)))

(defun lsp-handle-did-close (params)
  "Handle textDocument/didClose."
  (let* ((td (cdr (assoc "textDocument" params :test #'string=)))
         (uri (cdr (assoc "uri" td :test #'string=))))
    (remhash uri *lsp-documents*)
    ;; Clear diagnostics
    (lsp-notify "textDocument/publishDiagnostics"
                `(("uri" . ,uri) ("diagnostics" . ,(vector))))
    (lsp-log "Closed: ~A" uri)))

;;* Dispatcher

(defun lsp-dispatch (msg)
  "Dispatch an incoming LSP JSON-RPC message."
  (let* ((id (cdr (assoc "id" msg :test #'string=)))
         (method (cdr (assoc "method" msg :test #'string=)))
         (params (cdr (assoc "params" msg :test #'string=))))
    (cond
      ((string= method "initialize")
       (lsp-handle-initialize id params))
      ((string= method "initialized")
       (lsp-handle-initialized params))
      ((string= method "shutdown")
       (lsp-handle-shutdown id))
      ((string= method "exit")
       (throw 'lsp-exit (if *lsp-shutdown-requested* 0 1)))
      ((string= method "textDocument/didOpen")
       (lsp-handle-did-open params))
      ((string= method "textDocument/didChange")
       (lsp-handle-did-change params))
      ((string= method "textDocument/didSave")
       (lsp-handle-did-save params))
      ((string= method "textDocument/didClose")
       (lsp-handle-did-close params))
      ;; Unknown request (has id) — respond with method not found
      ((and id method)
       (lsp-respond-error id -32601 (format nil "Method not found: ~A" method)))
      ;; Unknown notification — ignore
      (t
       (lsp-log "Ignoring: ~A" method)))))

;;* Main loop

(defun lsp-main-loop ()
  "Main LSP server loop. Reads messages from stdin, dispatches them."
  (lsp-log "Hactar LSP server started (version ~A)" *hactar-version*)
  (catch 'lsp-exit
    (loop
      (let ((msg (lsp-read-message)))
        (cond
          ((null msg)
           (lsp-log "stdin EOF, shutting down LSP")
           (return 0))
          (t
           (handler-case
               (lsp-dispatch msg)
             (error (e)
               (lsp-log "Dispatch error: ~A" e)
               (let ((id (cdr (assoc "id" msg :test #'string=))))
                 (when id
                   (lsp-respond-error id -32603
                                      (format nil "Internal error: ~A" e))))))))))))

;;* Entry point

(defun start-lsp ()
  "Start the LSP server."
  (let ((*lsp-initialized* nil)
        (*lsp-shutdown-requested* nil)
        (*lsp-documents* (make-hash-table :test 'equal))
        (*lsp-client-capabilities* nil))
    (let ((exit-code (lsp-main-loop)))
      (or exit-code 0))))

(define-sub-command lsp (args)
  "Start Hactar as a Language Server Protocol (LSP) server over stdio."
  (declare (ignore args))
  ;; Load models config so checker targets are available
  (load-models-config (get-models-config-path))
  (let ((exit-code (start-lsp)))
    (uiop:quit exit-code)))
