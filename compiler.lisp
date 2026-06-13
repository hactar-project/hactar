;; A universal compiler/transpiler. Think of it like parenscript but for any target language.
(in-package :hactar)
(defvar *compiler-targets* (make-hash-table :test 'eq)
  "Registry mapping target keywords to target-definition structs.")

(defstruct target-definition
  "Defines a compilation target."
  (name nil :type (or null keyword))
  (parent nil :type (or null keyword))
  (emitters (make-hash-table :test 'eq) :type hash-table)
  (macros (make-hash-table :test 'eq) :type hash-table)
  (checkers nil :type list)
  (setup-fn nil :type (or null function))
  (finalize-fn nil :type (or null function))
  (file-extension "js" :type string))

;;; Forward declarations for checker (defined in checker.lisp)
(declaim (ftype (function (keyword list) list) run-checks))

(defvar *current-target* nil "The active target-definition during compilation.")
(defvar *current-vfs* nil "The active VFS during compilation.")
(defvar *indent-level* 0 "Current indentation depth.")

(defun js-file-p ()
  "Return T if the current VFS file is a JavaScript/TypeScript file needing semicolons."
  (let ((file (vfs-current-file *current-vfs*)))
    (or (null file)
        (let ((ext (pathname-type (pathname file))))
          (or (null ext)
              (member ext '("js" "ts" "tsx" "jsx" "mts") :test #'string-equal))))))
(defvar *compilation-errors* nil "List of errors/warnings accumulated.")

(defmacro deftarget (name &key parent file-extension setup finalize)
  "Define a new compilation target."
  `(progn
     (setf (gethash ,name *compiler-targets*)
           (make-target-definition
            :name ,name
            :parent ,parent
            :file-extension ,(or file-extension "js")
            :setup-fn ,(when setup `(lambda () ,@(if (listp (car setup)) setup (list setup))))
            :finalize-fn ,(when finalize `(lambda () ,@(if (listp (car finalize)) finalize (list finalize))))))
     ,name))

(defmacro defemit (target form-name lambda-list &body body)
  "Define how TARGET compiles FORM-NAME."
  (let ((fixed-ll (substitute '&rest '&body lambda-list)))
    `(let ((tgt (gethash ,target *compiler-targets*)))
       (unless tgt (error "Target ~A not defined." ,target))
       (setf (gethash ',form-name (target-definition-emitters tgt))
             (lambda ,fixed-ll ,@body)))))

(defun find-emitter (target-def symbol)
  "Find the emitter for SYMBOL, walking the parent chain."
  (or (gethash symbol (target-definition-emitters target-def))
      (when (target-definition-parent target-def)
        (let ((parent (gethash (target-definition-parent target-def) *compiler-targets*)))
          (when parent (find-emitter parent symbol))))))

(defun find-macro (target-def symbol)
  "Find the macro expander for SYMBOL, walking the parent chain."
  (or (gethash symbol (target-definition-macros target-def))
      (when (target-definition-parent target-def)
        (let ((parent (gethash (target-definition-parent target-def) *compiler-targets*)))
          (when parent (find-macro parent symbol))))))

(defmacro defjsmacro (target form-name lambda-list &body body)
  "Define a JS macro for TARGET. Body returns a Lisp form to be compiled."
  (let ((fixed-ll (substitute '&rest '&body lambda-list)))
    `(let ((tgt (gethash ,target *compiler-targets*)))
       (unless tgt (error "Target ~A not defined." ,target))
       (setf (gethash ',form-name (target-definition-macros tgt))
             (lambda ,fixed-ll ,@body)))))

(defun detect-target-from-forms (forms)
  "Detect the compilation target from a (target :keyword) form in FORMS.
   Returns the target keyword, or :javascript as default."
  (dolist (form forms)
    (when (and (listp form)
               (symbolp (car form))
               (string= (symbol-name (car form)) "TARGET")
               (keywordp (second form)))
      (return-from detect-target-from-forms (second form))))
  :javascript)

;;* Reader utilities

(defun read-all-forms (source-string)
  "Read all Lisp forms from SOURCE-STRING. Returns a list of forms.
   Uses :invert readtable-case so that mixed-case symbols (e.g. Request) preserve their case."
  (let* ((*package* (find-package :hactar))
         (*readtable* (copy-readtable *readtable*)))
    (setf (readtable-case *readtable*) :invert)
    (with-input-from-string (stream source-string)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            collect form))))

(defun invert-case-symbol-name (sym)
  "Return the symbol name with case restored from :invert readtable convention.
   All-uppercase (originally all-lowercase) -> downcase.
   Mixed-case (originally mixed) -> preserve as-is."
  (let ((name (symbol-name sym)))
    (if (every (lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) name)
        (string-downcase name)
        name)))

;;* Emission helpers
(defun compiler-emit (string)
  "Write STRING to the current VFS file."
  (vfs-write *current-vfs* string))

(defun compiler-emitln (string)
  "Write STRING + newline to the current VFS file."
  (vfs-writeln *current-vfs* string))

(defun compiler-prepend (string)
  "Prepend STRING to the beginning of the current VFS file."
  (vfs-prepend *current-vfs* string))

(defun emit-indent ()
  "Emit spaces for current indentation level."
  (compiler-emit (make-string (* *indent-level* 2) :initial-element #\Space)))

(defmacro with-indent (&body body)
  "Execute BODY with increased indentation."
  `(let ((*indent-level* (1+ *indent-level*)))
     ,@body))

(defun kebab-to-camel (name-string)
  "Convert a kebab-case string to camelCase."
  (let* ((parts (cl-ppcre:split "-" name-string))
         (first-part (string-downcase (first parts)))
         (rest-parts (mapcar #'string-capitalize (rest parts))))
    (apply #'concatenate 'string first-part rest-parts)))

(defun strip-earmuffs (name-string)
  "Strip leading/trailing * from a name string."
  (let ((s name-string))
    (when (and (> (length s) 0) (char= (char s 0) #\*))
      (setf s (subseq s 1)))
    (when (and (> (length s) 0) (char= (char s (1- (length s))) #\*))
      (setf s (subseq s 0 (1- (length s)))))
    s))

;;* Core
(defun compile-form (form)
  "Compile a single form. Dispatches to macros first, then emitters."
  (cond
    ((null form) (compiler-emit "null"))
    ((atom form) (emit-atom form))
    ((listp form)
     (let* ((op (car form))
            (args (cdr form))
            (macro-fn (find-macro *current-target* op)))
       (if macro-fn
           (compile-form (apply macro-fn args))
           (let ((emitter (find-emitter *current-target* op)))
             (if emitter
                 (apply emitter args)
                 (emit-funcall op args))))))))

(defun compile-toplevel (forms &key target (vfs (make-vfs)) (check t))
  "Compile a list of Lisp forms using TARGET, writing output into VFS.
   If TARGET is nil, it is auto-detected from (target :xxx) forms in FORMS."
  (let* ((target (or target (detect-target-from-forms forms)))
         (target-def (gethash target *compiler-targets*))
         (*current-target* target-def)
         (*current-vfs* vfs)
         (*indent-level* 0)
         (*compilation-errors* nil))
    (unless target-def
      (error "Unknown compilation target: ~A" target))

    (when check
      (let ((results (run-checks target forms)))
        (let ((errors (remove-if-not (lambda (r) (eq :error (check-result-level r))) results)))
          (when errors
            (dolist (e errors)
              (format *error-output* "Compile error: ~A~%" (check-result-message e)))))))

    (when (target-definition-setup-fn target-def)
      (funcall (target-definition-setup-fn target-def)))

    (dolist (form forms)
      (cond
        ;; (in-file "path") — switch VFS file
        ((and (listp form) (symbolp (car form)) (string= (symbol-name (car form)) "IN-FILE"))
         (vfs-select-file vfs (second form)))
        ;; (target :keyword) — metadata, skip
        ((and (listp form) (eq (car form) 'target))
         nil)
        ;; Normal form
        (t
         (unless (vfs-current-file vfs)
           (vfs-select-file vfs (format nil "output.~A" (target-definition-file-extension target-def))))
         (compile-form form)
         (unless (and (listp form)
                      (symbolp (car form))
                      (string= (symbol-name (car form)) "IN-FILE"))
           (compiler-emitln "")))))
    ;; Run finalize
    (when (target-definition-finalize-fn target-def)
      (funcall (target-definition-finalize-fn target-def)))
    vfs))

(defun check-toplevel (forms &key target)
  "Check a list of Lisp forms without compiling. Returns a list of check-results."
  (let ((target (or target (detect-target-from-forms forms))))
    (run-checks target forms)))

(defun compile-string (source &key target (check t))
  "Convenience: compile a SOURCE string. Returns VFS.
   Target is auto-detected from source if not specified."
  (let ((forms (read-all-forms source)))
    (compile-toplevel forms :target target :check check)))

(defun check-string (source &key target)
  "Convenience: check a SOURCE string without compiling.
   Target is auto-detected from source if not specified."
  (let ((forms (read-all-forms source)))
    (check-toplevel forms :target target)))

(defun compile-file-to-string (source &key target)
  "Compile SOURCE string, return the content of the first output file as a string.
   Target is auto-detected from source if not specified."
  (let* ((vfs (compile-string source :target target :check nil))
         (paths (vfs-all-paths vfs)))
    (when paths (vfs-get-content vfs (first paths)))))

(defun compile-output-tags (vfs &optional (stream *standard-output*))
  "Output compiled VFS contents in <file> tag format to STREAM."
  (vfs-flush-to-tags vfs stream))

(defun compile-output-org (vfs &key output-dir dry)
  "Output compiled VFS contents in org-mode format.
   If DRY, writes to *standard-output*. Otherwise writes to output.org in OUTPUT-DIR."
  (let ((tangle-dir (or output-dir ".")))
    (if dry
        (vfs-flush-to-org vfs *standard-output* tangle-dir)
        (let ((org-path (merge-pathnames "output.org" (or (and output-dir (uiop:ensure-directory-pathname output-dir))
                                                          *repo-root*))))
          (ensure-directories-exist org-path)
          (with-open-file (s org-path :direction :output :if-exists :supersede
                                       :if-does-not-exist :create)
            (vfs-flush-to-org vfs s tangle-dir))
          (format t "Wrote org file: ~A~%" (uiop:native-namestring org-path))))))

(defun compile-output-lisp (vfs &key output-dir dry)
  "Output compiled VFS contents in lisp format.
   If DRY, writes to *standard-output*. Otherwise writes to output.lisp in OUTPUT-DIR."
  (if dry
      (vfs-flush-to-lisp vfs *standard-output*)
      (let ((lisp-path (merge-pathnames "output.lisp" (or (and output-dir (uiop:ensure-directory-pathname output-dir))
                                                           *repo-root*))))
        (ensure-directories-exist lisp-path)
        (with-open-file (s lisp-path :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
          (vfs-flush-to-lisp vfs s))
        (format t "Wrote lisp file: ~A~%" (uiop:native-namestring lisp-path)))))

(defun compile-output-disk (vfs output-dir)
  "Flush compiled VFS contents to disk at OUTPUT-DIR."
  (let ((out-path (uiop:ensure-directory-pathname output-dir)))
    (let ((written (vfs-flush-to-disk vfs out-path)))
      (format t "Wrote ~A files to ~A~%" (length written) (uiop:native-namestring out-path)))))

(defun compile-read-source-forms (files)
  "Read and concatenate all Lisp forms from FILES (list of paths).
   Returns the combined forms list, or NIL and prints errors."
  (let ((all-forms nil))
    (dolist (file-path files)
      (let ((full-path (if (uiop:absolute-pathname-p file-path)
                           file-path
                           (merge-pathnames file-path (or *repo-root* (uiop:getcwd))))))
        (unless (probe-file full-path)
          (format t "File not found: ~A~%" full-path)
          (return-from compile-read-source-forms nil))
        (let ((source (uiop:read-file-string full-path)))
          (setf all-forms (append all-forms (read-all-forms source))))))
    all-forms))

(define-command compile (args)
  "Compile Lisp source files to a target language.
Usage: /compile file1.lisp [file2.lisp ...] [-o output-dir] [-f format] [--dry]
Target is auto-detected from (target :xxx) declarations in source files.
If --format is set, outputs in that format (org, lisp). Otherwise flushes to --output dir.
If neither --format nor --output is set, defaults to dry run with <file> tag output."
  (block compile
    (let* ((output-dir (getf args :output))
           (format-opt (getf args :format))
           (dry (or (getf args :dry)
                    (and (null output-dir) (null format-opt))))
           (pos-args (append (uiop:ensure-list (getf args :subcommand))
                             (uiop:ensure-list (getf args :args))))
           (files pos-args))
      (unless files
        (format t "No input files specified.~%")
        (return-from compile nil))
      (let ((all-forms (compile-read-source-forms files)))
        (unless all-forms (return-from compile nil))
        (let ((vfs (compile-toplevel all-forms :check t)))
          (cond
            ((string-equal format-opt "org")
             (compile-output-org vfs :output-dir output-dir :dry dry))
            ((string-equal format-opt "lisp")
             (compile-output-lisp vfs :output-dir output-dir :dry dry))
            (output-dir
             (compile-output-disk vfs output-dir))
            (t
             (compile-output-tags vfs)))))))
  :slash t
  :sub t
  :acp (lambda (cmd-args)
         (let* ((parsed (parse-cli-args-s
                         (format nil "compile ~{~A~^ ~}" cmd-args)
                         '(("o" . "output") ("f" . "format"))))
                (format-opt (getf parsed :format))
                (output-dir (getf parsed :output))
                (pos-args (append (uiop:ensure-list (getf parsed :subcommand))
                                  (uiop:ensure-list (getf parsed :args))))
                (files pos-args))
           (cond
             ((null files)
              `(("text" . "No input files specified.")))
             (t
              (let ((all-forms (compile-read-source-forms files)))
                (if (null all-forms)
                    `(("text" . "Failed to read source files."))
                    (let* ((vfs (compile-toplevel all-forms :check t))
                           (paths (vfs-all-paths vfs))
                           (file-data (loop for p in paths
                                            collect `(("path" . ,p)
                                                      ("content" . ,(vfs-get-content vfs p)))))
                           (text-output
                             (with-output-to-string (s)
                               (cond
                                 ((string-equal format-opt "org")
                                  (compile-output-org vfs :output-dir output-dir :dry t))
                                 ((string-equal format-opt "lisp")
                                  (compile-output-lisp vfs :output-dir output-dir :dry t))
                                 (t
                                  (compile-output-tags vfs s))))))
                      `(("text" . ,text-output)
                        ("data" . (("files" . ,(coerce file-data 'vector))
                                   ,@(when format-opt `(("format" . ,format-opt)))))))))))))
  :cli-options ((:short "o" :long "output" :description "Output directory for compiled files")
                (:short "f" :long "format" :description "Output format: org, lisp")
                (:long "dry" :description "Dry run, output to stdout")))
