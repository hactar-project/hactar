;;* Lisp-RPC API — core API functions exposed to the LLM in Lisp-RPC mode
(in-package :hactar)

;;** Filesystem API

(defapi read-file (path)
  "Read the contents of a file at PATH (relative to project root). Also adds the file to context."
  :permissions :auto
  :category :filesystem
  :returns "File content as a string"
  :examples '((hactar::read-file "src/main.lisp"))
  (let ((full-path (merge-pathnames path *repo-root*)))
    (unless (probe-file full-path)
      (error "File not found: ~A" path))
    (add-file-to-context (uiop:native-namestring full-path))
    (uiop:read-file-string full-path :external-format :utf-8)))

(defapi write-file (path content)
  "Write CONTENT to file at PATH (relative to project root). Creates directories as needed."
  :permissions :confirm
  :category :filesystem
  :side-effects "Creates or overwrites a file"
  :returns "Confirmation string with byte count"
  :examples '((hactar::write-file "src/utils.lisp" "(defun greet () (format t \"hello~%\"))"))
  (let ((full-path (merge-pathnames path *repo-root*)))
    (ensure-directories-exist full-path)
    (with-open-file (s full-path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
      (write-string content s))
    (when *git-autocommit*
      (git-add (list full-path))
      (let ((msg (or (generate-commit-message)
                     (format nil "Update ~A" path))))
        (git-commit msg)))
    (format nil "Wrote ~A bytes to ~A" (length content) path)))

(defapi replace-in-file (path search replace)
  "Replace the first occurrence of SEARCH text with REPLACE text in file at PATH."
  :permissions :confirm
  :category :filesystem
  :side-effects "Modifies a file in place"
  :returns "Confirmation string"
  :examples '((hactar::replace-in-file "src/main.lisp" "(old-code)" "(new-code)"))
  (let ((full-path (merge-pathnames path *repo-root*)))
    (unless (probe-file full-path)
      (error "File not found: ~A" path))
    (let* ((content (uiop:read-file-string full-path :external-format :utf-8))
           (pos (cl:search search content)))
      (unless pos
        (error "Search text not found in ~A" path))
      (let ((modified (concatenate 'string
                                   (subseq content 0 pos)
                                   replace
                                   (subseq content (+ pos (length search))))))
        (with-open-file (s full-path :direction :output
                                     :if-exists :supersede
                                     :external-format :utf-8)
          (write-string modified s))
        (when *git-autocommit*
          (git-add (list full-path))
          (let ((msg (or (generate-commit-message)
                         (format nil "Update ~A" path))))
            (git-commit msg)))
        (format nil "Replaced text in ~A" path)))))

(defapi file-exists-p (path)
  "Check if a file exists at PATH (relative to project root)."
  :permissions :auto
  :category :filesystem
  :returns "T if file exists, NIL otherwise"
  :examples '((hactar::file-exists-p "src/main.lisp"))
  (not (null (probe-file (merge-pathnames path *repo-root*)))))

(defapi list-directory (path)
  "List files in directory at PATH (relative to project root)."
  :permissions :auto
  :category :filesystem
  :returns "List of filenames as strings"
  :examples '((hactar::list-directory "src/"))
  (let* ((full-path (uiop:ensure-directory-pathname (merge-pathnames path *repo-root*)))
         (entries (uiop:directory-files full-path)))
    (mapcar (lambda (e) (uiop:native-namestring (uiop:enough-pathname e *repo-root*)))
            entries)))

;;** Context API

(defapi add-to-context (path)
  "Add a file to the chat context window."
  :permissions :auto
  :category :context
  :returns "Confirmation string"
  :examples '((hactar::add-to-context "src/main.lisp"))
  (let ((full-path (uiop:native-namestring (merge-pathnames path *repo-root*))))
    (add-file-to-context full-path)
    (format nil "Added ~A to context" path)))

(defapi drop-from-context (path)
  "Remove a file from the chat context window."
  :permissions :auto
  :category :context
  :returns "Confirmation string"
  :examples '((hactar::drop-from-context "src/main.lisp"))
  (let ((full-path (uiop:native-namestring (merge-pathnames path *repo-root*))))
    (drop-file-from-context full-path)
    (format nil "Dropped ~A from context" path)))

(defapi context-files ()
  "List all files currently in the chat context."
  :permissions :auto
  :category :context
  :returns "List of file path strings"
  :examples '((hactar::context-files))
  (mapcar (lambda (f) (uiop:native-namestring (uiop:enough-pathname f *repo-root*)))
          *files*))

(defapi search-project (query)
  "Search project files for QUERY using ripgrep. Returns matching filenames."
  :permissions :auto
  :category :context
  :returns "List of file paths containing matches"
  :examples '((hactar::search-project "defun main"))
  (or (search-files-with-rg query *repo-root*) '()))

;;** Shell API

(defapi sh (command)
  "Execute a shell command in the project directory. Returns stdout + stderr."
  :permissions :confirm
  :category :shell
  :side-effects "Executes an arbitrary shell command"
  :returns "Command output as a string"
  :examples '((hactar::sh "make test"))
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list *shell* "-c" command)
                        :output :string
                        :error-output :string
                        :ignore-error-status t
                        :directory *repo-root*)
    (if (zerop exit-code)
        (format nil "~A~@[~A~]" output
                (when (and error-output (> (length error-output) 0))
                  (format nil "~%stderr: ~A" error-output)))
        (format nil "Command failed (exit ~A):~%~A~%~A" exit-code output error-output))))

(defapi sh-read (command)
  "Execute a read-only shell command (ls, cat, grep, etc.). Auto-approved."
  :permissions :auto
  :category :shell
  :returns "Command output as a string"
  :examples '((hactar::sh-read "ls -la src/"))
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list *shell* "-c" command)
                        :output :string
                        :error-output :string
                        :ignore-error-status t
                        :directory *repo-root*)
    (declare (ignore error-output))
    (if (zerop exit-code)
        output
        (format nil "Command failed (exit ~A):~%~A" exit-code output))))

;;** Git API

(defapi git-diff ()
  "Show the current git diff (unstaged changes)."
  :permissions :auto
  :category :git
  :returns "Diff output as string"
  :examples '((hactar::git-diff))
  (run-git-command '("diff") :ignore-error t))

(defapi git-status ()
  "Show git status."
  :permissions :auto
  :category :git
  :returns "Status output as string"
  :examples '((hactar::git-status))
  (run-git-command '("status" "--short") :ignore-error t))

(defapi git-commit-changes (message)
  "Stage all changes and commit with MESSAGE."
  :permissions :confirm
  :category :git
  :side-effects "Creates a git commit"
  :returns "Confirmation string"
  :examples '((hactar::git-commit-changes "Fix type error in parser"))
  (run-git-command '("add" "-A") :ignore-error t)
  (run-git-command (list "commit" "-m" message) :ignore-error t)
  (format nil "Committed: ~A" message))

(define-command ls-rpc (args)
		(rpc-hello *hactar-version*))
