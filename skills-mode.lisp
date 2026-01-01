;; wip skills handling
(in-package :hactar)

(defun parse-skill-frontmatter (content)
  "Parses YAML frontmatter from SKILL.md content."
  (let ((scanner (cl-ppcre:create-scanner "^---\\s*\\n(.*?)\\n---\\s*\\n" :single-line-mode t)))
    (multiple-value-bind (match regs) (cl-ppcre:scan-to-strings scanner content)
      (when (and match (> (length regs) 0))
        (let ((yaml-str (aref regs 0)))
          (handler-case
              (cl-yaml:parse yaml-str)
            (error (e)
              (format t "Error parsing YAML frontmatter: ~A~%" e)
              nil)))))))

(defun get-skill-metadata (skill-dir)
  "Reads SKILL.md in skill-dir and returns metadata hash table or nil."
  (let ((skill-file (merge-pathnames "SKILL.md" skill-dir)))
    (when (probe-file skill-file)
      (let* ((content (uiop:read-file-string skill-file))
             (meta (parse-skill-frontmatter content)))
        (when meta
          (setf (gethash "dir" meta) skill-dir)
          (setf (gethash "file" meta) skill-file)
          meta)))))

(defun list-skills ()
  "Returns a list of metadata hash tables for all available skills."
  (let ((skills '()))
    (ensure-directories-exist *hactar-skills-path*)
    (dolist (dir (uiop:subdirectories *hactar-skills-path*))
      (let ((meta (get-skill-metadata dir)))
        (when meta
          (push meta skills))))
    (sort skills #'string< :key (lambda (m) (gethash "name" m)))))

(defun validate-skill-name (name)
  "Validates skill name against spec."
  (and (<= 1 (length name) 64)
       (cl-ppcre:scan "^[a-z0-9]+(-[a-z0-9]+)*$" name)
       (not (cl-ppcre:scan "--" name))))

(defun validate-skill (skill-dir)
  "Validates a skill directory against the spec."
  (let ((meta (get-skill-metadata skill-dir))
        (errors '()))
    (unless meta
      (push "Missing or invalid SKILL.md with frontmatter." errors)
      (return-from validate-skill errors))

    (let ((name (gethash "name" meta))
          (desc (gethash "description" meta))
          (dir-name (car (last (pathname-directory skill-dir)))))
      
      (unless name
        (push "Missing 'name' field in frontmatter." errors))
      (when name
        (unless (validate-skill-name name)
          (push (format nil "Invalid name format: '~A'. Must be lowercase alphanumeric, no consecutive hyphens, no start/end hyphen." name) errors))
        (unless (string= name dir-name)
          (push (format nil "Name mismatch: frontmatter name '~A' does not match directory name '~A'." name dir-name) errors)))
      
      (unless desc
        (push "Missing 'description' field in frontmatter." errors))
      (when (and desc (> (length desc) 1024))
        (push "Description exceeds 1024 characters." errors)))
    
    errors))

(defun run-skills-list ()
  "Implementation of skills.list command."
  (let ((skills (list-skills)))
    (if skills
        (progn
          (format t "~&Available Skills:~%")
          (dolist (skill skills)
            (format t "  ~A: ~A~%" (gethash "name" skill) (gethash "description" skill))))
        (format t "No skills found in ~A~%" *hactar-skills-path*))))

(define-command skills.list (args)
  "List available skills."
  (declare (ignore args))
  (run-skills-list)
  :slash t :sub t)

(defun run-skills-load (args)
  "Implementation of skills.load command."
  (let* ((pos-args (append (uiop:ensure-list (getf args :subcommand))
                           (uiop:ensure-list (getf args :args))))
         (name (first pos-args))
         (forms-p (getf args :forms))
         (ref-p (getf args :ref)))
    (unless name
      (format t "Usage: /skills.load <name> [--forms] [--ref]~%")
      (return-from run-skills-load))
    
    (let* ((skill-dir (merge-pathnames (format nil "~A/" name) *hactar-skills-path*))
           (skill-file (merge-pathnames "SKILL.md" skill-dir)))
      (if (probe-file skill-file)
          (progn
            (add-file-to-context (uiop:native-namestring skill-file))
            (format t "Loaded skill: ~A~%" name)
            (when forms-p
              (let ((forms-file (merge-pathnames "references/FORMS.md" skill-dir)))
                (if (probe-file forms-file)
                    (add-file-to-context (uiop:native-namestring forms-file))
                    (format t "Forms file not found for skill ~A~%" name))))
            (when ref-p
              (let ((ref-file (merge-pathnames "references/REFERENCE.md" skill-dir)))
                (if (probe-file ref-file)
                    (add-file-to-context (uiop:native-namestring ref-file))
                    (format t "Reference file not found for skill ~A~%" name)))))
          (format t "Skill '~A' not found.~%" name)))))

(define-command skills.load (args)
  "Load a skill into context. Usage: /skills.load <name> [--forms] [--ref]"
  (run-skills-load args)
  :slash t :sub nil
  :cli-options ((:long "forms" :description "Load forms file")
                (:long "ref" :description "Load reference file")))

(defun run-skills-search ()
  "Implementation of skills.search command."
  (let* ((skills (list-skills))
         (items (loop for s in skills
                      collect `((:item . ,(gethash "name" s))
                                (:preview . ,(format nil "Name: ~A~%Description: ~A~%Path: ~A"
                                                     (gethash "name" s)
                                                     (gethash "description" s)
                                                     (gethash "dir" s)))))))
    (if items
        (let* ((selected (fuzzy-select items))
               (name (cdr (assoc :item selected))))
          (when name
            (run-skills-load (list :subcommand (list name)))))
        (format t "No skills found.~%"))))

(define-command skills.search (args)
  "Search skills and load one."
  (declare (ignore args))
  (run-skills-search)
  :slash t :sub t)

(defun run-skills-validate (args)
  "Implementation of skills.validate command."
  (let ((target (first args)))
    (unless target
      (format t "Usage: /skills.validate <name_or_path>~%")
      (return-from run-skills-validate))
    
    (let ((dir (if (probe-file target)
                   (uiop:ensure-directory-pathname target)
                   (merge-pathnames (format nil "~A/" target) *hactar-skills-path*))))
      
      (if (uiop:directory-exists-p dir)
          (let ((errors (validate-skill dir)))
            (if errors
                (format t "Validation FAILED for ~A:~%~{  - ~A~%~}" (uiop:native-namestring dir) errors)
                (format t "Validation PASSED for ~A~%" (uiop:native-namestring dir))))
          (format t "Skill directory not found: ~A~%" dir)))))

(define-command skills.validate (args)
  "Validate a skill directory or name."
  (run-skills-validate args)
  :slash t :sub t)

(defun run-skills-doc (args)
  "Implementation of skills.doc command."
  (let ((format-type (or (first args) "markdown"))
        (skills (list-skills)))
    (cond
      ((string-equal format-type "markdown")
       (format t "| Name | Description |~%|---|---|~%")
       (dolist (s skills)
         (format t "| ~A | ~A |~%" (gethash "name" s) (gethash "description" s))))
      ((string-equal format-type "org")
       (format t "| Name | Description |~%|---|---|~%") ;; Org tables look similar enough for simple output
       (dolist (s skills)
         (format t "| ~A | ~A |~%" (gethash "name" s) (gethash "description" s))))
      (t (format t "Unsupported format: ~A~%" format-type)))))

(define-command skills.doc (args)
  "Generate documentation for available skills."
  (run-skills-doc args)
  :slash t :sub t)

(defun copy-dir (src dst)
  "Recursively copy directory."
  (uiop:run-program (list "cp" "-r" (uiop:native-namestring src) (uiop:native-namestring dst))))

(defun run-skills-add (args)
  "Implementation of skills.add command."
  (let ((source (first args))
        (subpath (second args)))
    (unless source
      (format t "Usage: /skills.add <source> [path_in_repo]~%")
      (return-from run-skills-add))

    (ensure-directories-exist *hactar-skills-path*)

    ;; Expand shorthand
    (when (and (not (str:starts-with? "http" source))
               (not (str:starts-with? "." source))
               (not (str:starts-with? "/" source))
               (not (str:ends-with? ".zip" source))
               (cl-ppcre:scan "^[^/]+/[^/]+$" source))
      (setf source (format nil "https://github.com/~A.git" source)))

    (cond
      ;; Local Directory
      ((and (probe-file source) (uiop:directory-exists-p source))
       (let* ((dir-name (car (last (pathname-directory (uiop:ensure-directory-pathname source)))))
              (dest (merge-pathnames (format nil "~A/" dir-name) *hactar-skills-path*)))
         (copy-dir source dest)
         (format t "Added skill from local path to ~A~%" dest)))

      ;; Zip File
      ((str:ends-with? ".zip" source)
       (if (probe-file source)
           (progn
             (uiop:run-program (list "unzip" "-o" (uiop:native-namestring source) "-d" (uiop:native-namestring *hactar-skills-path*)))
             (format t "Unzipped skill(s) to ~A~%" *hactar-skills-path*))
           (format t "Zip file not found: ~A~%" source)))

      ;; Git Repo
      ((or (str:starts-with? "http" source) (str:starts-with? "git@" source))
       (let ((temp-dir (uiop:ensure-directory-pathname (format nil "/tmp/hactar-skill-clone-~A/" (uuid:make-v4-uuid)))))
         (ensure-directories-exist temp-dir)
         (format t "Cloning ~A...~%" source)
	 (format t "Clone command ~A" (list "git" "clone" "--depth" "1" source (uiop:native-namestring temp-dir)))
         (multiple-value-bind (out err code)
             (uiop:run-program (list "git" "clone" "--depth" "1" source (uiop:native-namestring temp-dir))
                               :ignore-error-status t :output :string :error-output :string)
           (if (zerop code)
               (progn
                 (if subpath
                     ;; Copy specific subdirectory
                     (let ((src-subdir (merge-pathnames (uiop:ensure-directory-pathname subpath) temp-dir)))
                       (if (uiop:directory-exists-p src-subdir)
                           (if (probe-file (merge-pathnames "SKILL.md" src-subdir))
                               ;; It is a single skill
                               (let* ((dir-name (car (last (pathname-directory src-subdir))))
                                      (dest (merge-pathnames (format nil "~A/" dir-name) *hactar-skills-path*)))
                                 (copy-dir src-subdir dest)
                                 (format t "Added skill ~A~%" dir-name))
                               ;; It might be a directory containing skills
                               (let ((found-skills nil))
                                 (dolist (d (uiop:subdirectories src-subdir))
                                   (when (probe-file (merge-pathnames "SKILL.md" d))
                                     (let* ((dir-name (car (last (pathname-directory d))))
                                            (dest (merge-pathnames (format nil "~A/" dir-name) *hactar-skills-path*)))
                                       (copy-dir d dest)
                                       (format t "Added skill ~A~%" dir-name)
                                       (setf found-skills t))))
                                 (unless found-skills
                                   (format t "No skills found in ~A~%" subpath))))
                           (format t "Subdirectory ~A not found in repo.~%" subpath)))
                     ;; Copy all directories that look like skills (have SKILL.md)
                     (dolist (d (uiop:subdirectories temp-dir))
                       (when (probe-file (merge-pathnames "SKILL.md" d))
                         (let* ((dir-name (car (last (pathname-directory d))))
                                (dest (merge-pathnames (format nil "~A/" dir-name) *hactar-skills-path*)))
                           (copy-dir d dest)
                           (format t "Added skill ~A~%" dir-name))))))
               (format t "Git clone failed: ~A~%" err))
           (uiop:delete-directory-tree temp-dir :validate t))))
      
      (t (format t "Unknown source type: ~A~%" source)))))

(define-command skills.add (args)
  "Add a skill from a source.
   Usage: /skills.add <source> [path_in_repo]
   Source can be:
     - anthropics/skills (shorthand for GitHub)
     - https://github.com/...
     - ./local/path
     - file.zip"
  (run-skills-add args)
  :slash t :sub t)

(defun run-skills-help (args)
  "Implementation of skills.help command."
  (declare (ignore args))
  (format t "Skills Commands:~%")
  (format t "  skills.list                        - List available skills~%")
  (format t "  skills.search                      - Search skills and load one~%")
  (format t "  skills.load <name> [--forms] [--ref] - Load a skill into context~%")
  (format t "  skills.add <source>                - Add a skill from a source~%")
  (format t "  skills.validate <path>             - Validate a skill directory or name~%")
  (format t "  skills.doc [format]                - Generate documentation for available skills~%")
  (format t "  skills.help                        - Show this help message~%"))

(define-command skills.help (args)
  "Show help for skills commands."
  (run-skills-help args)
  :slash t :sub t)

;; (define-command butts (args)
;;   (declare (ignore args))
;;   (format t "Hey")
;;   :slash t :sub nil)
