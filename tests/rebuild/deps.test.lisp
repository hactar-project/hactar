(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(defparameter *system-tiers*
  '(("hactar-fzf" . 0) ("hactar-md" . 0) ("hactar-org" . 0) ("hactar-fern" . 0)
    ("hactar-llm" . 1) ("hactar-llm-live" . 1) ("hactar-jsonrpc" . 1) ("hactar-vfs" . 1)
    ("hactar-watch" . 1) ("hactar-sandbox" . 1) ("hactar-tui" . 1)
    ("hactar-core" . 2)
    ("hactar-acp" . 3) ("hactar-mcp" . 3) ("hactar-http" . 3)
    ("hactar-proxy" . 3) ("hactar-knowledge" . 3) ("hactar-rag" . 3)
    ("hactar-codegen" . 3)
    ("hactar-modes-web" . 4) ("hactar-modes-ruby" . 4)
    ("hactar-modes-systems" . 4) ("hactar-modes-shell" . 4)
    ("hactar-modes-lisp" . 4) ("hactar-modes-services" . 4)
    ("hactar-starters" . 4)
    ("hactar" . 5) ("hactarsh" . 5) ("ruhe" . 5)
    ("hyperfractal" . 5) ("hactar-pro" . 5)))

(defparameter *tier-0-allowed-external-deps*
  '("uiop" "str" "cl-ppcre"))

(defparameter *allowed-same-tier-edges*
  '(("hactar-knowledge" . "hactar-rag")
    ("hactar-llm-live" . "hactar-llm")))

(defun %dep-name (dep)
  (string-downcase
   (string (cond
             ((and (consp dep) (eq (first dep) :feature)) (third dep))
             ((consp dep) (second dep))
             (t dep)))))

(defun %find-repo-system (system-name)
  (or (asdf:find-system system-name nil)
      (let* ((root (asdf:system-source-directory "hactar-core"))
             (asd (merge-pathnames (format nil "~A.asd" system-name) root)))
        (when (probe-file asd)
          (asdf:load-asd asd)
          (asdf:find-system system-name nil)))))

(defun %system-deps (system-name)
  (let ((sys (%find-repo-system system-name)))
    (when sys
      (mapcar #'%dep-name (asdf:system-depends-on sys)))))

(defun %core-source-files ()
  (let ((core (asdf:find-system "hactar-core")))
    (when core
      (remove-if-not #'probe-file
                     (mapcar #'asdf:component-pathname
                             (asdf:component-children core))))))

(test deps-strictly-point-down
  (dolist (entry *system-tiers*)
    (let* ((name (car entry))
           (tier (cdr entry))
           (deps (%system-deps name)))
      (when deps
        (dolist (dep deps)
          (let ((dep-tier (cdr (assoc dep *system-tiers* :test #'string=))))
            (when dep-tier
              (is (or (< dep-tier tier)
                      (and (= dep-tier tier)
                           (member (cons name dep) *allowed-same-tier-edges*
                                   :test #'equal)))
                  "~A (tier ~D) must not depend on ~A (tier ~D)"
                  name tier dep dep-tier))))))))

(test deps-graph-is-acyclic
  (let ((visited (make-hash-table :test 'equal))
        (visiting (make-hash-table :test 'equal)))
    (labels ((visit (name)
               (unless (gethash name visited)
                 (is-false (gethash name visiting)
                           "Dependency cycle detected at ~A" name)
                 (setf (gethash name visiting) t)
                 (dolist (dep (%system-deps name))
                   (when (assoc dep *system-tiers* :test #'string=)
                     (visit dep)))
                 (remhash name visiting)
                 (setf (gethash name visited) t))))
      (dolist (entry *system-tiers*)
        (when (%find-repo-system (car entry))
          (visit (car entry)))))))

(test deps-tier-0-uses-only-blessed-external-base
  (dolist (entry *system-tiers*)
    (when (= 0 (cdr entry))
      (dolist (dep (%system-deps (car entry)))
        (is (member dep *tier-0-allowed-external-deps* :test #'string=)
            "Tier 0 system ~A has unblessed dependency ~A"
            (car entry) dep)))))

(test deps-core-excludes-tui-adapters-and-packs
  (let ((forbidden '("hactar-tui" "hactar-acp" "hactar-mcp" "hactar-http"
                     "hactar-proxy" "hactar-knowledge" "hactar-rag"
                     "hactar-codegen" "hactar-starters")))
    (dolist (dep (%system-deps "hactar-core"))
      (is (not (or (member dep forbidden :test #'string=)
                   (search "hactar-modes-" dep)))
          "hactar-core must never depend on ~A" dep)))
  (dolist (path (%core-source-files))
    (let ((text (string-downcase (uiop:read-file-string path))))
      (dolist (prefix '("hactar-tui:" "hactar-acp:" "hactar-mcp:"
                        "hactar-http:" "hactar-proxy:" "hactar-knowledge:"
                        "hactar-rag:" "hactar-modes-"))
        (is (not (search prefix text))
            "Core source ~A must not reference ~A"
            (file-namestring path) prefix)))))

(test deps-codegen-depends-only-on-fern-and-vfs
  (let ((repo-deps (remove-if-not
                    (lambda (d) (assoc d *system-tiers* :test #'string=))
                    (%system-deps "hactar-codegen"))))
    (is (equal '("hactar-fern" "hactar-vfs")
               (sort (copy-list repo-deps) #'string<)))))
