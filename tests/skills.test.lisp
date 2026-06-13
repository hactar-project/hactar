(in-package :hactar-tests)

(def-suite skills-tests
  :description "Tests for skills management")

(in-suite skills-tests)

(test parse-skill-frontmatter
  "Test parsing YAML frontmatter from SKILL.md content"
  (let ((content "---
name: my-skill
description: A cool skill
---
Some other content"))
    (let ((meta (hactar::parse-skill-frontmatter content)))
      (is (string= "my-skill" (gethash "name" meta)))
      (is (string= "A cool skill" (gethash "description" meta)))))
  (let ((bad-content "no frontmatter here"))
    (is (null (hactar::parse-skill-frontmatter bad-content)))))

(test validate-skill-name
  "Test validation of skill names"
  (is-true (hactar::validate-skill-name "my-skill"))
  (is-true (hactar::validate-skill-name "skill"))
  (is-true (hactar::validate-skill-name "skill-123"))
  (is-false (hactar::validate-skill-name "my--skill")) ; consecutive hyphens
  (is-false (hactar::validate-skill-name "-skill")) ; starts with hyphen
  (is-false (hactar::validate-skill-name "skill-")) ; ends with hyphen
  (is-false (hactar::validate-skill-name "My-Skill")) ; uppercase
  (is-false (hactar::validate-skill-name "my_skill")) ; underscore
  (is-false (hactar::validate-skill-name ""))) ; empty

(test validate-skill
  "Test validation of a skill directory"
  (uiop:with-temporary-file (:pathname temp-file)
    (let* ((temp-dir (uiop:pathname-directory-pathname temp-file))
           (skill-dir (merge-pathnames "my-skill/" temp-dir))
           (skill-file (merge-pathnames "SKILL.md" skill-dir)))
      (ensure-directories-exist skill-dir)
      (unwind-protect
          (progn
            ;; Valid skill
            (with-open-file (s skill-file :direction :output :if-does-not-exist :create :if-exists :supersede)
              (format s "---~%name: my-skill~%description: Test description~%---~%"))
            (is (null (hactar::validate-skill skill-dir)))

            ;; Missing name
            (with-open-file (s skill-file :direction :output :if-does-not-exist :create :if-exists :supersede)
              (format s "---~%description: Test description~%---~%"))
            (is-true (member "Missing 'name' field in frontmatter." (hactar::validate-skill skill-dir) :test #'string=))

            ;; Name mismatch
            (with-open-file (s skill-file :direction :output :if-does-not-exist :create :if-exists :supersede)
              (format s "---~%name: other-skill~%description: Test description~%---~%"))
            (let ((errors (hactar::validate-skill skill-dir)))
              (is-true errors)
              (is-true (search "Name mismatch" (first errors)))))
        (uiop:delete-directory-tree skill-dir :validate t)))))

(test list-skills
  "Test listing available skills"
  (uiop:with-temporary-file (:pathname temp-file)
    (let* ((temp-dir (uiop:pathname-directory-pathname temp-file))
           (hactar::*hactar-skills-path* (merge-pathnames "hactar-test-skills/" temp-dir))
           (skill1-dir (merge-pathnames "skill-one/" hactar::*hactar-skills-path*))
           (skill2-dir (merge-pathnames "skill-two/" hactar::*hactar-skills-path*)))
      (ensure-directories-exist skill1-dir)
      (ensure-directories-exist skill2-dir)
      (unwind-protect
          (progn
            (with-open-file (s (merge-pathnames "SKILL.md" skill1-dir) :direction :output :if-does-not-exist :create)
              (format s "---~%name: skill-one~%description: First skill~%---~%"))
            (with-open-file (s (merge-pathnames "SKILL.md" skill2-dir) :direction :output :if-does-not-exist :create)
              (format s "---~%name: skill-two~%description: Second skill~%---~%"))

            (let ((skills (hactar::list-skills)))
              (is (= 2 (length skills)))
              (is (string= "skill-one" (gethash "name" (first skills))))
              (is (string= "skill-two" (gethash "name" (second skills))))))
        (uiop:delete-directory-tree hactar::*hactar-skills-path* :validate t)))))

(test skills-list-formats
  "Test formatting handlers for skills.list"
  (uiop:with-temporary-file (:pathname temp-file)
    (let* ((temp-dir (uiop:pathname-directory-pathname temp-file))
           (hactar::*hactar-skills-path* (merge-pathnames "hactar-test-skills-fmt/" temp-dir))
           (skill1-dir (merge-pathnames "test-skill/" hactar::*hactar-skills-path*)))
      (ensure-directories-exist skill1-dir)
      (unwind-protect
          (progn
            (with-open-file (s (merge-pathnames "SKILL.md" skill1-dir) :direction :output :if-does-not-exist :create)
              (format s "---~%name: test-skill~%description: Format test skill~%---~%"))

            ;; Get the format handlers
            (let ((json-fn (hactar::get-format-handler "/skills.list" :json))
                  (yaml-fn (hactar::get-format-handler "/skills.list" :yaml))
                  (xml-fn (hactar::get-format-handler "/skills.list" :xml))
                  (md-fn (hactar::get-format-handler "/skills.list" :markdown))
                  (org-fn (hactar::get-format-handler "/skills.list" :org-mode))
                  (acp-fn (gethash "/skills.list" hactar::*acp-commands*)))

              ;; Verify each format produces expected structure
              (is-true json-fn)
              (let ((json-out (funcall json-fn nil)))
                (is-true (search "test-skill" json-out))
                (is-true (search "Format test skill" json-out)))

              (is-true yaml-fn)
              (let ((yaml-out (funcall yaml-fn nil)))
                (is-true (search "- name: test-skill" yaml-out))
                (is-true (search "  description: Format test skill" yaml-out)))

              (is-true xml-fn)
              (let ((xml-out (funcall xml-fn nil)))
                (is-true (search "<skills>" xml-out))
                (is-true (search "<name>test-skill</name>" xml-out))
                (is-true (search "<description>Format test skill</description>" xml-out)))

              (is-true md-fn)
              (let ((md-out (funcall md-fn nil)))
                (is-true (search "| Name | Description |" md-out))
                (is-true (search "| test-skill | Format test skill |" md-out)))

              (is-true org-fn)
              (let ((org-out (funcall org-fn nil)))
                (is-true (search "| Name | Description |" org-out))
                (is-true (search "| test-skill | Format test skill |" org-out)))

              (is-true acp-fn)
              (let ((acp-out (funcall acp-fn nil)))
                (is (string= "1 skills available." (cdr (assoc "text" acp-out :test #'string=))))
                (let ((data (cdr (assoc "data" acp-out :test #'string=))))
                  (is (= 1 (length data)))
                  (is (string= "test-skill" (cdr (assoc "name" (aref data 0) :test #'string=))))))))
        (uiop:delete-directory-tree hactar::*hactar-skills-path* :validate t)))))
