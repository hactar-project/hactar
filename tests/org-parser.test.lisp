(in-package :hactar-tests)

(def-suite org-mode-parser-tests
  :description "Tests for org-mode parsing and manipulation.")

(in-suite org-mode-parser-tests)

(defparameter *test-org-content*
  (format nil "#+TITLE: Test Document~%#+AUTHOR: Tester~%~%* Headline 1 :tag1:~%:PROPERTIES:~%:ID:       headline-1-id~%:END:~%Content under H1.~%** Sub-headline 1.1~%Sub content.~%* Headline 2~%Content H2.~%* Headline 3~%Content H3.~%"))

(test parse-full-org-test
  "Test parsing a complete org-mode string with the new parser."
  (let* ((parsed (org-mode-parser:parse-org-string *test-org-content*))
         (settings (org-mode-parser:document-settings parsed))
         (entries (org-mode-parser:node-children parsed)))
    ;; Check Settings
    (is (string= (cdr (assoc :TITLE settings)) "Test Document"))
    (is (string= (cdr (assoc :AUTHOR settings)) "Tester"))

    ;; Check Number of Entries (should be 3 top-level headings in the tree structure)
    (is (= (length entries) 3))

    ;; Verify all entries are headings, not paragraphs
    (is (every (lambda (entry) (typep entry 'org-mode-parser:org-heading)) entries))

    ;; Check Headline 1
    (let ((h1 (first entries)))
      (is (typep h1 'org-mode-parser:org-heading))
      (is (= (org-mode-parser:heading-level h1) 1))
      (is (string= (org-mode-parser:heading-title h1) "Headline 1"))
      (is (equal (org-mode-parser:heading-tags h1) '("tag1")))

      ;; Check that Headline 1 has Sub-headline 1.1 as a child
      (let* ((h1-children (org-mode-parser:node-children h1))
             (props-drawer (find-if (lambda (child)
                                     (typep child 'org-mode-parser:org-properties-drawer))
                                   h1-children))
             (sub-headings (remove-if-not (lambda (child)
                                           (typep child 'org-mode-parser:org-heading))
                                         h1-children)))
        (is-true props-drawer)
        (is (string= (cdr (assoc :ID (org-mode-parser:node-properties props-drawer)))
                    "headline-1-id"))
        ;; Headline 1 should have one sub-heading child
        (is (= (length sub-headings) 1))
        (is (= (org-mode-parser:heading-level (first sub-headings)) 2))
        (is (string= (org-mode-parser:heading-title (first sub-headings)) "Sub-headline 1.1"))))))
