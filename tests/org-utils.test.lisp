(in-package :hactar-tests)

(def-suite org-mode-tests
  :description "Tests for org-mode string/regex manipulation functions.")

(in-suite org-mode-tests)

(defparameter *org-mode-test-content*
  (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID:       h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID:       sh1.1~%:END:~%Content for SH1.1.~%* Headline 2 :tagB:~%:PROPERTIES:~%:ID:       h2~%:END:~%Content for H2.~%*** Sub-sub-headline 2.1.1~%:PROPERTIES:~%:ID:       ssh2.1.1~%:END:~%Content for SSH2.1.1."))

(defparameter *org-mode-filter-test-content*
  (format nil "* Include Me :tagA:~%Content A~%* Exclude Me :nocontext:~%Content B~%** Also Exclude Me (Child) :tagC:~%Content C~%* Include Me Too :tagD:~%Content D~%* Exclude Me Too :nocontext:othertag:~%Content E"))

(test get-headline-level-test
  "Test extracting headline level."
  (is (= (org-mode::get-headline-level "* Headline 1") 1))
  (is (= (org-mode::get-headline-level "*** Headline 3") 3))
  (is (null (org-mode::get-headline-level "Not a headline")))
  (is (null (org-mode::get-headline-level "*No space")))
  (is (null (org-mode::get-headline-level ""))))

(test get-property-test
  "Test extracting property value from drawer lines."
  (let ((lines '("  :PROPERTIES:" "  :ID:       test-id" "  :CUSTOM:   Value " "  :END:")))
    (is (string= (org-mode::get-property "ID" lines) "test-id"))
    (is (string= (org-mode::get-property "CUSTOM" lines) "Value"))
    (is (null (org-mode::get-property "NONEXISTENT" lines))))
  (let ((lines '("No properties here")))
    (is (null (org-mode::get-property "ID" lines)))))

(test adjust-headline-levels-test
  "Test adjusting headline levels in an org string."
  (let ((input (format nil "* Level 1~%** Level 2~%* Level 1 again")))
    (is (string= (org-mode::adjust-headline-levels input 1)
                 (format nil "** Level 1~%*** Level 2~%** Level 1 again")))
    (is (string= (org-mode::adjust-headline-levels input -1)
                 (format nil "* Level 1~%* Level 2~%* Level 1 again")))
    (is (string= (org-mode::adjust-headline-levels input -5)
                 (format nil "* Level 1~%* Level 2~%* Level 1 again")))
    (is (string= (org-mode::adjust-headline-levels input 0) input))
    (is (string= (org-mode::adjust-headline-levels "" 1) ""))
    (is (string= (org-mode::adjust-headline-levels nil 1) ""))))

(test insert-child-test
  "Test inserting content as a child."
  (let* ((new-content (format nil "* New Child~%:PROPERTIES:~%:ID: nc1~%:END:~%Child content."))
         (expected (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1.~%** New Child~%:PROPERTIES:~%:ID: nc1~%:END:~%Child content.~%* Headline 2 :tagB:~%:PROPERTIES:~%:ID: h2~%:END:~%Content for H2.~%*** Sub-sub-headline 2.1.1~%:PROPERTIES:~%:ID: ssh2.1.1~%:END:~%Content for SSH2.1.1."))
         (result (org-mode:insert-child *org-mode-test-content* "h1" new-content)))
    ;; Insert under H1 (level 1), new content becomes level 2, inserted after SH1.1
    (is (string= result expected))

    ;; Insert under SH1.1 (level 2), new content becomes level 3
    (let* ((new-content-2 (format nil "** Another Child~%More content."))
           (expected-2 (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1.~%*** Another Child~%More content.~%* Headline 2 :tagB:~%:PROPERTIES:~%:ID: h2~%:END:~%Content for H2.~%*** Sub-sub-headline 2.1.1~%:PROPERTIES:~%:ID: ssh2.1.1~%:END:~%Content for SSH2.1.1."))
           (result-2 (org-mode:insert-child *org-mode-test-content* "sh1.1" new-content-2)))
      (is (string= result-2 expected-2)))

    ;; Target ID not found
    (is (null (org-mode:insert-child *org-mode-test-content* "nonexistent" new-content)))))

(test insert-sibling-test
  "Test inserting content as a sibling."
  (let* ((new-content (format nil "* New Sibling~%:PROPERTIES:~%:ID: ns1~%:END:~%Sibling content."))
         (expected (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1.~%* New Sibling~%:PROPERTIES:~%:ID: ns1~%:END:~%Sibling content.~%* Headline 2 :tagB:~%:PROPERTIES:~%:ID: h2~%:END:~%Content for H2.~%*** Sub-sub-headline 2.1.1~%:PROPERTIES:~%:ID: ssh2.1.1~%:END:~%Content for SSH2.1.1."))
         (result (org-mode:insert-sibling *org-mode-test-content* "h1" new-content)))
    ;; Insert as sibling of H1 (level 1), new content stays level 1, inserted after SH1.1
    (is (string= result expected))

    ;; Insert as sibling of SH1.1 (level 2), new content becomes level 2
    (let* ((new-content-2 (format nil "* Another Sibling~%More content."))
           (expected-2 (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1.~%** Another Sibling~%More content.~%* Headline 2 :tagB:~%:PROPERTIES:~%:ID: h2~%:END:~%Content for H2.~%*** Sub-sub-headline 2.1.1~%:PROPERTIES:~%:ID: ssh2.1.1~%:END:~%Content for SSH2.1.1."))
           (result-2 (org-mode:insert-sibling *org-mode-test-content* "sh1.1" new-content-2)))
      (is (string= result-2 expected-2)))

    ;; Target ID not found
    (is (null (org-mode:insert-sibling *org-mode-test-content* "nonexistent" new-content)))))

(test delete-headline-test
  "Test deleting a headline and its content/subheadings."
  ;; Delete H1 (should remove H1 and SH1.1)
  (let* ((expected (format nil "* Headline 2 :tagB:~%:PROPERTIES:~%:ID: h2~%:END:~%Content for H2.~%*** Sub-sub-headline 2.1.1~%:PROPERTIES:~%:ID: ssh2.1.1~%:END:~%Content for SSH2.1.1."))
         (result (org-mode:delete-headline *org-mode-test-content* "h1")))
    (is (string= result expected)))

  ;; Delete SH1.1 (should only remove SH1.1)
  (let* ((expected (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%* Headline 2 :tagB:~%:PROPERTIES:~%:ID: h2~%:END:~%Content for H2.~%*** Sub-sub-headline 2.1.1~%:PROPERTIES:~%:ID: ssh2.1.1~%:END:~%Content for SSH2.1.1."))
         (result (org-mode:delete-headline *org-mode-test-content* "sh1.1")))
    (is (string= result expected)))

  ;; Delete H2 (should remove H2 and SSH2.1.1)
  (let* ((expected (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1."))
         (result (org-mode:delete-headline *org-mode-test-content* "h2")))
    (is (string= result expected)))

  ;; Delete SSH2.1.1 (should only remove SSH2.1.1)
  (let* ((expected (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1.~%* Headline 2 :tagB:~%:PROPERTIES:~%:ID: h2~%:END:~%Content for H2."))
         (result (org-mode:delete-headline *org-mode-test-content* "ssh2.1.1")))
    (is (string= result expected)))

  ;; Target ID not found
  (is (null (org-mode:delete-headline *org-mode-test-content* "nonexistent"))))

(test get-tags-from-headline-test
  "Test extracting tags from a headline string."
  (is (equal (org-mode::get-tags-from-headline "* Headline :tag1:tag2:") '("tag1" "tag2")))
  (is (equal (org-mode::get-tags-from-headline "** Headline :single:") '("single")))
  (is (null (org-mode::get-tags-from-headline "* Headline without tags")))
  (is (null (org-mode::get-tags-from-headline "* Headline :tag: not at end")))
  (is (string= " " (car (org-mode::get-tags-from-headline "* Headline : :"))))
  (is (null (org-mode::get-tags-from-headline ""))))

(test select-headlines-by-tag-test
  "Test selecting headlines by tags."
  ;; Select by :tagB: (H1 and H2)
  (let* ((expected (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1.~%* Headline 2 :tagB:~%:PROPERTIES:~%:ID: h2~%:END:~%Content for H2.~%*** Sub-sub-headline 2.1.1~%:PROPERTIES:~%:ID: ssh2.1.1~%:END:~%Content for SSH2.1.1."))
         (result (org-mode:select-headlines-by-tag *org-mode-test-content* '("tagB"))))
    (is (string= result expected)))

  ;; Select by :tagA: (only H1)
  (let* ((expected (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1."))
         (result (org-mode:select-headlines-by-tag *org-mode-test-content* '("tagA"))))
    (is (string= result expected)))

  ;; Select by :tagA: and :tagB: (only H1)
  (let* ((expected (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1."))
         (result (org-mode:select-headlines-by-tag *org-mode-test-content* '("tagA" "tagB"))))
    (is (string= result expected)))

  ;; Select by :tagC: (only SH1.1 - but selection includes parent H1) - This needs clarification.
  (let* ((expected (format nil "** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1."))
         (result (org-mode:select-headlines-by-tag *org-mode-test-content* '("tagC"))))
    (is (string= result expected)))

  ;; Select by non-existent tag
  (let* ((result (org-mode:select-headlines-by-tag *org-mode-test-content* '("nonexistent"))))
    (is (string= result "")))

  ;; Select with empty tag list
  (let* ((result (org-mode:select-headlines-by-tag *org-mode-test-content* '())))
    (is (string= result *org-mode-test-content*)))
  (let* ((expected (format nil "* Headline 1 :tagA:tagB:~%:PROPERTIES:~%:ID: h1~%:END:~%Content for H1.~%** Sub-headline 1.1 :tagC:~%:PROPERTIES:~%:ID: sh1.1~%:END:~%Content for SH1.1."))
         (result (org-mode:select-headlines-by-tag *org-mode-test-content* '("taga"))))
    (is (string= result expected))))

(test filter-headlines-test
  "Test filtering headlines by tags."
  (let ((content (format nil "* Headline 1 :tagA:~%Content 1~%** Sub 1 :tagB:~%Content 1.1~%* Headline 2 :tagC:~%Content 2~%* Headline 3 :tagA:tagD:~%Content 3")))
    ;; Filter :tagA: (removes H1 and H3)
    (let ((expected (format nil "* Headline 2 :tagC:~%Content 2"))
          (result (org-mode:filter-headlines content '("tagA"))))
      (is (string= result expected)))

    ;; Filter :tagB: (removes Sub 1 only)
    (let ((expected (format nil "* Headline 1 :tagA:~%Content 1~%* Headline 2 :tagC:~%Content 2~%* Headline 3 :tagA:tagD:~%Content 3"))
          (result (org-mode:filter-headlines content '("tagB"))))
      (is (string= result expected)))

    ;; Filter :tagC: and :tagD: (removes H2 and H3)
    (let ((expected (format nil "* Headline 1 :tagA:~%Content 1~%** Sub 1 :tagB:~%Content 1.1"))
          (result (org-mode:filter-headlines content '("tagC" "tagD"))))
      (is (string= result expected)))

    ;; Filter non-existent tag (no change)
    (let ((result (org-mode:filter-headlines content '("nonexistent"))))
      (is (string= result content)))

    ;; Filter with empty tag list (no change)
    (let ((result (org-mode:filter-headlines content '())))
      (is (string= result content)))

    (let ((expected (format nil "* Headline 2 :tagC:~%Content 2"))
          (result (org-mode:filter-headlines content '("taga"))))
      (is (string= result expected)))

    (is (string= (org-mode:filter-headlines "" '("tagA")) ""))
    (let ((content-preamble (format nil "Preamble~%* Headline 1 :tagA:~%Content 1")))
      (is (string= (org-mode:filter-headlines content-preamble '("tagA")) "Preamble")))))

(test upsert-files-section-creates-new
  "When no 'Files' heading exists, create it at level 1 and insert blocks content."
  (let* ((input (format nil "* One~%Body~%"))
         (blocks (format nil "#+begin_src lisp src/app.lisp~%(+ 1 2)~%#+end_src~%"))
         (result (org-mode:upsert-files-section input blocks))
         (expected (format nil "* One~%Body~%* Files~%#+begin_src lisp src/app.lisp~%(+ 1 2)~%#+end_src")))
    (is (string= result expected))))

(test upsert-files-section-updates-existing
  "Replace existing 'Files' section contents without affecting other headings."
  (let* ((input (format nil "* Files~%OLD~%* Another~%X~%"))
         (blocks (format nil "NEW~%"))
         (result (org-mode:upsert-files-section input blocks))
         (expected (format nil "* Files~%NEW~%* Another~%X")))
    (is (string= result expected))))

(test upsert-files-section-custom-title-and-level
  "Support custom heading title and level when inserting."
  (let* ((input (format nil "* Root~%R~%"))
         (blocks (format nil "DATA~%"))
         (result (org-mode:upsert-files-section input blocks :heading-title "Artifacts" :level 2))
         (expected (format nil "* Root~%R~%** Artifacts~%DATA")))
    (is (string= result expected))))

(test upsert-docs-section-creates-new
  "Create a Documentation section with child headings and src blocks when none exists."
  (let* ((input (format nil "* Other~%X~%"))
         (docs '(((:TITLE . "First Doc") (:SOURCE_ID . "doc-1") (:CONTENT . "First content line"))
                 ((:TITLE . "Second Doc") (:SOURCE_ID . "doc-2") (:CONTENT . "Second content"))))
         (result (org-mode:upsert-docs-section input docs))
         (expected (format nil "* Other~%X~%* Documentation~%** First Doc~%:PROPERTIES:~%:SOURCE_ID: doc-1~%:END:~%#+begin_src text~%First content line~%#+end_src~%** Second Doc~%:PROPERTIES:~%:SOURCE_ID: doc-2~%:END:~%#+begin_src text~%Second content~%#+end_src")))
    (is (string= result expected))))

(test upsert-docs-section-updates-existing
  "Replace existing Documentation section children with new ones."
  (let* ((input (format nil "* Documentation~%** Old Doc~%:PROPERTIES:~%:SOURCE_ID: old~%:END:~%#+begin_src text~%old~%#+end_src~%* Tail~%Z"))
         (docs `(((:TITLE . "New Doc A") (:SOURCE_ID . "new-a") (:CONTENT . "Alpha"))
                ((:TITLE . "New Doc B") (:SOURCE_ID . "new-b") (:CONTENT . ,(format nil "Beta line 1~%Beta line 2")))))
         (result (org-mode:upsert-docs-section input docs))
         (expected (format nil "* Documentation~%** New Doc A~%:PROPERTIES:~%:SOURCE_ID: new-a~%:END:~%#+begin_src text~%Alpha~%#+end_src~%** New Doc B~%:PROPERTIES:~%:SOURCE_ID: new-b~%:END:~%#+begin_src text~%Beta line 1~%Beta line 2~%#+end_src~%* Tail~%Z"))
         )
    (is (string= result expected))))

(test upsert-docs-section-custom-title-and-level
  "Support custom heading title and level for docs section insertion."
  (let* ((input (format nil "* Root~%R~%"))
         (docs '(((:TITLE . "Inner One") (:SOURCE_ID . "i1") (:CONTENT . "Line A"))))
         (result (org-mode:upsert-docs-section input docs :heading-title "Docs" :level 2))
         (expected (format nil "* Root~%R~%** Docs~%*** Inner One~%:PROPERTIES:~%:SOURCE_ID: i1~%:END:~%#+begin_src text~%Line A~%#+end_src")))
    (is (string= result expected))))
