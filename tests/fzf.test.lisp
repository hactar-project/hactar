(in-package :hactar-tests)

(def-suite fzf-tests
  :description "Tests for fzf.lisp functions.")

(in-suite fzf-tests)

(test fuzzy-select-simple-test
  "Test fuzzy-select-simple with valid, invalid, and boundary inputs."
  (let ((items '(((:item . "Apple") (:preview . "A red fruit"))
                 ((:item . "Banana") (:preview . "A yellow fruit")))))
    ;; Test selecting valid first item
    (with-input-from-string (*standard-input* "1")
      (let ((res (hactar::fuzzy-select-simple items)))
        (is (equal (cdr (assoc :item res)) "Apple"))))

    ;; Test selecting valid second item
    (with-input-from-string (*standard-input* "2")
      (let ((res (hactar::fuzzy-select-simple items)))
        (is (equal (cdr (assoc :item res)) "Banana"))))

    ;; Test selecting out-of-bounds index (too large)
    (with-input-from-string (*standard-input* "3")
      (let ((res (hactar::fuzzy-select-simple items)))
        (is (null res))))

    ;; Test selecting 0 (cancel)
    (with-input-from-string (*standard-input* "0")
      (let ((res (hactar::fuzzy-select-simple items)))
        (is (null res))))

    ;; Test selecting negative index
    (with-input-from-string (*standard-input* "-1")
      (let ((res (hactar::fuzzy-select-simple items)))
        (is (null res))))

    ;; Test selecting non-numeric input
    (with-input-from-string (*standard-input* "invalid")
      (let ((res (hactar::fuzzy-select-simple items)))
        (is (null res))))))

(test fuzzy-select-raw-in-editor-test
  "Test fuzzy-select-raw in editor mode, bypassing curses."
  (let ((hactar::*in-editor* t)
        (items '(((:item . "Apple") (:preview . "A red fruit"))
                 ((:item . "Banana") (:preview . "A yellow fruit")))))
    ;; Single select (multi nil)
    (with-input-from-string (*standard-input* "1")
      (let ((res (hactar::fuzzy-select-raw items :multi nil)))
        (is (equal (cdr (assoc :item res)) "Apple"))))

    ;; Multi select (multi t)
    (with-input-from-string (*standard-input* "2")
      (let ((res (hactar::fuzzy-select-raw items :multi t)))
        (is (listp res))
        (is (= (length res) 1))
        (is (equal (cdr (assoc :item (first res))) "Banana"))))

    ;; Empty items list
    (is (null (hactar::fuzzy-select-raw nil)))))

(test fuzzy-select-test
  "Test unified fuzzy-select function with string and plist/alist inputs."
  (let ((hactar::*in-editor* t))
    ;; String list, single select
    (with-input-from-string (*standard-input* "2")
      (let ((res (hactar::fuzzy-select '("Apple" "Banana" "Cherry") :multi nil)))
        (is (string= res "Banana"))))

    ;; String list, multi select
    (with-input-from-string (*standard-input* "1")
      (let ((res (hactar::fuzzy-select '("Apple" "Banana" "Cherry") :multi t)))
        (is (equal res '("Apple")))))

    ;; Alist list, single select
    (with-input-from-string (*standard-input* "1")
      (let ((res (hactar::fuzzy-select '(((:item . "Apple") (:preview . "A red fruit"))
                                         ((:item . "Banana") (:preview . "A yellow fruit")))
                                       :multi nil)))
        (is (equal (cdr (assoc :item res)) "Apple"))))

    ;; Alist list, multi select
    (with-input-from-string (*standard-input* "2")
      (let ((res (hactar::fuzzy-select '(((:item . "Apple") (:preview . "A red fruit"))
                                         ((:item . "Banana") (:preview . "A yellow fruit")))
                                       :multi t)))
        (is (listp res))
        (is (= (length res) 1))
        (is (equal (cdr (assoc :item (first res))) "Banana"))))

    ;; Empty items list
    (is (null (hactar::fuzzy-select nil)))))

(test format-doc-for-fzf-preview-test
  "Test formatting documents into string representation for fzf preview."
  (let* ((doc '((:title . "My Doc")
                (:content . "This is the content.")
                (:tags . ("tag1" "tag2"))
                (:covers . "topic1")
                (:uri . "file:///some/path.txt")
                (:source . "user")))
         (res (hactar::format-doc-for-fzf-preview doc)))
    (is-true (str:containsp "Title: My Doc" res))
    (is-true (str:containsp "URI: file:///some/path.txt" res))
    (is-true (str:containsp "Source: user" res))
    (is-true (str:containsp "Tags: tag1, tag2" res))
    (is-true (str:containsp "Covers: topic1" res))
    (is-true (str:containsp "This is the content." res))))

(test fuzzy-select-doc-test
  "Test end-to-end selection of a document from a list."
  (let ((hactar::*in-editor* t)
        (docs '(((:title . "Doc A") (:content . "Content A"))
                ((:title . "Doc B") (:content . "Content B")))))
    ;; Select Doc B
    (with-input-from-string (*standard-input* "2")
      (let ((res (hactar::fuzzy-select-doc docs)))
        (is (equal (cdr (assoc :title res)) "Doc B"))
        (is (equal (cdr (assoc :content res)) "Content B"))))

    ;; Empty doc list
    (is (null (hactar::fuzzy-select-doc nil)))))
