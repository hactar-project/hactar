(in-package :hactar-tests)

(def-suite markdown-tests
  :description "Tests for markdown utilities")

(in-suite markdown-tests)

(test test-get-all-md-src-blocks
  (let* ((md (format nil "Some text.~%~%```lisp~%(defun foo ())~%```~%~%More text."))
         (blocks (hactar:get-all-md-src-blocks md)))
    (is (= 1 (length blocks)))
    (is (string= "lisp" (getf (first blocks) :language)))
    (is (string= (format nil "(defun foo ())~%") (getf (first blocks) :content)))))

(test test-render-markdown-to-html
  (let ((html (hactar:render-markdown-to-html "Hello *world*")))
    (is (search "<p>Hello <em>world</em></p>" html))))

(test test-insert-md-sibling
  (let ((p1 (make-instance 'cmark:paragraph-node))
        (p2 (make-instance 'cmark:paragraph-node))
        (doc (make-instance 'cmark:document-node)))
    (hactar:insert-md-child doc p1)
    (hactar:insert-md-sibling p1 p2)
    (is (= 2 (length (cmark:node-children doc))))
    (is (eq p2 (second (cmark:node-children doc))))))

(test test-insert-md-child
  (let ((doc (make-instance 'cmark:document-node))
        (p (make-instance 'cmark:paragraph-node)))
    (hactar:insert-md-child doc p)
    (is (= 1 (length (cmark:node-children doc))))
    (is (eq p (first (cmark:node-children doc))))))
