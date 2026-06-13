(in-package :hactar-tests)

(def-suite hypertext-tests
  :description "Tests for the hypertext generic documentation framework")

(in-suite hypertext-tests)

(test parse-uri-protocol-test
  "Verify parse-uri-protocol correctly extracts protocol and path."
  (multiple-value-bind (proto path) (hactar::parse-uri-protocol "docs:js/react/latest")
    (is (string= proto "docs"))
    (is (string= path "js/react/latest")))
  (multiple-value-bind (proto path) (hactar::parse-uri-protocol "apidoc:react")
    (is (string= proto "apidoc"))
    (is (string= path "react")))
  ;; Default fallback protocol is docs
  (multiple-value-bind (proto path) (hactar::parse-uri-protocol "react")
    (is (string= proto "docs"))
    (is (string= path "react"))))

(test defhypertext-registration-test
  "Verify defhypertext registers docs into protocol dispatcher registries."
  (let ((hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (eval '(hactar::defhypertext "docs:js/react/19.2"
                                 :title "React 19.2 Documentation"
                                 :content (format nil "# React 19.2~%React is a frontend framework.~%~%## Hooks~%Hooks let you use state.~%~%## State~%useState is a Hook.")
                                 :tags '("react" "js")))
    (let* ((dispatcher (gethash "docs" hactar::*protocol-dispatchers*))
           (doc (when dispatcher (gethash "js/react/19.2" (hactar::protocol-dispatcher-registry dispatcher)))))
      (is-true doc)
      (is (string= (getf doc :title) "React 19.2 Documentation"))
      (is (equal (getf doc :tags) '("react" "js")))
      ;; Check automatically extracted section headings
      (let ((sections (getf doc :sections)))
        (is (= (length sections) 2))
        (is (string= (getf (first sections) :title) "Hooks"))
        (is (string= (getf (second sections) :title) "State"))))))

(test resolve-hypertext-uri-formats-test
  "Verify resolve-hypertext-uri resolves extensions and formats output."
  (let ((hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::register-hypertext "docs:js/react/latest"
                                (list :title "React Doc"
                                      :content (format nil "# React~%```js~%const a = 1;~%```")
                                      :tags '("react")))
    ;; Test markdown resolution (default)
    (multiple-value-bind (content mime-type) (hactar::resolve-hypertext-uri "docs:js/react/latest")
      (is (search "# React" content))
      (is (string= mime-type "text/markdown")))
    ;; Test markdown resolution via extension
    (multiple-value-bind (content mime-type) (hactar::resolve-hypertext-uri "docs:js/react/latest.md")
      (is (search "# React" content))
      (is (string= mime-type "text/markdown")))
    ;; Test JSON resolution
    (multiple-value-bind (content mime-type) (hactar::resolve-hypertext-uri "docs:js/react/latest.json")
      (is (search "content" content))
      (is (string= mime-type "application/json")))
    ;; Test Org resolution
    (multiple-value-bind (content mime-type) (hactar::resolve-hypertext-uri "docs:js/react/latest.org")
      (is (search "* React" content))
      (is (search "#+begin_src js" content))
      (is (string= mime-type "text/org")))))

(test resolve-hypertext-subpaths-test
  "Verify resolve-hypertext-uri matches section headings as sub-paths."
  (let ((hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::register-hypertext "docs:js/react/latest"
                                (list :title "React Doc"
                                      :content (format nil "# React~%Body~%~%## Hooks~%Use hooks inside components.")
                                      :tags '("react")))
    ;; Check sub-path hooks match
    (multiple-value-bind (content mime-type) (hactar::resolve-hypertext-uri "docs:js/react/latest/hooks")
      (is (search "Use hooks inside components." content))
      (is (string= mime-type "text/markdown")))))

(test dynamic-route-extensibility-test
  "Verify modes can register dynamic route patterns on dispatchers."
  (let ((hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::register-protocol-route "docs" "^js/react/([^/]+)/dynamic$"
                                     (lambda (version)
                                       (list :content (format nil "# React ~A Dynamic" version)
                                             :title "Dynamic React")))
    (multiple-value-bind (content mime-type) (hactar::resolve-hypertext-uri "docs:js/react/19.2/dynamic")
      (is (string= content "# React 19.2 Dynamic"))
      (is (string= mime-type "text/markdown")))))

(test filesystem-interface-two-way-sync-test
  "Verify rendering and parsing back of Org Aggregate format via dispatcher."
  (let ((hactar::*protocol-dispatchers* (make-hash-table :test 'equal)))
    (hactar::register-hypertext "docs:js/react/latest"
                                (list :title "React Doc"
                                      :content "React core library"
                                      :tags '("react")))
    (hactar::register-hypertext "docs:js/redux/latest"
                                (list :title "Redux Doc"
                                      :content "Redux state library"
                                      :tags '("redux")))
    ;; Render to Org
    (let ((org-text (hactar::render-protocol-as-org "docs")))
      (is (search "#+TITLE: docs Documentation" org-text))
      (is (search "* React Doc" org-text))
      (is (search ":URI: docs:js/react/latest" org-text))
      (is (search "* Redux Doc" org-text))
      (is (search ":URI: docs:js/redux/latest" org-text))

      ;; Now parse back to registry and verify
      (hactar::parse-protocol-from-org "docs" org-text)
      (let* ((dispatcher (gethash "docs" hactar::*protocol-dispatchers*))
             (doc-react (when dispatcher (gethash "js/react/latest" (hactar::protocol-dispatcher-registry dispatcher))))
             (doc-redux (when dispatcher (gethash "js/redux/latest" (hactar::protocol-dispatcher-registry dispatcher)))))
        (is-true doc-react)
        (is (string= (getf doc-react :title) "React Doc"))
        (is (string= (string-trim '(#\Space #\Newline #\Tab) (getf doc-react :content)) "React core library"))
        (is-true doc-redux)
        (is (string= (getf doc-redux :title) "Redux Doc"))
        (is (string= (string-trim '(#\Space #\Newline #\Tab) (getf doc-redux :content)) "Redux state library"))))))
