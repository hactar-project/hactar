(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

;;; ---- hactar-rag ----
(test rag-indexes-retrieves-and-reranks
  (let ((idx (hactar-rag:make-index)))
    (hactar-rag:index idx "a" "react router loaders and actions"
                      :embedding #(1.0 0.0 0.0))
    (hactar-rag:index idx "b" "postgres migrations in rails"
                      :embedding #(0.0 1.0 0.0))
    (let ((hits (hactar-rag:retrieve idx "react router"
                                     :embedding #(0.9 0.1 0.0) :limit 1)))
      (is (= 1 (length hits)))
      (is (string= "a" (getf (first hits) :id))))
    (let ((reranked (hactar-rag:rerank "rails postgres"
                                       (list (list :id "a" :text "react router")
                                             (list :id "b" :text "rails postgres guide")))))
      (is (string= "b" (getf (first reranked) :id))))))

;;; ---- hactar-knowledge ----
(test knowledge-docs-errors-import-wiki-and-search
  (hactar-knowledge:clear-knowledge-stores)
  (hactar-knowledge:defdoc react-router
    :title "React Router"
    :text "Use loaders and actions in React Router v7."
    :tags ("web" "router")
    :covers ("react-router"))
  (is (string= "React Router"
               (getf (hactar-knowledge:lookup-doc 'react-router) :title)))
  (is (= 1 (length (hactar-knowledge:docs-find :tags '("web")))))
  (multiple-value-bind (text mime entry)
      (hactar-knowledge:resolve-hypertext-uri "docs:react-router")
    (declare (ignore entry))
    (is (search "loaders" text))
    (is (string= "text/plain" mime)))
  ;; Org errors parsing
  (let ((errs (hactar-knowledge:parse-errors-from-org
               (format nil "* E404 Not Found :http:web:~%:PROPERTIES:~%:CODE: 404~%:STACK: web~%:END:~%Resource missing.~%"))))
    (is (= 1 (length errs)))
    (is-true (hactar-knowledge:lookup-error "404")))
  ;; Offline fetch protocol + import-docs
  (let ((hactar-knowledge:*fetch-fn*
          (lambda (url)
            (format nil "# Guide for ~A~%~%See [API](https://example.com/api)." url))))
    (let ((imported (hactar-knowledge:import-docs "https://example.com/guide"
                                                  :name "ext-guide")))
      (is (search "GUIDE FOR" (getf imported :text)))))
  ;; Knowledge semantic + lexical search via hactar-rag
  (let ((hits (hactar-knowledge:knowledge-search "loaders actions")))
    (is (plusp (length hits))))
  ;; Wiki-as-lisp
  (hactar-knowledge:defdocset stripe :version "2026-01" :stack-tags ("payments"))
  (hactar-knowledge:defendpoint stripe/charges.create :method :post :path "/v1/charges")
  (is (= 1 (length (hactar-knowledge:find-nodes-under "stripe" "charges")))))
