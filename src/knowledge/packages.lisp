(defpackage #:hactar-knowledge
  (:use #:cl)
  (:export #:*docs-store* #:*errors-store* #:*guides-store*
           #:*fetch-fn* #:*knowledge-index*
           #:clear-knowledge-stores
           #:register-doc #:lookup-doc #:docs-find #:defdoc
           #:register-guide #:lookup-guide #:defguide
           #:register-error #:lookup-error #:errors-find #:deferror
           #:parse-errors-from-org
           #:parse-hypertext-uri #:resolve-hypertext-uri
           #:fetch-url #:import-docs #:knowledge-search
           ;; Wiki-as-lisp
           #:*wiki-nodes* #:*wiki-docsets*
           #:wiki-node #:make-wiki-node #:wiki-node-namespace #:wiki-node-path
           #:wiki-node-type #:wiki-node-properties #:wiki-node-related
           #:wiki-docset #:make-wiki-docset #:wiki-docset-name
           #:register-wiki-docset #:register-wiki-node #:find-wiki-node
           #:find-nodes-under #:parse-wiki-uri
           #:defdocset #:defendpoint #:defcomponent))
