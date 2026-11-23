(defpackage #:hactar-rag
  (:use #:cl)
  (:export #:rag-index #:make-index #:clear-index
           #:index #:retrieve #:rerank #:cosine-similarity))
