(defpackage #:hyperfractal
  (:use #:cl)
  (:export #:*default-model* #:*default-format* #:*fetch-fn*
           #:*cache* #:*history*
           #:clear-cache #:cache-get #:cache-put
           #:extract-links #:resolve-url #:html->plaintext
           #:browse #:navigate-history))
