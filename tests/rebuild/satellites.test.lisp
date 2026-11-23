(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test hactarsh-runs-repl-over-tui-and-core
  (let ((out (make-string-output-stream))
        (in (make-string-input-stream (format nil "help~%quit~%"))))
    (hactarsh:start-shell :input in :output out)
    (let ((text (get-output-stream-string out)))
      (is (search "hactarsh>" text)))))

(test hyperfractal-browses-offline-and-caches
  (hyperfractal:clear-cache)
  (let ((hyperfractal:*fetch-fn*
          (lambda (url) (format nil "<h1>~A</h1><p>hello</p>" url)))
        (hactar-llm:*mock-llm-output* "# Example\n\nSee [Docs](/docs)."))
    (let ((res (hyperfractal:browse "https://example.com" :format :markdown)))
      (is (eq :ok (hactar-core:result-status res)))
      (is (equal '(("Docs" . "https://example.com/docs"))
                 (getf (hactar-core:result-data res) :links)))
      (let ((cached (hyperfractal:browse "https://example.com" :use-cache t)))
        (is-true (getf (hactar-core:result-data cached) :cached))))))

(test ruhe-parses-rss-and-compiles-feed-offline
  (let ((ruhe:*fetch-fn*
          (lambda (url)
            (declare (ignore url))
            "<rss><channel><item><title>Item One</title><link>https://x/1</link></item></channel></rss>"))
        (hactar-llm:*mock-llm-output* "* Summary\n- Item One"))
    (let ((res (ruhe:ruhe-compile
                (format nil "* Feed~%See [News](https://example.com/rss.xml)~%"))))
      (is (eq :ok (hactar-core:result-status res)))
      (is (search "Item One" (getf (hactar-core:result-data res) :rendered))))))

(test pro-reports-status-cleanly
  (with-temp-project (root)
    (let* ((empty-dir (merge-pathnames "missing-pro/" root))
           (absent (hactar-pro:pro-status empty-dir))
           (present (hactar-pro:pro-status root)))
      (is (eq :ok (hactar-core:result-status absent)))
      (is (string= "absent" (hactar-core:result-message absent)))
      (is (eq :ok (hactar-core:result-status present)))
      (is (string= "present" (hactar-core:result-message present))))))
