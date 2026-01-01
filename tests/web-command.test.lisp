(in-package :hactar-tests)

(def-suite web-command-tests
  :description "Tests for defwebcommand, defwebroute, and web command execution.")

(in-suite web-command-tests)

(test match-route-pattern-literal-test
  "Test matching literal strings in route patterns."
  (let ((pattern '("newest" "posts")))
    (let ((bindings (hactar::match-route-pattern pattern '("newest" "posts"))))
      (is-true bindings)
      (is (listp bindings)))

    (is (null (hactar::match-route-pattern pattern '("newest" "comments"))))
    (is (null (hactar::match-route-pattern pattern '("newest"))))
    (is (null (hactar::match-route-pattern pattern '("newest" "posts" "extra"))))))

(test match-route-pattern-variable-test
  "Test matching variable bindings in route patterns."
  (let ((pattern '("user" username)))
    (let ((bindings (hactar::match-route-pattern pattern '("user" "alice"))))
      (is-true bindings)
      (is (string= (cdr (assoc 'username bindings)) "alice")))

    (is (null (hactar::match-route-pattern pattern '("users" "alice"))))
    (is (null (hactar::match-route-pattern pattern '("user"))))))

(test match-route-pattern-rest-test
  "Test matching &rest patterns."
  (let ((pattern '("newest" &rest args)))
    (let ((bindings (hactar::match-route-pattern pattern '("newest" "--limit" "10"))))
      (is-true bindings)
      (is (equal (cdr (assoc 'args bindings)) '("--limit" "10"))))

    (let ((bindings (hactar::match-route-pattern pattern '("newest"))))
      (is-true bindings)
      (is (null (cdr (assoc 'args bindings)))))

    (is (null (hactar::match-route-pattern pattern '("oldest" "--limit" "10"))))))

(test match-route-pattern-mixed-test
  "Test matching patterns with mixed literal, variable, and &rest."
  (let ((pattern '("post" post-id "comments" &rest args)))
    (let ((bindings (hactar::match-route-pattern pattern '("post" "123" "comments" "--sort" "new"))))
      (is-true bindings)
      (is (string= (cdr (assoc 'post-id bindings)) "123"))
      (is (equal (cdr (assoc 'args bindings)) '("--sort" "new"))))

    (is (null (hactar::match-route-pattern pattern '("post" "123" "--sort" "new"))))))

(test find-matching-route-priority-test
  "Test that routes are matched by priority (highest first)."
  (let* ((route1 (hactar::make-web-route :pattern '("test") :priority 5 :handler (lambda () 'route1) :bindings '()))
         (route2 (hactar::make-web-route :pattern '("test") :priority 10 :handler (lambda () 'route2) :bindings '()))
         (route3 (hactar::make-web-route :pattern '("test") :priority 1 :handler (lambda () 'route3) :bindings '()))
         (web-cmd (hactar::make-web-command :name "test-cmd" :routes (list route1 route2 route3))))

    (multiple-value-bind (matched-route bindings)
        (hactar::find-matching-route web-cmd '("test"))
      (is-true matched-route)
      (is (eq matched-route route2))
      (is (listp bindings)))))

(test find-matching-route-first-match-test
  "Test that the first matching route is returned when multiple routes match."
  (let* ((route1 (hactar::make-web-route :pattern '("post" &rest args) :priority 10 :handler (lambda (a) (declare (ignore a)) 'route1) :bindings '(args)))
         (route2 (hactar::make-web-route :pattern '("post" id) :priority 5 :handler (lambda (i) (declare (ignore i)) 'route2) :bindings '(id)))
         (web-cmd (hactar::make-web-command :name "test-cmd" :routes (list route1 route2))))

    (multiple-value-bind (matched-route bindings)
        (hactar::find-matching-route web-cmd '("post" "123"))
      (is-true matched-route)
      (is (listp bindings)))))

(test defwebcommand-registration-test
  "Test that defwebcommand registers a command in *web-commands*."
  ;; Define a simple test command
  (eval '(hactar::defwebcommand test-web-cmd
           "A test web command"
           (hactar::defwebroute test-action1 "Test action 1"
             ("action1") ()
             :priority 10
             '(list "action1"))
           (hactar::def-default-route ()
             '(list "default"))))

  (let ((web-cmd (gethash "test-web-cmd" hactar::*web-commands*)))
    (is-true web-cmd)
    (is (string= (hactar::web-command-name web-cmd) "test-web-cmd"))
    (is (string= (hactar::web-command-description web-cmd) "A test web command"))
    (is (= (length (hactar::web-command-routes web-cmd)) 1))
    (is-true (hactar::web-command-default-route web-cmd))))

(test execute-web-command-route-match-test
  "Test executing a web command with a matching route."
  (eval '(hactar::defwebcommand test-exec-cmd
           "Test execution"
           (hactar::defwebroute test-greet "Greet a person"
             ("greet" name) (name)
             :priority 10
             `(format nil "Hello, ~A!" ,name))))

  (let* ((output-stream (make-string-output-stream))
         (*standard-output* output-stream))
    (hactar::execute-web-command "test-exec-cmd" '("greet" "World"))
    (let ((output (get-output-stream-string output-stream)))
      (is (string= output "Hello, World!")))))

(test execute-web-command-default-route-test
  "Test executing a web command that falls back to default route."
  (eval '(hactar::defwebcommand test-default-cmd
           "Test default"
           (hactar::defwebroute test-specific "Specific route"
             ("specific") ()
             :priority 10
             '(format nil "Specific"))
           (hactar::def-default-route ()
             '(format nil "Default"))))

  (let* ((output-stream (make-string-output-stream))
         (*standard-output* output-stream))
    (hactar::execute-web-command "test-default-cmd" '("other" "args"))
    (let ((output (get-output-stream-string output-stream)))
      (is (string= output "Default")))))

(test execute-web-command-no-match-test
  "Test executing a web command with no matching route and no default."
  (eval '(hactar::defwebcommand test-nomatch-cmd
           "Test no match"
           (hactar::defwebroute test-specific-nomatch "Specific route for nomatch test"
             ("specific") ()
             :priority 10
             '(format nil "Specific"))))

  (let* ((output-stream (make-string-output-stream))
         (*standard-output* output-stream))
    (hactar::execute-web-command "test-nomatch-cmd" '("other" "args"))
    (let ((output (get-output-stream-string output-stream)))
      (is (search "No matching route" output)))))

(test hn-command-registration-test
  "Test that the hn command is registered correctly."
  (let ((web-cmd (gethash "hn" hactar::*web-commands*)))
    (is-true web-cmd)
    (is (string= (hactar::web-command-name web-cmd) "hn"))
    (is (> (length (hactar::web-command-routes web-cmd)) 0))
    (is-true (hactar::web-command-default-route web-cmd))))

(test hn-command-routes-test
  "Test that hn command has the expected routes."
  (let* ((web-cmd (gethash "hn" hactar::*web-commands*))
         (routes (hactar::web-command-routes web-cmd)))
    (is (> (length routes) 0))
    (is-true (hactar::web-command-default-route web-cmd))))

(test parse-rss-feed-basic-test
  "Test basic RSS feed parsing with mock data."
  (let ((mock-xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>Test Feed</title>
    <item>
      <title>Test Item 1</title>
      <link>https://example.com/1</link>
      <description>Description 1</description>
      <pubDate>Mon, 01 Jan 2024 00:00:00 GMT</pubDate>
    </item>
    <item>
      <title>Test Item 2</title>
      <link>https://example.com/2</link>
      <description>Description 2</description>
      <pubDate>Tue, 02 Jan 2024 00:00:00 GMT</pubDate>
    </item>
  </channel>
</rss>"))
    (let ((items (hactar::parse-rss-feed mock-xml)))
      (is (= (length items) 2))
      (let ((item1 (first items)))
        (is (string= (cdr (assoc :title item1)) "Test Item 1"))
        (is (string= (cdr (assoc :link item1)) "https://example.com/1"))
        (is (string= (cdr (assoc :description item1)) "Description 1")))
      (let ((item2 (second items)))
        (is (string= (cdr (assoc :title item2)) "Test Item 2"))))))

(test format-hn-items-markdown-test
  "Test formatting HN items as markdown."
  (let* ((items '(((:title . "First Post")
                   (:link . "https://example.com/1")
                   (:description . "Desc 1")
                   (:pubdate . "Date 1"))
                  ((:title . "Second Post")
                   (:link . "https://example.com/2")
                   (:description . "Desc 2")
                   (:pubdate . "Date 2"))))
         (markdown (hactar::format-hn-items-markdown items)))
    (is (search "- [First Post](https://example.com/1)" markdown))
    (is (search "- [Second Post](https://example.com/2)" markdown))))

(test hn-newest-route-execution-test
  "Test executing hn newest route with mocked fetch."
  (let ((mock-items '(((:title . "New Item")
                       (:link . "https://news.ycombinator.com/item?id=1")
                       (:description . "Description")
                       (:pubdate . "Date")))))
    (with-dynamic-stubs ((hactar::fetch-hn-feed (lambda (path &key limit)
                                                  (declare (ignore path limit))
                                                  mock-items)))
      (let* ((output-stream (make-string-output-stream))
             (*standard-output* output-stream))
        (hactar::get-hn-newest '())
        (let ((output (get-output-stream-string output-stream)))
          (is (search "New Item" output))
          (is (search "news.ycombinator.com" output)))))))

(test hn-top-route-execution-test
  "Test executing hn top route with mocked fetch."
  (let ((mock-items '(((:title . "Top Item")
                       (:link . "https://news.ycombinator.com/item?id=2")
                       (:description . "Description")
                       (:pubdate . "Date")))))
    (with-dynamic-stubs ((hactar::fetch-hn-feed (lambda (path &key limit)
                                                  (declare (ignore path limit))
                                                  mock-items)))
      (let* ((output-stream (make-string-output-stream))
             (*standard-output* output-stream))
        (hactar::get-hn-top '())
        (let ((output (get-output-stream-string output-stream)))
          (is (search "Top Item" output)))))))

(test hn-default-route-execution-test
  "Test executing hn default route (front page) with mocked fetch."
  (let ((mock-items '(((:title . "Front Page Item")
                       (:link . "https://news.ycombinator.com/item?id=3")
                       (:description . "Description")
                       (:pubdate . "Date")))))
    (with-dynamic-stubs ((hactar::fetch-hn-feed (lambda (path &key limit)
                                                  (declare (ignore path limit))
                                                  mock-items)))
      (let* ((output-stream (make-string-output-stream))
             (*standard-output* output-stream))
        (hactar::get-hn-front-page-md)
        (let ((output (get-output-stream-string output-stream)))
          (is (search "Hacker News Front Page" output))
          (is (search "Front Page Item" output)))))))

(test hn-limit-parsing-test
  "Test that --limit argument is parsed correctly in hn commands."
  (let ((mock-items (loop for i from 1 to 50
                         collect `((:title . ,(format nil "Item ~A" i))
                                  (:link . ,(format nil "https://example.com/~A" i))
                                  (:description . "Desc")
                                  (:pubdate . "Date")))))
    (with-dynamic-stubs ((hactar::fetch-hn-feed (lambda (path &key limit)
                                                  (declare (ignore path))
                                                  (if limit
                                                      (subseq mock-items 0 (min limit (length mock-items)))
                                                      mock-items))))
      (let* ((output-stream (make-string-output-stream))
             (*standard-output* output-stream))
        (hactar::get-hn-newest '("--limit" "5"))
        (let ((output (get-output-stream-string output-stream)))
          ;; Should have exactly 5 items
          (is (= (length (remove-if-not (lambda (line) (search "- [Item" line))
                                       (uiop:split-string output :separator '(#\Newline))))
                 5)))))))
