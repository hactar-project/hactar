(in-package :hactar-tests)

(def-suite worker-target-tests :description "Worker target tests.")
(in-suite worker-target-tests)

;;* Helpers
(defun worker-compile (source)
  (string-trim '(#\Space #\Newline #\Tab)
               (hactar::compile-file-to-string source :target :worker)))
;;* Tests
(test worker-defworker
  "defworker produces export default with async fetch."
  (let ((result (worker-compile
                 "(defworker (request env ctx)
                    (return (text-response \"hello\")))")))
    (is (search "export default" result))
    (is (search "async fetch(request, env, ctx)" result))
    (is (search "new Response" result))))

(test worker-json-response
  (let ((result (worker-compile "(json-response data :status 201)")))
    (is (search "JSON.stringify(data)" result))
    (is (search "201" result))
    (is (search "application/json" result))))

(test worker-text-response
  (let ((result (worker-compile "(text-response \"ok\")")))
    (is (search "new Response(\"ok\"" result))))

(test worker-inherits-typescript
  "Worker inherits all TypeScript features."
  (let ((result (worker-compile "(defun handle ((req :type 'Request)) :return 'Response
                                   (return (text-response \"ok\")))")))
    (is (search "function handle(req: Request): Response" result))))

(test worker-fetch-options-camelized
  (let ((result (worker-compile "(fetch url :method \"POST\" :headers (create \"Content-Type\" \"application/json\"))")))
    (is (search "fetch(url, { method: \"POST\", headers: { \"Content-Type\": \"application/json\" } })" result))))
