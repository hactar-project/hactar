(in-package :hactar-tests)

(def-suite copilot-tests
           :description "Tests for GitHub Copilot related functions.")

(in-suite copilot-tests)

(test copilot-list-models-vector-test
  "list-copilot-models returns model IDs when API returns a vector."
  (with-dynamic-stubs ((llm::copilot-api-request
                        (lambda (&rest args)
                          (declare (ignore args))
                          '((:data . #(((:id . "model-a"))
                                       ((:id . "model-b"))))))))
    (is (equal (llm:list-copilot-models) '("model-a" "model-b")))))

(test copilot-list-models-list-test
  "list-copilot-models handles list form for :data."
  (with-dynamic-stubs ((llm::copilot-api-request
                        (lambda (&rest args)
                          (declare (ignore args))
                          '((:data . (((:id . "x"))
                                      ((:id . "y"))))))))
    (is (equal (llm:list-copilot-models) '("x" "y")))))

(test copilot-api-request-success-test
  "copilot-api-request returns parsed JSON on success."
  (let ((body "{\"ok\":true}"))
    (with-dynamic-stubs ((llm:get-copilot-token (lambda () "token123"))
                         (drakma:http-request
                          (lambda (&rest args &key &allow-other-keys)
                            (declare (ignore args))
                            (values body 200))))
      (let ((resp (llm::copilot-api-request "/ping")))
        (is (equal (cdr (assoc :ok resp)) t))))))

(test copilot-api-request-error-test
  "copilot-api-request returns nil and logs on HTTP error."
  (let ((body "Not found"))
    (with-dynamic-stubs ((llm:get-copilot-token (lambda () "token123"))
                         (drakma:http-request
                          (lambda (&rest args &key &allow-other-keys)
                            (declare (ignore args))
                            (values body 404))))
      (let* ((output-stream (make-string-output-stream))
             (*standard-output* output-stream)
             (resp (llm::copilot-api-request "/bad")))
        (is (null resp))
        (let ((out (get-output-stream-string output-stream)))
          (is-true (search "API Error: HTTP 404" out)))))))

(test copilot-api-request-no-token-test
  "copilot-api-request signals error when token is missing."
  (with-dynamic-stubs ((llm:get-copilot-token (lambda () nil)))
    (signals error (llm::copilot-api-request "/ping"))))

(test copilot-complete-text-success-test
  "copilot-complete-text extracts content from first choice."
  (with-dynamic-stubs ((llm::copilot-api-request
                        (lambda (&rest args)
                          (declare (ignore args))
                          '((:choices . #(((:message . ((:content . "Generated text"))))))))))
    (is (string= (llm:copilot-complete-text "Prompt") "Generated text"))))

(test copilot-complete-text-empty-choices-test
  "copilot-complete-text returns nil on empty choices."
  (with-dynamic-stubs ((llm::copilot-api-request
                        (lambda (&rest args)
                          (declare (ignore args))
                          '((:choices . #())))))
    (is (null (llm:copilot-complete-text "Prompt")))))
