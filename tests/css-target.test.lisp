(in-package :hactar-tests)

(def-suite css-target-tests :description "CSS target tests.")
(in-suite css-target-tests)

(defun css-compile (source)
  "Compile SOURCE to CSS and return the first output file contents."
  (string-trim '(#\Space #\Newline #\Tab)
               (hactar::compile-file-to-string source :target :css)))

(test css-defcss-basic-rule
  "defcss emits a simple rule with declarations."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss global-styles
                    (:body
                      :margin 0
                      :background-color \"#fff\"))")))
    (is (search "body {" result)
        "body keyword should emit as bare tag, not :body")
    (is (search "margin: 0;" result))
    (is (not (search ":body" result))
        ":body pseudo-selector should not appear")
    (is (search "background-color: #fff;" result))))

(test css-keyword-selector-normalization
  "Keyword selectors normalize correctly."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss global-styles
                    (:root
                      :--primary-color \"#b45309\"))")))
    (is (search ":root {" result))
    (is (search "--primary-color: #b45309;" result))))

(test css-html-tag-selector-no-colon
  "HTML tag keyword selectors emit without : prefix."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss global-styles
                    (:body
                      :margin 0
                      :font-family \"sans-serif\"))")))
    (is (search "body {" result))
    (is (not (search ":body" result))
        "body should not have : prefix")))

(test css-html-tag-selector-html
  "html keyword emits as bare tag selector."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss s (:html :margin 0))")))
    (is (search "html {" result))
    (is (not (search ":html" result)))))

(test css-string-selector-emission
  "String selectors are emitted verbatim."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss global-styles
                    (\".chat-container\"
                      :display \"flex\"))")))
    (is (search ".chat-container {" result))
    (is (search "display: flex;" result))))

(test css-nested-child-selector
  "Nested child selectors are flattened with parent selector."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss global-styles
                    (\".chat-container\"
                      :display \"flex\"
                      (\"> .message-list\"
                        :flex 1
                        :padding 16)))")))
    (is (search ".chat-container {" result))
    (is (search ".chat-container > .message-list {" result))
    (is (search "flex: 1;" result))
    (is (search "padding: 16px;" result))))

(test css-nested-media-query
  "Nested media queries wrap the current selector."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss global-styles
                    (\".chat-container\"
                      :display \"flex\"
                      (\"@media (max-width: 600px)\"
                        :padding 8)))")))
    (is (search "@media (max-width: 600px) {" result))
    (is (search ".chat-container {" result))
    (is (search "padding: 8px;" result))))

(test css-numeric-values-get-px-units
  "Non-zero numeric CSS values get px units appended."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss s (\".box\" :padding 16 :margin 8 :border-radius 4))")))
    (is (search "padding: 16px;" result))
    (is (search "margin: 8px;" result))
    (is (search "border-radius: 4px;" result))))

(test css-unitless-properties
  "Properties like flex, font-weight, z-index emit without px units."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss s (\".x\" :flex 1 :font-weight 600 :z-index 10 :opacity 0.5))")))
    (is (search "flex: 1;" result))
    (is (search "font-weight: 600;" result))
    (is (search "z-index: 10;" result))
    (is (not (search "1px" result)))
    (is (not (search "600px" result)))
    (is (not (search "10px" result)))))

(test css-zero-value-no-units
  "Zero values are emitted without units."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss s (:body :margin 0 :padding 0))")))
    (is (search "margin: 0;" result))
    (is (not (search "0px" result))
        "Zero should not have px units")))

(test css-string-values-no-extra-units
  "String values are emitted verbatim without added units."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss s (\".x\" :padding \"12px 16px\" :font-size \"1rem\"))")))
    (is (search "padding: 12px 16px;" result))
    (is (search "font-size: 1rem;" result))))

(test css-top-level-media-query-no-double-nesting
  "Top-level @media query without parent selector does not double-nest."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss s
                    (\"@media (max-width: 600px)\"
                      :padding 8))")))
    (is (search "@media (max-width: 600px) {" result))
    (is (search "padding: 8px;" result))
    ;; Must NOT contain a second nested @media
    (is (= 1 (count-if (lambda (i)
                          (search "@media" result :start2 i :end2 (min (+ i 6) (length result))))
                        (loop for i from 0 below (length result) collect i)))
        "Should contain only one @media occurrence")
    (is (not (search "@media (max-width: 600px) {
  @media" result))
        "Should not have double-nested @media")))

(test css-multiple-top-level-rules
  "defcss supports multiple top-level rules."
  (let ((result (css-compile
                 "(in-file \"styles.css\")
                  (defcss global-styles
                    (:root
                      :--bg-color \"#f3f4f6\")
                    (:body
                      :margin 0))")))
    (is (search ":root {" result))
    (is (search "--bg-color: #f3f4f6;" result))
    (is (search "body {" result))
    (is (search "margin: 0;" result))))
