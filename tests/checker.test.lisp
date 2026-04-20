(in-package :hactar-tests)

(def-suite checker-tests :description "Checker/linter infrastructure tests.")
(in-suite checker-tests)

(test check-unsupported-form-detects-bad-ops
  "Unsupported forms are flagged."
  (let ((result (hactar::check-unsupported-form
                 :javascript
                 '(defclass foo () ())
                 '(defun defvar let))))
    (is-true result)
    (is (eq :error (hactar::check-result-level result)))
    (is (search "defclass" (hactar::check-result-message result)))))

(test check-unsupported-form-passes-good-ops
  "Supported forms pass."
  (let ((result (hactar::check-unsupported-form
                 :javascript
                 '(defun foo () nil)
                 '(defun defvar let))))
    (is (null result))))

(test check-disable-enable
  "Checks can be disabled and re-enabled."
  (hactar::disable-check 'hactar::jsx-anchor-valid-href)
  (is (not (hactar::check-enabled-p 'hactar::jsx-anchor-valid-href)))
  (hactar::enable-check 'hactar::jsx-anchor-valid-href)
  (is (hactar::check-enabled-p 'hactar::jsx-anchor-valid-href)))

(test check-jsx-anchor-missing-href
  "Anchor without href is flagged."
  (let* ((all-results (hactar::run-checks :redwood '((hactar::target :redwood) (hactar::a :class "link" "Click"))))
         (results (remove-if-not (lambda (r) (search "missing href" (hactar::check-result-message r))) all-results)))
    (is (>= (length results) 1))
    (is (eq :warning (hactar::check-result-level (first results))))))

(test check-jsx-anchor-empty-href
  "Anchor with empty href is flagged."
  (let* ((all-results (hactar::run-checks :redwood '((hactar::target :redwood) (hactar::a :href "" "Click"))))
         (results (remove-if-not (lambda (r) (search "invalid href" (hactar::check-result-message r))) all-results)))
    (is (>= (length results) 1))
    (is (search "invalid href" (hactar::check-result-message (first results))))))

(test check-jsx-anchor-valid-href-passes
  "Anchor with valid href passes."
  (let ((results (hactar::run-checks :redwood '((hactar::target :redwood) (hactar::a :href "/about" "About")))))
    (is (null results))))

(test check-jsx-anchor-disabled-rule
  "Disabled check rule is not run."
  (hactar::disable-check 'hactar::jsx-anchor-valid-href)
  (unwind-protect
       (let ((results (hactar::run-checks :redwood '((hactar::target :redwood) (hactar::a :class "x" "Click")))))
         (is (null results)))
    (hactar::enable-check 'hactar::jsx-anchor-valid-href)))

;; Target detection tests
(test detect-target-from-tsx
  "TSX files detect as :redwood."
  (is (eq :redwood (hactar::detect-target-from-extension "app.tsx"))))

(test detect-target-from-ts
  "TS files detect as :typescript."
  (is (eq :typescript (hactar::detect-target-from-extension "utils.ts"))))

(test detect-target-from-jsx
  "JSX files detect as :redwood."
  (is (eq :redwood (hactar::detect-target-from-extension "component.jsx"))))

(test detect-target-from-js
  "JS files detect as :javascript."
  (is (eq :javascript (hactar::detect-target-from-extension "index.js"))))

(test detect-target-from-css
  "CSS files detect as :css."
  (is (eq :css (hactar::detect-target-from-extension "styles.css"))))

(test detect-target-from-unknown
  "Unknown extensions default to :javascript."
  (is (eq :javascript (hactar::detect-target-from-extension "readme.txt"))))

;; offset-to-line-col tests
(test offset-to-line-col-start
  "Offset 0 is line 0, col 0."
  (let ((result (hactar::offset-to-line-col "hello" 0)))
    (is (= 0 (car result)))
    (is (= 0 (cdr result)))))

(test offset-to-line-col-same-line
  "Offset within first line."
  (let ((result (hactar::offset-to-line-col "hello world" 5)))
    (is (= 0 (car result)))
    (is (= 5 (cdr result)))))

(test offset-to-line-col-second-line
  "Offset on second line after newline."
  (let ((result (hactar::offset-to-line-col (format nil "ab~%cd") 4)))
    (is (= 1 (car result)))
    (is (= 1 (cdr result)))))

;; read-all-forms-with-positions tests
(test read-all-forms-with-positions-basic
  "Reads forms with start/end offsets."
  (let ((results (hactar::read-all-forms-with-positions "(defvar x 1) (defvar y 2)")))
    (is (= 2 (length results)))
    (is (equal '(hactar::defvar hactar::x 1) (first (first results))))
    (is (= 0 (second (first results))))
    (is (equal '(hactar::defvar hactar::y 2) (first (second results))))))

;; check-file-source tests
(test check-file-source-finds-issues
  "check-file-source returns results with source hints."
  (let* ((source "(target :redwood) (a :class \"x\" \"Click\")")
         (results (hactar::check-file-source source :file "test.tsx")))
    (is (>= (length results) 1))
    (let ((hint (hactar::check-result-source-hint (first results))))
      (is (not (null hint)))
      (is (string= "test.tsx" (hactar::source-hint-file hint))))))

(test check-file-source-clean-passes
  "check-file-source returns empty for clean code."
  (let* ((source "(target :redwood) (a :href \"/about\" \"About\")")
         (results (hactar::check-file-source source :file "test.tsx")))
    (is (= 0 (length results)))))

;; format-check-results-xml tests
(test format-check-results-xml-wraps-in-diagnostics
  "XML output wraps in <diagnostics> tags."
  (let ((output (with-output-to-string (s)
                  (hactar::format-check-results-xml nil s))))
    (is (search "<diagnostics>" output))
    (is (search "</diagnostics>" output))))

(test format-check-results-xml-includes-message
  "XML output includes diagnostic message."
  (let* ((r (hactar::make-check-result :level :warning :message "test warning"
                                        :source-hint (hactar::make-source-hint :file "f.tsx" :start-line 1 :start-char 2)))
         (output (with-output-to-string (s)
                   (hactar::format-check-results-xml (list r) s))))
    (is (search "<message>test warning</message>" output))
    (is (search "<severity>2</severity>" output))
    (is (search "<line>1</line>" output))))

;; format-check-results-json tests
(test format-check-results-json-produces-valid-output
  "JSON output contains diagnostics key and message."
  (let* ((r (hactar::make-check-result :level :error :message "bad thing"
                                        :source-hint (hactar::make-source-hint :file "x.js" :start-line 5)))
         (output (with-output-to-string (s)
                   (hactar::format-check-results-json (list r) s))))
    (is (search "bad thing" output))
    (is (search "diagnostics" output))))
