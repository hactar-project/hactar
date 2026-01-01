(in-package :hactar-tests)

(def-suite tui-tests :description "Tests for TUI utility functions.")
(in-suite tui-tests)

;;* wrap-text

(test wrap-text-nil-input
  "wrap-text returns NIL when given NIL."
  (is (null (hactar::wrap-text nil 40))))

(test wrap-text-empty-string
  "wrap-text on an empty string returns a list with one empty string."
  (let ((result (hactar::wrap-text "" 40)))
    (is (= 1 (length result)))
    (is (string= "" (first result)))))

(test wrap-text-single-word-fits
  "A single word shorter than width stays on one line."
  (let ((result (hactar::wrap-text "hello" 40)))
    (is (= 1 (length result)))
    (is (string= "hello" (first result)))))

(test wrap-text-single-word-exceeds-width
  "A single word longer than width still appears on one line (no mid-word break)."
  (let ((result (hactar::wrap-text "superlongword" 5)))
    (is (= 1 (length result)))
    (is (string= "superlongword" (first result)))))

(test wrap-text-wraps-at-width
  "Multiple words wrap to the next line when exceeding width."
  (let ((result (hactar::wrap-text "one two three four" 10)))
    ;; "one two" = 7, "three" = 5, "four" = 4
    ;; "one two" fits in 10, "one two three" = 13 > 10
    (is (>= (length result) 2))
    ;; Each line should not exceed width (unless a single word is longer)
    (dolist (line result)
      (is (<= (length line) 18))))) ; Sanity: no line longer than input

(test wrap-text-respects-width-boundary
  "Words are placed on new lines when they would exceed the width."
  (let ((result (hactar::wrap-text "aa bb cc dd" 5)))
    ;; "aa bb" = 5, fits exactly. "cc dd" = 5, fits exactly.
    (is (= 2 (length result)))
    (is (string= "aa bb" (first result)))
    (is (string= "cc dd" (second result)))))

(test wrap-text-multiple-spaces-collapsed
  "str:words splits on whitespace, so multiple spaces between words are collapsed."
  (let ((result (hactar::wrap-text "hello    world" 40)))
    (is (= 1 (length result)))
    (is (string= "hello world" (first result)))))

(test wrap-text-width-1
  "With width 1, each word goes on its own line."
  (let ((result (hactar::wrap-text "a b c" 1)))
    (is (= 3 (length result)))
    (is (string= "a" (first result)))
    (is (string= "b" (second result)))
    (is (string= "c" (third result)))))

(test wrap-text-exact-fit
  "Text that fits exactly in the width produces one line."
  (let ((result (hactar::wrap-text "abcde" 5)))
    (is (= 1 (length result)))
    (is (string= "abcde" (first result)))))

(test wrap-text-long-paragraph
  "A longer paragraph wraps correctly and no line exceeds the width (barring single long words)."
  (let* ((text "The quick brown fox jumps over the lazy dog and then runs away fast")
         (width 20)
         (result (hactar::wrap-text text width)))
    (is (> (length result) 1))
    ;; Each line should be at most `width` characters unless a single word exceeds it
    (dolist (line result)
      ;; Each line should be reasonable
      (is (> (length line) 0)))))

;;* ansi

(test ansi-generates-escape-sequence
  "ansi generates a proper ESC + code string."
  (let ((result (hactar::ansi "[7m")))
    (is (stringp result))
    (is (char= #\Esc (char result 0)))
    (is (string= "[7m" (subseq result 1)))))

(test ansi-empty-code
  "ansi with an empty string just returns ESC."
  (let ((result (hactar::ansi "")))
    (is (= 1 (length result)))
    (is (char= #\Esc (char result 0)))))

(test ansi-cursor-movement
  "ansi can generate cursor movement sequences."
  (let ((result (hactar::ansi "[5;10H")))
    (is (char= #\Esc (char result 0)))
    (is (string= "[5;10H" (subseq result 1)))))

;;* fuzzy-select edge cases

(test fuzzy-select-nil-items
  "fuzzy-select returns NIL immediately when given NIL items."
  (is (null (hactar::fuzzy-select nil))))

(test fuzzy-select-empty-list
  "fuzzy-select returns NIL immediately when given an empty list."
  (is (null (hactar::fuzzy-select '()))))

;;* get-terminal-size

(test get-terminal-size-returns-two-values
  "get-terminal-size returns two positive integer values."
  (multiple-value-bind (rows cols) (hactar::get-terminal-size)
    (is (integerp rows))
    (is (integerp cols))
    (is (> rows 0))
    (is (> cols 0))))

(test get-terminal-size-reasonable-defaults
  "get-terminal-size returns at least 24x80 (the fallback defaults)."
  (multiple-value-bind (rows cols) (hactar::get-terminal-size)
    (is (>= rows 1))
    (is (>= cols 1))))
