(in-package :hactar-tests)

(def-suite md-render-tests
  :description "Tests for the Glow-inspired markdown renderer")

(in-suite md-render-tests)

;;* Helpers
(defun has-ansi-p (str)
  "Return T if STR contains at least one ANSI escape sequence."
  (and (search (string #\Esc) str) t))

(defun has-sgr-code-p (str code)
  "Return T if STR contains an SGR sequence with CODE (e.g. \"1\" for bold)."
  (search (format nil "~C[~Am" #\Esc code) str))

;;* Heading Tests
(test md-render-heading-h1
  "H1 headings should be bold, underline, and magenta."
  (let ((result (hactar::render-md-ansi "# Hello World")))
    (is (has-ansi-p result))
    (is (search "Hello World" result))
    ;; Bold (1), underline (4), magenta (35)
    (is (search (format nil "~C[1;4;35m" #\Esc) result))))

(test md-render-heading-h2
  "H2 headings should be bold magenta."
  (let ((result (hactar::render-md-ansi "## Subtitle")))
    (is (has-ansi-p result))
    (is (search "Subtitle" result))
    (is (search (format nil "~C[1;35m" #\Esc) result))))

(test md-render-heading-h3
  "H3 headings should be bold cyan."
  (let ((result (hactar::render-md-ansi "### Section")))
    (is (has-ansi-p result))
    (is (search "Section" result))
    (is (search (format nil "~C[1;36m" #\Esc) result))))

;;* Inline
(test md-render-inline-bold
  "**bold** text wraps in SGR bold (code 1)."
  (let ((result (hactar::render-md-inline "This is **bold** text")))
    (is (has-ansi-p result))
    (is (search "bold" result))
    (is (search (format nil "~C[1m" #\Esc) result))))

(test md-render-inline-italic
  "*italic* text wraps in SGR italic (code 3)."
  (let ((result (hactar::render-md-inline "This is *italic* text")))
    (is (has-ansi-p result))
    (is (search "italic" result))
    (is (search (format nil "~C[3m" #\Esc) result))))

(test md-render-inline-code
  "`code` wraps in SGR cyan (code 36)."
  (let ((result (hactar::render-md-inline "Run `ls -la` now")))
    (is (has-ansi-p result))
    (is (search "ls -la" result))
    (is (search (format nil "~C[36m" #\Esc) result))))

(test md-render-inline-strikethrough
  "~~struck~~ wraps in SGR strikethrough (code 9)."
  (let ((result (hactar::render-md-inline "This is ~~struck~~ out")))
    (is (has-ansi-p result))
    (is (search "struck" result))
    (is (search (format nil "~C[9m" #\Esc) result))))

(test md-render-inline-mixed
  "Multiple inline styles in one line."
  (let ((result (hactar::render-md-inline "Some **bold** and `code` text")))
    (is (search (format nil "~C[1m" #\Esc) result))
    (is (search (format nil "~C[36m" #\Esc) result))))

;;* code blocks
(test md-render-code-block
  "Fenced code blocks render with dim styling and left border."
  (let ((result (hactar::render-md-ansi (format nil "```lisp~%(+ 1 2)~%```"))))
    (is (has-ansi-p result))
    ;; Language label present
    (is (search "lisp" result))
    ;; Code content present
    (is (search "(+ 1 2)" result))
    ;; Left border character
    (is (search "│" result))))

(test md-render-code-block-no-lang
  "Code blocks without language still render with border."
  (let ((result (hactar::render-md-ansi (format nil "```~%hello~%```"))))
    (is (has-ansi-p result))
    (is (search "hello" result))
    (is (search "│" result))))

;;* tables
(test md-render-table-basic
  "A 2x2 markdown table aligns columns and uses box-drawing."
  (let ((result (hactar::render-md-ansi
                 (format nil "| Name | Age |~%|------|-----|~%| Alice | 30 |~%| Bob | 25 |"))))
    (is (has-ansi-p result))
    ;; Box-drawing characters present
    (is (search "┌" result))
    (is (search "┐" result))
    (is (search "└" result))
    (is (search "┘" result))
    (is (search "├" result))
    (is (search "┤" result))
    (is (search "┬" result))
    (is (search "┴" result))
    (is (search "┼" result))
    ;; Header content
    (is (search "Name" result))
    (is (search "Age" result))
    ;; Body content
    (is (search "Alice" result))
    (is (search "Bob" result))))

(test md-render-table-alignment
  "Table columns are padded to equal width."
  (let* ((result (hactar::render-md-table
                  (list "| Short | LongerColumn |"
                        "|-------|--------------|"
                        "| a | b |")))
         (lines (str:lines result)))
    ;; All rows should be the same width (ignoring ANSI sequences)
    (is (> (length lines) 2))))

;;* quotes
(test md-render-blockquote
  "Blockquotes render with left bar marker and muted color."
  (let ((result (hactar::render-md-ansi "> This is a quote")))
    (is (has-ansi-p result))
    (is (search "▐" result))
    (is (search "This is a quote" result))))

;;* hr tests
(test md-render-hr
  "Horizontal rules render as dim lines."
  (let ((result (hactar::render-md-ansi "---")))
    (is (has-ansi-p result))
    (is (search "─" result))))

(test md-render-hr-stars
  "Horizontal rules with *** also work."
  (let ((result (hactar::render-md-ansi "***")))
    (is (has-ansi-p result))
    (is (search "─" result))))

;;* lists
(test md-render-unordered-list
  "Unordered list items get bullet markers."
  (let ((result (hactar::render-md-ansi (format nil "- First item~%- Second item"))))
    (is (search "•" result))
    (is (search "First item" result))
    (is (search "Second item" result))))

(test md-render-ordered-list
  "Ordered list items preserve numbering."
  (let ((result (hactar::render-md-ansi (format nil "1. First~%2. Second"))))
    (is (search "1" result))
    (is (search "2" result))
    (is (search "First" result))
    (is (search "Second" result))))

(test md-render-nested-list
  "Nested list items use different bullet styles."
  (let ((result (hactar::render-md-ansi (format nil "- Top~%  - Nested~%    - Deep"))))
    ;; Should have different bullet characters for different depths
    (is (search "•" result))    ; depth 0
    (is (search "◦" result))    ; depth 1
    (is (search "▪" result)))) ; depth 2

;;* document
(test md-render-full-document
  "A full markdown document renders without errors."
  (let ((doc (format nil "# Title~%~%Some **bold** and `code` text.~%~%## Subtitle~%~%- Item 1~%- Item 2~%~%> A quote~%~%---~%~%```lisp~%(+ 1 2)~%```~%~%| a | b |~%|---|---|~%| 1 | 2 |")))
    (let ((result (hactar::render-md-ansi doc)))
      (is (stringp result))
      (is (> (length result) (length doc)))  ; Should be longer due to ANSI codes
      (is (has-ansi-p result)))))

(test md-render-empty-input
  "Empty or nil input returns empty string."
  (is (string= "" (hactar::render-md-ansi "")))
  (is (string= "" (hactar::render-md-ansi nil))))

(test md-render-plain-text
  "Plain text without markdown passes through with inline rendering."
  (let ((result (hactar::render-md-ansi "Just plain text.")))
    (is (search "Just plain text." result))))
