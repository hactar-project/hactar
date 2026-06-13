;; A glow inspired markdown tui renderer
(in-package :hactar)

;;* Helpers
(defun md-ansi (text &rest codes)
  "Wrap TEXT in ANSI SGR escape sequences specified by CODES (strings).
   Example: (md-ansi \"hello\" \"1\" \"35\") → bold magenta."
  (if codes
      (format nil "~C[~{~A~^;~}m~A~C[0m" #\Esc codes text #\Esc)
      text))

;;* inlines
(defun render-md-inline (line)
  "Apply inline markdown styling to LINE.
   Handles: **bold**, *italic*, `code`, ~~strikethrough~~."
  (let ((s line))
    ;; Bold: **text**
    (setf s (cl-ppcre:regex-replace-all
             "\\*\\*([^*]+)\\*\\*" s
             (lambda (match r0)
               (declare (ignore match))
               (md-ansi r0 "1"))
             :simple-calls t))
    ;; Bold alternate: __text__
    (setf s (cl-ppcre:regex-replace-all
             "__([^_]+)__" s
             (lambda (match r0)
               (declare (ignore match))
               (md-ansi r0 "1"))
             :simple-calls t))
    ;; Italic: *text* (but not **text**)
    (setf s (cl-ppcre:regex-replace-all
             "(?<!\\*)\\*([^*]+)\\*(?!\\*)" s
             (lambda (match r0)
               (declare (ignore match))
               (md-ansi r0 "3"))
             :simple-calls t))
    ;; Italic alternate: _text_ (but not __text__)
    (setf s (cl-ppcre:regex-replace-all
             "(?<!_)_([^_]+)_(?!_)" s
             (lambda (match r0)
               (declare (ignore match))
               (md-ansi r0 "3"))
             :simple-calls t))
    ;; Strikethrough: ~~text~~
    (setf s (cl-ppcre:regex-replace-all
             "~~([^~]+)~~" s
             (lambda (match r0)
               (declare (ignore match))
               (md-ansi r0 "9"))
             :simple-calls t))
    ;; Inline code: `text` — render in cyan
    (setf s (cl-ppcre:regex-replace-all
             "`([^`]+)`" s
             (lambda (match r0)
               (declare (ignore match))
               (md-ansi r0 "36"))
             :simple-calls t))
    s))

;;* headlines
(defun render-md-heading (line)
  "Render a markdown heading line. Returns the styled string.
   H1: bold magenta + underline
   H2: bold magenta
   H3: bold cyan
   H4: bold blue
   H5: bold yellow
   H6: bold dim"
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^(#{1,6})\\s+(.*)" line)
    (if match
        (let* ((level (length (aref groups 0)))
               (text (aref groups 1))
               (styled-text
                 (case level
                   (1 (md-ansi text "1" "4" "35"))   ; bold underline magenta
                   (2 (md-ansi text "1" "35"))        ; bold magenta
                   (3 (md-ansi text "1" "36"))        ; bold cyan
                   (4 (md-ansi text "1" "34"))        ; bold blue
                   (5 (md-ansi text "1" "33"))        ; bold yellow
                   (6 (md-ansi text "1" "2"))         ; bold dim
                   (t (md-ansi text "1")))))
          styled-text)
        line)))
;;* tables
(defun parse-md-table-row (line)
  "Parse a markdown table row into a list of trimmed cell strings.
   Returns NIL for separator rows (e.g. |---|---|)."
  (when (cl-ppcre:scan "^[-:| ]+$" line)
    (return-from parse-md-table-row nil))
  (let ((cells (mapcar (lambda (c) (string-trim '(#\Space #\Tab) c))
                       (remove "" (str:split "|" line) :test #'string=))))
    cells))

(defun render-md-table (lines)
  "Render a contiguous markdown table (list of raw line strings) into
   a Unicode box-drawn, column-aligned ANSI string.
   ┌──────┬──────┐
   │ hdr  │ hdr  │
   ├──────┼──────┤
   │ cell │ cell │
   └──────┴──────┘"
  (let* ((rows (loop for l in lines
                     for cells = (parse-md-table-row l)
                     when cells collect cells))
         (ncol (if rows (reduce #'max rows :key #'length :initial-value 0) 0)))
    (when (zerop ncol)
      (return-from render-md-table ""))
    ;; Compute column widths
    (let ((widths (make-array ncol :initial-element 0)))
      (dolist (r rows)
        (loop for c in r for i from 0
              do (setf (aref widths i) (max (aref widths i) (length c)))))
      ;; Ensure minimum width of 3 per column
      (loop for i from 0 below ncol
            do (setf (aref widths i) (max 3 (aref widths i))))
      ;; Build the table
      (with-output-to-string (s)
        (labels ((hline (left mid right)
                   (write-string (md-ansi (format nil "~A~{~A~^~A~}~A"
                                                  left
                                                  (loop for i from 0 below ncol
                                                        collect (make-string (+ 2 (aref widths i))
                                                                             :initial-element #\─))
                                                  mid right) "2") s)
                   (terpri s))
                 (data-row (cells bold-p)
                   (write-string (md-ansi "│" "2") s)
                   (loop for i from 0 below ncol
                         for cell = (or (nth i cells) "")
                         for padded = (format nil " ~vA " (aref widths i) cell)
                         do (if bold-p
                                (write-string (md-ansi padded "1") s)
                                (write-string padded s))
                            (write-string (md-ansi "│" "2") s))
                   (terpri s)))
          ;; Top border
          (hline "┌" "┬" "┐")
          ;; Header row (first row, bold)
          (when rows
            (data-row (first rows) t))
          ;; Header/body separator
          (when (> (length rows) 1)
            (hline "├" "┼" "┤"))
          ;; Body rows
          (dolist (row (rest rows))
            (data-row row nil))
          ;; Bottom border
          (hline "└" "┴" "┘"))))))
;;* Blocks
(defun render-md-code-block (language code-lines)
  "Render a fenced code block with a language label and dimmed content.
   Returns a string with ANSI styling."
  (with-output-to-string (s)
    ;; Language label
    (when (and language (> (length language) 0))
      (write-string (md-ansi (format nil " ~A " language) "1" "7" "36") s)  ; bold inverse cyan
      (terpri s))
    ;; Code content with left border
    (dolist (line code-lines)
      (write-string (md-ansi "│ " "2" "36") s)  ; dim cyan border
      (write-string (md-ansi line "2") s)         ; dim content
      (terpri s))))

(defun render-md-blockquote (line)
  "Render a blockquote line (starting with >) with a left bar and muted color."
  (let ((text (cl-ppcre:regex-replace "^>\\s?" line "")))
    (format nil "~A ~A"
            (md-ansi "▐" "35")           ; magenta left bar
            (md-ansi (render-md-inline text) "3" "2"))))   ; italic + dim

(defun render-md-hr ()
  "Render a horizontal rule as a dim line."
  (md-ansi (make-string 40 :initial-element #\─) "2"))

(defun render-md-list-item (line)
  "Render a markdown list item with a styled bullet or number.
   Supports nested indentation with different bullet styles."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^(\\s*)([-*+])\\s+(.*)" line)
    (if match
        ;; Unordered list
        (let* ((indent (aref groups 0))
               (content (aref groups 2))
               (depth (floor (length indent) 2))
               (bullet (case (mod depth 3)
                         (0 "•")
                         (1 "◦")
                         (2 "▪"))))
          (format nil "~A~A ~A" indent (md-ansi bullet "35") (render-md-inline content)))
        ;; Try ordered list
        (multiple-value-bind (match2 groups2)
            (cl-ppcre:scan-to-strings "^(\\s*)(\\d+)[.)]\\s+(.*)" line)
          (if match2
              (let ((indent (aref groups2 0))
                    (num (aref groups2 1))
                    (content (aref groups2 2)))
                (format nil "~A~A. ~A" indent (md-ansi num "1" "35") (render-md-inline content)))
              line)))))

(defun render-md-ansi (text)
  "Render markdown TEXT to an ANSI-styled string for terminal display.
   Inspired by Glow: headings are bold+colored, code blocks get left borders,
   tables are box-drawn, blockquotes get bar markers, etc."
  (when (or (null text) (string= text ""))
    (return-from render-md-ansi (or text "")))
  (let ((lines (str:lines text))
        (out (make-string-output-stream))
        (in-code nil)
        (code-lang nil)
        (code-lines '())
        (table-buf '())
        (prev-blank nil))
    (labels ((flush-table ()
               (when table-buf
                 (write-string (render-md-table (nreverse table-buf)) out)
                 (setf table-buf '())))
             (flush-code ()
               (when code-lines
                 (write-string (render-md-code-block code-lang (nreverse code-lines)) out)
                 (setf code-lines '())
                 (setf code-lang nil))))
      (dolist (line lines)
        (cond
          ;; Code fence toggle
          ((cl-ppcre:scan "^\\s*```" line)
           (if in-code
               ;; Closing fence
               (progn
                 (flush-code)
                 (setf in-code nil))
               ;; Opening fence
               (progn
                 (flush-table)
                 (setf in-code t)
                 (setf code-lang
                       (let ((lang (cl-ppcre:regex-replace "^\\s*```\\s*" line "")))
                         (if (string= lang "") nil lang)))
                 (setf code-lines '()))))

          ;; Inside code block — accumulate
          (in-code
           (push line code-lines))

          ;; Markdown table row
          ((cl-ppcre:scan "^\\s*\\|.*\\|\\s*$" line)
           (push line table-buf))

          ;; Heading
          ((cl-ppcre:scan "^#{1,6}\\s" line)
           (flush-table)
           (write-line (render-md-heading line) out)
           (setf prev-blank nil))

          ;; Horizontal rule
          ((cl-ppcre:scan "^\\s*([-*_]){3,}\\s*$" line)
           (flush-table)
           (write-line (render-md-hr) out)
           (setf prev-blank nil))

          ;; Blockquote
          ((cl-ppcre:scan "^\\s*>" line)
           (flush-table)
           (write-line (render-md-blockquote line) out)
           (setf prev-blank nil))

          ;; Unordered list item
          ((cl-ppcre:scan "^\\s*[-*+]\\s+" line)
           (flush-table)
           (write-line (render-md-list-item line) out)
           (setf prev-blank nil))

          ;; Ordered list item
          ((cl-ppcre:scan "^\\s*\\d+[.)]\\s+" line)
           (flush-table)
           (write-line (render-md-list-item line) out)
           (setf prev-blank nil))

          ;; Blank line
          ((string= (string-trim '(#\Space #\Tab) line) "")
           (flush-table)
           (unless prev-blank
             (terpri out))
           (setf prev-blank t))

          ;; Regular paragraph text
          (t
           (flush-table)
           (write-line (render-md-inline line) out)
           (setf prev-blank nil))))
      ;; Flush any remaining state
      (flush-table)
      (flush-code))
    ;; Remove trailing newline for clean joining
    (string-right-trim '(#\Newline #\Return) (get-output-stream-string out))))
