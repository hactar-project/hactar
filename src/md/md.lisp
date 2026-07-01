(in-package #:hactar-md)

;;; Minimal, dependency-light Markdown: parse into a flat list of blocks,
;;; extract fenced code + links, and render a terminal-friendly string.

(defstruct (md-block (:conc-name md-block-)) type text (lang nil) (level 0))

(defun parse-markdown (source)
  "Parse SOURCE into an ordered list of MD-BLOCK structs.
Recognizes headings, fenced code (``` or ~~~), and paragraphs."
  (let ((lines (uiop:split-string source :separator '(#\Newline)))
        (blocks '()) (fence nil) (buf '()) (lang nil))
    (labels ((flush-para ()
               (when buf
                 (push (make-md-block :type :paragraph
                                      :text (format nil "~{~A~^~%~}" (nreverse buf)))
                       blocks)
                 (setf buf '()))))
      (dolist (line lines)
        (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
          (cond
            ;; fence toggle
            ((and (>= (length trimmed) 3)
                  (or (string= (subseq trimmed 0 3) "```")
                      (string= (subseq trimmed 0 3) "~~~")))
             (if fence
                 (progn
                   (push (make-md-block :type :code
                                        :text (format nil "~{~A~^~%~}" (nreverse buf))
                                        :lang lang) blocks)
                   (setf fence nil buf '() lang nil))
                 (progn (flush-para)
                        (setf fence t
                              lang (let ((l (string-trim '(#\Space #\Tab)
                                                         (subseq trimmed 3))))
                                     (and (plusp (length l)) l))))))
            (fence (push line buf))
            ;; heading
            ((and (plusp (length trimmed)) (char= (char trimmed 0) #\#))
             (flush-para)
             (let ((level (loop for ch across trimmed while (char= ch #\#) count t)))
               (push (make-md-block :type :heading
                                    :level level
                                    :text (string-trim '(#\Space #\#) trimmed))
                     blocks)))
            ((string= trimmed "") (flush-para))
            (t (push line buf)))))
      (flush-para)
      (nreverse blocks))))

(defun code-blocks (source &optional lang)
  "Return the text of fenced code blocks in SOURCE, optionally filtered by LANG."
  (loop for b in (parse-markdown source)
        when (and (eq (md-block-type b) :code)
                  (or (null lang)
                      (and (md-block-lang b)
                           (string-equal (md-block-lang b) lang))))
          collect (md-block-text b)))

(defun extract-links (source)
  "Return a list of (text . url) for [text](url) links in SOURCE."
  (let ((links '()))
    (cl-ppcre:do-register-groups (text url)
        ("\\[([^\\]]*)\\]\\(([^)]+)\\)" source)
      (push (cons text url) links))
    (nreverse links)))

(defun render-terminal (source)
  "Render SOURCE to a plain, terminal-friendly string."
  (with-output-to-string (out)
    (dolist (b (parse-markdown source))
      (ecase (md-block-type b)
        (:heading (format out "~A~%~A~%~%"
                          (string-upcase (md-block-text b))
                          (make-string (length (md-block-text b))
                                       :initial-element #\=)))
        (:code (format out "    ~A~%~%"
                       (cl-ppcre:regex-replace-all
                        (string #\Newline) (md-block-text b)
                        (format nil "~%    "))))
        (:paragraph (format out "~A~%~%" (md-block-text b)))))))
