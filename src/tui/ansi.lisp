(in-package #:hactar-tui)

;;; Data-driven ANSI color helpers and Markdown -> ANSI rendering.

(defun hex->rgb (hex)
  "Parse \"#RRGGBB\" into (values r g b), or NIL."
  (when (and (stringp hex) (= (length hex) 7) (char= (char hex 0) #\#))
    (let ((r (ignore-errors (parse-integer hex :start 1 :end 3 :radix 16)))
          (g (ignore-errors (parse-integer hex :start 3 :end 5 :radix 16)))
          (b (ignore-errors (parse-integer hex :start 5 :end 7 :radix 16))))
      (when (and r g b)
        (values r g b)))))

(defun ansi-fg (hex)
  (multiple-value-bind (r g b) (hex->rgb hex)
    (if r (format nil "~C[38;2;~D;~D;~Dm" #\Escape r g b) "")))

(defun ansi-bg (hex)
  (multiple-value-bind (r g b) (hex->rgb hex)
    (if r (format nil "~C[48;2;~D;~D;~Dm" #\Escape r g b) "")))

(defun ansi-reset ()
  (format nil "~C[0m" #\Escape))

(defun %ansi-bold (text &optional hex)
  (format nil "~C[1m~A~A~A" #\Escape (if hex (ansi-fg hex) "") text (ansi-reset)))

(defun %ansi-dim (text &optional hex)
  (format nil "~C[2m~A~A~A" #\Escape (if hex (ansi-fg hex) "") text (ansi-reset)))

(defun render-markdown-ansi (source &key (theme *theme*))
  "Render Markdown SOURCE using ANSI escapes parameterized by THEME."
  (let ((header-color (theme-color theme :header))
        (muted-color (theme-color theme :muted))
        (accent-color (theme-color theme :border-accent)))
    (with-output-to-string (out)
      (dolist (b (hactar-md:parse-markdown source))
        (ecase (hactar-md:md-block-type b)
          (:heading
           (format out "~A~%~%" (%ansi-bold (hactar-md:md-block-text b) header-color)))
          (:code
           (format out "~A~%"
                   (if accent-color
                       (format nil "~A    ~A~A"
                               (ansi-fg accent-color)
                               (hactar-md:md-block-text b)
                               (ansi-reset))
                       (format nil "    ~A" (hactar-md:md-block-text b)))))
          (:blockquote
           (format out "~A~%"
                   (%ansi-dim (format nil "| ~A" (hactar-md:md-block-text b))
                              muted-color)))
          (:list-item
           (format out "~A~%" (hactar-md:md-block-text b)))
          (:thematic-break
           (format out "~A~%" (%ansi-dim (make-string 40 :initial-element #\-) muted-color)))
          (:paragraph
           (format out "~A~%~%" (hactar-md:md-block-text b))))))))
