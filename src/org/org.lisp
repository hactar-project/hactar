(in-package #:hactar-org)

;;; Minimal org-mode: parse headlines and #+begin_src ... #+end_src blocks,
;;; and extract source blocks (used by Hactar's org-interface round-trip).

(defstruct (org-node (:conc-name org-node-))
  type title (level 0) (body "") (lang nil) header-args settings properties children
  parent tags todo priority)

(defun parse-org (source)
  "Parse SOURCE into an ordered list of ORG-NODE structs (headlines + src blocks)."
  (let ((lines (uiop:split-string source :separator '(#\Newline)))
        (nodes '()) (in-src nil) (buf '()) (lang nil))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ((and (>= (length trimmed) 12)
                (string-equal (subseq trimmed 0 12) "#+begin_src "))
           (setf in-src t buf '()
                 lang (string-trim '(#\Space) (subseq trimmed 11))))
          ((and in-src (string-equal trimmed "#+end_src"))
           (push (make-org-node :type :src :lang lang
                                :body (format nil "~{~A~^~%~}" (nreverse buf)))
                 nodes)
           (setf in-src nil buf '() lang nil))
          (in-src (push line buf))
          ((and (plusp (length trimmed)) (char= (char trimmed 0) #\*))
           (let ((level (loop for ch across trimmed while (char= ch #\*) count t)))
             (push (make-org-node :type :headline :level level
                                  :title (string-trim '(#\Space #\*) trimmed))
                   nodes))))))
    (nreverse nodes)))

(defun headlines (source)
  "Return the headline ORG-NODEs of SOURCE."
  (remove :headline (parse-org source) :key #'org-node-type :test-not #'eq))

(defun src-blocks (source &optional lang)
  "Return src-block ORG-NODEs of SOURCE, optionally filtered by LANG."
  (loop for n in (parse-org source)
        when (and (eq (org-node-type n) :src)
                  (or (null lang)
                      (and (org-node-lang n)
                           (string-equal (org-node-lang n) lang))))
          collect n))

(defun extract-src-blocks (source &optional (lang "lisp"))
  "Return the concatenated bodies of all #+begin_src LANG blocks in SOURCE."
  (with-output-to-string (out)
    (dolist (n (src-blocks source lang))
      (format out "~A~%" (org-node-body n)))))

(defun render-section (title body &key (level 1))
  "Render a single org section as a string."
  (format nil "~A ~A~%~%~A~%" (make-string level :initial-element #\*) title body))
