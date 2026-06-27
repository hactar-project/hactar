(in-package :hactar)

(defstruct fusion
  name
  options
  stages)

(defvar *fusion-registry* (make-hash-table :test 'eq)
  "Registry of defined fusions.")

(defmacro deffusion (name (&rest options) &body stages)
  "Define a fusion pipeline that uses Fern + diffusion model to extract code intelligence."
  (let ((trigger (getf options :trigger))
        (mode (getf options :mode))
        (watch (getf options :watch))
        (debounce (getf options :debounce 500))
        (output-form (getf options :output-form)))
    (declare (ignore trigger mode watch debounce output-form))
    `(progn
       (defun ,(intern (format nil "RUN-FUSION-~A" name) :hactar) (state)
         (let* ((project-root (project-cli-root state))
                (hactar-file (merge-pathnames ".hactar.lisp" project-root))
                ;; Extract variables bound by :as keywords in stages
                ,@(let ((bindings '()))
                    (dolist (stage stages)
                      (let ((stage-op (car stage))
                            (stage-args (cdr stage)))
                        (case stage-op
                          (:fern-extract
                           (let ((as-var (getf stage-args :as)))
                             (when as-var (push `(,as-var nil) bindings))))
                          (:diff-detect
                           (let ((as-vars (getf stage-args :as)))
                             (if (listp as-vars)
                                 (dolist (v as-vars) (push `(,v nil) bindings))
                                 (when as-vars (push `(,as-vars nil) bindings)))))
                          (:diffuse
                           (let ((as-var (getf stage-args :as)))
                             (when as-var (push `(,as-var nil) bindings))))
                          (:validate
                           (let ((as-var (getf stage-args :as)))
                             (when as-var (push `(,as-var nil) bindings)))))))
                    (nreverse bindings)))
           (declare (ignorable project-root hactar-file))
           ;; Execute each stage sequentially
           ,@(mapcar (lambda (stage)
                       (let ((stage-op (car stage))
                             (stage-args (cdr stage)))
                         (case stage-op
                           (:fern-extract
                            (let ((lang (getf stage-args :language))
                                  (files-expr (getf stage-args :files))
                                  (query-expr (getf stage-args :query))
                                  (as-var (getf stage-args :as)))
                              `(let ((resolved-files ,files-expr))
                                 (setf ,as-var '())
                                 (dolist (f resolved-files)
                                   (when (probe-file f)
                                     (let* ((source (uiop:read-file-string f))
                                            (tree (fern-parse ,lang source))
                                            (matches (fern-query-nodes tree ,query-expr)))
                                       (if matches
                                           (setf ,as-var (append ,as-var matches))
                                           (setf ,as-var
                                                 (append ,as-var
                                                         (list (make-fern-node :type :fallback
                                                                               :text "fallback")))))))))))
                           (:diff-detect
                            (let ((current-expr (getf stage-args :current))
                                  (prev-expr (getf stage-args :previous))
                                  (as-vars (getf stage-args :as)))
                              (if (listp as-vars)
                                  (let ((new-var (first as-vars))
                                        (changed-var (second as-vars)))
                                    `(let ((curr ,current-expr)
                                           (prev ,prev-expr))
                                       (declare (ignore prev))
                                       (setf ,new-var curr)
                                       (setf ,changed-var nil)))
                                  `(setf ,as-vars ,current-expr))))
                           (:diffuse
                            (let ((when-expr (getf stage-args :when))
                                  (prompt-name (getf stage-args :prompt))
                                  (args-expr (getf stage-args :args))
                                  (as-var (getf stage-args :as)))
                              `(when ,when-expr
                                 (let* ((prompt-tmpl (get-prompt ,prompt-name))
                                        (formatted (apply #'format nil prompt-tmpl ,args-expr)))
                                   (setf ,as-var (diffusion-complete formatted))))))
                           (:validate
                            (let ((forms-expr (getf stage-args :forms))
                                  (schema (getf stage-args :schema))
                                  (as-var (getf stage-args :as)))
                              `(let ((raw-text ,forms-expr))
                                 (when raw-text
                                   (let ((parsed (parse-diffusion-output raw-text))
                                         (sch ,schema))
                                     (setf ,as-var
                                           (remove-if-not
                                            (lambda (f)
                                              (if (listp sch)
                                                  (member (car f) sch)
                                                  (eq (car f) sch)))
                                            parsed)))))))
                           (:emit
                            (let ((forms-expr (getf stage-args :forms))
                                  (merge-expr (getf stage-args :merge)))
                              `(let ((forms-to-emit ,forms-expr))
                                 (when forms-to-emit
                                   (write-hactar-lisp forms-to-emit hactar-file :merge-p ,merge-expr))))))))
                     stages)
           state))
       ;; Register the metadata
       (setf (gethash ',name *fusion-registry*)
             (make-fusion :name ',name
                          :options ',options
                          :stages ',stages))
       ',name)))

(defun register-fusion (fusion)
  "Register a fusion in the global registry."
  (setf (gethash (fusion-name fusion) *fusion-registry*) fusion))

(defun activate-fusions-for-mode (mode state)
  "When a mode activates, find and start all fusions triggered by that mode."
  (loop for fusion being the hash-values of *fusion-registry*
        when (let ((fmode (getf (fusion-options fusion) :mode)))
               (and fmode mode (string-equal (string fmode) (string mode))))
        do (start-fusion fusion state)))

(defun start-fusion (fusion state)
  "Start a fusion: run initial scan."
  (run-fusion-pipeline fusion state))

(defun run-fusion-pipeline (fusion state)
  "Execute all stages of a fusion pipeline."
  (let ((fn (intern (format nil "RUN-FUSION-~A" (fusion-name fusion)) :hactar)))
    (if (fboundp fn)
        (funcall fn state)
        (error "Fusion function not found: ~A" fn))))

(defun run-active-fusions (state)
  "Run all fusions associated with active modes of the project."
  (dolist (mode (project-cli-stack state))
    (loop for fusion being the hash-values of *fusion-registry*
          when (let ((fmode (getf (fusion-options fusion) :mode)))
                 (and fmode mode (string-equal (string fmode) (string mode))))
          do (run-fusion-pipeline fusion state))))
