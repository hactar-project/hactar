(in-package #:hactar-rebuild)

(defstruct result
  "Protocol-independent result returned by rebuild routes, tools, and agents."
  (status :ok :type keyword)
  (message "" :type string)
  (data nil)
  (artifacts nil :type list))

(defun ok (&optional (message "") &key data artifacts)
  (make-result :status :ok :message (or message "") :data data :artifacts artifacts))

(defun err (message &key data)
  (make-result :status :error :message (princ-to-string message) :data data))

(defun result-ok-p (result)
  (eq (result-status result) :ok))

(defun render-result (result &optional (format :text))
  "Render RESULT for FORMAT. Only :TEXT is supported."
  (ecase format
    (:text
     (with-output-to-string (s)
       (format s "~A~%" (result-message result))
       (when (result-artifacts result)
         (format s "~%Artifacts:~%")
         (dolist (artifact (result-artifacts result))
           (format s "  - ~A~%" artifact)))))))
