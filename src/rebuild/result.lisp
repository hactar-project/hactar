(in-package #:hactar-rebuild)

(defstruct result
  "Protocol-independent result returned by rebuild routes, tools, and agents."
  (status :ok :type keyword)
  (message "" :type string)
  (data nil)
  (artifacts nil :type list))

(defun render-result (result &optional (format :text))
  "Render RESULT for FORMAT. Phase 1 only supports :TEXT."
  (ecase format
    (:text
     (with-output-to-string (s)
       (format s "~A~%" (result-message result))
       (when (result-artifacts result)
         (format s "~%Artifacts:~%")
         (dolist (artifact (result-artifacts result))
           (format s "  - ~A~%" artifact)))))))
