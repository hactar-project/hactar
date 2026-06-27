;;* Redwood SDK Mode
(in-package :hactar)

(defmode redwood
  "Cloudflare Workers-based Redwood SDK full-stack React framework mode."
  :when (member "redwood" *stack* :test #'string-equal)
  :rules "When working with Redwood SDK (rwsdk):
- Use src/worker.tsx as the primary server entry point.
- Define client-side entry in src/client.tsx.
- Define application structure via define-app() in worker.tsx.
- Put UI components under src/app/components/ or src/app/pages/.
- Access wrangler environment variables and bindings through env objects.
- Leverage vite and cloudflare plugins in configurations."
  :commands
  ((defmode-command redwood "redwood.info" (args)
     "Display Redwood SDK project structure summary."
     (declare (ignore args))
     (let ((pcli (get-project-cli *pcli-current-project*)))
       (if pcli
           (progn
             (format t "Redwood SDK Project: ~A~%" (project-cli-name pcli))
             (format t "Routes count: ~A~%" (length (project-routes (project-cli-name pcli))))
             (format t "Models count: ~A~%" (length (project-models (project-cli-name pcli)))))
           (format t "No active project CLI found.~%"))))))
