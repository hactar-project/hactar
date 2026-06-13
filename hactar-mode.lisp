;; hactar hactar-mode. we put hactar in your hactar!
;; mostly just a place to define the docs
(in-package :hactar)

(defdoc "Latest Hactar Docs" :source "hactar:docsets/hactar.0.1.org" :version "latest")

(define-command "hactar.dev.copyprompts" (args)
		"Copy prompts from the repository prompts/ directory to the user's config prompts/ directory."
		(declare (ignore args))
		(let* ((repo-dir (%to-pathname *hactar-repo-dir*))
		       (prompts-src (uiop:subpathname (uiop:ensure-directory-pathname repo-dir) "prompts/"))
		       (config-dir (%to-pathname *hactar-config-path*))
		       (prompts-dst (uiop:subpathname (uiop:ensure-directory-pathname config-dir) "prompts/")))
		  (ensure-directories-exist prompts-dst)
		  (if (uiop:directory-exists-p prompts-src)
		      (handler-case
			  (progn
			    (dolist (file (uiop:directory-files prompts-src))
			      (let ((dest-file (make-pathname :name (pathname-name file)
							      :type (pathname-type file)
							      :defaults prompts-dst)))
				(uiop:copy-file file dest-file)))
			    (format t "Successfully copied all prompts to ~A~%" (uiop:native-namestring prompts-dst)))
			(error (e)
			       (format t "Error copying prompts: ~A~%" e)))
		    (format t "Error: Source prompts directory does not exist at ~A~%" (uiop:native-namestring prompts-src)))))
