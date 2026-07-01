(in-package :hactar-tests)

(def-suite rebuild-foundation-tests
  :description "Greenfield Hactar rebuild foundation tests"
  :in hactar-tests)

(in-suite rebuild-foundation-tests)

(test rebuild-validator-rejects-out-of-surface-forms
  (multiple-value-bind (ok errors)
      (hactar-rebuild:validate-forms
       '((hactar-rebuild:defproject :stack ("common-lisp"))
         (hactar-rebuild:generated :updated-at "<now>"
           (defun sneaky () 42))))
    (is-false ok)
    (is-true (some (lambda (error)
                     (search "surface" error))
                   errors))))

(test rebuild-route-normalizes-path
  (hactar-rebuild:reset-registries)
  (eval '(hactar-rebuild:defroute routes.create
           :pattern "/routes.create --name={name} --path={path}"
           :coerce ((:path hactar-rebuild:ensure-leading-slash))
           :result (:cmd "routes.route" :name name :path path)))
  (is (equal '(:cmd "routes.route" :name "login" :path "/login")
             (hactar-rebuild:parse-route
              "/routes.create --name=login --path=login"))))

(test rebuild-load-hactar-file-resets-registries
  (let* ((dir (make-temp-dir))
         (file (merge-pathnames ".hactar.lisp" (uiop:ensure-directory-pathname dir))))
    (with-open-file (stream file :direction :output :if-exists :supersede
                                 :if-does-not-exist :create :external-format :utf-8)
      (format stream "(in-package :hactar-user)~%~%")
      (format stream "(generated :updated-at \"<now>\"~%")
      (format stream "  (deftool tsc () \"Type-check.\" (shell-cmd \"npx tsc --noEmit\")))~%"))
    (hactar-rebuild:load-hactar-file file)
    (is (= 1 (hactar-rebuild:registry-count :tools)))
    (hactar-rebuild:load-hactar-file file)
    (is (= 1 (hactar-rebuild:registry-count :tools)))))
