(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test route-normalizes-path
  (hactar-rebuild:reset-registries)
  (eval-in-user "(defroute routes.create :pattern \"/routes.create --name={name} --path={path}\" :coerce ((:path ensure-leading-slash)) :result (:cmd \"routes.route\" :name name :path path))")
  (is (equal '(:cmd "routes.route" :name "login" :path "/login")
             (hactar-rebuild:parse-route
              "/routes.create --name=login --path=login"))))

(test route-no-match-returns-nil
  (hactar-rebuild:reset-registries)
  (is (null (hactar-rebuild:parse-route "/nothing.here"))))

(test ensure-leading-slash
  (is (string= "/x" (hactar-rebuild:ensure-leading-slash "x")))
  (is (string= "/x" (hactar-rebuild:ensure-leading-slash "/x"))))
