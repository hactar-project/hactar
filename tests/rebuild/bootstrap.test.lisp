(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test bootstrap-generates-and-wires
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"react-router\":\"7\"}}")))
    (hactar-rebuild:bootstrap root)
    (is (probe-file (hactar-rebuild:hactar-file-path root)))
    (is (probe-file (merge-pathnames ".envrc" root)))
    (is (probe-file (merge-pathnames "hactar/build/hactar" root)))
    (is (search "PATH_add"
                (uiop:read-file-string (merge-pathnames ".envrc" root))))
    (hactar-rebuild:load-hactar-file (hactar-rebuild:hactar-file-path root))
    (is (gethash "dev" hactar-rebuild::*agents*))))

(test dispatch-runs-generated-agent
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"react-router\":\"7\"}}")))
    (hactar-rebuild:bootstrap root)
    (hactar-rebuild:load-hactar-file (hactar-rebuild:hactar-file-path root))
    (let ((r (hactar-rebuild:dispatch '("dev" "add a settings page"))))
      (is (eq :ok (hactar-rebuild:result-status r))))))

(test dispatch-runs-generated-action
  (with-temp-project (root :files '(("package.json" .
                                     "{\"dependencies\":{\"react-router\":\"7\"}}")))
    (hactar-rebuild:bootstrap root)
    (hactar-rebuild:load-hactar-file (hactar-rebuild:hactar-file-path root))
    (let ((r (hactar-rebuild:dispatch '("route.create" "--name=Dashboard"))))
      (is (eq :ok (hactar-rebuild:result-status r))))))

(test args-to-plist
  (let ((p (hactar-rebuild:args->plist '("--name=login" "--path" "/x"))))
    (is (string= "login" (getf p :name)))
    (is (string= "/x" (getf p :path)))))
