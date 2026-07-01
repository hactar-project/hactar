(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test sandbox-denies-unlisted-binary
  (hactar-rebuild:reset-registries)
  (eval-in-user "(defsandbox project :allow (\"git\" \"npx\"))")
  (signals error (hactar-rebuild:sandbox-check "project" "rm -rf /")))

(test sandbox-allows-listed-binary
  (hactar-rebuild:reset-registries)
  (eval-in-user "(defsandbox project :allow (\"git\" \"npx\"))")
  (finishes (hactar-rebuild:sandbox-check "project" "git status")))

(test sandbox-deny-list-blocks
  (hactar-rebuild:reset-registries)
  (eval-in-user "(defsandbox project :deny (\"rm -rf\"))")
  (signals error (hactar-rebuild:sandbox-check "project" "rm -rf /tmp/x")))
