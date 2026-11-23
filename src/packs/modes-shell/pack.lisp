(in-package #:hactar-user)

(hactar-core:load-pack-file
 (merge-pathnames "src/packs/modes-shell/content.lisp"
                  (asdf:system-source-directory "hactar-modes-shell")))
