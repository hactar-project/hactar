(in-package #:hactar-user)

(hactar-core:load-pack-file
 (merge-pathnames "src/packs/modes-web/content.lisp"
                  (asdf:system-source-directory "hactar-modes-web")))
