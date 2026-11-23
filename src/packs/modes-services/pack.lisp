(in-package #:hactar-user)

(hactar-core:load-pack-file
 (merge-pathnames "src/packs/modes-services/content.lisp"
                  (asdf:system-source-directory "hactar-modes-services")))
