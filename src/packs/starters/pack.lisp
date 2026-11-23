(in-package #:hactar-user)

(hactar-core:load-pack-file
 (merge-pathnames "src/packs/starters/content.lisp"
                  (asdf:system-source-directory "hactar-starters")))
