(in-package #:hactar-user)

(hactar-core:load-pack-file
 (merge-pathnames "src/packs/modes-ruby/content.lisp"
                  (asdf:system-source-directory "hactar-modes-ruby")))
