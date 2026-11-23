(defpackage #:hactar-vfs
  (:use #:cl)
  (:export #:vfs #:make-vfs #:vfs-root #:vfs-write #:vfs-read
           #:vfs-paths #:vfs-plan #:vfs-diff #:vfs-commit!
           #:clear-vfs #:with-staged-fs))
