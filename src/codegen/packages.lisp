(defpackage #:hactar-codegen
  (:use #:cl)
  (:export #:*compiler-targets* #:target-definition #:make-target-definition
           #:deftarget #:defemit #:defjsmacro #:defcheck
           #:compile-form #:compile-toplevel #:compile-string
           #:compile-file-to-string #:check-form #:check-toplevel
           #:check-string #:check-file-source #:run-checks
           #:emit #:target #:convert #:convert-redwood
           #:read-all-forms #:detect-target-from-forms
           #:detect-target-from-extension
           #:check-result #:make-check-result #:check-result-level
           #:check-result-message #:check-result-form #:check-result-source-hint
           #:source-hint #:make-source-hint
           #:disable-check #:enable-check #:check-enabled-p
           #:format-check-results-xml #:format-check-results-json
           #:compile-output-tags #:compile-output-org
           #:compile-output-lisp #:compile-output-disk
           #:make-vfs #:vfs-select-file #:vfs-current-file
           #:vfs-write #:vfs-writeln #:vfs-prepend
           #:vfs-all-paths #:vfs-get-content
           #:vfs-flush-to-org #:vfs-flush-to-lisp
           #:vfs-flush-to-tags #:vfs-flush-to-disk
           #:vfs-set-metadata #:vfs-get-metadata #:emit-buffer->vfs
           #:redwood-collect-source-files #:redwood-build-user-message
           #:scaffold-create-file #:scaffold-modify-file #:scaffold-install-pkg
           ;; DSL form heads used across targets
           #:in-file #:emit-atom #:emit-funcall #:js-file-p))
