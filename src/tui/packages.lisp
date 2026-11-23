(defpackage #:hactar-tui
  (:use #:cl)
  (:shadow #:*error-output*)
  (:export #:theme #:make-theme #:make-tui-theme #:theme-name
           #:theme-prompt #:theme-error-prefix #:theme-palette #:theme-color
           #:*theme* #:*themes* #:register-theme #:find-theme #:list-themes
           #:use-theme #:load-theme-file #:load-themes-dir
           #:ansi-fg #:ansi-bg #:ansi-reset #:hex->rgb #:render-markdown-ansi
           #:*input* #:*output* #:*error-output* #:*use-readline*
           #:with-tui #:read-input #:render #:pick #:pick-interactive
           #:spinner #:repl-loop
           #:hydra #:define-hydra #:find-hydra #:run-hydra))
