(uiop:define-package :hactar
  (:use #:cl #:cl-ppcre #:sqlite #:cl-toml #:fuzzy-match #:clingon #:cl-fad #:usocket #:imago #:cl-base64 #:cffi #:cl-charms)
  (:shadowing-import-from #:json
    #:encode-json-to-string)
  (:export #:main
           #:resize-and-encode-image
           ;; Code as Value API
           #:code-value
           #:hactar-init
           #:code/select
           #:code/from-string
           #:code/from-context
           #:code/describe
           #:code/source
           #:code/intent
           #:code/ask
           #:code/history
           #:code/parent
           #:code/original
           #:code/show
           #:code/summary
           #:get-xdg-config-dir
           #:find-model-by-name
           #:model-config-provider
           #:model-config-model-name
           #:hyperfractal-browse
           #:ruhe-init
           #:fetch-url-content
           #:*repo-root*
           #:*sqlite-vec-path*
	      #:get-all-md-src-blocks
   #:render-markdown-to-html
   #:insert-md-sibling
   #:insert-md-child))

(in-package :hactar)
(defpackage :nhooks
  (:use :common-lisp)
  (:import-from :serapeum
                #:*hook*
                #:add-hook
                #:remove-hook
                #:run-hooks
                #:run-hook
                #:run-hook-until-failure
                #:run-hook-until-success
                #:with-hook-restart)
  (:import-from #:alexandria
                #:required-argument)
  (:export
   #:*hook*
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:run-hook
   #:run-hook-with-args-until-failure
   #:run-hook-with-args-until-success
   #:with-disable-handler-restart
   #:default-combine-hook
   #:combine-hook-until-failure
   #:combine-hook-until-success
   #:combine-composed-hook
   #:find-handler
   #:disable-hook
   #:enable-hook
   #:define-hook
   #:find-hook
   #:define-hook-type
   ;; Handler class:
   #:handler
   #:name
   #:fn
   #:description
   #:place
   #:value
   ;; Hook class:
   #:hook
   #:handler-type
   #:handlers-alist
   #:handlers
   #:disabled-handlers
   #:combination
   ;; Pre-generated types:
   #:hook-void
   #:hook-string->string
   #:hook-number->number
   #:hook-any
   ;; Short hook helpers
   #:on
   #:once-on
   #:wait-on)
  (:documentation "A hook is an instance of the `nhooks:hook' class.
You can define new hook types with the `nhooks:define-hook-type' helper.
Examples:

  (nhooks:define-hook-type string->string (function (string) string))

defines the `hook-string->string' hook class.
This is equivalent to using `defclass' and overriding the `nhooks:handler-type'
slot.

You can then instantiate it:

    (defvar test-hook (make-instance 'nhooks:hook-void))

And add handlers to it:

    (nhooks:add-hook test-hook #'my-function)

To run the hook:

    (nhooks:run-hook test-hook)

Hook handlers can be automatically derived from named functions when calling
`hooks:add-hook'.  If you want to add an anonymous function, you'll have to
instantiate the handler manually:

    (nhooks:add-hook test-hook
                     (make-instance 'nhooks:handler
                                    :fn (lambda () (format t \"Hello!~%\"))
                                    :name 'my-anonymous-function))

You can customize the way handlers are composed by a hook:

    (let ((hook (make-instance 'nhooks:hook-number->number
                               :handlers (list #'add-1 #'multiply-by-2)
                               :combination #'nhooks:combine-composed-hook)))
      (nhooks:run-hook hook 17))
    ; => 35

Handlers can be enabled and disabled with `nhooks:enable-hook' and
`nhooks:disable-hook' respectively.

If the handler is meant to be a setter, the `nhooks:place' and `nhooks:value'
slots can be specified; this helps `nhooks:add-hook' to compare handlers and, in
particular, avoid duplicates in hooks.

Hooks can be defined globally and attached to arbitrary symbols or objects:

    (nhooks:define-hook 'nhooks:hook-number->number 'foo
                        :object my-object

There are also the convenience macros `nhooks:on' and `nhooks:once-on' to attach
a form to a hook and, for once-on, to ensure it's run only once."))
