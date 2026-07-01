(in-package #:hactar-rebuild-tests)
(in-suite rebuild-suite)

(test hooks-run-newest-first
  (hactar-rebuild:reset-registries)
  (eval-in-user "(defhook demo.hook :args (x) :doc \"d\")")
  (let ((log '()))
    (hactar-rebuild:add-handler 'demo.hook (lambda (x) (push (list :a x) log)))
    (hactar-rebuild:add-handler 'demo.hook (lambda (x) (push (list :b x) log)))
    (hactar-rebuild:run-hook 'demo.hook 7)
    ;; newest (:b) runs first, pushes first => log ends ((:a 7)(:b 7))
    (is (equal '((:a 7) (:b 7)) log))))

(test hook-handler-errors-are-isolated
  (hactar-rebuild:reset-registries)
  (eval-in-user "(defhook demo.hook2 :args (x) :doc \"d\")")
  (hactar-rebuild:add-handler 'demo.hook2 (lambda (x) (declare (ignore x))
                                             (error "boom")))
  (hactar-rebuild:add-handler 'demo.hook2 (lambda (x) (* x 2)))
  (is (equal '(14 nil)
             (handler-bind ((warning #'muffle-warning))
               (hactar-rebuild:run-hook 'demo.hook2 7)))))
