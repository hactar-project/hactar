;;;; ASDF System Definition for Hactar Editor

(asdf:defsystem #:hactar-editor
  :description "A Lisp-native text editor with AI-first design"
  :author "Hactar Project"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:cl-ppcre
               #:uiop)
  :components
  ((:module "core"
    :components
    ((:file "gap-buffer")
     (:file "buffer" :depends-on ("gap-buffer"))))
   (:file "keymap")
   (:file "main" :depends-on ("core" "keymap"))))
