;; simple cli wrapper around the llm lib
(defpackage #:llm-cli
  (:use #:cl)
  (:export #:main))

(in-package :llm-cli)

(defun main ()
  "Main entry point for the LLM CLI application."
  (rl:initialize-readline)
  (rl:bind-keyseq "\\C-l" #'rl:clear-screen)
  (unwind-protect
       (llm:run-cli)
    (rl:deactivate-readline)))
