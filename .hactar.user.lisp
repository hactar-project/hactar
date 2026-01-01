;; (format t "look ma I'm loaded?")

(in-package :hactar)

(defagent hello (name)
  "A simple helloworld agent that greets a person."
  :init (progn
          (setf (gethash 'loop-count (agent-instance-state agent-instance)) 0)
          (format t "Agent 'hello' initialized for ~A.~%" name))
  :run (progn
         (format t "Hello agent loop... (~A/3)~%" (1+ (gethash 'loop-count (agent-instance-state agent-instance))))
         (sleep 1)
         (incf (gethash 'loop-count (agent-instance-state agent-instance))))
  :stop-condition (>= (gethash 'loop-count (agent-instance-state agent-instance) 0) 3)
  :cleanup (get-llm-response (format nil "Say hello to ~A." name)))
