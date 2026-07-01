(asdf:defsystem #:hactar
  :description "Hactar executable: loads hactar-core, bootstraps, dispatches."
  :author "K-2052"
  :license "MIT"
  :depends-on (#:hactar-core)
  :build-operation "program-op"
  :build-pathname "hactar/build/hactar"
  :entry-point "hactar:main")
