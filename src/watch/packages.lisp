(defpackage #:hactar-watch
  (:use #:cl)
  (:export #:watcher #:watch #:unwatch #:watching-p #:on-change
           #:poll-watch #:emit-event #:pending-events #:drain-events))
