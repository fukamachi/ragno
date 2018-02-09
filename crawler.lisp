(defpackage #:ragno/crawler
  (:use #:cl)
  (:import-from #:ragno/spider
                #:spider
                #:scrape)
  (:import-from #:ragno/http
                #:*user-agent*)
  (:import-from #:ragno/errors
                #:ragno-concurrency-limit
                #:retry-after)
  (:import-from #:psychiq
                #:worker
                #:perform
                #:enqueue-in-sec)
  (:export #:crawler
           #:crawler-user-agent))
(in-package #:ragno/crawler)

(defclass crawler (spider psy:worker)
  ((user-agent :initarg :user-agent
               :initform "Ragno-Crawler"
               :accessor crawler-user-agent)))

(defmethod psy:perform ((crawler crawler) &rest args)
  (let ((uri (first args))
        (*user-agent* (crawler-user-agent crawler)))
    (handler-case
        (scrape crawler uri)
      (ragno-concurrency-limit (e)
        (vom:info "Retry ~S after ~S secs"
                  (class-name (class-of crawler))
                  (retry-after e))
        (psy:enqueue-in-sec (retry-after e)
                            (class-name (class-of crawler))
                            args)))))
