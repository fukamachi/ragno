(defpackage #:ragno/crawler
  (:use #:cl)
  (:import-from #:ragno/spider
                #:spider
                #:scrape)
  (:import-from #:ragno/http
                #:*user-agent*)
  (:import-from #:psychiq
                #:worker
                #:perform
                #:enqueue-in-sec)
  (:import-from #:quri
                #:uri
                #:render-uri)
  (:export #:crawler
           #:crawler-concurrency
           #:crawler-enqueue-interval
           #:crawler-user-agent))
(in-package #:ragno/crawler)

(defclass crawler (spider psy:worker)
  ((user-agent :initarg :user-agent
               :initform "Ragno-Crawler"
               :accessor crawler-user-agent)))

(defmethod psy:perform ((crawler crawler) &rest args)
  (let ((uri (first args))
        (*user-agent* (crawler-user-agent crawler)))
    (quri:uri-domain (quri:uri uri))
    (scrape crawler uri)))
