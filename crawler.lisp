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
           #:crawler-request-delay
           #:crawler-user-agent
           #:follow-links))
(in-package #:ragno/crawler)

(defclass crawler (spider psy:worker)
  ((request-delay :initarg :request-delay
                  :initform 0
                  :accessor crawler-request-delay)
   (user-agent :initarg :user-agent
               :initform "Ragno-Crawler"
               :accessor crawler-user-agent)))

(defmethod psy:perform ((crawler crawler) &rest args)
  (let ((uri (first args))
        (*user-agent* (crawler-user-agent crawler)))
    (quri:uri-domain (quri:uri uri))
    (scrape crawler uri)))

(defgeneric follow-links (crawler uri-or-uris)
  (:method ((crawler crawler) uri-or-uris)
    (loop for uri in (etypecase uri-or-uris
                       (list uri-or-uris)
                       (vector (coerce uri-or-uris 'list))
                       ((or string quri:uri) (list uri-or-uris)))
          for i from 1
          do (psy:enqueue-in-sec (* i (crawler-request-delay crawler))
                                 (class-name (class-of crawler))
                                 (list (etypecase uri
                                         (string uri)
                                         (quri:uri (quri:render-uri uri))))))))
