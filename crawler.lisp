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
  (:import-from #:bordeaux-threads)
  (:export #:crawler
           #:crawler-request-delay
           #:crawler-concurrency
           #:crawler-user-agent))
(in-package #:ragno/crawler)

(defclass crawler (spider psy:worker)
  ((request-delay :initarg :request-delay
                  :initform 5
                  :accessor crawler-request-delay)
   (concurrency :initarg :concurrency
                :initform 1
                :accessor crawler-concurrency)
   (user-agent :initarg :user-agent
               :initform "Ragno-Crawler"
               :accessor crawler-user-agent)

   (%lock :initform (bt:make-lock "crawler concurrent lock")
          :allocation :class)
   (%count :type integer
           :initform 0
           :allocation :class)
   (last-request-done-at :initform (get-universal-time)
                         :allocation :class)))

(defmethod psy:perform ((crawler crawler) &rest args)
  (with-slots (%lock %count
               last-request-done-at request-delay concurrency) crawler
    (bt:with-lock-held (%lock)
      (if (< %count concurrency)
          (incf %count)
          ;; Retry with some delay
          (progn
            (psy:enqueue-in-sec request-delay
                                (class-name (class-of crawler))
                                args)
            (return-from psy:perform))))
    (sleep (max (- (+ last-request-done-at request-delay) (get-universal-time))
                0))
    (unwind-protect
         (let ((uri (first args))
               (*user-agent* (crawler-user-agent crawler)))
           (scrape crawler uri))
      (bt:with-lock-held (%lock)
        (setf last-request-done-at (get-universal-time))
        (decf %count)))))
