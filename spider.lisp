(defpackage #:ragno/spider
  (:use #:cl
        #:ragno/errors)
  (:import-from #:ragno/response
                #:response-headers
                #:response-body)
  (:import-from #:ragno/http
                #:request)
  (:import-from #:quri)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:spider
           #:spider-max-redirects
           #:spider-concurrency-per-domain
           #:spider-request-delay
           #:parse
           #:fetch
           #:scrape))
(in-package #:ragno/spider)

(defclass spider ()
  ((max-redirects :initarg :max-redirects
                  :initform 5
                  :accessor spider-max-redirects)
   (concurrency-per-domain :initarg :concurrency-per-domain
                           :initform 1
                           :accessor spider-concurrency-per-domain)
   (request-delay :initarg :request-delay
                  :initform 5
                  :accessor spider-request-delay)

   (%spider-lock :initform (bt:make-lock "spider concurrent lock")
                 :allocation :class)
   (%concurrent-count :type hash-table
                      :initform (make-hash-table :test 'equal)
                      :allocation :class)
   (last-request-done-at :initform (get-universal-time)
                         :allocation :class)))

(defgeneric parse (spider response)
  (:method (spider response)
    (response-body response)))

(defgeneric fetch (spider uri)
  (:method (spider uri)
    (with-slots (%spider-lock %concurrent-count
                 request-delay
                 concurrency-per-domain last-request-done-at) spider
      (let ((domain (quri:uri-domain (quri:uri uri))))
        (bt:with-lock-held (%spider-lock)
          (if (< (gethash domain %concurrent-count 0) concurrency-per-domain)
              (incf (gethash domain %concurrent-count 0))
              (error 'ragno-concurrency-limit
                     :uri uri
                     :retry-after request-delay)))
        (unwind-protect
             (progn
               (sleep (max (- (+ last-request-done-at request-delay) (get-universal-time))
                           0))
               (vom:info "Fetching ~S" uri)
               (let ((start (get-internal-real-time)))
                 (prog1
                     (request uri
                              :max-redirects (spider-max-redirects spider))
                   (vom:info "Fetch done (~S sec)"
                             (/ (- (get-internal-real-time) start) 1000.0)))))
          (bt:with-lock-held (%spider-lock)
            (setf last-request-done-at (get-universal-time))
            (decf (gethash domain %concurrent-count 0))))))))

(defgeneric scrape (spider uri)
  (:method (spider uri)
    (parse spider (fetch spider uri))))
