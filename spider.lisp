(defpackage #:ragno/spider
  (:use #:cl
        #:ragno/errors)
  (:import-from #:ragno/response
                #:make-response
                #:response-headers
                #:response-body)
  (:import-from #:ragno/http
                #:request)
  (:import-from #:quri)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:spider
           #:spider-max-redirects
           #:parse
           #:fetch
           #:scrape))
(in-package #:ragno/spider)

(defclass spider ()
  ((max-redirects :initarg :max-redirects
                  :initform 5
                  :accessor spider-max-redirects)
   (concurrency :initarg :concurrency
                :initform 10
                :accessor spider-concurrency)

   (%concurrent-count :type hash-table
                      :initform (make-hash-table :test 'equal)
                      :allocation :class)))

(defgeneric parse (spider response)
  (:method (spider response)
    (response-body response)))

(defgeneric fetch (spider uri)
  (:method (spider uri)
    (with-slots (%concurrent-count concurrency) spider
      (let ((quri (quri:uri-domain (quri:uri uri))))
        (if (< (gethash quri %concurrent-count 0) concurrency)
            (incf (gethash quri %concurrent-count 0))
            (error 'ragno-concurrency-limit :uri uri))
        (unwind-protect
             (request uri
                      :max-redirects (spider-max-redirects spider))
          (decf (gethash quri %concurrent-count 0)))))))

(defgeneric scrape (spider uri)
  (:method (spider uri)
    (parse spider (fetch spider uri))))
