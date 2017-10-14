(defpackage #:ragno/spider
  (:use #:cl
        #:ragno/errors)
  (:import-from #:ragno/response
                #:make-response
                #:response-headers
                #:response-body)
  (:import-from #:ragno/http
                #:request)
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
                  :accessor spider-max-redirects)))

(defgeneric parse (spider response)
  (:method (spider response)
    (response-body response)))

(defgeneric fetch (spider uri)
  (:method (spider uri)
    (request uri
             :max-redirects (spider-max-redirects spider))))

(defgeneric scrape (spider uri)
  (:method (spider uri)
    (parse spider (fetch spider uri))))
