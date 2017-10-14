(defpackage #:ragno/errors
  (:use #:cl)
  (:import-from #:ragno/response
                #:response-uri
                #:response-status)
  (:import-from #:quri
                #:render-uri)
  (:export #:ragno-error
           #:ragno-fetch-error
           #:ragno-parse-error))
(in-package #:ragno/errors)

(define-condition ragno-error () ())

(define-condition ragno-fetch-error (ragno-error)
  ((response :initarg :response))
  (:report (lambda (condition stream)
             (let ((response (slot-value condition 'response)))
               (format stream "Fetch failed from '~A' (Code=~A)"
                       (quri:render-uri (response-uri response))
                       (response-status response))))))
