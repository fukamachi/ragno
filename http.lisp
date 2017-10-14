(defpackage #:ragno/http
  (:use #:cl
        #:ragno/errors)
  (:import-from #:ragno/response
                #:make-response)
  (:import-from #:quri)
  (:import-from #:dexador)
  (:export #:*user-agent*
           #:request))
(in-package #:ragno/http)

(defvar *user-agent* "Ragno-Crawler")

(defun request (uri &key max-redirects)
  (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
    (multiple-value-bind (body status headers last-uri)
        (dex:get uri
                 :max-redirects (or max-redirects 5)
                 :headers `(("user-agent" . ,*user-agent*)))
      (let ((response (make-response :body body :status status :headers headers :uri last-uri)))
        (unless (= status 200)
          (error 'ragno-fetch-error
                 :response response))
        response))))
