(defpackage #:ragno/response
  (:use #:cl)
  (:export #:response
           #:response-body
           #:response-status
           #:response-headers
           #:response-uri))
(in-package #:ragno/response)

(defstruct response
  body
  status
  headers
  uri)
