(ql:quickload "fiveam")
(ql:quickload "drakma")
(ql:quickload "hunchentoot")
(ql:quickload "bordeaux-threads")

(defpackage #:ngx-tagpurge-tests
  (:use #:cl #:5am))

(in-package #:ngx-tagpurge-tests)

(def-suite :basic)

(in-suite :basic)

(hunchentoot:define-easy-handler (home :uri "/") ()
  "Hello")

(bordeaux-threads:make-thread
 (lambda ()
   (format t "Starting hunchentoot~%")
   (hunchentoot:start
    (make-instance 'hunchentoot:easy-acceptor :port 9999))))

(uiop:run-program "build/nginx/sbin/nginx")
(format t "nginx started~%")

(test basic-test
  (is (string=
       "Hello"
       (drakma:http-request "http://localhost:8888"))))

(run! :basic)

(uiop:run-program "build/nginx/sbin/nginx -s stop")
(format t "nginx stopped~%")
