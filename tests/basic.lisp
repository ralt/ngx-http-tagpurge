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
  (setf (hunchentoot:header-out "foo") "bar")
  "Hello")

(bordeaux-threads:make-thread
 (lambda ()
   (format t "Starting hunchentoot~%")
   (hunchentoot:start
    (make-instance 'hunchentoot:easy-acceptor :port 9999))))

(uiop:run-program "build/nginx/sbin/nginx")
(format t "nginx started~%")

(defvar *url* "http://localhost:8888")

(test basic-test
  (is (string=
       "Hello"
       (drakma:http-request *url*))))

(test check-response-header
  (multiple-value-bind (response status headers)
      (drakma:http-request *url*)
    (declare (ignore response status))
    (format t "~A~%" headers)
    ;; Make sure we're getting our custom header
    (is (drakma:header-value :foo headers) "bar")
    ;; Make sure we're getting nginx custom tagpurge header
    (is (drakma:header-value :X-foo headers)
        "bar")))

(run! :basic)
(uiop:run-program "build/nginx/sbin/nginx -s stop")
(format t "nginx stopped~%")
