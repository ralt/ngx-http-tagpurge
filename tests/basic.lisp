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
  (setf (hunchentoot:header-out "cache-Tag") "bar")
  "Hello")

(hunchentoot:define-easy-handler (foo :uri "/foo") ()
  (setf (hunchentoot:header-out "cache-tag") "foo baz")
  "Foo")

(bordeaux-threads:make-thread
 (lambda ()
   (format t "Starting hunchentoot~%")
   (hunchentoot:start
    (make-instance 'hunchentoot:easy-acceptor :port 9999))))

(defun reset-directory (dir)
  (when (probe-file dir)
    (uiop:delete-directory-tree dir :validate t))
  (ensure-directories-exist dir))

(reset-directory (merge-pathnames "build/nginx/cache/" (uiop:getcwd)))
(reset-directory (merge-pathnames "build/tagpurge/" (uiop:getcwd)))

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
    (is (drakma:header-value :cache-tag headers) "bar")
    (is-true (probe-file "build/tagpurge/bar")))
  (multiple-value-bind (response status headers)
      (drakma:http-request "http://localhost:8888/foo")
    (declare (ignore response status))
    (format t "~A~%" headers)
    (is (drakma:header-value :cache-tag headers) "foo baz")
    (is-true (probe-file "build/tagpurge/foo"))
    (is-true (probe-file "build/tagpurge/baz"))))

(run! :basic)
(uiop:run-program "build/nginx/sbin/nginx -s stop")
(format t "nginx stopped~%")
