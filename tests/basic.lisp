(ql:quickload "fiveam")
(ql:quickload "drakma")
(ql:quickload "hunchentoot")
(ql:quickload "bordeaux-threads")
(ql:quickload "djula")

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

(hunchentoot:define-easy-handler (bar :uri "/bar") ()
  (setf (hunchentoot:header-out "cache-tag") "foo")
  "Bar")

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

(let ((error-log (merge-pathnames "build/nginx/logs/error.log" (uiop:getcwd))))
  (when (probe-file error-log)
    (delete-file error-log)))

(djula:add-template-directory "tests/")
(defparameter +nginx.conf+ (djula:compile-template* "nginx.conf"))

(defun write-nginx-conf (conf-dir conf)
  (let ((nginx-conf (merge-pathnames "nginx.conf" conf-dir)))
    (when (probe-file nginx-conf)
      (delete-file nginx-conf))
    (with-open-file (s nginx-conf
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :overwrite)
      (let ((template (djula:compile-template* conf)))
        (djula:render-template* template s
                                :pwd (uiop:getcwd))))))

(write-nginx-conf
 (merge-pathnames "build/nginx/conf/" (uiop:getcwd))
 (merge-pathnames "tests/nginx.conf" (uiop:getcwd)))

(uiop:run-program "build/nginx/sbin/nginx")
(format t "nginx started~%")

(test basic-test
  (is (string=
       "Hello"
       (drakma:http-request "http://localhost:8888/"))))

(test simple-tag-creation
  (multiple-value-bind (response status headers)
      (drakma:http-request "http://localhost:8888/")
    (declare (ignore response status))
    (is (drakma:header-value :cache-tag headers) "bar")
    (is-true (probe-file "build/tagpurge/bar"))))

(test two-tags-in-one-header
  (multiple-value-bind (response status headers)
      (drakma:http-request "http://localhost:8888/foo")
    (declare (ignore response status))
    (is (drakma:header-value :cache-tag headers) "foo baz")
    (is-true (probe-file "build/tagpurge/foo"))
    (is-true (probe-file "build/tagpurge/baz"))
    (is (uiop:read-file-string "build/tagpurge/foo")
        (uiop:read-file-string "build/tagpurge/baz"))
    (is-true (probe-file (uiop:read-file-line "build/tagpurge/foo")))))

(test another-url-for-same-tag
  (multiple-value-bind (response status headers)
      (drakma:http-request "http://localhost:8888/bar")
    (declare (ignore response status))
    (is (drakma:header-value :cache-tag headers) "foo")
    (is-true (probe-file "build/tagpurge/foo"))
    (is-true (= 2
                (length
                 (uiop:read-file-lines "build/tagpurge/foo"))))))

#|
(bordeaux-threads:make-thread
 (lambda ()
   (format t "Starting tagpurge-http-api~%")
   (uiop:run-program "build/purger/tagpurge-http-api --port 6666 --tagpurge-folder build/tagpurge/" :output t :error-output t)))

(test tagpurge-purge-tag
  (let ((cache-file (uiop:read-file-line "build/tagpurge/foo")))
    (is-true (probe-file cache-file))
    (drakma:http-request "http://localhost:6666/?tags=foo")
    (is-false (probe-file "build/tagpurge/foo"))
    (is-false (probe-file cache-file))))|#

(run! :basic)
(uiop:run-program "build/nginx/sbin/nginx -s stop")
(format t "nginx stopped~%")
