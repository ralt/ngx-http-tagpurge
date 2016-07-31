(in-package #:purger)

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :port
   :description "listening port"
   :short #\p
   :long "port"
   :arg-parser #'parse-integer
   :meta-var "PORT")
  (:name :tagpurge-folder
   :description "tagpurge folder"
   :short #\t
   :long "tagpurge-folder"
   :arg-parser #'identity
   :meta-var "TAGPURGE_FOLDER"))

(defun fatal (msg &rest args)
  (apply #'format t (format nil "fatal: ~A~%" msg) args)
  (uiop:quit -1))

(defmacro with-option ((options opt var) &body body)
  `(let ((,var (getf ,options ,opt)))
     (when ,var
       (progn ,@body))))

(defun help ()
  (opts:describe
   :prefix "HTTP Purge API for ngx-http-tagpurge"
   :usage-of "tagpurge-http-api"))

(defun main (args)
  (multiple-value-bind (options _)
      (handler-case
          (opts:get-opts args)
        (opts:unknown-option (condition)
          (fatal "option ~S is unknown" (opts:option condition)))
        (opts:missing-arg (condition)
          (fatal "option ~S needs an argument" (opts:option
                                                condition))))
    (declare (ignore _))
    (when (= (length args) 1)
      (help))
    (with-option (options :help _)
      (help))
    (unless (getf options :tagpurge-folder)
      (fatal "missing option: --tagpurge-folder"))
    (unless (getf options :port)
      (fatal "missing option: --port"))
    (with-option (options :tagpurge-folder folder)
      (setf *tagpurge-folder* folder))
    (with-option (options :port port)
      (hunchentoot:start
       (make-instance 'hunchentoot:easy-acceptor
                      :address "127.0.0.1"
                      :port port))
      (sb-impl::toplevel-repl nil))))
