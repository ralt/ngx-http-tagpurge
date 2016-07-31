(in-package #:purger)

(defvar *tagpurge-folder* nil
  "The folder where ngx-http-tagpurge stores its tag files.")

(defun purge-tag (tag)
  (let ((tag-file (merge-pathnames tag *tagpurge-folder*)))
    (with-open-file (f tag-file)
      (loop for line = (read-line f nil 'eof)
         until (eq line 'eof)
         do (delete-file line)))
    (delete-file tag-file)))

(hunchentoot:define-easy-handler (purge :uri "/") (tags)
  (unless tags
    (setf (hunchentoot:return-code*)
          hunchentoot:+http-not-found+)
    (return-from purge))
  (handler-case
      (mapcar #'purge-tag (uiop:split-string tags :separator '(#\,)))
    (file-error ()
      (setf (hunchentoot:return-code*)
            hunchentoot:+http-not-found+))
    (error ()
      (setf (hunchentoot:return-code*)
            hunchentoot:+http-internal-server-error+)))
  "")
