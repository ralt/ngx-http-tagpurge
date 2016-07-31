(asdf:defsystem #:purger
  :description "HTTP Purge API for ngx-http-tagpurge."
  :author "Florian Margaine <florian@margaine.com>"
  :license "BSD"
  :serial t
  :depends-on (:hunchentoot :unix-opts)
  :components ((:file "package")
               (:file "purger")
               (:file "main")))
