(in-package :cl-user)

(print ">>> Building system....")
(require 'asdf)
(asdf:disable-output-translations)
(require-quicklisp :version "2013-04-20")
(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (push this-file-dir asdf:*central-registry*))
(ql:quickload :test-grid-server)

(print ">>> saving image tg-server...")
(sb-ext:save-lisp-and-die (merge-pathnames "tg-server" *build-dir*)
                          :toplevel (lambda ()
                                      (tg-server:start :port (parse-integer (asdf::getenv "PORT"))
                                                       :smtp-user (asdf::getenv "SMTP_USER")
                                                       :smtp-password (asdf::getenv "SMTP_PASSWORD"))
                                      (loop (sleep 1000)))
                          :executable t
                          :purify t)

(print ">>> Done building system")
