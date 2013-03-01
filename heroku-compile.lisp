(in-package :cl-user)

(print ">>> Building system....")
(require 'asdf)
(asdf:disable-output-translations)
(require-quicklisp)
(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (push this-file-dir asdf:*central-registry*))
(ql:quickload :test-grid-server)

(print ">>> saving image tg-server...")
(sb-ext:save-lisp-and-die "tg-server"
                          :toplevel (lambda ()
                                      (tg-server:start :port (parse-integer (asdf::getenv "PORT"))
                                                       :smtp-password (asdf::getenv "SMTP_PASSWORD"))
                                      (loop (sleep 1000)))
                          :executable t
                          :purify t)

(print ">>> Done building system")

;; 1
