(in-package :cl-user)

(print ">>> Building system....")
(require 'asdf)
(asdf:disable-output-translations)
(require-quicklisp)
(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (push this-file-dir asdf:*central-registry*))
(ql:quickload :test-grid-server)
(print ">>> Done building system")

;; change the file just to invoke recompilation at heroku: 1 
