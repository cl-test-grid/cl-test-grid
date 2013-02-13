(in-package :cl-user)

(print ">>> Building system....")
(require 'asdf)
(asdf:disable-output-translations)
(require-quicklisp)
(ql:quickload :test-grid-server)
(print ">>> Done building system")

;; change the file just to invoke recompilation at heroku: 0