;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.
;;;
;;; This file is supposed to be LOADed by user: 
;;;
;;;        (load "crash-abcl.lisp")
;;;

(defparameter *this-file* (load-time-value (or *load-truename* #.*compile-file-pathname*)))
(defparameter *this-file-dir* (make-pathname :directory (pathname-directory *this-file*)))

;;; install quicklisp
(load (merge-pathnames "quicklisp.lisp" *this-file-dir*))

(handler-bind ((error #'(lambda (err)
                          (declare (ignore err))
                          (when (find-restart 'quicklisp-quickstart::load-setup)
                            (invoke-restart 'quicklisp-quickstart::load-setup)))))
  (quicklisp-quickstart:install :path (merge-pathnames "quicklisp/"
                                                       *this-file-dir*)))

;; switch quicklisp to particular distro version (to be sure the crash is reproducible)
(ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2012-05-20/distinfo.txt" :replace t :prompt nil)

(pushnew *this-file-dir* asdf:*central-registry* :test #'equal)
(asdf:operate 'asdf:load-op :test-grid)

;; run many test suites, which in result crashes ABCL
(test-grid::run-libtests)

