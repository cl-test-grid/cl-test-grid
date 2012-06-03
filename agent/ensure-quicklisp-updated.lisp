;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file is loaded by agent into a separate lisp process
;;;; it starts to update quicklisp.

(defparameter *this-file* (load-time-value (or *load-truename* #.*compile-file-pathname*)))
(defparameter *this-file-dir* (make-pathname :directory (pathname-directory *this-file*)))

(load (merge-pathnames "quicklisp.lisp" *this-file-dir*))

(handler-bind ((error #'(lambda (err)
                          (declare (ignore err))
                          (when (find-restart 'quicklisp-quickstart::load-setup)
                            (invoke-restart 'quicklisp-quickstart::load-setup)))))
  (quicklisp-quickstart:install :path (merge-pathnames "../work-dir/agent/quicklisp/"
                                                       *this-file-dir*)))

(defun do-quicklisp-update()
  (quicklisp:update-client :prompt nil)
  (quicklisp:update-all-dists :prompt nil)
  (ql-dist:version (ql-dist:dist "quicklisp")))
