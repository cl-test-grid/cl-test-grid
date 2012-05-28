;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.
;;;
;;; This file is supposed to be LOADed by user:
;;;
;;;        (load "ensure-quicklisp-updated.lisp")
;;;

(defparameter *this-file* (load-time-value (or *load-truename* #.*compile-file-pathname*)))
(defparameter *this-file-dir* (make-pathname :directory (pathname-directory *this-file*)))

(load (merge-pathnames "quicklisp.lisp" *this-file-dir*))

(handler-bind ((error #'(lambda (err)
                          (declare (ignore err))
                          (when (find-restart 'quicklisp-quickstart::load-setup)
                            (invoke-restart 'quicklisp-quickstart::load-setup)))))
  (quicklisp-quickstart:install :path (merge-pathnames "../work-dir/agent/quicklisp/"
                                                       *this-file-dir*)))

(quicklisp:update-client :prompt nil)
(quicklisp:update-all-dists :prompt nil)
