;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file is loaded by agent into a separate lisp process
;;;; to update quicklisp.

(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (load (merge-pathnames "quicklisp.lisp" this-file-dir)))

(defun install-quicklisp (install-dir)
  (handler-bind ((error #'(lambda (err)
                            (declare (ignore err))
                            (when (find-restart 'quicklisp-quickstart::load-setup)
                              (invoke-restart 'quicklisp-quickstart::load-setup)))))
    (quicklisp-quickstart:install :path install-dir)))


(defmacro fncall (funname &rest args)
  `(funcall (read-from-string ,funname) ,@args))

(defun do-quicklisp-update (install-dir)
  (install-quicklisp install-dir)
  (fncall "quicklisp:update-client" :prompt nil)
  (fncall "quicklisp:update-all-dists" :prompt nil)
  (fncall "ql-dist:version" (fncall "ql-dist:dist" "quicklisp")))
