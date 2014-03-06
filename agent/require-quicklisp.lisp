;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; Tool to install Quicklisp for test-grid-agent
;;;; if Quicklisp is absent in the current lisp image

(defpackage #:tg-require-quicklisp
  (:use #:cl)
  (:shadow #:require)
  (:export #:require))

(in-package #:tg-require-quicklisp)

(defparameter *this-file* (or #.*compile-file-truename* *load-truename*))
(defparameter *this-dir* (make-pathname :directory (pathname-directory *this-file*)))

(defun require (&key agent-work-dir)
  (unless (find-package '#:quicklisp)
    (let* ((ql-dir (merge-pathnames "main-quicklisp/" agent-work-dir))
           (setup-file (merge-pathnames "setup.lisp" ql-dir)))
      (cond ((probe-file setup-file) (load setup-file))
            (t (load (merge-pathnames "quicklisp.lisp" *this-dir*))
               (funcall (read-from-string "quicklisp-quickstart:install")
                        :path ql-dir))))))
