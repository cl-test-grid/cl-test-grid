;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-contributors (lib-world)
  (dolist (run (getf (test-grid-data::read-db)
                     :runs))
    (let ((descr (test-grid-data::run-descr run)))
      (when (string= lib-world (getf descr :lib-world))
        (format t "~A ~A~%" 
                (getf descr :lisp)
                (getf (getf descr :contact) :email))))))

;; usage
;;(print-contributors "quicklisp 2012-07-03")
