;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-test-run-durations (lib-world)
  (let ((duration-alist '()))
    (dolist (run (getf (test-grid-data::read-db)
                       :runs))
      (let ((descr (test-grid-data::run-descr run)))
        (when (string= lib-world (getf descr :lib-world))
          (push (cons (getf descr :lisp)
                      (getf descr :run-duration))
                duration-alist))))
    (setf duration-alist (sort duration-alist #'string< :key #'car))
    (dolist (pair duration-alist)
      (format t "~A ~A~%"
              (car pair)
              (float (/ (cdr pair)
                        60))))))

;; usage:
;; (print-test-run-durations "quicklisp 2012-09-09")
