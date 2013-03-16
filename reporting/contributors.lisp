;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-contributors (db lib-world)
  "Helper function to print email with
test results summary for particular lib-world."
  (let ((result))
    (dolist (run (getf db :runs))
      (let ((descr (test-grid-data::run-descr run)))
        (when (string= lib-world (getf descr :lib-world))
          (push (list (getf descr :lisp)
                      (getf descr :contact-email))
                result))))
    (format t "cl-test-grid@googlegroups.com~{,~A~}~%"
            (remove-duplicates (sort (mapcar #'second result) #'string<)
                               :test #'string=))
    (format t "[cl-test-grid] test results summary~%~%")

    (format t "Test results summary for ~A:~%~%" lib-world)
    (format t "~{~{~A ~A~}~%~}"
            (sort result (tg-utils::obj-comparator #'first #'string<
                                                   #'second #'string<)))))