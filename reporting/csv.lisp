;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun export-to-csv (out &optional (db test-grid-data::*db*))
  (format out "Lib World,Lisp,Runner,LibName,Status,TestDuration~%")
  (dolist (run (getf db :runs))
    (let ((run-descr (test-grid-data::run-descr run)))
      (dolist (lib-result (test-grid-data::run-results run))
        (format out "~a,~a,~a,~a,~a,~a~%"
                (getf run-descr :lib-world)
                (getf run-descr :lisp)
                (getf (getf run-descr :contact) :email)
                (string-downcase (getf lib-result :libname))
                (aggregated-status (getf lib-result :status))
                (float (getf lib-result :test-duration)))))))

