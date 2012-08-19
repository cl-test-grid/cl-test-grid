;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun export-to-csv (out db)
  (format out "Lib World,Lisp,Runner,LibName,Status,TestDuration~%")
  (do-results (result db)
    (format out "~a,~a,~a,~a,~a,~a~%"
            (lib-world result)
            (lisp result)
            (contact-email result)
            (string-downcase (libname result))
            (aggregated-status (status result))
            (float (test-duration result)))))


