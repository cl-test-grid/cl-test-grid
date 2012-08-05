;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(let ((problems '()))
  (do-results (record (test-grid-data::read-db))
    (when (and (search "ecl" (lisp record))
               (string= "quicklisp 2012-07-03" (lib-world record))
               (member (status record) '(:load-failed :timeout :crash)))
      (push record problems)))
  (sort problems #'string< :key #'status)
  (mapcar (lambda (record) (list (libname record) (status record)))
          problems))
