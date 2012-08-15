;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun distinct (prop-getter db &key (test #'equal))
  (let ((distinct (make-hash-table :test test)))
    (do-results (result db)
      (let ((val (funcall prop-getter result)))
        (setf (gethash val distinct)
              val)))
    (alexandria:hash-table-keys distinct)))

(defun largest (prop-getter db &key (count 1) (predicate #'string>))
  (let* ((all (distinct prop-getter db))
         (sorted (sort all predicate)))
    (subseq sorted 0 count)))
