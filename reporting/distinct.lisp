;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun distinct-old (prop-getter db &key (test #'equal) where)
  (let ((distinct (make-hash-table :test test)))
    (do-results (result db :where where)
      (let ((val (funcall prop-getter result)))
        (setf (gethash val distinct)
              val)))
    (alexandria:hash-table-keys distinct)))

(defun largest (prop-getter db &key (count 1) (predicate #'string>) where)
  (let* ((all (distinct-old prop-getter db :where where))
         (sorted (sort all predicate)))
    (subseq sorted 0 count)))
