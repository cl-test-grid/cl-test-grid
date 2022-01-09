;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun list-props (object prop-readers)
  (mapcar (lambda (prop-reader)
            (funcall prop-reader object))
          prop-readers))

(defun distinct (objects object-prop-readers)
  (remove-duplicates (mapcar (lambda (object)
                               (list-props object object-prop-readers))
                             objects)
                     :test #'equal))

(assert (alexandria:set-equal '((1) (7))
                              (distinct '((:a 1 :b 2) (:a 1 :b 3) (:a 7 :b 1))
                                        (list (test-grid-utils::plist-getter :a)))
                              :test #'equal))

(defun distinct1 (objects object-prop-reader)
  (alexandria:flatten (distinct objects (list object-prop-reader))))

(assert (alexandria:set-equal '(1 7)
                              (distinct1 '((:a 1 :b 2) (:a 1 :b 3) (:a 7 :b 1))
                                         (test-grid-utils::plist-getter :a))
                              :test #'equal))

(defun largest (prop-reader objects &key (count 1) (predicate #'string>))
  (let* ((distinct (distinct objects (list prop-reader)))
         (flat (alexandria:flatten distinct))
         (sorted (sort flat predicate)))
    (subseq sorted 0 count)))
