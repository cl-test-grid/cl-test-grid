;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-quicklisp-diff-report (report-file
                                    all-results
                                    old-quicklisp
                                    new-quicklisp)
  (let* ((new-ql-results (my-time ("last-ql-results...")
                           (subset all-results
                                   (lambda (result)
                                     (string= (lib-world result) new-quicklisp)))))
         (old-ql-results (my-time ("prev-ql-results...")
                           (subset all-results
                                   (lambda (result)
                                     (string= (lib-world result) old-quicklisp)))))
         ;; only consider results for lisps which were tested on both quicklisp versions
         (new-ql-lisps (remove-duplicates (mapcar #'lisp new-ql-results) :test #'string=))
         (old-ql-lisps (remove-duplicates (mapcar #'lisp old-ql-results) :test #'string=))
         (common-lisps (intersection new-ql-lisps old-ql-lisps :test #'string=))
         (common-lisp-p (lambda (lisp) (member lisp common-lisps :test #'string=)))
         ;; now compute the diff between the results of two quicklisps,
         ;; considering only results from lisps tested on both versions.
         (diff (my-time ("fast-exclusive-or...")
                 (fast-exclusive-or (subset new-ql-results common-lisp-p :key #'lisp)
                                    (subset old-ql-results common-lisp-p :key #'lisp)
                                    :test #'equal
                                    :key (lambda (result)
                                           (list (libname result)
                                                 (lisp result)
                                                 (result-spec result)))))))
    (my-time ("print-pivot...")
      (print-pivot report-file
                   diff
                   :rows '((lisp string<) (libname string<))
                   :cols '((lib-world string<))
                   :cell-printer #'results-cell-printer))))
