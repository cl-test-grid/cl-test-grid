;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-quicklisp-diff-report (report-file
                                    all-failures
                                    new-quicklisp
                                    old-quicklisp)
  (let* ((new-ql-fails (my-time ("last-ql-fails...")
                         (subset all-failures
                                 (lambda (failure)
                                   (string= (lib-world failure) new-quicklisp)))))
         (old-ql-fails (my-time ("prev-ql-fails...")
                         (subset all-failures
                                 (lambda (failure)
                                   (string= (lib-world failure) old-quicklisp)))))
         ;; only consider results for lisps which were tested on both quicklisp versions
         (new-ql-lisps (remove-duplicates (mapcar #'lisp new-ql-fails) :test #'string=))
         (old-ql-lisps (remove-duplicates (mapcar #'lisp old-ql-fails) :test #'string=))
         (common-lisps (intersection new-ql-lisps old-ql-lisps :test #'string=))
         (common-lisp-p (lambda (lisp) (member lisp common-lisps :test #'string=)))
         ;; now compute the diff between the results of two quicklisps,
         ;; considering only results from lisps tested on both versions.
         (diff (my-time ("fast-exclusive-or...")
                 (fast-exclusive-or (subset new-ql-fails common-lisp-p :key #'lisp)
                                    (subset old-ql-fails common-lisp-p :key #'lisp)
                                    :test #'equal
                                    :key (lambda (fail)
                                           (list (libname fail)
                                                 (lisp fail)
                                                 (fail-spec fail)))))))
    (my-time ("print-pivot...")
      (print-pivot report-file
                   diff
                   :rows '((lisp string<) (libname string<))
                   :cols '((lib-world string<))
                   :cell-printer (lambda (out cell-data)
                                   (dolist (fail cell-data)
                                     (format out "~A</br>" (failure-log-link fail #'fail-spec))))))))
