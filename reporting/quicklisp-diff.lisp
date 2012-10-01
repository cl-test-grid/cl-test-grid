;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defparameter *last-quicklisp* "quicklisp 2012-09-09")
(defparameter *prev-quicklisp* "quicklisp 2012-08-11")

(defun print-quicklisp-diff-report (failures)
  (let* ((last-ql-fails (my-time ("last-ql-fails...")
                          (remove-if-not (lambda (failure)
                                           (string= (lib-world failure) *last-quicklisp*))
                                         failures)))
         (prev-ql-fails (my-time ("prev-ql-fails...")
                          (remove-if-not (lambda (failure)
                                           (string= (lib-world failure) *prev-quicklisp*))
                                         failures)))
         ;; only consider results for lisps which were tested on both quicklisp versions
         (last-ql-lisps (alexandria:flatten (distinct last-ql-fails (list #'lisp))))
         (prev-ql-lisps (alexandria:flatten (distinct prev-ql-fails (list #'lisp))))
         (common-lisps (intersection last-ql-lisps prev-ql-lisps :test #'string=))
         (common-lisp-p (lambda (lisp) (member lisp common-lisps :test #'string=)))
         ;; now compute the diff between the results of two quicklisps,
         ;; considering only results from lisps tested on both versions.
         (diff (my-time ("fast-exclusive-or...")
                 (fast-exclusive-or (remove-if-not common-lisp-p last-ql-fails :key #'lisp)
                                    (remove-if-not common-lisp-p prev-ql-fails :key #'lisp)
                                    :test #'equal
                                    :key (lambda (fail)
                                           (list (libname fail)
                                                 (lisp fail)
                                                 (fail-spec fail)))))))
    (my-time ("print-pivot...")
      (print-pivot "quicklisp-diff.html"
                   diff
                   (list #'lisp #'libname) (list #'string< #'string<)
                   (list #'lib-world) (list #'string>)
                   (lambda (out cell-data)
                     (dolist (fail cell-data)
                       (format out "~A</br>" (failure-log-link fail #'fail-spec))))))))


;;; Usage
#|
git clone git@github.com:cl-test-grid/cl-test-grid.git
git clone git@github.com:cl-test-grid/cl-test-grid-results.git

(pushnew "cl-test-grid/" asdf:*central-registry* :test #'equal)
(ql:quickload :test-grid-reporting)

(let* ((db (test-grid-data:read-db))
       (all-results (select db))
       (all-failures (list-failures all-results)))
  (test-grid-reporting::print-quicklisp-diff-report all-failures))

the result is stored in cl-test-grid/reports-generated/quicklisp-diff.html

|#
