;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defparameter *abcl-1.0.1-impl* "abcl-1.0.1-svn-13750-13751-fasl38-linux-java")
(defparameter *last-abcl* "abcl-1.1.0-dev-svn-14149-fasl39-linux-java")

(defun print-abcl-page (failures)
  (let* ((last-quicklisp "quicklisp 2012-09-09")
         (last-abcl-fails (remove-if-not (lambda (failure)
                                           (and (string= (lisp failure) *last-abcl*)
                                                (string= (lib-world failure) last-quicklisp)))
                                         failures))
         (abcl-1.0.1-fails (remove-if-not (lambda (failure)
                                            (and (string= (lisp failure) *abcl-1.0.1-impl*)
                                                 (string= (lib-world failure) last-quicklisp)))
                                          failures))
         (diff (set-exclusive-or last-abcl-fails
                                 abcl-1.0.1-fails
                                 :test (lambda (fail-a fail-b)
                                         (and (eq (libname fail-a) (libname fail-b))
                                              (equal (fail-spec fail-a) (fail-spec fail-b)))))))
    (print-pivot "abcl.html"
                 diff
                 (list #'libname) (list #'string<)
                 (list #'lib-world #'lisp) (list #'string> #'string<)
                 (lambda (out cell-data)
                   (dolist (fail cell-data)
                     (format out "~A</br>" (failure-log-link fail #'fail-spec)))))))


;;; Usage
#|
git clone git@github.com:cl-test-grid/cl-test-grid.git
git clone git@github.com:cl-test-grid/cl-test-grid-results.git

(pushnew "cl-test-grid/" asdf:*central-registry* :test #'equal)
(ql:quickload :test-grid-reporting)

(let* ((db (test-grid-data:read-db))
       (all-results (select db))
       (all-failures (list-failures all-results)))
  (test-grid-reporting::print-abcl-page all-failures))

 the result is stored in cl-test-grid/reports-generated/abcl.html

|#

