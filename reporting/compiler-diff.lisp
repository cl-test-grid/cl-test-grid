;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-compiler-diff (all-failures
                            last-quicklisp
                            last-lisp prev-lisp
                            report-file-name)
  "Prints pivot with difference between failures
of two copilers (LAST-LISP PREV-LISP) on the
lib-world specified by LAST-QUICKLISP. The
resulting .html file is save to
reports-generated/<REPORT-FILE-NAME>."
  (let* ((last-lisp-fails (remove-if-not (lambda (failure)
                                           (and (string= (lib-world failure) last-quicklisp)
                                                (search last-lisp (lisp failure))))
                                         all-failures))
         (prev-lisp-fails (remove-if-not (lambda (failure)
                                           (and (string= (lib-world failure) last-quicklisp)
                                                (search prev-lisp (lisp failure))))
                                         all-failures))
         (diff (fast-exclusive-or last-lisp-fails
                                  prev-lisp-fails
                                  :test #'equal
                                  :key (lambda (fail)
                                         (list (libname fail) (fail-spec fail))))))
    (print-pivot report-file-name
                 diff
                 :rows '((libname string<))
                 :cols '((lib-world string>) (lisp string<))
                 :cell-printer (lambda (out cell-data)
                                 (dolist (fail cell-data)
                                   (format out "~A</br>" (failure-log-link fail #'fail-spec)))))))