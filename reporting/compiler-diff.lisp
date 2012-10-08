;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-compiler-diff (report-file
                            all-failures
                            last-quicklisp
                            new-lisp old-lisp)
  "Prints pivot with difference between failures
of two copilers - NEW-LISP and OLD-LISP - on the
lib-world specified by LAST-QUICKLISP. The
resulting .html file is save to
reports-generated/<REPORT-FILE>."
  (let* ((new-lisp-fails (subset all-failures
                                 (lambda (failure)
                                   (and (string= (lib-world failure) last-quicklisp)
                                        (search new-lisp (lisp failure))))))
         (old-lisp-fails (subset all-failures
                                 (lambda (failure)
                                   (and (string= (lib-world failure) last-quicklisp)
                                        (search old-lisp (lisp failure))))))
         (diff (fast-exclusive-or new-lisp-fails
                                  old-lisp-fails
                                  :test #'equal
                                  :key (lambda (fail)
                                         (list (libname fail) (fail-spec fail))))))
    (print-pivot report-file
                 diff
                 :rows '((libname string<))
                 :cols '((lib-world string>) (lisp string<))
                 :cell-printer (lambda (out cell-data)
                                 (dolist (fail cell-data)
                                   (format out "~A</br>" (failure-log-link fail #'fail-spec)))))))