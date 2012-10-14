;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-compiler-diff (report-file
                            all-results
                            last-quicklisp
                            new-lisp old-lisp)
  "Prints pivot with difference between results
of two copilers - NEW-LISP and OLD-LISP - on the
lib-world specified by LAST-QUICKLISP. The
resulting .html file is save to
reports-generated/<REPORT-FILE>."
  (let* ((new-lisp-results (subset all-results
                                   (lambda (result)
                                     (and (string= (lib-world result) last-quicklisp)
                                          (search new-lisp (lisp result))))))
         (old-lisp-results (subset all-results
                                   (lambda (result)
                                     (and (string= (lib-world result) last-quicklisp)
                                          (search old-lisp (lisp result))))))
         (diff (fast-exclusive-or new-lisp-results
                                  old-lisp-results
                                  :test #'equal
                                  :key (lambda (result)
                                         (list (libname result) (result-spec result))))))
    (print-pivot report-file
                 diff
                 :rows '((libname string<))
                 :cols '((lib-world string>) (lisp string<))
                 :cell-printer #'results-cell-printer)))