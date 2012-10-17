;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-compiler-diff (report-file
                            all-results
                            last-quicklisp
                            old-lisp new-lisp)
  "Prints pivot with difference between results
of two copilers - NEW-LISP and OLD-LISP - on the
lib-world specified by LAST-QUICKLISP. The
resulting .html file is save to
reports-generated/<REPORT-FILE>."
  (let* ((new-lisp-results (or (subset all-results
                                       (lambda (result)
                                         (and (string= (lib-world result) last-quicklisp)
                                              (search new-lisp (lisp result)))))
                               (cerror "Continue with empty result set."
                                       "No results found for ~A and ~A."
                                       new-lisp last-quicklisp)))
         (old-lisp-results (or (subset all-results
                                       (lambda (result)
                                         (and (string= (lib-world result) last-quicklisp)
                                              (search old-lisp (lisp result)))))
                               (cerror "Continue with empty result set."
                                       "No results found for ~A and ~A."
                                       old-lisp last-quicklisp)))
         (diff (fast-exclusive-or new-lisp-results
                                  old-lisp-results
                                  :test #'equal
                                  :key (lambda (result)
                                         (list (libname result) (result-spec result)))))
         ;; we can not be sure that (string< old-lisp new-lisp) == t,
         ;; so create another comparator fuction whch guarantees that
         ;; the old-lisp is always in the left column.
         (two-lisps (list old-lisp new-lisp))
         (lisp-comparator (lambda (lisp-a lisp-b)
                            (< (position lisp-a two-lisps :test #'string=)
                               (position lisp-b two-lisps :test #'string=)))))
    (print-pivot report-file
                 diff
                 :rows '((libname string<))
                 :cols `((lib-world string>) (lisp ,lisp-comparator))
                 :cell-printer #'results-cell-printer)))
