;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-compiler-diff (report-file
                            all-results
                            quicklisp1
                            lisp1
                            lisp2
                            &optional (quicklisp2 quicklisp1))
  "Prints pivot with difference between results
of two copilers - NEW-LISP and OLD-LISP - on the
lib-world specified by LAST-QUICKLISP. The
resulting .html file is save to
reports-generated/<REPORT-FILE>."
  (let* ((lisp1-results (or (subset all-results
                                    (lambda (result)
                                      (and (string= (lib-world result) quicklisp1)
                                           (search lisp1 (lisp result)))))
                            (cerror "Continue with empty result set."
                                    "No results found for ~A and ~A."
                                    lisp1 quicklisp1)))
         (lisp2-results (or (subset all-results
                                    (lambda (result)
                                      (and (string= (lib-world result) quicklisp2)
                                           (search lisp2 (lisp result)))))
                            (cerror "Continue with empty result set."
                                    "No results found for ~A and ~A."
                                    lisp2 quicklisp2)))
         (diff (fast-exclusive-or lisp1-results
                                  lisp2-results
                                  :test #'equal
                                  :key (lambda (result)
                                         (list (libname result) (result-spec result))))))
    (print-pivot report-file
                 diff
                 :rows '((libname string<))
                 :cols `((lib-world ,(tg-utils::ordering-comparator (list quicklisp1 quicklisp2) #'string=))
                         (lisp ,(tg-utils::ordering-comparator (list lisp1 lisp2) #'string=)))
                 :cell-printer #'results-cell-printer)))
