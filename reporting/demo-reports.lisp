;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; The reports described on the results-overview.html.

(in-package #:test-grid-reporting)

(defun print-demo-reports (all-results)
  (let ((some-results (subset all-results
                              (lambda (result)
                                (and (member (libname result) '(:alexandria :let-plus))
                                     (member (lib-world result)
                                             '("quicklisp 2012-09-09" "quicklisp 2012-08-11")
                                             :test #'string=))))))
    ;; pivots

    (print-pivot "demo/some-results.html"
                 some-results
                 :rows '((lib-world string>) (lisp string<))
                 :cols '((libname string<))
                 :cell-printer (lambda (out cell-data)
                                 (dolist (result cell-data)
                                   (format out
                                           "<a href=\"~a\">~a</a></br>"
                                           (log-uri result)
                                           (result-spec result)))))

    (print-pivot "demo/some-results2.html"
                 some-results
                 :rows '((lib-world string>) (lisp string<))
                 :cols '((libname string<))
                 :cell-printer #'results-cell-printer))


  ;; diff, compare results to find regressions
  (print-compiler-diff "demo/abcl-diff.html"
                       all-results
                       "quicklisp 2012-09-09"
                       "abcl-1.1.0-dev-svn-14157-fasl39-linux-java"
                       "abcl-1.0.1-svn-13750-13751-fasl38-linux-java")

  (print-quicklisp-diff-report "demo/quicklisp-diff.html"
                               all-results
                               "quicklisp 2012-09-09"
                               "quicklisp 2012-08-11")

  ;; failures by dependencies

  (print-load-failures "demo/ecl-load-failures.html"
                       all-results
                       "ecl-12.7.1-ce653d88-linux-x86-lisp-to-c"
                       "quicklisp 2012-09-09"))
