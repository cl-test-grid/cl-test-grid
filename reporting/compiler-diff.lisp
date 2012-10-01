;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-compiler-diff (all-failures
                            last-quicklisp
                            last-lisp prev-lisp
                            report-file-name)
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
       (all-results (test-grid-reporting::select db))
       (all-failures (test-grid-reporting::list-failures all-results)))
  (print-compiler-diff all-failures "quicklisp 2012-09-09"
                       "ecl-12.7.1-91356f2d-linux-x86-lisp-to-c"
                       "ecl-12.7.1-ce653d88-linux-x86-lisp-to-c"
                       "ecl-lisp-to-c-diff.html")
  (print-compiler-diff all-failures "quicklisp 2012-09-09"
                       "ecl-12.7.1-91356f2d-linux-x86-bytecode"
                       "ecl-12.7.1-ce653d88-linux-x86-bytecode"
                       "ecl-bytecode-diff.html"))

The result is stored in cl-test-grid/reports-generated/ecl-lisp-to-c-diff.html
and cl-test-grid/reports-generated/ecl-bytecode-iff.html

|#
