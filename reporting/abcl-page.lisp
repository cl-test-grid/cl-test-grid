;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defparameter *abcl-1.0.1-impl* "abcl-1.0.1-svn-13750-13751-fasl38-win-x64")
(defparameter *abcl-1.1.0-dev-impl* "abcl-1.1.0-dev-svn-14092-fasl39-win-x64")
(defparameter *abcl-diff-quicklisp* "quicklisp 2012-08-11")

;;; Finding difference between the test results
(defun abcl-diff (db)
  (let ((old-results (select db :where (lambda (result)
                                         (and (string= (lisp result) *abcl-1.0.1-impl*)
                                              (string= (lib-world result) *abcl-diff-quicklisp*)))))
        (new-results (select db :where (lambda (result)
                                         (and (string= (lisp result) *abcl-1.1.0-dev-impl*)
                                              (string= (lib-world result) *abcl-diff-quicklisp*)))))
        diff)
    (flet ((comparable-p (old-result new-result)
             (eq (libname old-result)
                 (libname new-result))))
      (dolist (new-result new-results)
        (dolist (old-result old-results)
          (when (and (comparable-p old-result new-result)
                     (not (equal (status old-result)
                                 (status new-result))))
            (push (cons old-result new-result)
                  diff)))))
    diff))

;;; Rendering the diff into HTML page
(defun print-abcl-diff (out diff)
  (format out "<h3>Difference in test results between ~A and ~A on ~A.</h3>~%"
          *abcl-1.0.1-impl* *abcl-1.1.0-dev-impl* *abcl-diff-quicklisp*)
  (format out "<table border=\"1\">~%")
  (format out "<tr><th>library</th><th>~A</th><th>~A</th></tr>~%"
          *abcl-1.0.1-impl* *abcl-1.1.0-dev-impl*)
  (dolist (item diff)
    (let ((old-result (car item))
          (new-result (cdr item)))
      (format out "<tr><th>~(~A~)</th><td>~A</td><td>~A</td></tr>~%"
              (libname old-result)
              (log-link old-result 'status)
              (log-link new-result 'status))))
  (format out "</table>~%")
  (format out "<p>~%")
  (format out "Source code: <a href=\"https://github.com/cl-test-grid/cl-test-grid/blob/master/reporting/abcl-page.lisp\">abcl-page.lisp</a>.~%")
  (format out "The code accesses test results using siple API defined here: <a href=\"https://github.com/cl-test-grid/cl-test-grid/blob/master/reporting/do-results.lisp\">do-results.lisp</a> and several HTML printing helper functions.~%")
  (format out "</p>~%"))

(defun print-abcl-page (db)
  (with-report-file (out "abcl.html")
    (report-page out
                 "ABCL 1.0.1 vs 1.1.0"
                 (with-output-to-string (s)
                   (print-abcl-diff s (abcl-diff db))))))

;;; Usage
#|
git clone git@github.com:cl-test-grid/cl-test-grid.git
git clone git@github.com:cl-test-grid/cl-test-grid-results.git

(pushnew "cl-test-grid/" asdf:*central-registry* :test #'equal)
(ql:quickload :test-grid-reporting)

(test-grid-reporting::print-abcl-page (test-grid-data:read-db))

 the result is stored in cl-test-grid/reports-generated/abcl.html

|#

