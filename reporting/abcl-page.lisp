;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defparameter *abcl-1.0.1-impl* "abcl-1.0.1-svn-13750-13751-fasl38-win-x64")
(defparameter *abcl-diff-quicklisp* "quicklisp 2012-08-11")

;;; Finding difference between the test results
(defclass abcl-diff ()
  ((regressions :documentation "List of pairs (<old result> . <new result>) where new result has a regression."
                :type list
                :accessor regressions
                :initform nil)
   (improvements :documentation "List of pairs (<old result> . <new result> where new result is better than old result"
                 :type list
                 :accessor improvements
                 :initform nil)))

(defun abcl-diff (db last-abcl-dev-identifier last-quicklisp)
  (let ((old-results (select db :where (lambda (result)
                                         (and (string= (lisp result) *abcl-1.0.1-impl*)
                                              (string= (lib-world result) last-quicklisp)))))
        (new-results (select db :where (lambda (result)
                                         (and (string= (lisp result) last-abcl-dev-identifier)
                                              (string= (lib-world result) last-quicklisp)))))
        (diff (make-instance 'abcl-diff)))
    (flet ((comparable-p (old-result new-result)
             (eq (libname old-result)
                 (libname new-result))))
      (dolist (new-result new-results)
        (dolist (old-result old-results)
          (when (and (comparable-p old-result new-result)
                     (not (equal (status old-result)
                                 (status new-result))))
            (cond ((has-regressions-p (status new-result)
                                      (status old-result))
                   (push (cons old-result new-result)
                         (regressions diff)))
                  ((has-regressions-p (status old-result)
                                      (status new-result))
                   (push (cons old-result new-result)
                         (improvements diff)))
                  (t (error "Two statuses are not EQUAL, but neither has regressions compared to the other: ~A ~A"
                            (status old-result) (status new-result))))))))
    diff))

;;; Rendering the diff into HTML page
(defun print-abcl-diff (out diff last-abcl-dev-identifier last-quicklisp)
  (format out "<h3>Difference in test results between ~A and ~A on ~A.</h3>~%"
          *abcl-1.0.1-impl* last-abcl-dev-identifier last-quicklisp)
  (flet ((print-table (pair-list)
           (cond ((null pair-list)
                  (format out "None~%"))
                 (t
                  (format out "<table border=\"1\">~%")
                  (format out "<tr><th>library</th><th>~A</th><th>~A</th></tr>~%"
                          *abcl-1.0.1-impl* last-abcl-dev-identifier)
                  (dolist (item pair-list)
                    (let ((old-result (car item))
                          (new-result (cdr item)))
                      (format out "<tr><th>~(~A~)</th><td>~A</td><td>~A</td></tr>~%"
                              (libname old-result)
                              (log-link old-result 'status)
                              (log-link new-result 'status))))
                  (format out "</table>~%")))))
    (format out "<h4>Regressions:</h4>~%")
    (print-table (regressions diff))
    (format out "<h4>Improvements:</h4>~%")
    (print-table (improvements diff)))
  (format out "<p>~%")
  (format out "Source code: <a href=\"https://github.com/cl-test-grid/cl-test-grid/blob/master/reporting/abcl-page.lisp\">abcl-page.lisp</a>.~%")
  (format out "The code accesses test results using siple API defined here: <a href=\"https://github.com/cl-test-grid/cl-test-grid/blob/master/reporting/do-results.lisp\">do-results.lisp</a> and uses several HTML printing helper functions.~%")
  (format out "</p>~%"))

(defun print-abcl-page (db)
  (let ((last-abcl-dev-identifier (first (largest #'lisp db :count 1
                                                  :where (lambda (result)
                                                           (search "abcl" (lisp result))))))
        (last-quicklisp (first (largest #'lib-world db :count 1))))
    (with-report-file (out "abcl.html")
      (report-page out
                   "ABCL 1.0.1 vs 1.1.0"
                   (with-output-to-string (s)
                     (print-abcl-diff s
                                      (abcl-diff db last-abcl-dev-identifier last-quicklisp)
                                      last-abcl-dev-identifier
                                      last-quicklisp))))))

;;; Usage
#|
git clone git@github.com:cl-test-grid/cl-test-grid.git
git clone git@github.com:cl-test-grid/cl-test-grid-results.git

(pushnew "cl-test-grid/" asdf:*central-registry* :test #'equal)
(ql:quickload :test-grid-reporting)

(test-grid-reporting::print-abcl-page (test-grid-data:read-db))

 the result is stored in cl-test-grid/reports-generated/abcl.html

|#

