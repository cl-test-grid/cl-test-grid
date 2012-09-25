;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defparameter *last-ecl-version* "ce653d88")
(defparameter *last-ecl-quicklisp* "quicklisp 2012-08-11")

(defun last-ecl-p (result)
  (and (search "ecl" (lisp result))
       (search *last-ecl-version* (lisp result))
       (string= *last-ecl-quicklisp* (lib-world result))))

(defun ecl-abnormal-results (db)
  ;;; select interesting test results from the DB
  (let ((problems (select db :where (lambda (result)
                                      (and (last-ecl-p result)
                                           (member (status result) '(:load-failed :crash :timeout)))))))
    ;; order by lisp, status, libname
    (sort problems #'string<
          :key (lambda (record)
                 (format nil "~A~A~A"
                         (lisp record)
                         (status record)
                         (libname record))))))

;;; Rendering ECL abnormal results HTML page
(defun print-ecl-results (destination results)
  (format destination
          "<code><pre>~{~{~A ~A ~A~}~%~}</pre></code>"
          (mapcar (lambda (result)
                    (list (lisp result) (status result) (log-link result 'libname)))
                  results)))

(defun print-ecl-report (out db)
  (report-page out
               "ECL Abnormal Results"
               (with-output-to-string (s)
                 (format s "<h3>Abnormal test results for ECL ~A on quicklisp ~A</h3>~%"
                         *last-ecl-version* *last-ecl-quicklisp*)
                 (print-ecl-results s (ecl-abnormal-results db)))))

;;; Filtered pivot reports - contain only ECL data,
;;; or only abnormal ecl data, with various rotations

(defun print-ecl-pivots (db)
  (let ((all-results (build-joined-index db :where #'last-ecl-p))
        (abnormal-results (build-joined-index db :where (lambda (result)
                                                          (and (last-ecl-p result)
                                                               (member (status result) '(:load-failed :crash :timeout)))))))
    (flet ((print-report (result-index
                          filename
                          row-fields row-fields-sort-predicates
                          col-fields col-fields-sort-predicates)
             (with-report-file (out filename)
               (pivot-report-old out
                                 result-index
                                 row-fields row-fields-sort-predicates
                                 col-fields col-fields-sort-predicates))))

      (print-report all-results
                    "ecl-pivot_lisp_ql-lib.html"
                    '(:lisp) (list #'string<)
                    '(:lib-world :libname) (list #'string> #'string<))
      (print-report all-results
                    "ecl-pivot_lib_ql-lisp.html"
                    '(:libname) (list #'string<)
                    '(:lib-world :lisp) (list #'string> #'string<))
      (print-report abnormal-results
                    "ecl-pivot-abnormal_lisp_ql-lib.html"
                    '(:lisp) (list #'string<)
                    '(:lib-world :libname) (list #'string> #'string<))
      (print-report abnormal-results
                    "ecl-pivot-abnormal_lib_ql-lisp.html"
                    '(:libname) (list #'string<)
                    '(:lib-world :lisp) (list #'string> #'string<)))))

(defun print-ecl-pages (db all-failures)
  (with-report-file (out "ecl-abnormal-results.html")
    (print-ecl-report out db))
  (print-ecl-pivots db)
  (print-load-failures all-failures
                       "ecl-12.7.1-ce653d88-linux-x86-lisp-to-c"
                       "quicklisp 2012-09-09"
                       "ecl-load-failures.html"))
