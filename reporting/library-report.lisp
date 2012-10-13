;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun print-library-reports (all-results)
  (let* ((last-quicklisps (largest #'lib-world all-results :count 2))
         (recent-results (subset all-results
                                 (lambda (r) (member (lib-world r)
                                                     last-quicklisps
                                                     :test #'string=))))
         (libnames (remove-duplicates (mapcar #'libname recent-results))))
    (dolist (libname libnames)
      (save-report (format nil "library/~(~A~).html" libname)
                   (library-report recent-results libname "../")))))

(defun library-report (all-results libname &optional (reports-root-dir-relative-path ""))
  (let* ((results (subset all-results (lambda (r) (eq libname (libname r)))))
         (test-case-results (subset all-results (lambda (r)
                                                  (and (eq libname (libname r))
                                                       (eq :test-case (first (result-spec r))))))))
    (with-output-to-string (str)
      (let ((html-template:*string-modifier* #'cl:identity))
        (html-template:fill-and-print-template
         (src-file "library-report-template.html")
         (list :library-name (html-template:escape-string (string-downcase libname))
               :results-table (pivot-table-html4 results
                                                 :cols '((lib-world string>))
                                                 :rows '((lisp string<))
                                                 :cell-printer #'results-cell-printer)
               :resuts-by-testcases (cond ((not (member libname test-grid-testsuites:*all-libs*))
                                           "The library does not have a testsuite adapter for CL Test Grid.")
                                          ((null test-case-results)
                                           "THere are no failed tescases.")
                                          (t (pivot-table-html4 test-case-results
                                                                :cols '((lib-world string>))
                                                                :rows `((,(lambda (r)
                                                                                  (format nil "~A"
                                                                                          (result-spec r)))
                                                                          string<))
                                                                :cell-printer (lambda (out cell-data)
                                                                                (results-cell-printer out cell-data #'lisp)))))
               :time (test-grid-agent::pretty-fmt-time (get-universal-time))
               :reports-root-dir-relative-path reports-root-dir-relative-path)
         :stream str)))))

