;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

;; -------------- the reporting source code directory -----------;;
(defun src-dir()
  (asdf:system-relative-pathname :test-grid-reporting #P"reporting/"))

(defun src-file (file-name)
  (merge-pathnames file-name (src-dir)))

;; ------ file system location for generated reports ------

(defun reports-dir ()
  (merge-pathnames "reports-generated/"
                   (merge-pathnames #P"../" (src-dir))))

(defun with-report-file-impl (filename handler-func)
  (let ((reports-dir (reports-dir)))
    (with-open-file (out (merge-pathnames filename reports-dir)
                         :direction :output
                         :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (funcall handler-func out))))

(defmacro with-report-file ((out-stream-var filename) &body body)
  `(with-report-file-impl ,filename #'(lambda (,out-stream-var) ,@body)))

;;; =========== print all the reports at once =============

(defun filter-lib-results (db predicate)
  (list :version (getf db :version)
        :runs (mapcar (lambda (run)
                        (list :descr (getf run :descr)
                              :results (remove-if-not (lambda (lib-result)
                                                        (funcall predicate lib-result run))
                                                      (getf run :results))))
                      (getf db :runs))))

(defun generate-reports (db)
  (let* (;; Old reports can work only with lib-result objects representing
         ;; testsuite results, but not load tests.
         ;; Compute filtered DB where only testsuite results are persent.
         (filtered-db (filter-lib-results db (lambda (lib-result test-run)
                                               (declare (ignore test-run))
                                               (getf lib-result :status))))
         (all-results (select db))
         (all-failures (list-failures all-results)))

    (with-report-file (out "test-runs-report.html")
      (write-sequence (test-runs-report filtered-db) out))

    (with-report-file (out "export.csv")
      (export-to-csv out filtered-db))

    (let* ((last-lib-worlds (largest 'lib-world filtered-db :count 3))
           (joined-index (build-joined-index filtered-db :where (lambda (record)
                                                                  (member (lib-world record)
                                                                          last-lib-worlds
                                                                          :test #'string=)))))
      (print-pivot-reports joined-index)

      (with-report-file (out "quicklisp-diff-old.html")
        (print-all-quicklisps-diff-report out joined-index)))

    (time (print-quicklisp-diff-report all-failures))
    (time (print-ecl-pages filtered-db all-failures))
    (time (print-abcl-page all-failures))))

