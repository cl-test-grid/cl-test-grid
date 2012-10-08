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

(defun reports-root-dir-relative-path (report-file)
  (let ((dir (pathname-directory report-file)))
    (if dir
        (progn
          (assert (eq :relative (car dir)))
          (format nil "~{../~*~}" (cdr dir)))
        "")))

(assert (string= "" (reports-dir-relative-path "abc.html")))
(assert (string= "../" (reports-dir-relative-path "demo/abc.html")))
(assert (string= "../../" (reports-dir-relative-path "demo/subdir/abc.html")))

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
         (filtered-db (my-time ("filter-lib-results...")
                        (filter-lib-results db (lambda (lib-result test-run)
                                                 (declare (ignore test-run))
                                                 (getf lib-result :status)))))
         (all-failures (my-time ("list-failures...")
                         (list-failures db))))

    (my-time ("test runs..")
      (with-report-file (out "test-runs-report.html")
        (write-sequence (test-runs-report filtered-db) out)))

    (my-time ("CSV ...")
      (with-report-file (out "export.csv")
        (export-to-csv out filtered-db)))

    (let* ((last-lib-worlds (largest 'lib-world filtered-db :count 3))
           (joined-index (my-time ("build-joined-index...")
                           (build-joined-index filtered-db :where (lambda (record)
                                                                    (member (lib-world record)
                                                                            last-lib-worlds
                                                                            :test #'string=))))))
      (my-time ("pivot reports...~%")
        (print-old-pivots joined-index))

      (my-time ("old Quicklisp diff report...~%")
        (with-report-file (out "quicklisp-diff-old.html")
          (print-all-quicklisps-diff-report out joined-index))))

    (my-time ("Quicklisp diff...~%")
      (print-quicklisp-diff-report "quicklisp-diff.html"
                                   all-failures
                                   "quicklisp 2012-09-09"
                                   "quicklisp 2012-08-11"))

    (my-time ("ECL load failures...~%")
      (print-load-failures "ecl-load-failures.html"
                           all-failures
                           "ecl-12.7.1-ce653d88-linux-x86-lisp-to-c"
                           "quicklisp 2012-09-09"))

    (let ((last-abcl "abcl-1.1.0-dev-svn-14164-fasl39-linux-java")
          (abcl-1.0.1 "abcl-1.0.1-svn-13750-13751-fasl38-linux-java"))
      (my-time ("ABCL diff...~%")
        (print-compiler-diff "abcl.html"
                             all-failures
                             "quicklisp 2012-09-09"
                             last-abcl
                             abcl-1.0.1))
      (my-time ("ABCL load failures...~%")
        (print-load-failures "abcl-load-failures.html"
                             all-failures
                             last-abcl
                             "quicklisp 2012-09-09")))
    (my-time ("CCL load failures...~%")
      (print-load-failures "ccl-load-failures.html"
                           all-failures
                           "ccl-1.8-f95-linux-x86"
                           "quicklisp 2012-09-09"))
    (my-time ("ACL load failures...~%")
      (print-load-failures "acl-load-failures.html"
                           all-failures
                           "acl-8.2a-linux-x86"
                           "quicklisp 2012-09-09"))
    (my-time ("CMUCL load failures...~%")
      (print-load-failures "cmucl-load-failures.html"
                           all-failures
                           "cmu-20c_release-20c__20c_unicode_-linux-x86"
                           "quicklisp 2012-09-09"))
    (my-time ("SBCL load failures...~%")
      (print-load-failures "sbcl-load-failures.html"
                           all-failures
                           "sbcl-1.0.57-linux-x86"
                           "quicklisp 2012-09-09"))))
