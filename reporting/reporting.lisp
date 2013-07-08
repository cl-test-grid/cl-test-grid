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

(assert (string= "" (reports-root-dir-relative-path "abc.html")))
(assert (string= "../" (reports-root-dir-relative-path "demo/abc.html")))
(assert (string= "../../" (reports-root-dir-relative-path "demo/subdir/abc.html")))

(defun with-report-file-impl (filename handler-func)
  (let* ((reports-dir (reports-dir))
         (output-file (merge-pathnames filename reports-dir)))
    (ensure-directories-exist output-file)
    (with-open-file (out output-file
                         :direction :output
                         :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (funcall handler-func out))))

(defmacro with-report-file ((out-stream-var filename) &body body)
  `(with-report-file-impl ,filename #'(lambda (,out-stream-var) ,@body)))

(defun save-report (file report-str)
  (with-report-file (out file)
    (write-sequence report-str out))
  nil)

;;; =========== print all the reports at once =============

(defun filter-lib-results (db predicate)
  (test-grid-data::updated-plist db :runs
                                 (mapcar (lambda (run)
                                           (tg-data::updated-plist run :results
                                                                   (remove-if-not  (lambda (lib-result)
                                                                                     (funcall predicate lib-result run))
                                                                                   (getf run :results))))
                                         (getf db :runs))))

(defun generate-reports (db)
  (let* ((all-results (my-time ("list-results...")
                        (list-results db)))
         (last-three-quicklisps (largest #'lib-world all-results :count 3))
         (new-quicklisp (first last-three-quicklisps))
         (prev-quicklisp (second last-three-quicklisps))
         ;; Old reports can work only with lib-result objects representing
         ;; testsuite results, but not load tests.
         ;; Compute filtered DB where only testsuite results are persent.
         (filtered-db (filter-lib-results db (lambda (lib-result test-run)
                                               (declare (ignore test-run))
                                               (getf lib-result :status)))))

    (my-time ("test runs..")
      (with-report-file (out "test-runs-report.html")
        (write-sequence (test-runs-report filtered-db) out)))

    (my-time ("CSV ...")
      (with-report-file (out "export.csv")
        (export-to-csv out filtered-db)))

    (my-time ("old pivot reports...~%")
      (print-old-pivots filtered-db last-three-quicklisps))

    (my-time ("Quicklisp diff...~%")
      (print-quicklisp-diff-report "quicklisp-diff.html"
                                   all-results
                                   prev-quicklisp
                                   new-quicklisp))

    (my-time ("Quicklisp diff2...~%")
      (print-quicklisp-diff-report2 "quicklisp-diff2.html"
                                    all-results
                                    prev-quicklisp
                                    new-quicklisp))
    (my-time ("library reports...")
      (print-library-reports all-results))

    (print-compiler-reports all-results new-quicklisp)))

(defun print-compiler-reports (all-results new-quicklisp)
  (my-time ("ECL bytecode load failures...~%")
    (print-load-failures "ecl-load-failures-bytecode.html"
                         all-results
                         "ecl-12.12.1-unknown-linux-x86-bytecode"
                         new-quicklisp))
  (my-time ("ECL lisp-to-c load failures...~%")
    (print-load-failures "ecl-load-failures-lisp-to-c.html"
                         all-results
                         "ecl-12.12.1-unknown-linux-x86-lisp-to-c"
                         new-quicklisp))

  (my-time ("ABCL load failures...~%")
    (print-load-failures "abcl-load-failures.html"
                         all-results
                         "abcl-1.1.1-fasl39-linux-x86"
                         new-quicklisp))

  (my-time ("CCL load failures...~%")
    (print-load-failures "ccl-load-failures.html"
                         all-results
                         "ccl-1.8-f95-linux-x86"
                         new-quicklisp))
  (my-time ("SBCL load failures...~%")
    (print-load-failures "sbcl-load-failures.html"
                         all-results
                         "sbcl-1.1.1-linux-x86"
                         new-quicklisp))

  ;; ACL 8.2 license has expired and 9.0 doesn't run on my linux
  ;;(my-time ("ACL load failures...~%")
  ;;  (print-load-failures "acl-load-failures.html"
  ;;                       all-results
  ;;                       "acl-8.2a-linux-x86"
  ;;                       new-quicklisp))

  ;; CMUCL results are not yet collected
  ;; (my-time ("CMUCL load failures...~%")
  ;;   (print-load-failures "cmucl-load-failures.html"
  ;;                        all-results
  ;;                        "cmu-20c_release-20c__20c_unicode_-linux-x86"
  ;;                        new-quicklisp))
  )
