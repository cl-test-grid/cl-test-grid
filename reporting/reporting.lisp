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
                                           (test-grid-data::updated-plist run :results
                                                                          (remove-if-not (lambda (lib-result)
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
         (all-results (my-time ("list-results...")
                        (list-results db)))
         (last-two-quicklisps (largest #'lib-world all-results :count 2))
         (new-quicklisp (first last-two-quicklisps))
         (prev-quicklisp (second last-two-quicklisps)))

    (my-time ("test runs..")
      (with-report-file (out "test-runs-report.html")
        (write-sequence (test-runs-report filtered-db) out)))

    (my-time ("CSV ...")
      (with-report-file (out "export.csv")
        (export-to-csv out filtered-db)))

    (let* ((last-lib-worlds (largest-old 'lib-world filtered-db :count 3))
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
                                   all-results
                                   prev-quicklisp
                                   new-quicklisp))

    (my-time ("library reports...")
      (print-library-reports all-results))

    (print-demo-reports all-results)
    (print-compiler-reports all-results new-quicklisp)))

(defun print-compiler-reports (all-results new-quicklisp)
  (my-time ("ECL bytecode load failures...~%")
    (print-load-failures "ecl-load-failures-bytecode.html"
                         all-results
                         "ecl-12.7.1-9e0d6e50-linux-x64-bytecode"
                         new-quicklisp))
  (my-time ("ECL lisp-to-c load failures...~%")
    (print-load-failures "ecl-load-failures-lisp-to-c.html"
                         all-results
                         "ecl-12.7.1-9e0d6e50-linux-x64-lisp-to-c"
                         new-quicklisp))

  ;; old ECL version hasn't (yet) been tested on the
  ;; new quicklisp dist version, so we have nothing to
  ;; compare with
  ;;
  ;; (let ((new-ecl "ecl-12.7.1-bca1f405-linux-x86-lisp-to-c")
  ;;       (old-ecl "ecl-12.7.1-ce653d88-linux-x86-lisp-to-c"))
  ;;   (print-compiler-diff "ecl-lisp-to-c.html"
  ;;                        all-results
  ;;                        new-quicklisp
  ;;                        old-ecl
  ;;                        new-ecl))
  ;; (let ((new-ecl "ecl-12.7.1-bca1f405-linux-x86-bytecode")
  ;;       (old-ecl "ecl-12.7.1-ce653d88-linux-x86-bytecode"))
  ;;   (print-compiler-diff "ecl-bytecode.html"
  ;;                        all-results
  ;;                        new-quicklisp
  ;;                        old-ecl
  ;;                        new-ecl))

  (let ((new-abcl "abcl-1.2.0-dev-svn-14300-fasl39-linux-x86")
        (old-abcl "abcl-1.0.1-svn-13750-13751-fasl38-linux-java"))
    (my-time ("ABCL diff...~%")
      (print-compiler-diff "abcl.html"
                           all-results
                           new-quicklisp
                           old-abcl
                           new-abcl))
    (my-time ("ABCL load failures...~%")
      (print-load-failures "abcl-load-failures.html"
                           all-results
                           new-abcl
                           new-quicklisp)))
  (my-time ("CCL load failures...~%")
    (print-load-failures "ccl-load-failures.html"
                         all-results
                         "ccl-1.8-f95-linux-x86"
                         new-quicklisp))
  (my-time ("ACL load failures...~%")
    (print-load-failures "acl-load-failures.html"
                         all-results
                         "acl-8.2a-linux-x86"
                         new-quicklisp))
  ;; CMUCL results are not yet collected
  ;; (my-time ("CMUCL load failures...~%")
  ;;   (print-load-failures "cmucl-load-failures.html"
  ;;                        all-results
  ;;                        "cmu-20c_release-20c__20c_unicode_-linux-x86"
  ;;                        new-quicklisp))
  (my-time ("SBCL load failures...~%")
    (print-load-failures "sbcl-load-failures.html"
                         all-results
                         "sbcl-1.1.1-linux-x86"
                         new-quicklisp)))
