;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun quicklisp-diff-items (all-results
                             old-quicklisp
                             new-quicklisp)
  (let* ((new-ql-results (my-time ("last-ql-results...")
                           (subset all-results
                                   (lambda (result)
                                     (string= (lib-world result) new-quicklisp)))))
         (old-ql-results (my-time ("prev-ql-results...")
                           (subset all-results
                                   (lambda (result)
                                     (string= (lib-world result) old-quicklisp)))))
         ;; Only consider results of lisp/libraries/tests
         ;; performed on both quicklisps, so that set-exclusive-or
         ;; computed below contains results because test has different
         ;; outcome on two quicklisp, but not because one of the quicklisps
         ;; doesn't have this combination tested at all.
         ;;
         ;; Also, it is convenient to consider only results tested on the
         ;; same machines, because different machines may have/have not
         ;; native libraries installed, have different default locale
         ;; (which affects source code reading of some libraries).
         ;; As we don't have any real identifier for machine, we approximate
         ;; it by user contact email - combined with lisp implementation identifier,
         ;; which includes OS and platform, the chances are high that the same
         ;; combination is from the same machine.
         (new-ql-tests (group-by new-ql-results (list #'lisp #'libname #'result-spec-test #'contact-email)))
         (old-ql-tests (group-by old-ql-results (list #'lisp #'libname #'result-spec-test #'contact-email)))
         (tested-on-both-lib-worlds-p (lambda (result)
                                        (let ((key (list (lisp result)
                                                         (libname result)
                                                         (result-spec-test result)
                                                         (contact-email result))))
                                          (and (gethash key new-ql-tests)
                                               (gethash key old-ql-tests))))))
    ;; now compute the diff between the results of two quicklisps,
    ;; considering only results from lisps tested on both versions.
    (my-time ("fast-exclusive-or...")
      (fast-exclusive-or (subset new-ql-results tested-on-both-lib-worlds-p)
                         (subset old-ql-results tested-on-both-lib-worlds-p)
                         :test #'equal
                         :key (lambda (result)
                                (list (libname result)
                                      (lisp result)
                                      (result-spec result)))))))

(defun print-quicklisp-diff-report2 (report-file
                                     all-results
                                     old-quicklisp
                                     new-quicklisp)
  (print-pivot report-file
               (quicklisp-diff-items all-results old-quicklisp new-quicklisp)
               :rows '((libname string<) (lisp string<))
               :cols `((lib-world ,(tg-utils::ordering-comparator (list old-quicklisp new-quicklisp) #'string=)))
               :cell-printer #'results-cell-printer))

(defun tests-failed-on-new (all-results old-lib-world new-lib-world)
  "Results from the two lib worlds, which have failures on the NEW-LIB-WORLD"
  (flet ((test-specification (result)
           (list (tg-rep::libname result)
                 (tg-rep::lisp result)
                 (tg-rep::result-spec-test result)))
         (fill-hash (hash-table list &key (key #'identity))
           (dolist (elem list)
             (setf (gethash (funcall key elem) hash-table) t))
           hash-table))
    (let* ((failed-new-results (tg-rep::subset all-results
                                               (lambda (r)
                                                 (and (string= (tg-rep::lib-world r) new-lib-world)
                                                      (tg-rep::failure-p r)))))
           (test-specs (fill-hash (make-hash-table :size (length failed-new-results)
                                                   :test #'equal)
                                  failed-new-results
                                  :key #'test-specification)))
      (tg-rep::subset all-results
                      (lambda (r)
                        (and (member (tg-rep::lib-world r)
                                     (list old-lib-world new-lib-world)
                                     :test #'string=)
                             (gethash (test-specification r) test-specs)))))))
