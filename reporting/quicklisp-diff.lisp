;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun result-spec-test (result)
  "Consider result-spec as a pair of test and outcome.
For example (:load \"some-system\" :ok),
here (:load \"some-system\") is the test and :ok is the outcome.
Or (:whole-test-suite :ok) - :whole-test-suite is a test
and :ok is an outcome.

Returns test part of the result-spec."

  (case (car (result-spec result))
    (:whole-test-suite :whole-test-sute)
    (otherwise (subseq (result-spec result) 0 2)))

  ;; TODO: the result-spec syntax is irregular
  ;; because test is sometime specified by two values -
  ;; (:load "some-system"), and sometimes just as
  ;; :whole-test-suite. For better regularity
  ;; it would be good to add test suite name
  ;; (actuall the project name, because
  ;; we merge all the test suites of a project
  ;; into a single testsuite).
  ;;
  ;; In short. Consider changing
  ;; (:whole-test-site :ok) to
  ;; (:whole-test-suite :alexandria :ok).
  )

(defun print-quicklisp-diff-report (report-file
                                    all-results
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
                                               (gethash key old-ql-tests)))))
         ;; now compute the diff between the results of two quicklisps,
         ;; considering only results from lisps tested on both versions.
         (diff (my-time ("fast-exclusive-or...")
                 (fast-exclusive-or (subset new-ql-results tested-on-both-lib-worlds-p)
                                    (subset old-ql-results tested-on-both-lib-worlds-p)
                                    :test #'equal
                                    :key (lambda (result)
                                           (list (libname result)
                                                 (lisp result)
                                                 (result-spec result))))))
         ;; comparator function whch guarantees that
         ;; the old-quicklisp is always in the left column.
         (two-lib-worlds (list old-quicklisp new-quicklisp))
         (lib-world-comparator (lambda (lib-world-a lib-world-b)
                                 (< (position lib-world-a two-lib-worlds :test #'string=)
                                    (position lib-world-b two-lib-worlds :test #'string=)))))
    (my-time ("print-pivot...")
      (print-pivot report-file
                   diff
                   :rows '((lisp string<) (libname string<))
                   :cols `((lib-world ,lib-world-comparator))
                   :cell-printer #'results-cell-printer))))
