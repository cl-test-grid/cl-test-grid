;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun list-results (db)
  (mapcan #'results (list-lib-results db)))

(defun results (lib-result)
  (nconc (testsuite-results lib-result)
         (asdf-load-results lib-result)))

(defun testsuite-results (lib-result)
  (let* ((suite-status (status lib-result))
         (suite-results (etypecase suite-status
                          (keyword (list (list :whole-test-suite suite-status)))
                          (list (let* ((failures (getf suite-status :failed-tests))
                                       (expected-to-fail (getf suite-status :known-to-fail))
                                       (known-failures (intersection expected-to-fail
                                                                     failures
                                                                     :test #'string=))
                                       (just-failures (set-difference failures
                                                                      known-failures
                                                                      :test #'string=))
                                       (unexpected-oks (set-difference expected-to-fail
                                                                       failures
                                                                       :test #'string=)))
                                  (flet ((as-results (testcases status)
                                           (mapcar (lambda (testcase)
                                                     (list :test-case testcase status))
                                                   testcases)))
                                    (nconc (as-results just-failures :fail)
                                           (as-results known-failures :known-fail)
                                           (as-results unexpected-oks :unexpected-ok))))))))
    (mapcar (lambda (suite-result)
              (make-instance 'result :lib-result lib-result :result-spec suite-result))
            suite-results)))

(defun asdf-load-results (lib-result)
  (mapcar (lambda (load-result)
            (make-instance 'result
                           :lib-result lib-result
                           :result-spec (list :load
                                              (system-name load-result)
                                              (load-status load-result))
                           :load-result load-result))
          (load-results lib-result)))

(defclass result ()
  ((lib-result :type joined-lib-result :initarg :lib-result :reader lib-result)
   (load-result :type (or null list) :initarg :load-result :initform nil :reader load-result)
   (result-spec :type list :initarg :result-spec :reader result-spec)))

(defmethod lisp ((item result))
  (lisp (lib-result item)))
(defmethod lib-world ((item result))
  (lib-world (lib-result item)))
(defmethod libname ((item result))
  (libname (lib-result item)))
(defmethod log-blob-key ((item result))
  (if (load-result item)
      (log-blob-key (load-result item))
      (log-blob-key (lib-result item))))
(defmethod log-byte-length ((item result))
  (if (load-result item)
      (log-byte-length (load-result item))
      (log-byte-length (lib-result item))))
(defmethod contact-email ((item result))
  (contact-email (lib-result item)))
(defmethod system-name ((item result))
  (when (load-result item)
    (system-name (load-result item))))

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type t :identity t)
    (format stream "~S ~S ~S ~S ~S"
            (lib-world result)
            (lisp result)
            (libname result)
            (result-spec result)
            (log-uri result))))