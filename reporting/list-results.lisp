;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun list-results (db)
  (mapcan #'results (list-lib-results db)))

(defun results (lib-result)
  (append (testsuite-results lib-result)
          (asdf-load-results lib-result)))

(defun testsuite-results (lib-result)
  (let* ((suite-status (status lib-result))
         (suite-results (etypecase suite-status
                          (null '())
                          (keyword (list (list :whole-test-suite suite-status)))
                          (list (let* ((failures (getf suite-status :failed-tests))
                                       (expected-to-fail (getf suite-status :known-to-fail)))
                                  (if (and (null failures) (null expected-to-fail))
                                      '((:whole-test-suite :ok))
                                      (let* ((known-failures (intersection expected-to-fail
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
                                                 (as-results unexpected-oks :unexpected-ok))))))))))
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

(defgeneric lisp-impl-type (obj))
(defparameter +lisp-impl-types+
  (alexandria:alist-hash-table (mapcar (lambda (keyword)
                                         (cons (string-downcase keyword)
                                               keyword))
                                       '(:abcl :acl :ccl :clisp :corman :cmu
                                         :ecl :gcl :lw :mcl :mkcl :sbcl :scl :symbolics :xcl))
                               :test #'equal))

(defmethod lisp-impl-type ((implementation-identifier string))
  (let* ((dash-pos (or (position #\- implementation-identifier)
                      (error "invalid implementaiton-identifier: ~A" implementation-identifier)))
         (impl-type (subseq implementation-identifier 0 dash-pos)))
    (or (gethash impl-type +lisp-impl-types+)
        (error "unknown lisp implementaion type: ~A" impl-type))))

(defmethod lisp ((item result))
  (lisp (lib-result item)))
(defmethod lisp-impl-type ((item result))
  (lisp-impl-type (lisp item)))
(defmethod lib-world ((item result))
  (lib-world (lib-result item)))
(defmethod libname ((item result))
  (libname (lib-result item)))
(defmethod test-run-time ((item result))
  (test-run-time (lib-result item)))
(defmethod test-run-duration ((item result))
  (test-run-duration (lib-result item)))
(defmethod log-blob-key ((item result))
  (if (load-result item)
      (log-blob-key (load-result item))
      (log-blob-key (lib-result item))))
(defmethod log-byte-length ((item result))
  (if (load-result item)
      (log-byte-length (load-result item))
      (log-byte-length (lib-result item))))
(defgeneric duration (item))
(defmethod duration ((item result))
  (if (load-result item)
      (load-duration (load-result item))
      (test-duration (lib-result item))))
(defmethod contact-email ((item result))
  (contact-email (lib-result item)))
(defmethod system-name ((item result))
  (when (load-result item)
    (system-name (load-result item))))
(defmethod fail-condition-type ((item result))
  (if (load-result item)
      (fail-condition-type (load-result item))
      (fail-condition-type (lib-result item))))
(defmethod fail-condition-text ((item result))
  (if (load-result item)
      (fail-condition-text (load-result item))
      (fail-condition-text (lib-result item))))

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type t :identity t)
    (format stream "~S ~S ~S ~S ~S"
            (lib-world result)
            (lisp result)
            (libname result)
            (result-spec result)
            (log-uri result))))

(defun result-spec-outcome (result)
  "Consider result-spec as a pair of test and outcome.
For example (:load \"some-system\" :ok),
here (:load \"some-system\") is the test and :ok is the outcome.
Or (:whole-test-suite :ok) - :whole-test-suite is a test
and :ok is an outcome.

Returns the outcome part of the result-spec."
  (let ((result-spec (result-spec result)))
    (case (car result-spec)
      (:whole-test-suite (second result-spec))
      (otherwise (third result-spec)))))

(defun failure-p (result)
  (case (result-spec-outcome result)
    ((:fail :crash :timeout :known-fail :unexpected-ok :no-resource) t)))

(defun ffi-grovel-failure-p (result)
  (string= "CFFI-GROVEL:GROVEL-ERROR" (fail-condition-type result)))

(defun ffi-failure-p (result)
  (let ((fail-type (fail-condition-type result))
        (fail-text (fail-condition-text result)))
    (or (and fail-type
             (member fail-type
                     '("CFFI:LOAD-FOREIGN-LIBRARY-ERROR" )
                     :test #'string-equal))
        (and (find-if (lambda (txt) (search txt fail-text))
                      '("Undefined foreign symbol"
                        "Don't know how to REQUIRE JNA"
                        "Don't know how to COMMON-LISP:REQUIRE JNA"
                        "CFFI requires CLISP compiled with dynamic FFI support"))))))
