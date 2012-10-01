;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

;;; Interface

;; Macro
;;
;;    do-results ((result-record-var db &key where) &body body)
;;
;; Iterates over all test resutls in the DB satisfying
;; predicate WHERE (if specified) and for each result record
;; executes the BODY, binding the RESULT-RECORD-VAR to the result record.

;;; Every lib-result record has these fields:
(defgeneric libname (item))           ;; library name - a keyword, like :babel, :alexandria, etc.
(defgeneric lisp (item))              ;; lisp implementation identifier - a string
(defgeneric lib-world (item))         ;; a string, like "quicklisp 2012-07-03"
(defgeneric status (item))            ;; test status, like :ok, :fail, :timeout, :crash, :no-resource, (:failed-tests (<list of test case name string>) :known-to-fail (<list of test case names marked by the test suite autor as "known">))
(defgeneric log-blob-key (item))      ;; the key under which the log produced by this test is stored online at https://cl-test-grid.appspot.com/blob?key=<key>
(defgeneric log-byte-length (item))   ;; length of the log file
(defgeneric load-results (item))      ;; List of load results for every ASDF system provided by that project (see below for the description of load-result object
(defgeneric test-duration (item))     ;; time spend by the testsuite (may include compilation and load time)
(defgeneric contact-email (item))     ;; email of the person who run the test
(defgeneric test-run-time (item))     ;; when the test run was started (test-run is an internal term for testing a set of libraries on a single lisp implementation; today test runs include 56 libraries)
(defgeneric test-run-duration (item)) ;; duration of the test-run.

;;; Every load-result object has these fields:
(defgeneric system-name (load-result)) ;; ASDF system name - a string
(defgeneric load-status (load-result))      ;; one of the following keywords: :ok, :fail, :timeout, :crash
;; LOG-BLOB-KEY        ;; the generic functions defined above have methods on load-results too.
;; LOG-BYTE-LENGTH     ;; they refer online stored output produced during the asdf system load
(defgeneric load-duration (load-result)) ;; Time spend when loading the library. May include download time.

;; A functional wrapper around do-results.
;; Returns list of results satisfying the predicate
;; WHERE. If WHERE is omitted, returns all
;; the results.
(defgeneric select (db &key where))

;;; Implementation

(defun do-results-impl (db handler &key where)
  "HANDER is a function of one argument - test result record.
WHERE is a predicate of one argument - test result record."
    (dolist (test-run (getf db :runs))
      (dolist (lib-result (test-grid-data::run-results test-run))
        (let ((record (make-instance 'joined-lib-result
                                     :lib-result lib-result
                                     :test-run test-run)))
          (when (or (not where)
                    (funcall where record))
            (funcall handler record))))))

(defmacro do-results ((result-record-var db &key where) &body body)
  `(do-results-impl ,db
     (alexandria:named-lambda do-results-body (,result-record-var)
       ,@body)
     :where ,where))

(defmethod select (db &key where)
  (let ((results))
    (do-results (result db :where where)
      (push result results))
    results))

;;; RESULT record. Implemented as a lib result with a reference to it's test run.
(defclass joined-lib-result ()
  ((test-run :initarg :test-run :accessor test-run)
   (lib-result :initarg :lib-result :accessor lib-result)))

(defmethod lisp ((item joined-lib-result))
  (getf (test-grid-data::run-descr (test-run item)) :lisp))
(defmethod lib-world ((item joined-lib-result))
  (getf (test-grid-data::run-descr (test-run item)) :lib-world))
(defmethod test-run-time ((item joined-lib-result))
  (getf (test-grid-data::run-descr (test-run item)) :time))
(defmethod test-run-duration ((item joined-lib-result))
  (getf (test-grid-data::run-descr (test-run item)) :run-duration))
(defmethod contact-email ((item joined-lib-result))
  (getf (getf (test-grid-data::run-descr (test-run item))
              :contact)
        :email))
(defmethod libname ((item joined-lib-result))
  (getf (lib-result item) :libname))
(defmethod status ((item joined-lib-result))
  (getf (lib-result item) :status))
(defmethod test-duration ((item joined-lib-result))
  (getf (lib-result item) :test-duration))
(defmethod log-byte-length ((item joined-lib-result))
  (getf (lib-result item) :log-byte-length))
(defmethod log-blob-key ((item joined-lib-result))
  (getf (lib-result item) :log-blob-key))
(defmethod load-results ((item joined-lib-result))
  (getf (lib-result item) :load-results))

;;; The load-result objects are implemented by the same plist
;;; found in DB. So, the methods are specialized on list.
(defmethod system-name ((item list))
  (getf item :system))
(defmethod load-status ((item list))
  (getf item :status))
(defmethod log-byte-length ((item list))
  (getf item :log-byte-length))
(defmethod log-blob-key ((item list))
  (getf item :log-blob-key))
(defmethod load-duration ((item list))
  (getf item :load-duration))
