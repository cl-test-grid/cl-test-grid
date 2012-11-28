;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file contains implementation of test-grid-blobstore
;;;; interface which is able store copy of test results
;;;; on local file system before submitting the results
;;;; as usually.

;;;
;;; Interface
;;;

(defpackage #:local-result-store
  (:use :cl)
  (:export
   ;; the class
   #:local-result-store
   ;; function to retrieve the results
   ;; collected by local-result-store
   #:list-test-run-results))

;; local-results-store is created like this:
#|
(make-instance 'local-result-store:local-result-store
               :delegate-to <original blobstore>
               :results-directory #P"/test/")
|#

(defgeneric local-result-store:list-test-run-results (results-directory)
  (:documentation "Returns the list of test run results accumulated in the RESULTS-DIRECORY"))

;;;
;;; Implementation
;;;

(in-package #:local-result-store)

(defclass delegating-blobstore ()
  ((delegate-to :accessor delegate-to
                :initarg :delegate-to
                :initform (error ":delegate-to parameter must be specified"))))

(defmethod test-grid-blobstore:submit-files ((blobstore delegating-blobstore)
                                             id-pathname-alist)
  (test-grid-blobstore:submit-files (delegate-to blobstore)
                                    id-pathname-alist))

(defmethod test-grid-blobstore:submit-run-info ((blobstore delegating-blobstore)
                                                run-info)
  (test-grid-blobstore:submit-run-info (delegate-to blobstore)
                                       run-info))

(defmethod test-grid-blobstore:tell-admin ((blobstore delegating-blobstore)
                                           subject
                                           body)
  (test-grid-blobstore:tell-admin (delegate-to blobstore)
                                  subject
                                  body))

(defclass local-result-store (delegating-blobstore)
  ((results-directory :accessor results-directory
                      :initarg :results-directory
                      :initform (error ":results-directory parameter must be specified"))))

(defmethod test-grid-blobstore:submit-run-info ((blobstore local-result-store)
                                                run-info)
  (let* ((run-descr (getf run-info :descr))
         (out-file-name (format nil "~A-~A-results.lisp"
                                (getf run-descr :lisp)
                                (test-grid-agent::fmt-time (getf run-descr :time))))
         (out-file (merge-pathnames out-file-name (results-directory blobstore))))
    (log:info "storing copy of test results locally, in the file ~A" out-file)
    (ensure-directories-exist out-file)
    (with-open-file (out out-file
                         :direction :output
                         :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                         :if-exists :error
                         :if-does-not-exist :create)
      (test-grid-data::print-test-run out run-info)))

  (call-next-method))

(defmethod list-test-run-results (results-directory)
  (let ((test-runs '()))
    (dolist (child (cl-fad:list-directory results-directory))
      (when (search "-results" (pathname-name child))
        (push (test-grid-utils::safe-read-file child) test-runs)))
    test-runs))
