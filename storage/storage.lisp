;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(defpackage #:test-grid-storage
  (:nicknames :tg-storage)
  (:use :cl)
  (:import-from #:sptm #:data #:version #:sync #:read-local-snapshot)
  (:export
   ;; replica creation
   #:make-replica
   ;; replica attributes
   #:name
   #:data
   #:version
   ;; replica operations
   #:sync
   #:read-local-snapshot
   ;; convenience method to record the most often used transaction
   #:add-test-run))

(in-package #:test-grid-storage)

(defgeneric make-replica (name local-snapshot-file))
(defgeneric name (replica))

(defclass replica (sptm::replica) ())

;; Amazon Web Service credentials for cl-test-grid-user account
;; in the form (<Access Key Id>  <Secret Access Key>).
;; This user is not the owner of our S3 bucket and SimpleDB domain,
;; but has access to them (wich can be quickly revoked in case
;; of any hooliganism)
(defparameter *cl-test-grid-user-credentials*
  '("AKIAJS4QAUS7CU5BK5MA" "wf4CbpVQHwuD9LkS+7Dby3exfc7PTv1upvZewIa0"))

(defun make-transaction-log (name)
  (make-instance 'sptm::aws-transaction-log
                 :name name
                 :simpledb-domain "cltestgrid"
                 :s3-bucket "cl-test-grid"
                 :credentials *cl-test-grid-user-credentials*))

(defun transaction-allowed-p (func-symbol)
  (member func-symbol '(test-grid-data:add-test-run
                        test-grid-data:remove-test-runs
                        test-grid-data::schema-change-006)))

(defmethod make-replica (name local-snapshot-file)
  (make-instance 'replica
                 :transaction-log (make-transaction-log name)
                 :transaction-checker 'transaction-allowed-p
                 :local-snapshot-file local-snapshot-file))

(defmethod name ((replica replica))
  (sptm::name (sptm::transaction-log replica)))

;; override the local snapshow serialization
;; to pretty format the data
(defmethod sptm::save-local-snapshot ((replica replica))
  (let ((versioned-data (sptm::vdata replica)))
    (with-open-file (out (sptm::local-snapshot-file replica)
                         :direction :output
                         :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "(:version ~a~%" (sptm::version versioned-data))
      (format out " :data ")
      (test-grid-data::print-db out
                                (sptm::data versioned-data)
                                7)
      (format out ")")))
  replica)

;; convenience method to record the most often used transaction
(defun add-test-run (storage-name test-run)
  (let ((log (make-transaction-log storage-name)))
    (sptm::record-transaction log 'test-grid-data::add-test-run (list test-run))))
