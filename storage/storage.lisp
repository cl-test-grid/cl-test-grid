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
   ;; lazily create a replica, or get a cached version (convenience function)
   #:get-replica
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

(defclass replica (sptm:replica) ())

;; Amazon Web Service credentials for cl-test-grid-user account
;; in the form (<Access Key Id>  <Secret Access Key>).
;; This user is not the owner of our S3 bucket and SimpleDB domain,
;; but has access to them (wich can be quickly revoked in case
;; of any hooliganism)
(defparameter *cl-test-grid-user-credentials*
  (sptm:decredfuscate
   '("do mdadaea gnisada dahhbsnkbzbvn"
     "nraicgaqbp anasbdhagbok ah bnay kza zb ieama d bybq lbec d takbznan p a iatcgbkdai")))

(defun make-transaction-log (name)
  (make-instance 'sptm:aws-transaction-log
                 :name name
                 :simpledb-domain "cltestgrid"
                 :s3-bucket "cl-test-grid"
                 :credentials *cl-test-grid-user-credentials*))

(defun transaction-allowed-p (func-symbol)
  (member func-symbol '(test-grid-data:add-test-run
                        test-grid-data:remove-test-runs
                        test-grid-data:remove-lib-results
                        test-grid-data::schema-change-006
                        test-grid-data:update-run-descr)))

(defmethod make-replica (name local-snapshot-file)
  (make-instance 'replica
                 :transaction-log (make-transaction-log name)
                 :transaction-checker 'transaction-allowed-p
                 :local-snapshot-file local-snapshot-file))

(defmethod name ((replica replica))
  (sptm:name (sptm:transaction-log replica)))

;; Override the local snapshot serialization
;; to format the data based on our knowledge
;; of the data structure we keep in the storage.
(defmethod sptm:save-local-snapshot ((replica replica))
  (let ((versioned-data (sptm:vdata replica)))
    (with-open-file (out (sptm:local-snapshot-file replica)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type 'character
                         :external-format tg-utils:*utf-8-external-format*)
      (format out "(:version ~a~%" (sptm:version versioned-data))
      (format out " :data ")
      (test-grid-data::print-db out
                                (sptm:data versioned-data)
                                7)
      (format out ")")))
  replica)

;; convenience method to record the most often used transaction
(defun add-test-run (storage-name test-run)
  (let ((log (make-transaction-log storage-name)))
    (sptm:record-transaction log 'test-grid-data::add-test-run (list test-run))))

;;; A cache of replicas - a convenience functionality.
;;;
;;; I have several reporting scripts, and don't want to use separate
;;; replica instance for each script, because reading snapshot is long
;;; even from local disc.
;;;
;;; Therefore a global cache of replicas is provided here. The cache
;;; key is the storage name.

(defvar *replicas* (make-hash-table :test #'equal))

(defun get-replica (storage-name &key (snapshot-directory (asdf:system-source-directory :test-grid-storage)))
  ;; no syncrhonization, as we expect it be used from SLIME interactively, concurrency is very unlikely
  (or (gethash storage-name *replicas*)
      (let ((r (make-replica storage-name
                             (uiop:subpathname snapshot-directory
                                               (format nil "db-~A.lisp" storage-name)))))
        (setf (gethash storage-name *replicas*)
              r))))
