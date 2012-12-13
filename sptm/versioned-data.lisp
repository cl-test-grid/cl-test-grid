;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(in-package #:sptm)

(defclass versioned-data ()
  ((version :type fixnum
            :accessor version
            :initform 0
            :initarg :version)
   (data :type t
         :accessor data
         :initform nil
         :initarg :data)))

(defmethod print-object ((vdata versioned-data) stream)
  (print-unreadable-object (vdata stream :type t :identity t)
    (format stream "~D ~S"
            (version vdata)
            (data vdata))))

;;; transactions

(defgeneric version (transaction))
(defgeneric func (transaction))
(defgeneric args (transaction))

(defmethod apply-transaction ((d versioned-data) transaction)
  (make-instance 'versioned-data
                 :data (apply (func transaction)
                              (data d)
                              (args transaction))
                 :version (version transaction)))

;;; transaction log

(defgeneric empty-p (log)) ;; todo: rename to has-transactions-p
(defgeneric min-transaction-version (log))
(defgeneric max-transaction-version (log))
(defgeneric list-transactions (log after-version))
(defgeneric snapshot-version (log))
(defgeneric get-snapshot (log))
(defgeneric save-snapshot (log versioned-data))
(defgeneric persist-funcall (log func-symbol args-without-data-arg))
(defgeneric commit-version (log version-number presist-funcall-result)
  (:documentation
   "Returns true if the version is commited successfully, 
false if the version is not commited due to concurrency
conflict (somebody else already commited such version).
Signals error in case of problems."))

;;; play transaction log

(defmethod roll-forward (log versioned-data &optional (transaction-checker (constantly 't)))
  (assert
   ;; if there are transactions in the log,
   ;; their db versions start not later
   ;; than right after the db version of
   ;; snapshot
   (or (empty-p log)
       (>= (snapshot-version log)
           (1- (min-transaction-version log)))))
  
  (if (empty-p log)
      (if (> (snapshot-version log)
             (version versioned-data))
          (get-snapshot log)
          versioned-data)
      (let ((cur-vdata (if (< (version versioned-data)
                              (1- (min-transaction-version log)))
                           ;; Our versioned-data is so outdated, that transaction
                           ;; log doesn't have all the transactions necessary to roll forward.
                           (get-snapshot log)
                           versioned-data)))
        (assert (>= (version cur-vdata) (1- (min-transaction-version log))))
        (flet ((apply-transaction* (vdata transaction)
                 (unless (funcall transaction-checker (func transaction))
                   (error "The transaction ~A retrieved from transaction log specifies function ~S forbidden by the transaction checker"
                          transaction (func transaction)))
                 (apply-transaction vdata transaction)))
          (reduce #'apply-transaction* (list-transactions log (version cur-vdata))
                  :initial-value cur-vdata)))))

;;; perform new transactions

(defmethod exec-transaction (log vdata func-symbol args &optional (transaction-checker (constantly t)))
  "Apply the specified function to (DATA VDATA) and the specified ARGS,
and try to commit the transaction to the transaction log as version (1+ (VERSION VDATA)).

If a record for this new version already exists in the transaction log,
pull all the new changes from transaction log and apply them to VDATA.
Then repeat the whole process.

Returns the new VERSIONED-DATA holding the return value of the function
and the version at which the transaction was commited to the LOG."
  (let ((new-data (apply func-symbol (data vdata) args))
        (fcall (persist-funcall log func-symbol args)))
    (loop
         (if (commit-version log (1+ (version vdata)) fcall)
             (return (make-instance 'versioned-data
                                    :version (1+ (version vdata))
                                    :data new-data))
             (setf vdata (roll-forward log vdata transaction-checker)
                   new-data (apply func-symbol (data vdata) args))))))

(defmethod record-transaction (log                               
                               func-symbol
                               args-without-data-arg)
  "Not all transactions have consistency requirements.

For example SPTM-EXAMPLE:WITHDRAW transaction has a consistency
requirement - it signals an error if the account doesn't have
enough funds.

On the other hand TEST-GRID-DATA:ADD-TEST-RUN just adds
an element to the list of test runs and it doesn't care what
is the previous content of this list.

The transactions without consistency requirements are safe
to record in transaction log at whatever the next version
is available. They don't need to be executed locally first
on the synchronized version of data.

RECORD-TRANSACTION function serves exactly this purpose - 
records function call to the transaction log as the
next available version."
  (let ((last-version (if (empty-p log)
                          1
                          (max-transaction-version log)))
        (funcall-name (persist-funcall log func-symbol args-without-data-arg)))
    (loop for ver from (1+ last-version)
       until (commit-version log ver funcall-name)
       finally (return ver))))

