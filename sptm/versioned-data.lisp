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

(defgeneric version (transaction)
  (:documentation "The version the data will have after
the transaction is applied."))

(defgeneric func (transaction)
  (:documentation "Symbol fbound to a function. The function should accept
the value of (data versioned-data) as the first argument and and number
of other arguments. The return value of the function is new version
of data."))

(defgeneric args (transaction)
  (:documentation "The list of values to be passed to the function
as the arguments except for the first argument."))

(defmethod apply-transaction ((d versioned-data) transaction)
  (make-instance 'versioned-data
                 :data (apply (func transaction)
                              (data d)
                              (args transaction))
                 :version (version transaction)))

;;; transaction log

(defgeneric min-transaction-version (log &key if-absent)
  (:documentation "The minimal version of transactions
stored in the transaction log.

 >= 1

Absense of transactions in the transaction log handled
according the IF-ABSENT argument. If it is :ERROR (the default),
then error is signalled. When IF-ABSEND is any other value,
the value is just returned."))

(defgeneric max-transaction-version (log &key if-absent)
  (:documentation "The maximum version of transactions
stored in the transaction log.

 >= 1

Absense of transactions in the log is handled according
to IF-ABSENT the same way as described for MIN-TRANSACTION-VERSION."))

(defgeneric list-transactions (log after-version)
  (:documentation "List of all transactions in the log
having version > AFTER-VERSION. The transactions
are odered ascendingly by version."))

;; Transaction log provides the following two functions
;; for commiting transactions. Two functions allow transaction
;; log to store the function name and arguments a speparate
;; storage, and for the caller of these two functions to
;; avoid repeated transfer of function name and arguments
;; in case of concurrency collision.
;;
;; First save the function name and arguments using
;; the PERSIST-FUNCALL. Then pass the return value of PRESIST-FUNCALL
;; to the COMMIT-VERSION. In case of concurrency collision COMMIT-VERSION
;; returns false and we can retry the transaction locally and
;; call COMMIT-VERSION again, without repeating PRESIST-FUNCALL.
(defgeneric persist-funcall (log func-symbol args-without-data-arg))
(defgeneric commit-version (log version-number presist-funcall-result)
  (:documentation
   "Returns true if the version is commited successfully, 
false if the version is not commited due to concurrency
collision (someone else already commited such version).
Signals error in case of problems."))

(defgeneric snapshot-version (log &key if-absent)
  (:documentation "The maximum version of the VERSIONED-DATA saved by
SAVE-SNAPSHOT.

Absense of any snapshots log is handled according
to IF-ABSENT the same way as described for MIN-TRANSACTION-VERSION."))

(defgeneric get-snapshot (log)
  (:documentation
   "Returns the VERSIONED-DATA with the maximum version
of the saved by SAVE-SNAPSHOT. Signals an error if
there is no saved snapshots."))

(defgeneric save-snapshot (log versioned-data))

;;; play transaction log

(defmethod roll-forward (log versioned-data &optional (transaction-checker (constantly 't)))
  (assert (>= (version versioned-data) 0))

  (let ((min-tx-ver (min-transaction-version log :if-absent :absent)))
    (if (eq :absent min-tx-ver)
        ;; The log doesn't have transaction records
        ;; Check if a snaphost is present that is later
        ;; than our version of data.
        (if (> (snapshot-version log :if-absent 0)
               (version versioned-data))
            (get-snapshot log)
            versioned-data)
        (let ((cur-vdata (if (< (version versioned-data)
                                (1- min-tx-ver))
                             ;; Our versioned-data is so outdated, that transaction
                             ;; log doesn't have all the transactions necessary to roll forward.
                             ;;
                             ;; Then we start from scratch. There must be a snaphsot.
                             (get-snapshot log)
                             versioned-data)))
          (assert (>= (version cur-vdata) (1- min-tx-ver)))
          (flet ((apply-transaction* (vdata transaction)
                   (unless (funcall transaction-checker (func transaction))
                     (error "The transaction ~A retrieved from transaction log specifies function ~S forbidden by the transaction checker"
                            transaction (func transaction)))
                   (apply-transaction vdata transaction)))
            (reduce #'apply-transaction* (list-transactions log (version cur-vdata))
                    :initial-value cur-vdata))))))

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
to record into transaction log at whatever the next version
is available. They don't need to be executed locally first
on the synchronized version of data.

RECORD-TRANSACTION function serves exactly this purpose - 
records function call to the transaction log at the
next available version."
  (let ((last-version (max-transaction-version log :if-absent 0))
        (funcall-name (persist-funcall log func-symbol args-without-data-arg)))
    (loop for ver from (1+ last-version)
       until (commit-version log ver funcall-name)
       finally (return ver))))
