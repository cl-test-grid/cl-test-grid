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
(defgeneric args (transaction cur-data))

(defmethod apply-transaction ((d versioned-data) transaction)
  (make-instance 'versioned-data
                 :data (apply (func transaction)
                              (args transaction (data d)))
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

(defmethod roll-forward (versioned-data log &optional (transaction-checker (constantly 't)))
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

(defmethod record-transaction (log                               
                               func-symbol
                               args-without-data-arg)
  (let ((last-version (if (empty-p log)
                          1
                          (max-transaction-version log)))
        (funcall-name (persist-funcall log func-symbol args-without-data-arg)))
    (loop for ver from (1+ last-version)
       until (commit-version log ver funcall-name)
       finally (return ver))))

(defmethod exec-transaction (log vdata func-symbol args)
  (let ((new-data (apply func-symbol (data vdata) args))
        (fcall (persist-funcall log func-symbol args)))
    (loop
         (if (commit-version log (1+ (version vdata)) fcall)
             (return (make-instance 'versioned-data
                                    :version (1+ (version vdata))
                                    :data new-data))
             (setf vdata (roll-forward vdata log)
                   new-data (apply func-symbol (data vdata) args))))))

;;; convinience

(defun initial-value (db value)
  "This function is expected to be often used
in the first transaction, to initialize the persistent
data to some data structure. Just returns the VALUE, 
so that after such transaction the persistent data
is initialized to this VALUE."
  (unless (null db)
    (error "DB is already initialized to some value"))
  value)