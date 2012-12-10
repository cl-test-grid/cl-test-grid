;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(in-package :sptm)

(defclass replica ()
  ((vdata
    :type versioned-data
    :accessor vdata
    :initform (make-instance 'versioned-data))
   (transaction-log
    :accessor transaction-log
    :initarg :transaction-log
    :initform (error ":transaction-log is required"))
   (transaction-checker
    :type (or symbol function)
    :accessor transaction-checker
    :initarg :transaction-checker
    :initform (constantly t))
   (local-snapshot-file
    :accessor local-snapshot-file
    :initarg :local-snapshot-file
    :initform (error ":local-snapshot-file is required"))))

(defgeneric read-local-snapshot (replica))
(defgeneric save-local-snapshot (replica))

(defmethod data ((replica replica))
  (data (vdata replica)))

(defmethod version ((replica replica))
  (version (vdata replica)))

(defmethod read-local-snapshot ((replica replica))
  (let ((plist (test-grid-utils::safe-read-file (local-snapshot-file replica))))
    (setf (vdata replica)
          (make-instance 'versioned-data
                         :version (getf plist :version)
                         :data (getf plist :data))))
  replica)

(defmethod save-local-snapshot ((replica replica))
  (let ((versioned-data (vdata replica)))
    (test-grid-utils::write-to-file (list :version (version versioned-data)
                                          :data (data versioned-data))
                                    (local-snapshot-file replica)))
  replica)

(defun sync (replica)
  (when (and (zerop (version replica))
             (probe-file (local-snapshot-file replica)))
    (read-local-snapshot replica))
  (let* ((cur-vdata (vdata replica))
         (new-vdata (roll-forward cur-vdata
                                  (transaction-log replica)
                                  (transaction-checker replica))))
    (when (not (eq new-vdata (vdata replica)))
      (setf (vdata replica) new-vdata)
      (save-local-snapshot replica)))
  replica)

;;; convenience replica-based wrappers around exec-transaction

(defun repli-exec (replica func-symbol args)
  (unless (funcall (transaction-checker replica) func-symbol)
    (cerror "Continue, appying this forbidden function."
            "You are trying to execute and record to the transaction log a function, forbidded by transaction-checker of this replica."))
  (let ((new-vdata (exec-transaction (transaction-log replica)
                                     (vdata replica)
                                     func-symbol
                                     args)))
    (setf (vdata replica) new-vdata)))

(defun repli-exec-save (replica func-symbol args)
  (repli-exec replica func-symbol args)
  (save-local-snapshot replica))

