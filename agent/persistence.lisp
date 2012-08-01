;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

;;; Interface

(defgeneric init-persistence (file)
  (:documentation
   "Creates a persistence object which will
store it's data in the file. If the FILE
exitst, loads the data saved there."))
(defgeneric set-agent-id (persistence id))
(defgeneric get-agent-id (persistence))
(defgeneric mark-tested (persistence lib-world lisp))
(defgeneric tested-p (persistence lib-world lisp))

;;; Implementation

(defclass persistence ()
   ;; plist with data
  ((state :type list
          :initform nil
          :accessor state)
   (file :initarg :file
         :initform (error ":file is required")
         :accessor file)))

(defun save (persistence)
  ;; todo: sort and newline for each record
  (test-grid-utils::write-to-file (state persistence)
                                  (file persistence)))

(defmethod init-persistence (file)
  (let ((p (make-instance 'persistence :file file)))
    (when (probe-file file)
      (setf (state p)
            (test-grid-utils::safe-read-file file)))
    p))

(defmethod get-agent-id ((persistence persistence))
  (getf (state persistence) :agent-id))

(defmethod set-agent-id ((persistence persistence) id)
  (setf (getf (state persistence) :agent-id)
        id)
  (save persistence))

(defmethod mark-tested ((persistence persistence) lib-world lisp)
  (pushnew (list lib-world lisp)
           (getf (state persistence) :done-tests)
           :test #'equal)
  (save persistence))

(defmethod tested-p ((persistence persistence) lib-world lisp)
  (member (list lib-world lisp)
          (getf (state persistence) :done-tests)
          :test #'equal))
