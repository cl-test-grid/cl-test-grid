;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

;;; Note is either text or a ticket reference
(defclass ticket () ())
(defclass launchpad-ticket (ticket)
  ((id :type string
       :accessor id
       :initarg :id
       :initform (error ":id is required"))))

(defun note-body-p (obj)
  (or (stringp obj)
      (typep obj 'ticket)))

(defmethod print-object ((ticket launchpad-ticket) stream)
  (print-unreadable-object (ticket stream :type t :identity t)
    (format stream "~S" (id ticket))))

(defgeneric ticket-url (ticket))
(defmethod ticket-url ((ticket launchpad-ticket))
  (concatenate 'string
               "http://bugs.launchpad.net/bugs/"
               (id ticket)))

(defun lp-ticket (id)
  (make-instance 'launchpad-ticket :id id))

;;; Note database

(defun make-note-db ()
  (make-hash-table :test 'equal))

(defun set-note (db fields field-values body)
  (let ((index (or (gethash fields db)
                    (setf (gethash fields db)
                          (make-hash-table :test 'equal)))))
    (setf (gethash field-values index) body)))

(defun note (db fields field-values)
  (let ((index (gethash fields db)))
    (when index
      (gethash field-values index))))

(let ((db (make-note-db)))
  (set-note db '(lisp) '("sbcl") "this is a note for SBCL")
  (assert (string= (note db '(lisp) '("sbcl"))
                   "this is a note for SBCL")))

(defun notes (db result)
  (let ((notes nil))
    (maphash (lambda (fields index)
               (let* ((field-vals (mapcar (lambda (field)
                                           (funcall field result))
                                          fields))
                      (note (gethash field-vals index)))
                 (when note
                   (push note notes))))
             db)
    notes))

(defun fill-notes (note-spec-list)
  (flet ((as-list (val)
           (if (consp val)
               val
               (list val))))
    (let ((db (make-note-db)))
      (labels ((fill-rec (fields field-vals spec)
                 (if (note-body-p spec)
                     (set-note db fields field-vals spec)
                     (let* ((cur-field (first spec))
                            (cur-field-vals (as-list (second spec)))
                            (fields (cons cur-field fields)))
                       (dolist (cur-field-val cur-field-vals)
                         (let ((field-vals (cons cur-field-val field-vals)))
                           (dolist (subspec (cddr spec))
                             (fill-rec fields field-vals subspec))))))))
        (dolist (spec note-spec-list)
          (fill-rec '() '() spec)))
      db)))

(defparameter *note-db*
  (fill-notes `((lib-world "quicklisp 2013-08-13"
                 (libname :com.informatimago
                  (failure-p t "author informed"))
                 (libname :asdf-dependency-grovel
                  (system-name "asdf-dependency-grovel"
                   (failure-p t "needs ASDF 3")))
                 (libname :xcvb
                  (failure-p t
                   (lisp ("acl-9.0a-win-x86" "ccl-1.8-f95-win-x64" "ccl-1.8-f95-win-x86" "sbcl-1.0.57.0.debian-linux-x64")
                     "needs ASDF 3")))
                 (libname :exscribe
                  (failure-p t
                    "needs ASDF 3"))
                 (libname (:periods :cambl)
                  (failure-p t
                    ,(lp-ticket "1229050")))))))
#|
(notes *note-db* (first (subset *all-results* (lambda (r) (and
                                                           (eq (libname r) :cambl)
                                                           (failure-p r)
                                                           (string= (lib-world r) "quicklisp 2013-08-13"))))))

(time (dolist (r *all-results*)
        (notes *note-db* r)))
|#