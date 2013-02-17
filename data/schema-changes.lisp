;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-data)

#|
 DB schema change history:
 0 - initial
 1 - cl-routes and cl-closure-template are renamed to routes and closure-template
 2 - routes and closure-template are renamed back to cl-routes and cl-closure-template
 3 - bknr.datastore is renamed to bknr-datastore, in order to match the Quicklisp release name
 4 - the :load-failed status of testsutes is replaced by just :fail
 5 - the :version field of DB is renamed to :schema
 6 - the :contact (:email "someone@host.com") construct is changed to just :contact-email "someone@host.com"
|#

(defun schema-change-006 (db)
  (unless (= 5 (getf db :schema))
    (error "Expected database schema is 5"))
  (list :schema 6
        :runs (mapcar (lambda (run)
                        (let* ((descr (copy-list (getf run :descr)))
                               (email (getf (getf descr :contact) :email)))
                          (remf descr :contact)
                          (setf (getf descr :contact-email) email)
                          (updated-plist run :descr descr)))
                      (getf db :runs))))