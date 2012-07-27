;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;
;;; Fake implementation of test-grid-blobstore API;
;;; may be used for testing when we don't want to really
;;; upload test results to server
;;;

(in-package #:test-grid-agent)

(defclass fake-blobstore () ())

(defmethod test-grid-blobstore:submit-files ((blobstore fake-blobstore) id-pathname-alist)
  (log:info "fake-blobstore.submit-files called")
  ;; return random fake blob IDs for every file
  (let ((random-state (make-random-state t)))
    (mapcar (lambda (id-pathname-cons)
              (cons (car id-pathname-cons)
                    (format nil "fake-~36,10,'0r" (random most-positive-fixnum random-state))))
            id-pathname-alist)))

(defmethod test-grid-blobstore:submit-run-info ((blobstore fake-blobstore) run-info)
  (declare (ignore run-info))
  (log:info "fake-blobstore.submit-run-info called"))

(defmethod tell-admin ((blobstore fake-blobstore) subject body)
  (declare (ignore subject body))
  (log:info "fake-blobstore.tell-admin called"))