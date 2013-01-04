;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; We sometimes need to list all the logs in database, or
;;;; logs of some particular test run. Use cases when we need
;;;; this are migration of logs from one online storage to
;;;; another, or deleting logs from online storage when removing
;;;; test run from database.
;;;;
;;;; Terminology ambiguity: we sometimes call them logs, sometimes blobs.
;;;; It means the same - the output produced by lisp processes running
;;;; tests, and stored in files and published online.
;;;;
;;;; This file contains convenience utilities to list logs recorded in the database.
 
(in-package #:test-grid-data)

(defclass blob ()
  ((name :type string :initarg :name :accessor name)
   (size :type fixnum :initarg :size :accessor size)))

(defun get-blob (plist)
  (let ((name (getf plist :log-blob-key)))
    (when name
      (make-instance 'blob
                     :name name
                     :size (getf plist :log-byte-length)))))

(defun test-run-blobs (test-run)
  (let (blobs)
    (flet ((maybe-collect-blob (plist)
             (let ((blob (get-blob plist)))
               (when blob
                 (push blob blobs)))))
      (dolist (lib-result (getf test-run :results))
        (maybe-collect-blob lib-result)
        (dolist (load-result (getf lib-result :load-results))
          (maybe-collect-blob load-result))))
    blobs))

(defun list-blobs (db)
  (mapcan #'test-run-blobs (getf db :runs)))

(defun list-blob-keys (db)
  (mapcar #'name (list-blobs db)))
