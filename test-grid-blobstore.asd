;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-blobstore
  :description "Defines API for the online blobstore of the cl-test-grid project. 
The blobstore is used to store log files of libraries test sutes."
  :version "0.1.0"
  :serial t
  :components ((:file "test-grid-blobstore")))
