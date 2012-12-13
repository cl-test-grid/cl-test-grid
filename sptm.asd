;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:sptm
  :version "0.1.0"
  :serial t
  :depends-on (#:zaws
               #:zaws-xml
               #:zs3
               #:gzip-stream
               #:babel
               #:alexandria
               #:test-grid-utils)
  :components
  ((:module "sptm"
    :serial t
    :components
    ((:file "package")
     (:file "versioned-data")
     (:file "amazon-simple-db")
     (:file "aws-transaction-log")
     (:file "replica")))))
