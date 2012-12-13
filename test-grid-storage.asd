;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-storage
  :version "1.0.1"
  :serial t
  :depends-on (#:test-grid-data
               #:sptm)
  :components ((:module "storage"
                        :serial t
                        :components
                        ((:file "storage")))))
