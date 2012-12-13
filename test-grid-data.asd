;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-data
  :version "0.3.1"
  :serial t
  :depends-on (#:test-grid-utils #:alexandria)
  :components 
  ((:module "data"
    :serial t
    :components ((:file "data")))))
