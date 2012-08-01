;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-utils
  :version "0.3.1"
  :serial t
  :components 
  ((:module "utils"
    :serial t
    :components ((:file "utils")))))
