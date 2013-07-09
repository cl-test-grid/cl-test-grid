;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2012 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:nst-api
  :version "0.1.0"
  :serial t
  :depends-on (#:api-dsl)
  :components ((:file "nst-api")))
