;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2013 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:nst-api-impl
  :version "0.1.0"
  :serial t
  :depends-on (#:nst-api #:nst)
  :components ((:file "nst-api-impl")))
