;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:eos-sample-test-suite
  :version "0.1.0"
  :serial t
  :depends-on (#:eos)
  :components ((:file "eos-sample-test-suite")))
