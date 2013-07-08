;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2012 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:nst-sample-test-suite
                 :version "0.1.0"
                 :serial t
                 :depends-on (#:nst)
                 :components ((:file "nst-sample-test-suite")))
