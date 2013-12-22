;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:hu.dwim.stefil-api-impl
  :version "0.1.0"
  :serial t
  :depends-on (#:hu.dwim.stefil-api #:hu.dwim.stefil)
  :components ((:file "hu.dwim.stefil-api-impl")))
