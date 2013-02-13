;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-server
  :version "1.0.1"
  :serial t
  :depends-on (#:hunchentoot
               #:cl-smtp)
  :components 
    ((:module "server"
      :serial t
      :components
      ((:file "server")))))

