;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:local-package-aliases
  :version "0.0.1"
  :serial t
  :components 
  ((:module "local-package-aliases"
    :serial t
    :components ((:file "local-package-aliases")))))
