;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-admin
  :version "0.1.0"
  :serial t
  :depends-on (#:test-grid-data #:test-grid-utils #:cl-pop #:cl-mime #:cl-base64)
  :components
  ((:module "admin"
    :serial t
    :components
    ((:file "import-test-result-emails")))))
