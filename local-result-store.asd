;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:local-result-store
  :version "0.1.0"
  :serial t
  :depends-on (#:test-grid-blobstore
               #:test-grid-data
               #:test-grid-utils
               #:test-grid-agent ; just to reuse test-grid-agent::fmt-time
               #:cl-fad)
  :components
  ((:file "local-result-store")))
