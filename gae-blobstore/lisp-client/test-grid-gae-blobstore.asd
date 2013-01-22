;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-gae-blobstore
  :description "Common Lisp client for the Google App Engine hosted blobstore of the cl-test-grid project."
  :version "0.1.0"
  :serial t
  :depends-on (#:drakma
               #:flexi-streams
               #:test-grid-utils
               #:log4cl
               #:gzip-stream)
  :components ((:file "test-grid-gae-blobstore")))
