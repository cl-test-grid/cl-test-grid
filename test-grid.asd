;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:test-grid
  :serial t
  :version "0.1.0"
  :depends-on (#:quicklisp)
  :components ((:file "test-grid")))

(defpackage #:test-grid-config (:export #:*src-base-dir*))
(defparameter test-grid-config:*src-base-dir* 
  (make-pathname :name nil :type nil :defaults *load-truename*))