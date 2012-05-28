;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-agent
  :version "0.3.1"
  :serial t
  :depends-on (#:test-grid 
               #:external-program
               #:trivial-features
               #:cl-fad
               #:log4cl)
  :components 
    ((:module "agent"
      :serial t
      :components
      ((:file "package")
       (:file "lisp-impls")
       (:file "agent")))))
