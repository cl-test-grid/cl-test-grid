;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-reporting
  :version "0.1.0"
  :serial t
  :depends-on (#:test-grid-data #:test-grid-utils #:test-grid-agent)
  :components
  ((:module "reporting"
    :serial t
    :components
    ((:file "package")
     (:file "do-results")
     (:file "distinct")
     (:file "reporting")
     (:file "test-runs")
     (:file "csv")
     (:file "pivot")
     (:file "regressions")
     (:file "common")
     (:file "ecl-pages")
     (:file "abcl-page")
     (:file "durations")
     (:file "contributors")))))
