;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-reporting
  :version "0.1.0"
  :serial t
  :depends-on (#:test-grid-data #:test-grid-utils #:test-grid-agent
               #:html-template #:cl-ppcre #:vecto)
  :components
  ((:module "reporting"
    :serial t
    :components
    ((:file "package")
     (:file "my-time")
     (:file "cl-ex")
     (:file "do-results")
     (:file "distinct")
     (:file "notes")
     (:file "html-printers")
     (:file "list-results")
     (:file "templating")
     (:file "reporting")
     (:file "test-runs")
     (:file "csv")
     (:file "pivot")
     (:file "quicklisp-diff")
     (:file "quicklisp-diff2")
     (:file "status-icon")
     (:file "status-icon-svg")
     (:file "status-icon-png")
     (:file "library-report")
     (:file "regressions")
     (:file "meta-info-from-quicklisp")
     (:file "dependencies-and-blockers")
     (:file "load-failures")
     (:file "compiler-diff")
     (:file "demo-reports")
     (:file "durations")
     (:file "contributors")))))
