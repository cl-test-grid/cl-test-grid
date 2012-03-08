;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-tests
  :version "0.1.0"
  :serial t
  :depends-on (#:test-grid #:test-grid-reporting)
  :components ((:file "test-grid-tests")))

(defpackage #:test-grid-tests-config (:export #:*src-base-dir*))
(defparameter test-grid-tests-config:*src-base-dir*
  (make-pathname :name nil :type nil :defaults *load-truename*))

;; make sample test suites available to ASDF
(pushnew (merge-pathnames "sample-test-suites/"
                          test-grid-tests-config:*src-base-dir*)
         asdf:*central-registry*
         :test #'equal)

