;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:test-grid-tests
  :version "0.1.0"
  :serial t
  :depends-on (#:test-grid-testsuites #:test-grid-reporting)
  :components ((:file "test-grid-tests")))

;; make sample test suites available to ASDF
(let ((this-directory (make-pathname :name nil :type nil :defaults *load-truename*)))
  (cl:pushnew (merge-pathnames #P"testsuites/sample-test-suites/" this-directory)
              asdf:*central-registry*
              :test #'equal))

