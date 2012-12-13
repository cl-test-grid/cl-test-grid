;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

;; make the subcomponents available to ASDF
(let ((this-directory (make-pathname :name nil :type nil :defaults *load-truename*)))
  (cl:pushnew (merge-pathnames #P"testsuites/apis-of-test-frameworks/" this-directory)
              asdf:*central-registry*
              :test #'equal))

(asdf:defsystem #:test-grid-testsuites
  :version "0.3.1"
  :serial t
  :depends-on (#:quicklisp #:test-grid-utils #:rt-api #:lift-api #:fiveam-api #:eos-api #:stefil-api)
  :components
  ((:module "testsuites"
    :serial t
    :components ((:file "testsuites")))))
