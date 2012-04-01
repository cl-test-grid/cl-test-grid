;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(defpackage #:test-grid-config (:export #:*src-base-dir*))
(defparameter test-grid-config:*src-base-dir*
  (make-pathname :name nil :type nil :defaults *load-truename*))

;; make the subcomponents available to ASDF
(cl:pushnew (merge-pathnames "apis-of-test-frameworks/"
                             test-grid-config:*src-base-dir*)
            asdf:*central-registry*
            :test #'equal)

(asdf:defsystem #:test-grid
  :version "0.3.1"
  :serial t
  :depends-on (#:quicklisp #:test-grid-blobstore #:rt-api #:lift-api #:fiveam-api #:eos-api #:stefil-api)
  :components ((:file "utils")
               (:file "test-grid")))
