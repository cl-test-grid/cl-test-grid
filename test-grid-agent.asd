;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

;; make the blobstore implementation available to ASDF
(let ((this-dir (make-pathname :name nil :type nil :defaults *load-truename*)))
  (cl:pushnew (merge-pathnames "gae-blobstore/lisp-client/" this-dir)
              asdf:*central-registry*
              :test #'equal))

(asdf:defsystem #:test-grid-agent
  :version "0.3.1"
  :serial t
  :depends-on (#:test-grid
               #:test-grid-blobstore
               #:test-grid-gae-blobstore
               #:alexandria
               #:external-program
               #:trivial-features
               #:cl-fad
               #:log4cl
               #:fare-memoization)
  :components 
    ((:module "agent"
      :serial t
      :components
      ((:file "package")
       (:file "lisp-exe") 
       (:file "with-response-file")
       (:file "perform-test-run")
       (:file "persistable-state")
       (:file "fake-blobstore")
       (:file "agent")))))