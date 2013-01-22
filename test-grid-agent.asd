;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

;; make the blobstore implementation available to ASDF
(let ((this-dir (make-pathname :name nil :type nil :defaults *load-truename*)))
  (cl:pushnew (merge-pathnames "gae-blobstore/lisp-client/" this-dir)
              asdf:*central-registry*
              :test #'equal))

(asdf:defsystem #:test-grid-agent
  :version "1.0.1"
  :serial t
  :depends-on (#:test-grid-utils
               #:test-grid-data
               #:test-grid-testsuites
               #:test-grid-gae-blobstore
               #:test-grid-storage
               #:alexandria
               #:external-program
               #:trivial-features
               #:cl-fad
               #:log4cl
               #:fare-memoization
               #:usocket
               #:trivial-backtrace)
  :components 
    ((:module "agent"
      :serial t
      :components
      (#+ccl
       (:file "lisp-exe-ccl") 
       (:file "lisp-exe") 
       (:file "package")
       (:file "with-response-file")
       (:file "complete-test-run")
       (:file "submit-results")
       (:file "persistence")
       (:file "as-singleton-agent")
       (:file "generate-id")
       (:file "project-lister")
       (:file "agent")
       (:file "api-compatible-p")))))
