;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.
;;;
;;; This file is supposed to be LOADed by user: 
;;;
;;;        (load "run-agent2.lisp")
;;;

(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))

  (pushnew this-file-dir asdf:*central-registry* :test #'equal))

(asdf:operate 'asdf:load-op :test-grid-agent)
(test-grid-agent::main)
