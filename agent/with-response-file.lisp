;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

(defmacro with-response-file ((file-var) &body body)
  `(with-response-file-impl (lambda (,file-var) ,@body)))

(defun update-testing-quicklisp (lisp-exe)
  (log:info "Ensuring the quicklisp used to download the libraries being tested is updated to the recent version...")
  (let ((quicklisp-version
         (with-response-file (response-file)
           (run-lisp-process lisp-exe
                             `(progn
                                (load ,(truename (src-file "proc-update-quicklisp.lisp")))
                                (with-open-file (cl-user::out ,response-file
                                                              :direction :output
                                                              :if-exists :supersede
                                                              :if-does-not-exist :create)
                                  (pprint (cl-user::do-quicklisp-update) cl-user::out)))))))
    (log:info "Quicklisp update process finished, current quicklisp version: ~A." quicklisp-version)
    quicklisp-version))

