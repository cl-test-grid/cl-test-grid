;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

;;; When we run lisp code in an external lisp process,
;;; and want to return some value form that process.
;;; We use a temporary file where the external process
;;; stores the response and we READ that response
;;; from the file.
(defun with-response-file-impl (body-func)
  (let* ((response-file-name (format nil
                                     "response~A.lisp"
                                     (random #.(1- (expt 2 64)))))
         (response-file (workdir-file response-file-name)))
    (unwind-protect (progn (funcall body-func response-file)
                           (test-grid::safe-read-file response-file))
      (when (probe-file response-file)
        (delete-file response-file)))))

(defmacro with-response-file ((file-var) &body body)
  `(with-response-file-impl (lambda (,file-var) ,@body)))
