;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

;;; When we run lisp code in an external lisp process,
;;; and want to return some value form that process,
;;; we use a temporary file where the external process
;;; stores the response. We then READ the response
;;; from that file.
;;;
;;; The WITH-RESPONSE-FILE macro generates temporary
;;; file name, executes BODY, and tries to read
;;; an object from the temporary file, and
;;; returns the object read.
;;;
;;; In case of problem reading the response (e.g.
;;; the temporary file is absent after the BODY
;;; execution), the NO-RESPONSE error condition
;;; is signalled.

(define-condition caused-condition (condition)
  ((cause :accessor cause
          :type (or null condition)
          :initarg :cause
          :initform nil)))

(define-condition no-response (caused-condition simple-error)
  ())

(defun with-response-file-impl (body-func)
  (let* ((response-file-name (format nil
                                     "response~A.lisp"
                                     (random #.(1- (expt 2 64)))))
         (response-file (workdir-file response-file-name)))
    (unwind-protect (progn (funcall body-func response-file)
                           (handler-case
                               (test-grid::safe-read-file response-file)
                             (serious-condition (condition)
                               (error 'no-response :cause condition
                                      :format-control "Error reading response file ~A. Caused by serious condition \"~A\" of type ~A."
                                      :format-arguments (list response-file condition (type-of condition))))))
      (when (probe-file response-file)
        (delete-file response-file)))))

(defmacro with-response-file ((file-var) &body body)
  `(with-response-file-impl (lambda (,file-var) ,@body)))
