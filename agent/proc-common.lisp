;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file contains common utilities usefull for most
;;;; of the child lisp processes started by agent.

(in-package #:cl-user)

(defun set-response (response-file value)
  "Save the resposne for the parent process."
  (with-open-file (out response-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (pprint value out)))

(defun catching-problems (body-func on-problem-func)
  "Runs BODY-FUNC and returns it's result. But if BODY-FUNC causes any
problems (signals SERIOS-CONDITION or enter debugger),
logs the problem description to *STANDARD-OUTPUT* and
invokes ON-PROBLEM-FUNC."
  (let ((*debugger-hook* #'(lambda (condition me-or-my-encapsulation)
                             (declare (ignore me-or-my-encapsulation))
                             (format t
                                     "INVOKE-DEBUGGER is called with condition of type ~A: ~A.~%"
                                     (type-of condition)
                                     condition)
                             (funcall on-problem-func))))
    ;; Even despite we prevent entering the interactive debugger,
    ;; we capture all the SERIOUS-CONDITIONS signalled by BODY-FUNC,
    ;; because we don't want our caller to see such signals.
    (handler-case
        (funcall body-func)
      (serious-condition (condition)
        (format t
                "~&Unhandled SERIOUS-CONDITION of type ~A is signaled: ~A~%"
                (type-of condition)
                condition)
        (funcall on-problem-func)))))

(defun saving-output (file body)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (let* ((*standard-output* stream)
           (*error-output* stream))
      (funcall body))))

