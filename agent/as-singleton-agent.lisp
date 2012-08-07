;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; Preventing more than one agent to run in parallel.
;;;; This is important, becuase the agents will
;;;; interfere via working directory file system,
;;;; at least via persistence.lisp.
;;;;
;;;; Implemented by opening a socket on a constant port.
;;;; If it is successful - we are the only running agent,
;;;; otherwise another agent instance is running.

(in-package #:test-grid-agent)

(define-condition another-agent-is-running (simple-error) ())

(defun execute-as-singleton (agent body-func)
  (handler-case
      (let ((s (usocket:socket-listen "localhost" (singleton-lock-port agent))))
        (unwind-protect (funcall body-func)
          (usocket:socket-close s)))
    (usocket:address-in-use-error ()
      (error 'another-agent-is-running
             :format-control "Another agent seems to be already running - our \"lock\" TCP port ~A is already in use."
             :format-arguments (list (singleton-lock-port agent))))))

(defmacro as-singleton ((agent) &body body)
  `(execute-as-singleton ,agent (alexandria:named-lambda as-singleton-agent-body ()
                                  ,@body)))