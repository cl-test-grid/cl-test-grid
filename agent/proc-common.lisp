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

;;; backtrace printing, copy-pasted form ASDF git at 2013-02-19

(defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
  "Establish safe CL reader options around the evaluation of BODY"
  `(call-with-safe-io-syntax #'(lambda () (let ((*package* (find-package ,package))) ,@body))))

(defun call-with-safe-io-syntax (thunk &key (package :cl))
  (with-standard-io-syntax
    (let ((*package* (find-package package))
          (*read-default-float-format* 'double-float)
          (*print-readably* nil)
          (*read-eval* nil))
      (funcall thunk)))))

(defun raw-print-backtrace (&key (stream *debug-io*) count)
  "Print a backtrace, directly accessing the implementation"
  (declare (ignorable stream count))
  #+abcl
  (let ((*debug-io* stream)) (top-level::backtrace-command count))
  #+allegro
  (let ((*terminal-io* stream)
        (*standard-output* stream)
        (tpl:*zoom-print-circle* *print-circle*)
        (tpl:*zoom-print-level* *print-level*)
        (tpl:*zoom-print-length* *print-length*))
    (tpl:do-command "zoom"
      :from-read-eval-print-loop nil
      :count t
      :all t))
  #+clisp
  (system::print-backtrace :out stream :limit count)
  #+(or clozure mcl)
  (let ((*debug-io* stream))
    (ccl:print-call-history :count count :start-frame-number 1)
    (finish-output stream))
  #+(or cmu scl)
  (let ((debug:*debug-print-level* *print-level*)
        (debug:*debug-print-length* *print-length*))
    (debug:backtrace most-positive-fixnum stream))
  #+ecl
  (si::tpl-backtrace)
  #+lispworks
  (let ((dbg::*debugger-stack*
         (dbg::grab-stack nil :how-many (or count most-positive-fixnum)))
        (*debug-io* stream)
        (dbg:*debug-print-level* *print-level*)
        (dbg:*debug-print-length* *print-length*))
    (dbg:bug-backtrace nil))
  #+sbcl
  (sb-debug:backtrace
   #.(if (find-symbol* "*VERBOSITY*" "SB-DEBUG" nil) :stream '(or count most-positive-fixnum))
   stream))

(defun print-backtrace (&rest keys &key stream count)
  (declare (ignore stream count))
  (with-safe-io-syntax (:package :cl)
    (let ((*print-readably* nil)
          (*print-circle* t)
          (*print-miser-width* 75)
          (*print-length* nil)
          (*print-level* nil)
          (*print-pretty* t))
      (ignore-errors (apply 'raw-print-backtrace keys)))))

;;; end of backtrace printing

(defun catching-problems (body-func on-problem-func)
  "Runs BODY-FUNC and returns it's result. But if BODY-FUNC causes any
problems (signals SERIOS-CONDITION or enters debugger),
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
        (print-backtrace :stream *standard-output*)
        (funcall on-problem-func)))))

(defun saving-output (file body)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (let* ((*standard-output* stream)
           (*error-output* stream))
      (funcall body))))
