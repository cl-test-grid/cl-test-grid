;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file contains common utilities usefull for most
;;;; of the child lisp processes started by agent.

(defpackage #:test-grid-proc-common (:use :cl))
(in-package #:test-grid-proc-common)

;;; utf-8 external format, if supported by the lisp implementation.
;;; copy/pasted from ASDF
#+(or abcl (and allegro ics) (and (or clisp cmu ecl mkcl) unicode)
      clozure lispworks (and sbcl sb-unicode) scl)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :asdf-unicode *features*))

(defparameter *utf-8-external-format*
  #+(and asdf-unicode (not clisp)) :utf-8
  #+(and asdf-unicode clisp) charset:utf-8
  #-asdf-unicode :default
  "Default :external-format argument to pass to CL:OPEN and also
CL:LOAD or CL:COMPILE-FILE to best process a UTF-8 encoded file.
On modern implementations, this will decode UTF-8 code points as CL characters.
On legacy implementations, it may fall back on some 8-bit encoding,
with non-ASCII code points being read as several CL characters;
hopefully, if done consistently, that won't affect program behavior too much.")
;;; --- end of the ASDF copy/paste ---

(defun cl-user::set-response (response-file value)
  "Save the resposne for the parent process."
  (with-open-file (out (ensure-directories-exist response-file)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type 'character
                       :external-format *utf-8-external-format*)
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
      (funcall thunk))))

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
;; The only change I did to the original ASDF code, to avoid copy/pasing large asdf:find-symbol* function:
;;  #.(if (find-symbol* "*VERBOSITY*" "SB-DEBUG" nil) :stream '(or count most-positive-fixnum))
   #.(if (ignore-errors (find-symbol "*VERBOSITY*" "SB-DEBUG")) :stream '(or count most-positive-fixnum))
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
problems (signals SERIOUS-CONDITION or enters debugger),
logs the problem description to *STANDARD-OUTPUT* and
invokes ON-PROBLEM-FUNC. The ON-PROBLEM-FUNC can transfer control
if it wants to prevent the lisp to hang in the debugger."
  (let ((*debugger-hook* #'(lambda (condition me-or-my-encapsulation)
                             (declare (ignore me-or-my-encapsulation))
                             (format t
                                     "INVOKE-DEBUGGER is called with condition of type ~A: ~A.~%"
                                     (type-of condition)
                                     condition)
                             (print-backtrace :stream *standard-output*)
                             (funcall on-problem-func condition))))
    (handler-bind
        ((serious-condition (lambda (condition)
                              (format t
                                      "~&Unhandled SERIOUS-CONDITION of type ~A is signaled: ~A~%"
                                      (type-of condition)
                                      condition)
                              (print-backtrace :stream *standard-output*)
                              (funcall on-problem-func condition))))
      (funcall body-func))))

(defun wrap-status-impl (body-fn)
  (catching-problems (lambda () (list :status (funcall body-fn)))
                     (lambda (condition)
                       (return-from wrap-status-impl
                         (list :status :fail
                               :fail-condition-type (let ((*package* (find-package :keyword)))
                                                      (prin1-to-string (type-of condition)))
                               :fail-condition-text (princ-to-string  condition))))))

(defmacro cl-user::wrap-status (&body body)
  "Executes BODY. If BODY finishes successfully,
returns list in the form (:STATUS <body return value>).
If BODY signals any SERIOUS-CONDITION or enters debugger,
the condition and its backtrace are logged to *STANDARD-OUTPUT*
and a list in the form
   (:STATUS :FAIL :FAIL-CONDITION-TYPE <condition type> :FAIL-CONDITION-TEXT <condition text>)
where <condition type> and <condition text> are strings.
The <condition type> includes package name, e.g. \"COMMON-LISP:SIMPLE-ERROR\"."
  `(wrap-status-impl (lambda () ,@body)))

(defun cl-user::saving-output (file body)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create
                          :element-type 'character
                          :external-format *utf-8-external-format*)
    (let* ((*standard-output* stream)
           (*error-output* stream))
      (funcall body))))
