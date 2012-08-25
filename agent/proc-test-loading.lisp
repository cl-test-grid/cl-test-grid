;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; This file is loaded into a child lisp process to test
;;;; whether particular ASDF system can be quickloaded on
;;;; this lisp implementation.

(in-package :cl-user)

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

(defun test-loading (system-name)
  (catching-problems (lambda ()
                       (ql:quickload system-name))
                     (lambda ()
                       (return-from test-loading :fail)))
  :ok)

(defun saving-output (file body)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (let* ((*standard-output* stream)
           (*error-output* stream))
      (funcall body))))

(defun test-loading-main (log-file system-name private-quicklisp-dir asdf-output-root-dir)
  (let ((lib-dir (merge-pathnames (make-pathname :directory '(:relative "dists" "quicklisp" "software"))
                                  private-quicklisp-dir))
        (libs-output-dir (merge-pathnames (make-pathname :directory '(:relative "private-quicklisp"))
                                          asdf-output-root-dir)))

    (add-asdf-output-translation lib-dir libs-output-dir))

  (saving-output log-file
                 (lambda ()
                   (format t "  *features*:        ~(~A~)~%~%" (sort (copy-list *features*) #'string<))
                   (test-loading system-name))))


