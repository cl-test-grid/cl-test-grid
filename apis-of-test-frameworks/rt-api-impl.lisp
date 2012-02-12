(defpackage #:rt-api-impl
  (:use :cl :rt-api))

(in-package #:rt-api-impl)

(defun clean ()  
  (rt:rem-all-tests)
  (setf rt::*expected-failures* nil))

(defun failed-tests ()
  (rt:pending-tests))

(defun known-to-fail ()
  rt::*expected-failures*)

(defun do-tests (&key compiled-p)
  (let ((rt::*compile-tests*))
    (rt:do-tests)))