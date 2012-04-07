(defpackage #:rt-api-impl
  (:use :cl :rt-api))

(in-package #:rt-api-impl)

(defun q (package-designator symbol-name)
  (intern (string symbol-name) package-designator))

(defun clean (&optional (rt-package :rtest))  
  (when (find-package rt-package)
    (funcall (q rt-package :rem-all-tests))
    (set (q rt-package '*expected-failures*) nil)))

(defun failed-tests (&optional (rt-package :rtest))
  (funcall (q rt-package 'pending-tests)))

(defun known-to-fail (&optional (rt-package :rtest))
  (symbol-value (q rt-package '*expected-failures*)))

(defun do-tests (&key compiled-p (rt-package :rtest))
  (progv (list (q rt-package '*compile-tests*))
         (list compiled-p)
    (funcall (q rt-package 'do-tests))))
