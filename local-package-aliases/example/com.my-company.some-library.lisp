(defpackage com.my-company.some-library
  (:use cl)
  (:export :func))

(in-package :com.my-company.some-library)

(defun func () "hello")
