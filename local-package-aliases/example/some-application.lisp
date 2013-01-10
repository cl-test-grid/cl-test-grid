(defpackage some-application (:use cl))
(in-package :some-application)
(local-package-aliases:set :com.my-company.some-library :lib)

(defun main ()
  ($lib:func))
