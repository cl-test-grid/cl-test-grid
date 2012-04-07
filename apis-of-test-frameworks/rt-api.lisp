(defpackage #:rt-api
  (:use :cl #:api-dsl)
  (:export #:clean 
           #:do-tests
           #:failed-tests 
           #:known-to-fail))


(in-package #:rt-api)

;; string deisgnator, according to CLHS
;; http://www.lispworks.com/documentation/lw60/CLHS/Body/26_glo_s.htm#string_designator
(deftype string-designator () '(or character symbol string))

;; package deisgnator, according to CLHS
;; http://www.lispworks.com/documentation/lw60/CLHS/Body/26_glo_p.htm#package_designator
(deftype package-designator () '(or package string-designator))

;; RT is often copy-pasted into another package and included in that form
;; into various projects sources (e.g. SBCL has sb-rt; named-readtables
;; test suite also includes rt.lisp).
;;
;; For the purpose of using RT code contained in any package,
;; all the RT-API functions accept as a parameter the package
;; designator of the package containing the desired copy of RT.
(proclfun clean (&optional ((rt-package package-designator) :rtest)) null
  "Helper function to assist running test suites created using the RT 
test framework. The problem is that RT uses global storage for all
the tests; in result if we previously loaded any test system,
after loading another test system the global RT test suite
contains the tests of _both_ libraries.")

(proclfun do-tests (&key (:compiled-p t) ((:rt-package package-designator) :rtest)) boolean
  "Calls (let ((rt::*compile-tests* compiled-p)) (rt:do-tests)).")

(proclfun failed-tests (&optional ((rt-package package-designator) :rtest)) list
  "List of failed test names. Test names are symbols.")

(proclfun known-to-fail (&optional ((rt-package package-designator) :rtest)) list
  "List of test names which are known to fail. Test names are symbols.")
