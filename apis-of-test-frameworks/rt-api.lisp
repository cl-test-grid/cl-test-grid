(defpackage #:rt-api
  (:use :cl #:api-dsl)
  (:export #:clean 
           #:do-tests
           #:failed-tests 
           #:known-to-fail))


(in-package #:rt-api)

(proclfun clean () null
  "Helper function to assist running test suites created using the RT 
test framework. The problem is that RT uses global storage for all
the tests; in result if we previously loaded any test system,
after loading another test system the global RT test suite
contains the tests of _both_ libraries.")

(proclfun do-tests (&key (:compiled-p t)) boolean
  "Calls (let ((rt::*compile-tests* compiled-p)) (rt:do-tests)).")

(proclfun failed-tests () list
  "List of failed test names. Test names are symbols.")

(proclfun known-to-fail () list
  "List of test names which are known to fail. Test names are symbols.")
