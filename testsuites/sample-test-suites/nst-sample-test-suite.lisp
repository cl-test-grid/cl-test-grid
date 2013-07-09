(defpackage #:nst-sample-test-suite
    (:use #:cl))

(in-package #:nst-sample-test-suite)

(nst:def-fixtures simple-constants
    (:documentation "Prepare some simple constants.")
  (+one+ 1)
  (+two+ 2)
  (+constants+ (list +one+ +two+)))

(nst:def-test-group simple-tests (simple-constants)
  (nst:def-test one-is-one (:eql 1) +one+)
  (nst:def-test two-is-two (:eql 2) +two+))

(nst:def-criterion-alias (:plusp)
    `(:apply plusp :true))

(nst:def-test-group sequence-tests (simple-constants)
  (nst:def-test sequence-equals (:seq (:eql 1) (:eql 2)) +constants+)
  (nst:def-test sequence-plusp  (:each (:plusp)) +constants+))

(nst:def-test-group mixed-bag (simple-constants)
  (nst:def-test error-test (:true) (error "Yo! Error!"))
  (nst:def-test addition (:eql 3) (+ +one+ +two+))
  (nst:def-test bad-addition (:eql 3) (+ +one+ +one+)))