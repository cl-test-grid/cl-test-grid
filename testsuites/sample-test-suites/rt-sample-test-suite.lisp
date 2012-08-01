(defpackage #:rt-sample-test-suite
  (:use #:cl))

(in-package #:rt-sample-test-suite)

(rt:deftest test-1
    1 2)

(rt:deftest test-2
    5 5)

(rt:deftest test-3
    1 1)

(push 'test-3 rt::*expected-failures*)

(rt:deftest test-4
    (error "some error"))

