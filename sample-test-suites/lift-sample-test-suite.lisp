(defpackage #:lift-sample-test-suite
  (:use #:cl :lift))

(in-package #:lift-sample-test-suite)

(deftestsuite sample-lift-suite () ())  

(addtest (ensure-same (+ 1 1) 2))

(addtest (ensure-same (+ 2 2) 3))

(addtest (sample-lift-suite) ;; test-suite-name
  2-plus-2-is-3 ;; test name
  (ensure-same (+ 2 2) 3))

(addtest (sample-lift-suite :expected-error t)
  expected-error-test
  (ensure-same (/ 1 0) 3))

(addtest (sample-lift-suite :expected-failure t)
  expected-failure-test
  (ensure-same (+ 2 2) 3))

(addtest (sample-lift-suite :expected-problem t)
  expected-problem-test
  (ensure-same (+ 2 2) 3))

(addtest (sample-lift-suite :expected-failure t)
  unexpected-no-failure-test
  (ensure-same (+ 2 2) 4))

(addtest (sample-lift-suite :expected-error t)
  unexpected-no-error-test
  (ensure-same (+ 2 2) 4))

(addtest (sample-lift-suite :expected-error t)
  expected-error-but-fail-test
  (ensure-same (+ 2 2) 3))