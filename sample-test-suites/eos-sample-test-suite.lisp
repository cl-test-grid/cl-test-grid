(defpackage #:eos-sample-test-suite
  (:use #:cl :eos))

(in-package #:eos-sample-test-suite)

(def-suite :sample-eos-suite)
(in-suite :sample-eos-suite)

(test ok-test
  (is (= (+ 2 2) 4)))

(test fail-test
  (is (= (+ 2 2) 3)))

(test error-test
  (is (= (/ 1 0) 1)))

(test (skipped-test :depends-on error-test)
  (is (= (+ 2 2) 4)))