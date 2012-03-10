(defpackage #:fiveam-sample-test-suite
  (:use #:cl :fiveam))

(in-package #:fiveam-sample-test-suite)

(def-suite :sample-fiveam-suite)
(in-suite :sample-fiveam-suite)

(test ok-test
  (is (= (+ 2 2) 4)))

(test fail-test
  (is (= (+ 2 2) 3)))

(test error-test
  (is (= (/ 1 0) 1)))

(test (skipped-test :depends-on error-test)
  (is (= (+ 2 2) 4)))