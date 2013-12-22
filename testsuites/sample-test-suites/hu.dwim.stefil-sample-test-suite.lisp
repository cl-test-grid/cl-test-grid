(defpackage #:hu.dwim.stefil-sample-test-suite
  (:use #:cl :hu.dwim.stefil))

(in-package #:hu.dwim.stefil-sample-test-suite)

(defsuite sample-stefil-suite)
(in-suite sample-stefil-suite)

(deftest ok-test ()
  (is (= (+ 2 2) 4)))

(deftest one-fail-test ()
  (is (= (+ 2 2) 3)))

(deftest two-fails-test ()
  (is (= (+ 2 2) 3))
  (is (= (+ 1 1) 2))
  (is (= (+ 3 3) 7)))

(deftest error-test ()
  (is (= 7 7))
  (is (= (/ 1 0) 1)))

(deftest all-fails-expected-test ()
  (with-expected-failures
    (is (= (+ 2 2) 3))
    (is (= (+ 1 1) 2))
    (is (= (+ 3 3) 7))))

(deftest not-all-fails-expected-test ()
  (is (= (+ 2 2) 3))
  (with-expected-failures
    (is (= (+ 1 1) 2))
    (is (= (+ 3 3) 7))))
