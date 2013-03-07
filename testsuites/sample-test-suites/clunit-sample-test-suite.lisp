(defpackage #:clunit-sample-test-suite
  (:use #:cl :clunit))

(in-package #:clunit-sample-test-suite)

(defsuite NumberSuite ())
(defsuite FloatSuite (NumberSuite))
(defsuite IntegerSuite (NumberSuite))
(deftest test-int1 (IntegerSuite)
  (assert-true (= 2 -2))
  (assert-true (= 1 -1)))
(deftest test-int2 (IntegerSuite)
  (assert-true (= 2 2)))
(deftest test-float1 (FloatSuite)
  (assert-true (= 1.0 -1.0)))
