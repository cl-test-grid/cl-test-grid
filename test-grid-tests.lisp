(defpackage #:test-grid-tests
  (:use :cl))

(in-package #:test-grid-tests)

(defun test-rt-api ()
  (test-grid::require-impl '#:rt-api)
  (rt-api:clean)

  (asdf:clear-system :rt-sample-test-suite)
  (asdf:operate 'asdf:load-op :rt-sample-test-suite)

  (let ((status (test-grid::run-rt-test-suite)))
    (and (test-grid::set= (getf status :failed-tests)
                          '("test-1" "test-4")
                          :test #'string=)
         (test-grid::set= (getf status :known-to-fail)
                          '("test-3")
                          :test #'string=))))

(defun test-lift-api ()
  (test-grid::require-impl '#:lift-api)
  (asdf:operate 'asdf:load-op :lift-sample-test-suite)
  (let ((status (test-grid::run-lift-test-suite :sample-lift-suite)))
    (and (test-grid::set= (getf status :failed-tests)
			  '("sample-lift-suite.test-2"
			    "sample-lift-suite.2-plus-2-is-3"
			    "sample-lift-suite.expected-error-test"
			    "sample-lift-suite.expected-failure-test"
			    "sample-lift-suite.expected-problem-test"
			    "sample-lift-suite.expected-error-but-fail-test")
			  :test #'string=)
	 (test-grid::set= (getf status :known-to-fail)
			  '("sample-lift-suite.expected-error-test"
			    "sample-lift-suite.expected-failure-test"
			    "sample-lift-suite.expected-problem-test"
			    "sample-lift-suite.unexpected-no-failure-test"
			    "sample-lift-suite.unexpected-no-error-test"
			    "sample-lift-suite.expected-error-but-fail-test")
			  :test #'string=))))

(defun test-aggregated-status ()
  (and (eq :ok (test-grid-reporting::aggregated-status :ok))
       (eq :fail (test-grid-reporting::aggregated-status :fail))
       (eq :no-resource (test-grid-reporting::aggregated-status :no-resource))
       (eq :fail (test-grid-reporting::aggregated-status '(:failed-tests ("a") :known-to-fail ("b"))))
       (eq :unexpected-ok (test-grid-reporting::aggregated-status '(:failed-tests () :known-to-fail ("b"))))
       (eq :fail (test-grid-reporting::aggregated-status '(:failed-tests ("a") :known-to-fail ())))
       (eq :known-fail (test-grid-reporting::aggregated-status '(:failed-tests ("a") :known-to-fail ("a"))))
       (eq :ok (test-grid-reporting::aggregated-status '(:failed-tests () :known-to-fail ())))))

; to run the tests: 
(assert (and (test-rt-api)
	     (test-lift-api)
	     (test-aggregated-status)))
