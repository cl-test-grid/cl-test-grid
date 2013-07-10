(defpackage #:test-grid-tests
  (:use :cl)
  (:export #:run-tests))

(in-package #:test-grid-tests)

(defun test-rt-api ()
  (test-grid-testsuites::clean-rt)

  (asdf:clear-system :rt-sample-test-suite)
  (ql:quickload :rt-sample-test-suite)

  (let ((status (test-grid-testsuites::run-rt-test-suite)))
    (and (alexandria:set-equal (getf status :failed-tests)
                               '("test-1" "test-4")
                               :test #'string=)
         (alexandria:set-equal (getf status :known-to-fail)
                               '("test-3")
                               :test #'string=))))

(defun test-lift-api ()
  (ql:quickload :lift-sample-test-suite)
  (let ((status (test-grid-testsuites::run-lift-test-suite :sample-lift-suite)))
    (and (alexandria:set-equal (getf status :failed-tests)
                               '("sample-lift-suite.test-2"
                                 "sample-lift-suite.2-plus-2-is-3"
                                 "sample-lift-suite.expected-error-test"
                                 "sample-lift-suite.expected-failure-test"
                                 "sample-lift-suite.expected-problem-test"
                                 "sample-lift-suite.expected-error-but-fail-test")
                               :test #'string=)
	 (alexandria:set-equal (getf status :known-to-fail)
                               '("sample-lift-suite.expected-error-test"
                                 "sample-lift-suite.expected-failure-test"
                                 "sample-lift-suite.expected-problem-test"
                                 "sample-lift-suite.unexpected-no-failure-test"
                                 "sample-lift-suite.unexpected-no-error-test"
                                 "sample-lift-suite.expected-error-but-fail-test")
                               :test #'string=))))

(defun test-fiveam-api ()
  (ql:quickload :fiveam-sample-test-suite)
  (let ((status (test-grid-testsuites::run-fiveam-test-suite :sample-fiveam-suite)))
    (and (alexandria:set-equal (getf status :failed-tests)
                               '("fiveam-sample-test-suite.error-test"
                                 "fiveam-sample-test-suite.fail-test")
                               :test #'string=)
	 (null (getf status :known-to-fail)))))

(defun test-eos-api ()
  (ql:quickload :eos-sample-test-suite)
  (let ((status (test-grid-testsuites::run-eos-test-suites :sample-eos-suite)))
    (and (alexandria:set-equal (getf status :failed-tests)
                               '("eos-sample-test-suite.error-test"
                                 "eos-sample-test-suite.fail-test")
                               :test #'string=)
	 (null (getf status :known-to-fail)))))

(defun test-stefil-api ()
  (ql:quickload :stefil-sample-test-suite)
  (let ((status (test-grid-testsuites::run-stefil-test-suite (read-from-string "stefil-sample-test-suite::sample-stefil-suite"))))
    (and (alexandria:set-equal (getf status :failed-tests)
                               '("sample-stefil-suite.one-fail-test"
                                 "sample-stefil-suite.two-fails-test"
                                 "sample-stefil-suite.error-test"
                                 "sample-stefil-suite.all-fails-expected-test"
                                 "sample-stefil-suite.not-all-fails-expected-test")
                               :test #'string=)
         (null (getf status :known-to-fail)))))

(defun test-clunit-api ()
  (ql:quickload :clunit-sample-test-suite)
  (let ((status (test-grid-testsuites::run-clunit-test-suite (read-from-string "clunit-sample-test-suite::NumberSuite"))))
    (and (alexandria:set-equal (getf status :failed-tests)
                               '("clunit-sample-test-suite::test-float1"
                                 "clunit-sample-test-suite::test-int1")
                               :test #'string=)
         (null (getf status :known-to-fail)))))

(defun test-nst-api ()
  (ql:quickload :nst-sample-test-suite)
  (let ((status (test-grid-testsuites::run-nst-test-suites
                   :nst-sample-test-suite)))
    (and (alexandria:set-equal
            (getf status :failed-tests)
            '("nst-sample-test-suite::mixed-bag::error-test"
              "nst-sample-test-suite::mixed-bag::bad-addition")
            :test #'string=)
	 (null (getf status :known-to-fail)))))

(defun test-aggregated-status ()
  (and (eq :ok (test-grid-reporting::aggregated-status :ok))
       (eq :fail (test-grid-reporting::aggregated-status :fail))
       (eq :no-resource (test-grid-reporting::aggregated-status :no-resource))
       (eq :crash (test-grid-reporting::aggregated-status :crash))
       (eq :timeout (test-grid-reporting::aggregated-status :timeout))
       (eq :fail (test-grid-reporting::aggregated-status '(:failed-tests ("a") :known-to-fail ("b"))))
       (eq :unexpected-ok (test-grid-reporting::aggregated-status '(:failed-tests () :known-to-fail ("b"))))
       (eq :fail (test-grid-reporting::aggregated-status '(:failed-tests ("a") :known-to-fail ())))
       (eq :known-fail (test-grid-reporting::aggregated-status '(:failed-tests ("a") :known-to-fail ("a"))))
       (eq :ok (test-grid-reporting::aggregated-status '(:failed-tests () :known-to-fail ())))))

(defun run-tests ()
  (format t "~&~%assert result: ~S"
          (assert (and (test-rt-api)
                       (test-lift-api)
                       (test-fiveam-api)
                       (test-eos-api)
                       (test-stefil-api)
                       (test-clunit-api)
                       (test-nst-api)
                       (test-aggregated-status)))))
