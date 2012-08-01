(defpackage #:eos-api-impl
  (:use #:cl :eos-api))

(in-package #:eos-api-impl)

(defun run-test-suite (test-suite-name)
  (format t "Running test suite ~A~%" test-suite-name)
  (let ((result (eos:run test-suite-name)))
    (eos:explain (make-instance 'eos::detailed-text-explainer)
		    result)
    result))

(defun run-test-suites (&rest test-suite-names)
  (mapcan #'run-test-suite test-suite-names))

(defun fmt-test-case (test-status)
  (let* ((test-case (eos::test-case test-status))
	 (name (eos::name test-case)))
    (format nil "~(~A.~A~)" 
	    (package-name (symbol-package name))
	    (symbol-name name))))

(defun failed-tests (test-suite-result)
  (let ((failed '()))
    (dolist (test-status test-suite-result)
      (when (typep test-status 
		   '(or eos::test-failure eos::unexpected-test-failure))
	(push (fmt-test-case test-status) failed)))
    failed))
