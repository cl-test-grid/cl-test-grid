(defpackage #:fiveam-api-impl
  (:use #:cl :fiveam-api))

(in-package #:fiveam-api-impl)

(defun run-test-suite (test-suite-name)
  (let ((result (fiveam:run test-suite-name)))
    (fiveam:explain (make-instance 'fiveam::detailed-text-explainer)
		    result)
    result))

(defun fmt-test-case (test-status)
  (let* ((test-case (fiveam::test-case test-status))
	 (name (fiveam::name test-case)))
    (format nil "~(~A.~A~)" 
	    (package-name (symbol-package name))
	    (symbol-name name))))

(defun failed-tests (test-suite-result)
  (let ((failed '()))
    (dolist (test-status test-suite-result)
      (when (typep test-status 
		   '(or fiveam::test-failure fiveam::unexpected-test-failure))
	(push (fmt-test-case test-status) failed)))
    failed))
