(defpackage #:hu.dwim.stefil-api-impl
  (:use #:cl :hu.dwim.stefil-api))

(in-package #:hu.dwim.stefil-api-impl)

(defun print-details (test-suite-result)
    (format t "~&~%-------------------------------------------------------------------------------------------------~%")
    (format t "The test result details (printed by cl-test-grid in addition to the standard Stefil output above)~%")
    (format t "~%(DESCRIBE RESULT):~%")
    (describe test-suite-result)
    (format t "~&~%Individual failures:~%")
    (map 'nil
         (lambda (descr) (format t "~A~%" descr))
         (hu.dwim.stefil::failure-descriptions-of test-suite-result)))

(defun run-test-suite (test-suite-name)
  (let* ((*debug-io* *standard-output*)
         (result (hu.dwim.stefil:funcall-test-with-feedback-message test-suite-name)))
    (print-details result)
    result))

(defun test-name (failure-description)
  (let ((hierarchical-names  (mapcar 
                              #'(lambda (context) 
                                  (hu.dwim.stefil::name-of
                                   (hu.dwim.stefil::test-of context)))
                              (hu.dwim.stefil::test-context-backtrace-of failure-description))))
    (format nil "~(~{~A~^.~}~)" (nreverse hierarchical-names))))

(defun failed-tests (test-suite-result)
  (remove-duplicates (map 'list
                          #'test-name
                          (hu.dwim.stefil::failure-descriptions-of test-suite-result))
                     :test #'string=))