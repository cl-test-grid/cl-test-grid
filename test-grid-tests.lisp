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

(defun test-aggregated-status ()
  (and (eq :ok (test-grid::aggregated-status :ok))
       (eq :fail (test-grid::aggregated-status :fail))
       (eq :no-resource (test-grid::aggregated-status :no-resource))
       (eq :fail (test-grid::aggregated-status '(:failed-tests ("a") :known-to-fail ("b"))))
       (eq :unexpected-ok (test-grid::aggregated-status '(:failed-tests () :known-to-fail ("b"))))
       (eq :fail (test-grid::aggregated-status '(:failed-tests ("a") :known-to-fail ())))
       (eq :known-fail (test-grid::aggregated-status '(:failed-tests ("a") :known-to-fail ("a"))))
       (eq :ok (test-grid::aggregated-status '(:failed-tests () :known-to-fail ())))))

; to run the tests: 
(and (test-rt-api)
     (test-aggregated-status))
; expected to return T
