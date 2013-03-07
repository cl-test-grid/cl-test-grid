(defpackage #:clunit-api-impl
  (:use #:cl :clunit-api))

(in-package #:clunit-api-impl)

(defun run-test-suite (test-suite-name)
  (clunit:run-suite test-suite-name))

(defun failed-tests (test-suite-result)
  (mapcar (lambda (rep)
            (let ((*package* (find-package '#:keyword)))
              (format nil "~(~S~)" (slot-value rep 'clunit::test-name))))
          (remove-if (lambda (rep)
                       (slot-value rep 'clunit::passed-p))
                     (slot-value test-suite-result 'clunit::test-reports))))