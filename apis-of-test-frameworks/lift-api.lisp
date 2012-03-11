(defpackage #:lift-api
  (:use :cl #:api-dsl)
  (:export #:run-test-suite
           #:failed-tests 
           #:known-to-fail))


(in-package #:lift-api)

(proclfun run-test-suite ((test-suite-name (or string symbol))) t
  "Runs the Lift test suite specified by TEST-SUITE-NAME and
returns an opaque result object. The result object may be passed
to the FAILED-TESTS function to retreive the list of failed tests.")

(proclfun failed-tests ((test-suite-result t)) list
  "List of failed test names. Test names are downcased strings.
The TEST-SUITE-RESULT parameter must be a result of the RUN-TEST-SUITE
function.")

(proclfun known-to-fail ((test-suite-name symbol)) list
  "List of test names which are known to fail in the specified Lift
test suite. Test names are downcased strings.")
