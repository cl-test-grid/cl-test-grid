(defpackage #:fiveam-api
  (:use :cl #:api-dsl)
  (:export #:run-test-suite
           #:failed-tests))

(in-package #:fiveam-api)

(proclfun run-test-suite ((test-suite-spec symbol)) t
  "Runs the FiveAM test suite specified by TEST-SUITE-SPEC and
returns an opaque result object. The result object may be passed
to the FAILED-TESTS function to retreive the list of failed tests.")

(proclfun failed-tests ((test-suite-result t)) list
  "List of failed test names. Test names are downcased strings.
The TEST-SUITE-RESULT parameter must be a result of the RUN-TEST-SUITE
function.")
